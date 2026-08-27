(in-package #:lispbsd)

;;;; -- Tests --

(defparameter *test-failures* nil
  "Accumulated failure descriptions from the current test run.")

(defparameter *test-count* 0
  "Number of assertions evaluated in the current test run.")


(defmacro test-assert (form &optional description)
  "Record a failure when FORM is false."
  `(progn
     (incf *test-count*)
     (unless ,form
       (push (or ,description ',form) *test-failures*))))


;; Geometry tests bind *system-font* to the fixed 8x8 font so their
;; pixel coordinates stay stable regardless of the default face.


(-> test-object-id () t)
(defun test-object-id ()
  "Identifiers are 32-character hexadecimal strings."
  (let ((id (make-object-id)))
    (test-assert (typep id 'object-id))
    (test-assert (= 32 (length id)))))


(-> test-hosted-world () t)
(defun test-hosted-world ()
  "A hosted world starts operational with machine resources."
  (let ((world (make-hosted-world :name "test-world")))
    (unwind-protect
         (progn
           (test-assert (eq (world-phase world) ':operational))
           (test-assert (string= (world-name world) "test-world"))
           (test-assert (typep (world-runtime world) 'sbcl-runtime))
           (test-assert (world-generation world))
           (test-assert (find ':world-started (world-events world)
                              :key #'event-kind))
           (test-assert (plusp (machine-processors (world-machine world))))
           (test-assert (find "lo" (world-resources world)
                              :key #'resource-name
                              :test #'string=)))
      (world-shutdown world)
      (test-assert (eq (world-phase world) ':stopped)))))


(-> test-authority () t)
(defun test-authority ()
  "Authority grants, denials, revocation, and delegation behave."
  (let* ((world (make-hosted-world :name "authority-world"))
         (set (world-authority-root world))
         (subject (make-object-id))
         (target world))
    (unwind-protect
         (progn
           (test-assert (authorized-p set world ':inspect target))
           (test-assert (not (authorized-p set subject ':inspect target)))
           (let ((grant (grant-authority set subject ':inspect target
                                         :delegable-p t)))
             (test-assert (authorized-p set subject ':inspect target))
             (test-assert (not (authorized-p set subject ':modify target)))
             (let ((delegate (make-object-id)))
               (with-delegated-authority (set grant delegate)
                 (test-assert (authorized-p set delegate ':inspect target)))
               (test-assert (not (authorized-p set delegate ':inspect target))))
             (revoke-authority grant)
             (test-assert (not (authorized-p set subject ':inspect target))))
           (handler-case
               (progn
                 (check-authority set subject ':inspect target)
                 (test-assert nil "check-authority should have denied"))
             (authority-denied ()
               (test-assert t))))
      (world-shutdown world))))


(-> test-generation-roundtrip () t)
(defun test-generation-roundtrip ()
  "Generation manifests round-trip through the filesystem."
  (let* ((runtime (make-sbcl-runtime))
         (generation (make-generation (make-object-id) runtime
                                      :source-revision "test"))
         (path (merge-pathnames
                (format nil "lispbsd-generation-~A.lisp"
                        (generation-id generation))
                (uiop:temporary-directory))))
    (unwind-protect
         (let ((read-back (progn
                            (generation-write generation path)
                            (generation-read path))))
           (test-assert (string= (generation-id generation)
                                 (generation-id read-back)))
           (test-assert (string= (generation-world-id generation)
                                 (generation-world-id read-back)))
           (test-assert (string= (generation-runtime-name read-back) "SBCL"))
           (test-assert (string= (generation-source-revision read-back)
                                 "test")))
      (ignore-errors (delete-file path)))))


(-> test-event-queries () t)
(defun test-event-queries ()
  "Event logs answer kind, involvement, and time-range queries."
  (let ((log (make-event-log))
        (subject (make-object-id)))
    (emit-event log ':first :source 'alpha)
    (emit-event log ':second :payload (list :subject subject))
    (emit-event log ':first :source 'beta)
    (test-assert (= 2 (length (events-of-kind log ':first))))
    (test-assert (= 1 (length (events-of-kind log ':second))))
    (test-assert (null (events-of-kind log ':third)))
    (test-assert (= 1 (length (events-involving log 'alpha)))
                 "source involvement is found")
    (let ((involving (events-involving log subject)))
      (test-assert (= 1 (length involving))
                   "payload involvement is found")
      (test-assert (eq ':second (event-kind (first involving)))))
    (test-assert (null (events-involving log 'gamma)))
    (test-assert (= 3 (length (events-since log 0))))
    (test-assert (null (events-since log (+ (current-timestamp) 10))))))


(-> test-activity-mailbox () t)
(defun test-activity-mailbox ()
  "Activities receive sent objects and stop cleanly."
  (let ((world (make-hosted-world :name "activity-world")))
    (unwind-protect
         (let* ((received nil)
                (activity (make-activity
                           "mailbox"
                           (lambda (self)
                             (setf received (receive self :timeout 2)))
                           :world world)))
           (start-activity activity)
           (send activity 'ping)
           (activity--join activity :timeout 2)
           (test-assert (eq received 'ping))
           (test-assert (eq (activity-state activity) ':stopped)))
      (world-shutdown world))))


(-> test-activity-failure () t)
(defun test-activity-failure ()
  "An unhandled condition fails the activity and records an event."
  (let ((world (make-hosted-world :name "failure-world")))
    (unwind-protect
         (let ((activity (make-activity
                          "fail"
                          (lambda (self)
                            (declare (ignore self))
                            (error "boom"))
                          :world world)))
           (start-activity activity)
           (activity--join activity :timeout 2)
           (test-assert (eq (activity-state activity) ':failed))
           (test-assert (activity-condition activity))
           (test-assert (find ':activity-failed (world-events world)
                              :key #'event-kind)))
      (world-shutdown world))))


(-> test-exec () t)
(defun test-exec ()
  "The Exec evaluates forms and records errors."
  (let ((world (make-hosted-world :name "exec-world")))
    (unwind-protect
         (let* ((exec (make-exec :world world :package (find-package '#:lispbsd)))
                (ok (exec-evaluate exec "(+ 1 2)"))
                (bad (exec-evaluate exec "(error \"nope\")"))
                (unreadable (exec-evaluate exec "(unbalanced")))
           (test-assert (equal (exec-entry-values ok) '(3)))
           (test-assert (null (exec-entry-condition ok)))
           (test-assert (exec-entry-condition bad))
           (test-assert (equal (exec-entry-form bad) '(error "nope")))
           (test-assert (exec-entry-condition unreadable))
           (test-assert (equal (exec-entry-form unreadable) "(unbalanced"))
           (test-assert (= 3 (length (exec-history exec)))))
      (world-shutdown world))))


(-> test-inspector-and-definitions () t)
(defun test-inspector-and-definitions ()
  "The inspector and definition browser describe live objects."
  (let ((world (make-hosted-world :name "inspect-world")))
    (unwind-protect
         (let ((parts (inspect-parts world))
               (definition (find-definition 'make-object-id :kind ':function)))
           (test-assert (eq (cdr (assoc :class parts)) 'world))
           (test-assert (assoc 'world-id parts))
           (test-assert definition)
           (test-assert (eq (definition-kind definition) ':function))
           (test-assert (find-if (lambda (definition)
                                   (eq (definition-name definition)
                                       'make-object-id))
                                 (list-definitions
                                  :package (find-package '#:lispbsd)
                                  :kind ':function))))
      (world-shutdown world))))


(-> test-runtime-identity () t)
(defun test-runtime-identity ()
  "The SBCL adapter reports a coherent identity."
  (let* ((runtime (make-sbcl-runtime))
         (identity (runtime-identity runtime))
         (function (runtime-compile-definition runtime '(lambda (x) (1+ x)))))
    (test-assert (string= (getf identity :name) "SBCL"))
    (test-assert (getf identity :version))
    (test-assert (= 2 (funcall function 1)))
    (test-assert (getf (runtime-gc-information runtime) :dynamic-space-size))))


(-> test-bitmap () t)
(defun test-bitmap ()
  "1-bit drawing, clipping, and raster operations preserve pixels."
  (let ((bitmap (make-bitmap 8 4)))
    (test-assert (= 8 (bitmap-width bitmap)))
    (test-assert (= 4 (bitmap-height bitmap)))
    (test-assert (equal '("........" "........" "........" "........")
                        (bitmap-ascii bitmap)))
    (setf (bitmap-pixel bitmap 1 1) 1)
    (bitmap-fill bitmap :x 4 :y 0 :width 3 :height 2 :bit 1)
    (bitmap-draw-line bitmap :x0 0 :y0 3 :x1 7 :y1 3)
    (test-assert (equal '("....###." ".#..###." "........" "########")
                        (bitmap-ascii bitmap))))
  (let ((box (make-bitmap 5 5)))
    (bitmap-draw-rectangle box :x 0 :y 0 :width 5 :height 5)
    (test-assert (equal '("#####" "#...#" "#...#" "#...#" "#####")
                        (bitmap-ascii box))))
  (let ((source (make-bitmap 3 3 :initial-element 1))
        (destination (make-bitmap 6 6)))
    (bitblt source destination :dx 2 :dy 1)
    (test-assert (equal '("###" "###" "###")
                        (bitmap-ascii destination :x 2 :y 1 :width 3 :height 3)))
    (bitblt source destination :dx 2 :dy 1 :operation ':xor)
    (test-assert (equal '("..." "..." "...")
                        (bitmap-ascii destination :x 2 :y 1 :width 3 :height 3)))
    (bitblt source destination :dx -1 :dy -1)
    (test-assert (= 1 (bitmap-pixel destination 0 0)))
    (test-assert (= 0 (bitmap-pixel destination 5 5))))
  (dolist (case '((:src 1 0 1)
                  (:ior 1 0 1)
                  (:ior 0 1 1)
                  (:xor 1 1 0)
                  (:and 1 1 1)
                  (:and 1 0 0)
                  (:not-src 1 0 0)
                  (:clear 1 1 0)
                  (:set 0 0 1)
                  (:not 0 1 0)))
    (destructuring-bind (operation source destination expected) case
      (test-assert (= expected (raster-op operation source destination))
                   (format nil "raster-op ~S ~A ~A" operation source destination))))
  (handler-case
      (progn
        (make-bitmap 0 4)
        (test-assert nil "make-bitmap should have rejected a zero width"))
    (invalid-bitmap-size ()
      (test-assert t)))
  (handler-case
      (progn
        (bitblt (make-bitmap 1 1) (make-bitmap 1 1) :operation ':nope)
        (test-assert nil "bitblt should have rejected an unknown operation"))
    (unknown-raster-operation ()
      (test-assert t))))


(-> test-font () t)
(defun test-font ()
  "The fixed font renders printable ASCII onto 1-bit bitmaps."
  (let ((font *fixed-font*))
    (test-assert (string= (bitmap-font-name font) "fixed-8x8"))
    (test-assert (= 8 (bitmap-font-width font)))
    (test-assert (= 8 (bitmap-font-height font)))
    (loop for code from 32 to 126
          for character = (code-char code)
          do (test-assert (not (eq (font-glyph font character)
                                   (bitmap-font-missing font)))
                          (format nil "missing glyph for ~S" character)))
    (test-assert (eq (font-glyph font (code-char 955))
                     (bitmap-font-missing font)))
    (test-assert (= 8 (font-height font)))
    (test-assert (= 7 (font-ascent font)))
    (test-assert (= 8 (glyph-advance (font-glyph font #\a))))
    (test-assert (= 0 (font-kerning font #\a #\b)))
    (test-assert (= 32 (font-text-width font "lisp")))
    (let ((expected (mapcar (lambda (row)
                              (substitute #\. #\Space row))
                            (rest (assoc #\! *fixed-font-art*))))
          (bitmap (make-bitmap 8 8)))
      (bitmap-draw-text bitmap font "!" :x 0 :y 0)
      (test-assert (equal expected (bitmap-ascii bitmap))))
    (let ((expected (mapcar (lambda (row)
                              (map 'string
                                   (lambda (character)
                                     (if (char= character #\#) #\. #\#))
                                   row))
                            (rest (assoc #\! *fixed-font-art*))))
          (bitmap (make-bitmap 8 8 :initial-element 1)))
      (bitmap-draw-text bitmap font "!" :x 0 :y 0 :bit 0)
      (test-assert (equal expected (bitmap-ascii bitmap))))
    (let ((bitmap (make-bitmap 8 8)))
      (bitmap-draw-text bitmap font "ab" :x -4 :y -2)
      (test-assert (= 1 (bitmap-pixel bitmap 0 0)))
      (test-assert (= 1 (bitmap-pixel bitmap 5 0))))
    (let ((missing (make-missing-glyph 8 8)))
      (test-assert (= 1 (bitmap-pixel missing 0 0)))
      (test-assert (= 1 (bitmap-pixel missing 4 4)))
      (test-assert (= 0 (bitmap-pixel missing 1 1))))))


(-> test-truetype-font () t)
(defun test-truetype-font ()
  "TrueType fonts load, measure, kern, and rasterize onto 1-bit bitmaps."
  (labels ((ink-somewhere-p (bitmap)
             (loop for y from 0 below (bitmap-height bitmap)
                     thereis (loop for x from 0 below (bitmap-width bitmap)
                                     thereis (plusp (bitmap-pixel bitmap x y))))))
    (let ((font (make-truetype-font
                 :path (asdf:system-relative-pathname
                        '#:lispbsd "assets/fonts/IBMPlexMono-Regular.ttf")
                 :size 14)))
      (test-assert (typep font 'truetype-font))
      (test-assert (plusp (font-height font)))
      (test-assert (< (font-ascent font) (font-height font)))
      (let ((m-glyph (font-glyph font #\M)))
        (test-assert (plusp (glyph-advance m-glyph)))
        (test-assert (ink-somewhere-p (glyph-bitmap m-glyph))
                     "the M glyph rasterizes some ink"))
      (test-assert (eq (font-glyph font #\M) (font-glyph font #\M))
                   "rendered glyphs are cached")
      (test-assert (= (glyph-advance (font-glyph font #\i))
                      (glyph-advance (font-glyph font #\M)))
                   "the face is monospaced")
      (test-assert (= (font-text-width font "MM")
                      (* 2 (glyph-advance (font-glyph font #\M)))))
      (test-assert (integerp (font-kerning font #\A #\V)))
      (test-assert (not (ink-somewhere-p (glyph-bitmap (font-glyph font #\Space))))
                   "the space glyph is empty")
      (let ((bitmap (make-bitmap 80 20)))
        (bitmap-draw-text bitmap font "Lisp" :x 1 :y 1)
        (test-assert (ink-somewhere-p bitmap)
                     "drawing TrueType text produces ink")))))


(-> test-window () t)
(defun test-window ()
  "Windows stack, take focus, hit test, and compose onto the screen."
  (let* ((*system-font* *fixed-font*)
         (desktop (make-desktop :width 32 :height 32))
         (bottom  (make-window :title "b" :x 2 :y 2 :width 20 :height 16))
         (top     (make-window :title "t" :x 8 :y 6 :width 20 :height 16)))
    (desktop-attach-window desktop bottom)
    (desktop-attach-window desktop top)
    (test-assert (equal (list bottom top) (desktop-windows desktop)))
    (test-assert (eq (desktop-focus desktop) top))
    (test-assert (eq top (desktop-window-at desktop 10 8)))
    (test-assert (eq bottom (desktop-window-at desktop 3 3)))
    (test-assert (null (desktop-window-at desktop 0 31)))
    (window-raise bottom)
    (test-assert (eq bottom (desktop-window-at desktop 10 8)))
    (window-lower bottom)
    (test-assert (equal (list bottom top) (desktop-windows desktop)))
    (window-hide top)
    (test-assert (eq (desktop-focus desktop) bottom))
    (test-assert (eq bottom (desktop-window-at desktop 10 8)))
    (bitmap-fill (window-content-bitmap bottom) :bit 1)
    (let ((screen (desktop-compose desktop)))
      (test-assert (= 0 (bitmap-pixel screen 0 0)) "background is white")
      (test-assert (= 1 (bitmap-pixel screen 23 7)) "shadow dither ink")
      (test-assert (= 0 (bitmap-pixel screen 22 7)) "shadow dither paper")
      (test-assert (= 1 (bitmap-pixel screen 6 18)) "shadow below the window")
      (test-assert (= 1 (bitmap-pixel screen 2 2)) "window border corner")
      (test-assert (= 1 (bitmap-pixel screen 21 17)) "window border far corner")
      (test-assert (= 1 (bitmap-pixel screen 19 4)) "focused title bar is ink")
      (test-assert (= 1 (bitmap-pixel screen 5 13)) "title separator line")
      (test-assert (= 1 (bitmap-pixel screen 3 14)) "content pixel is ink"))
    (window-show top)
    (let ((screen (desktop-compose desktop)))
      (test-assert (= 1 (bitmap-pixel screen 8 6)) "top window border corner")
      (test-assert (= 0 (bitmap-pixel screen 25 8)) "unfocused title bar is paper"))
    (desktop-focus-window desktop top)
    (test-assert (eq (desktop-focus desktop) top))
    (window-move bottom :x 4)
    (test-assert (= 4 (window-x bottom)))
    (test-assert (= 2 (window-y bottom)))
    (desktop-detach-window desktop top)
    (test-assert (null (window-desktop top)))
    (test-assert (eq (desktop-focus desktop) bottom))
    (handler-case
        (progn
          (window-raise top)
          (test-assert nil "window-raise should require attachment"))
      (window-not-attached ()
        (test-assert t)))
    (handler-case
        (progn
          (desktop-attach-window desktop bottom)
          (test-assert nil "attach should reject an attached window"))
      (window-already-attached ()
        (test-assert t))))
  (handler-case
      (progn
        (make-window :width 10 :height 12)
        (test-assert nil "make-window should reject too-small geometry"))
    (invalid-window-geometry ()
      (test-assert t))))


(-> test-input () t)
(defun test-input ()
  "Pointer and key events route through focus, stacking, and grabs."
  (let* ((*system-font* *fixed-font*)
         (desktop  (make-desktop :width 32 :height 32))
         (received nil)
         (handler  (lambda (window event)
                     (push (list window event) received)))
         (bottom   (make-window :title "b" :x 2 :y 2 :width 20 :height 16
                                :event-handler handler))
         (top      (make-window :title "t" :x 8 :y 6 :width 20 :height 16
                                :event-handler handler)))
    (desktop-attach-window desktop bottom)
    (desktop-attach-window desktop top)
    (let ((press (make-pointer-event :x 5 :y 15 :action ':press :button ':left)))
      (test-assert (eq bottom (desktop-dispatch-event desktop press)))
      (test-assert (eq (desktop-focus desktop) bottom))
      (test-assert (eq (desktop-pointer-grab desktop) bottom))
      (test-assert (equal (list top bottom) (desktop-windows desktop)))
      (test-assert (eq (first (first received)) bottom)))
    (let ((move (make-pointer-event :x 25 :y 20 :action ':move)))
      (test-assert (eq bottom (desktop-dispatch-event desktop move))
                   "grab should win over the window under the pointer"))
    (let ((release (make-pointer-event :x 25 :y 20 :action ':release
                                       :button ':left)))
      (test-assert (eq bottom (desktop-dispatch-event desktop release)))
      (test-assert (null (desktop-pointer-grab desktop))))
    (let ((move (make-pointer-event :x 25 :y 20 :action ':move)))
      (test-assert (eq top (desktop-dispatch-event desktop move))
                   "after release, motion routes by position"))
    (let ((key (make-key-event :key ':a :character #\a)))
      (test-assert (eq bottom (desktop-dispatch-event desktop key))
                   "key events go to the focused window"))
    (test-assert (= 5 (length received)))
    (setf received nil)
    (let ((press (make-pointer-event :x 0 :y 31 :action ':press :button ':left)))
      (test-assert (null (desktop-dispatch-event desktop press)))
      (test-assert (null received))
      (test-assert (null (desktop-pointer-grab desktop))))
    (test-assert (eq ':border (window-region-at bottom 2 2)))
    (test-assert (eq ':title-bar (window-region-at bottom 5 5)))
    (test-assert (eq ':content (window-region-at bottom 5 15)))
    (test-assert (null (window-region-at bottom 0 0)))
    (multiple-value-bind (content-x content-y)
        (window-point->content bottom 5 15)
      (test-assert (= 2 content-x))
      (test-assert (= 1 content-y)))
    (multiple-value-bind (content-x content-y)
        (window-point->content bottom 5 5)
      (test-assert (null content-x))
      (test-assert (null content-y)))
    (desktop-dispatch-event desktop (make-pointer-event :x 3 :y 3
                                                        :action ':press
                                                        :button ':left))
    (test-assert (equal (list bottom 1 1) (desktop-window-drag desktop))
                 "a title bar press starts a drag")
    (desktop-dispatch-event desktop (make-pointer-event :x 10 :y 9
                                                        :action ':move))
    (test-assert (= 9 (window-x bottom)))
    (test-assert (= 8 (window-y bottom)))
    (desktop-dispatch-event desktop (make-pointer-event :x 10 :y 9
                                                        :action ':release
                                                        :button ':left))
    (test-assert (null (desktop-window-drag desktop)))
    (desktop-dispatch-event desktop (make-pointer-event :x 20 :y 20
                                                        :action ':move))
    (test-assert (= 9 (window-x bottom))
                 "motion after the release no longer drags")
    (desktop-dispatch-event desktop (make-pointer-event :x 12 :y 22
                                                        :action ':press
                                                        :button ':left))
    (test-assert (null (desktop-window-drag desktop))
                 "a content press does not start a drag")
    (desktop-dispatch-event desktop (make-pointer-event :x 12 :y 22
                                                        :action ':release
                                                        :button ':left)))
  (let* ((*system-font* *fixed-font*)
         (desktop (make-desktop :width 8 :height 8)))
    (test-assert (null (desktop-dispatch-event desktop
                                               (make-key-event :key ':a)))
                 "key events with no focused window go nowhere")))


(-> test-bitmap-io () t)
(defun test-bitmap-io ()
  "Bitmaps round-trip through PBM files and screenshots capture the screen."
  (let ((path (merge-pathnames (format nil "lispbsd-pbm-~A.pbm" (make-object-id))
                               (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (let ((bitmap (make-bitmap 10 3)))
             (bitmap-draw-rectangle bitmap :x 0 :y 0 :width 10 :height 3)
             (setf (bitmap-pixel bitmap 4 1) 1)
             (bitmap-write-pbm bitmap path)
             (let ((read-back (bitmap-read-pbm path)))
               (test-assert (= 10 (bitmap-width read-back)))
               (test-assert (= 3 (bitmap-height read-back)))
               (test-assert (equal (bitmap-ascii bitmap)
                                   (bitmap-ascii read-back)))))
           (let* ((*system-font* *fixed-font*)
                  (desktop (make-desktop :width 24 :height 24)))
             (desktop-attach-window desktop (make-window :title "s" :x 2 :y 2
                                                         :width 20 :height 16))
             (desktop-screenshot desktop path)
             (let ((screen (bitmap-read-pbm path)))
               (test-assert (= 24 (bitmap-width screen)))
               (test-assert (= 0 (bitmap-pixel screen 0 0))
                            "screenshot keeps the white background")
               (test-assert (= 1 (bitmap-pixel screen 22 8))
                            "screenshot keeps the drop shadow")
               (test-assert (= 1 (bitmap-pixel screen 2 2))
                            "screenshot keeps the window border")))
           (with-open-file (stream path :direction ':output
                                        :if-exists ':supersede)
             (write-string "not a bitmap" stream))
           (handler-case
               (progn
                 (bitmap-read-pbm path)
                 (test-assert nil "bitmap-read-pbm should reject garbage"))
             (invalid-bitmap-file ()
               (test-assert t))))
      (ignore-errors (delete-file path)))))


(-> test-exec-window () t)
(defun test-exec-window ()
  "Typing into a focused Exec window evaluates forms and renders results."
  (let* ((*system-font* *fixed-font*)
         (desktop (make-desktop :width 160 :height 120))
         (exec-window (make-exec-window :world nil
                                        :package (find-package '#:lispbsd)
                                        :title "Exec"
                                        :x 4 :y 4 :width 120 :height 80))
         (window (exec-window-window exec-window)))
    (desktop-attach-window desktop window)
    (test-assert (eq exec-window (window-application window)))
    (labels ((type-string (string)
               (loop for character across string
                     do (desktop-dispatch-event
                         desktop
                         (make-key-event :key ':character
                                         :character character))))

             (press-key (key)
               (desktop-dispatch-event desktop (make-key-event :key key))))
      (type-string "(+ 1 2)")
      (test-assert (string= "(+ 1 2)" (exec-window-input exec-window)))
      (press-key ':return)
      (test-assert (string= "" (exec-window-input exec-window)))
      (let ((history (exec-history (exec-window-exec exec-window))))
        (test-assert (= 1 (length history)))
        (test-assert (equal '(3) (exec-entry-values (first history)))))
      (let ((content (window-content-bitmap window))
            (expected (make-bitmap 8 8)))
        (bitmap-draw-text expected *fixed-font* "3" :x 0 :y 0)
        (test-assert (equal (bitmap-ascii expected)
                            (bitmap-ascii content :x 2 :y 10
                                          :width 8 :height 8))
                     "result line is drawn in the content bitmap"))
      (type-string "ab")
      (press-key ':backspace)
      (test-assert (string= "a" (exec-window-input exec-window)))
      (press-key ':backspace)
      (press-key ':backspace)
      (test-assert (string= "" (exec-window-input exec-window)))
      (type-string "(oops")
      (press-key ':return)
      (let ((history (exec-history (exec-window-exec exec-window))))
        (test-assert (= 2 (length history)))
        (test-assert (exec-entry-condition (second history))
                     "unreadable input is recorded, not signaled")))))


(-> test-inspector-window () t)
(defun test-inspector-window ()
  "The inspector window navigates, descends, and renders object parts."
  (let* ((*system-font* *fixed-font*)
         (desktop (make-desktop :width 160 :height 80))
         (inspector-window (make-inspector-window :object (list 1 2 3)
                                                  :x 2 :y 2
                                                  :width 120 :height 60))
         (window (inspector-window-window inspector-window)))
    (desktop-attach-window desktop window)
    (test-assert (eq inspector-window (window-application window)))
    (test-assert (equal '(1 2 3) (inspector-window-object inspector-window)))
    (test-assert (= 0 (inspector-window-selection inspector-window)))
    (let ((content (window-content-bitmap window)))
      (test-assert (= 1 (bitmap-pixel content 100 11))
                   "selected part line is inverted")
      (test-assert (= 0 (bitmap-pixel content 100 20))
                   "unselected part line stays paper"))
    (labels ((press-key (key)
               (desktop-dispatch-event desktop (make-key-event :key key))))
      (press-key ':down)
      (test-assert (= 1 (inspector-window-selection inspector-window)))
      (press-key ':down)
      (press-key ':down)
      (test-assert (= 2 (inspector-window-selection inspector-window))
                   "selection clamps at the last part")
      (press-key ':up)
      (test-assert (= 1 (inspector-window-selection inspector-window)))
      (press-key ':return)
      (test-assert (equal '(2 3) (inspector-window-object inspector-window))
                   "descending inspects the selected part's value")
      (test-assert (= 0 (inspector-window-selection inspector-window)))
      (press-key ':backspace)
      (test-assert (equal '(1 2 3) (inspector-window-object inspector-window)))
      (press-key ':backspace)
      (test-assert (equal '(1 2 3) (inspector-window-object inspector-window))
                   "ascending at the root stays put"))
    (desktop-dispatch-event desktop (make-pointer-event :x 10 :y 42
                                                        :action ':press
                                                        :button ':left))
    (test-assert (= 2 (inspector-window-selection inspector-window))
                 "a pointer press selects the part under the pointer")
    (desktop-dispatch-event desktop (make-pointer-event :x 10 :y 42
                                                        :action ':release
                                                        :button ':left))))


(-> test-presentation () t)
(defun test-presentation ()
  "Presentations tie window content regions to live objects."
  (let* ((*system-font* *fixed-font*)
         (window (make-window :x 0 :y 0 :width 40 :height 40))
         (first-presentation (window-present window 'alpha
                                             :type ':symbol
                                             :x 2 :y 2
                                             :width 10 :height 8))
         (second-presentation (window-present window 'beta
                                              :type ':symbol
                                              :x 5 :y 5
                                              :width 10 :height 8)))
    (test-assert (eq second-presentation (window-presentation-at window 7 18))
                 "the newest presentation wins where regions overlap")
    (test-assert (eq first-presentation (window-presentation-at window 4 15)))
    (test-assert (null (window-presentation-at window 0 0))
                 "points outside the content region present nothing")
    (test-assert (eq 'beta (presentation-object second-presentation)))
    (test-assert (eq ':symbol (presentation-type second-presentation)))
    (window-clear-presentations window)
    (test-assert (null (window-presentations window))))
  (let* ((*system-font* *fixed-font*)
         (desktop (make-desktop :width 160 :height 120))
         (exec-window (make-exec-window :world nil
                                        :package (find-package '#:lispbsd)
                                        :x 4 :y 4 :width 120 :height 80))
         (window (exec-window-window exec-window)))
    (desktop-attach-window desktop window)
    (loop for character across "(list 1 2)"
          do (desktop-dispatch-event desktop
                                     (make-key-event :key ':character
                                                     :character character)))
    (desktop-dispatch-event desktop (make-key-event :key ':return))
    (let ((form-presentation (window-presentation-at window 8 18))
          (value-presentation (window-presentation-at window 8 27))
          (input-presentation (window-presentation-at window 8 36)))
      (test-assert form-presentation)
      (test-assert (eq ':form (presentation-type form-presentation)))
      (test-assert (equal '(list 1 2)
                          (presentation-object form-presentation)))
      (test-assert value-presentation)
      (test-assert (eq ':value (presentation-type value-presentation)))
      (test-assert (equal '(1 2) (presentation-object value-presentation)))
      (test-assert (null input-presentation)
                   "the input line presents nothing"))))


(-> test-exec-inspect () t)
(defun test-exec-inspect ()
  "Clicking a presented value in the Exec opens an inspector on it."
  (let* ((*system-font* *fixed-font*)
         (desktop (make-desktop :width 400 :height 300))
         (exec-window (make-exec-window :world nil
                                        :package (find-package '#:lispbsd)
                                        :x 4 :y 4 :width 120 :height 80))
         (window (exec-window-window exec-window)))
    (desktop-attach-window desktop window)
    (loop for character across "(list 1 2)"
          do (desktop-dispatch-event desktop
                                     (make-key-event :key ':character
                                                     :character character)))
    (desktop-dispatch-event desktop (make-key-event :key ':return))
    (desktop-dispatch-event desktop (make-pointer-event :x 8 :y 27
                                                        :action ':press
                                                        :button ':left))
    (desktop-dispatch-event desktop (make-pointer-event :x 8 :y 27
                                                        :action ':release
                                                        :button ':left))
    (test-assert (= 2 (length (desktop-windows desktop))))
    (let ((application (window-application (desktop-focus desktop))))
      (test-assert (typep application 'inspector-window)
                   "the new focused window is an inspector")
      (test-assert (equal '(1 2) (inspector-window-object application))
                   "the inspector shows the clicked value"))
    (desktop-dispatch-event desktop (make-pointer-event :x 8 :y 46
                                                        :action ':press
                                                        :button ':left))
    (desktop-dispatch-event desktop (make-pointer-event :x 8 :y 46
                                                        :action ':release
                                                        :button ':left))
    (test-assert (= 2 (length (desktop-windows desktop)))
                 "clicking an unpresented point opens nothing")))


(-> test-window-shadow () t)
(defun test-window-shadow ()
  "Drop shadows dither onto the background but never onto other windows."
  (let* ((*system-font* *fixed-font*)
         (desktop (make-desktop :width 48 :height 48))
         (left (make-window :title "l" :x 2 :y 2 :width 20 :height 16))
         (right (make-window :title "r" :x 24 :y 2 :width 20 :height 16))
         (screen (desktop-screen desktop)))
    (desktop-attach-window desktop left)
    (desktop-attach-window desktop right)
    (desktop-focus-window desktop left)
    (window-raise left)
    (desktop-compose desktop)
    (test-assert (= 1 (bitmap-pixel screen 22 8))
                 "shadow falls on the background")
    (test-assert (= 1 (bitmap-pixel screen 6 18))
                 "shadow falls below the window")
    (test-assert (= 0 (bitmap-pixel screen 25 7))
                 "shadow never falls on another window")
    (test-assert (= 1 (bitmap-pixel screen 22 20))
                 "shadow resumes past the covering window")))


(-> test-window-close-box () t)
(defun test-window-close-box ()
  "The title bar close box detaches the window on click."
  (let* ((*system-font* *fixed-font*)
         (desktop (make-desktop :width 64 :height 64))
         (window (make-window :title "w" :x 4 :y 4 :width 40 :height 30)))
    (desktop-attach-window desktop window)
    (test-assert (eq ':close-box (window-region-at window 36 8)))
    (test-assert (eq ':title-bar (window-region-at window 10 8)))
    (desktop-compose desktop)
    (test-assert (= 0 (bitmap-pixel (desktop-screen desktop) 33 6))
                 "close box outline is inverted on the focused bar")
    (test-assert (= 1 (bitmap-pixel (desktop-screen desktop) 32 6))
                 "the bar around the close box stays ink")
    (desktop-dispatch-event desktop (make-pointer-event :x 36 :y 8
                                                        :action ':press
                                                        :button ':left))
    (test-assert (null (desktop-windows desktop)))
    (test-assert (null (window-desktop window)))
    (test-assert (null (desktop-focus desktop))))
  (let* ((*system-font* *fixed-font*)
         (narrow (make-window :x 0 :y 0 :width 11 :height 20)))
    (test-assert (eq ':title-bar (window-region-at narrow 5 5))
                 "a window too narrow for a close box has no close region")))


(-> test-system-font () t)
(defun test-system-font ()
  "The system font drives chrome metrics and application windows."
  (test-assert (typep *system-font* 'truetype-font)
               "the shipped truetype face is the system font")
  (test-assert (= (window-title-bar-height)
                  (+ (font-height *system-font*) 2)))
  (let* ((desktop (make-desktop :width 400 :height 300))
         (exec-window (make-exec-window :world nil
                                        :package (find-package '#:lispbsd)
                                        :x 10 :y 10 :width 300 :height 200)))
    (desktop-attach-window desktop (exec-window-window exec-window))
    (loop for character across "(+ 1 2)"
          do (desktop-dispatch-event desktop
                                     (make-key-event :key ':character
                                                     :character character)))
    (desktop-dispatch-event desktop (make-key-event :key ':return))
    (test-assert (equal '(3)
                        (exec-entry-values
                         (first (exec-history
                                 (exec-window-exec exec-window)))))
                 "the exec evaluates under truetype metrics")
    (desktop-compose desktop)
    (test-assert (= 1 (bitmap-pixel (desktop-screen desktop) 10 10))
                 "the window border composes under truetype metrics")))


(-> test-journal () t)
(defun test-journal ()
  "The mutation journal appends, reads back, marks durable, and survives crashes."
  (let* ((path (merge-pathnames (format nil "lispbsd-journal-~A.lisp"
                                        (make-object-id))
                                (uiop:temporary-directory)))
         (journal (make-journal :path path)))
    (unwind-protect
         (progn
           (multiple-value-bind (records truncated-p) (journal-records journal)
             (test-assert (null records))
             (test-assert (not truncated-p)))
           (let ((record (journal-append journal ':definition-mutation
                                         (list ':name 'frobnicate
                                               ':form '(defun frobnicate () 42)))))
             (journal-append journal ':checkpoint (list ':note "before rewrite"))
             (multiple-value-bind (records truncated-p) (journal-records journal)
               (test-assert (= 2 (length records)))
               (test-assert (not truncated-p))
               (test-assert (eq ':definition-mutation
                                (journal-record-kind (first records))))
               (test-assert (equal '(defun frobnicate () 42)
                                   (getf (journal-record-payload (first records))
                                         ':form)))
               (test-assert (string= (journal-record-id record)
                                     (journal-record-id (first records)))))
             (test-assert (not (journal-durable-p journal
                                                  (journal-record-id record))))
             (journal-mark-durable journal (journal-record-id record))
             (test-assert (journal-durable-p journal (journal-record-id record))))
           (with-open-file (stream path :direction ':output
                                        :if-exists ':append)
             (write-string "(:id \"deadbeef\" :timestamp 12" stream))
           (multiple-value-bind (records truncated-p) (journal-records journal)
             (test-assert (= 3 (length records))
                          "a crashed final form leaves earlier records readable")
             (test-assert truncated-p "the truncated tail is reported"))
           (handler-case
               (progn
                 (journal-append journal ':bad (list (make-bitmap 1 1)))
                 (test-assert nil "unprintable payloads should be rejected"))
             (unwritable-journal-record ()
               (test-assert t)))
           (multiple-value-bind (records truncated-p) (journal-records journal)
             (declare (ignore truncated-p))
             (test-assert (= 3 (length records))
                          "a rejected payload writes nothing")))
      (ignore-errors (delete-file path)))))


(-> test-world-mutation () t)
(defun test-world-mutation ()
  "Durable mutations journal, install, mark durable, and replay."
  (let* ((path (merge-pathnames (format nil "lispbsd-mutation-~A.lisp"
                                        (make-object-id))
                                (uiop:temporary-directory)))
         (world (make-hosted-world :name "mutation-world"
                                   :journal-path path)))
    (unwind-protect
         (progn
           (test-assert (typep (world-journal world) 'journal))
           (let ((record (world-mutate-definition
                          world
                          '(defun lispbsd-test-frob () 41)
                          :note "first")))
             (test-assert (journal-durable-p (world-journal world)
                                             (journal-record-id record)))
             (test-assert (= 41 (funcall (symbol-function 'lispbsd-test-frob))))
             (test-assert (find ':definition-mutated (world-events world)
                                :key #'event-kind))
             (test-assert (equal (journal-record-id record)
                                 (generation-mutation-head
                                  (world-generation world)))
                          "the new generation heads the mutation lineage"))
           (handler-case
               (progn
                 (world-mutate-definition world
                                          '(defun lispbsd-test-frob () 42)
                                          :check (lambda ()
                                                   (error "check failed")))
                 (test-assert nil "a failing check should signal"))
             (world-mutation-error (condition)
               (test-assert (world-mutation-error-record condition))
               (test-assert (find ':definition-mutation-failed
                                  (world-events world)
                                  :key #'event-kind))))
           (test-assert (= 42 (funcall (symbol-function 'lispbsd-test-frob)))
                        "a failed check leaves the live definition installed")
           (fmakunbound 'lispbsd-test-frob)
           (test-assert (= 1 (world-replay-journal world))
                        "replay reinstalls only durable mutations")
           (test-assert (= 41 (funcall (symbol-function 'lispbsd-test-frob)))
                        "replay restores the durable definition"))
      (world-shutdown world)
      (ignore-errors (delete-file path))
      (fmakunbound 'lispbsd-test-frob)))
  (let ((world (make-hosted-world :name "ephemeral-world")))
    (unwind-protect
         (handler-case
             (progn
               (world-mutate-definition world '(defun nope () nil))
               (test-assert nil "ephemeral worlds should refuse mutations"))
           (world-journal-missing ()
             (test-assert t)))
      (world-shutdown world))))


(defclass mock-network-substrate (network-substrate)
  ((mock-network-substrate-state
    :initform ':down
    :accessor mock-network-substrate-state
    :documentation "Interface state stored by the mock.")
   (mock-network-substrate-addresses
    :initform nil
    :accessor mock-network-substrate-addresses
    :documentation "Addresses stored by the mock."))
  (:documentation "In-memory network substrate for protocol tests."))

(defmethod network-substrate-interface-state ((substrate mock-network-substrate)
                                              interface)
  (declare (ignore interface))
  (mock-network-substrate-state substrate))

(defmethod network-substrate-set-interface-state ((substrate mock-network-substrate)
                                                  interface state)
  (declare (ignore interface))
  (setf (mock-network-substrate-state substrate) state))

(defmethod network-substrate-interface-addresses ((substrate mock-network-substrate)
                                                  interface)
  (declare (ignore interface))
  (mock-network-substrate-addresses substrate))

(defmethod network-substrate-add-interface-address ((substrate mock-network-substrate)
                                                    interface address)
  (declare (ignore interface))
  (push address (mock-network-substrate-addresses substrate)))

(defmethod network-substrate-remove-interface-address ((substrate mock-network-substrate)
                                                       interface address)
  (declare (ignore interface))
  (setf (mock-network-substrate-addresses substrate)
        (remove address (mock-network-substrate-addresses substrate))))


(-> test-network-control () t)
(defun test-network-control ()
  "Interface operations run through the substrate protocol."
  (dolist (case '(("	inet 10.0.2.15/24 broadcast 10.0.2.255 flags 0x0"
                   :ipv4 "10.0.2.15" 24)
                  ("	inet6 fe80::5054:ff:fe12:3456%wm0/64 flags 0x8"
                   :ipv6 "fe80::5054:ff:fe12:3456" 64)
                  ("	inet 127.0.0.1/8 flags 0x0"
                   :ipv4 "127.0.0.1" 8)))
    (destructuring-bind (line family value prefix) case
      (let ((address (netbsd-network--parse-address-line line)))
        (test-assert address (format nil "parse ~S" line))
        (when address
          (test-assert (eq family (network-address-family address)))
          (test-assert (string= value (network-address-value address)))
          (test-assert (eql prefix (network-address-prefix-length address)))))))
  (test-assert (null (netbsd-network--parse-address-line "	status: active")))
  (test-assert (null (netbsd-network--parse-address-line "")))
  (test-assert (netbsd-network--flags-up-p
                "wm0: flags=0x8843<UP,BROADCAST,RUNNING,SIMPLEX,MULTICAST> mtu 1500"))
  (test-assert (not (netbsd-network--flags-up-p
                     "wm0: flags=0x8802<BROADCAST,SIMPLEX,MULTICAST> mtu 1500")))
  (let ((interface (make-instance 'network-interface :name "mock0"))
        (*network-substrate* (make-instance 'mock-network-substrate)))
    (test-assert (eq ':down (interface-state interface)))
    (interface-up interface)
    (test-assert (eq ':up (interface-state interface)))
    (let ((address (make-network-address :value "10.0.0.5"
                                         :family ':ipv4
                                         :prefix-length 24)))
      (interface-add-address interface address)
      (test-assert (member address (interface-addresses interface)))
      (interface-remove-address interface address)
      (test-assert (null (interface-addresses interface))))
    (interface-down interface)
    (test-assert (eq ':down (interface-state interface))))
  (let ((interface (make-instance 'network-interface :name "none0"))
        (*network-substrate* nil))
    (handler-case
        (progn
          (interface-up interface)
          (test-assert nil "operations need a substrate"))
      (network-operation-unsupported ()
        (test-assert t)))))


(-> test-menu () t)
(defun test-menu ()
  "Pop-up menus take all input, highlight, choose, and dismiss."
  (let* ((*system-font* *fixed-font*)
         (desktop (make-desktop :width 64 :height 64))
         (chosen nil)
         (items (list (make-menu-item :label "aa"
                                      :action (lambda ()
                                                (setf chosen ':aa)))
                      (make-menu-item :label "bb"
                                      :action (lambda ()
                                                (setf chosen ':bb)))
                      (make-menu-item :label "cc"))))
    (let ((menu (desktop-open-menu desktop items :x 10 :y 10)))
      (test-assert (eq menu (desktop-menu desktop)))
      (test-assert (= 0 (menu-selection menu)))
      (desktop-dispatch-event desktop (make-pointer-event :x 12 :y 24
                                                          :action ':move))
      (test-assert (= 1 (menu-selection menu))
                   "motion highlights the entry under the pointer")
      (desktop-dispatch-event desktop (make-key-event :key ':down))
      (test-assert (= 2 (menu-selection menu)))
      (desktop-dispatch-event desktop (make-key-event :key ':up))
      (test-assert (= 1 (menu-selection menu)))
      (desktop-compose desktop)
      (let ((screen (desktop-screen desktop)))
        (test-assert (= 1 (bitmap-pixel screen 10 10)) "menu border")
        (test-assert (= 1 (bitmap-pixel screen 28 22))
                     "highlighted entry is inverted")
        (test-assert (= 0 (bitmap-pixel screen 28 14))
                     "other entries stay paper"))
      (desktop-dispatch-event desktop (make-key-event :key ':return))
      (test-assert (eq ':bb chosen) "return chooses the highlighted entry")
      (test-assert (null (desktop-menu desktop))))
    (setf chosen nil)
    (desktop-open-menu desktop items :x 10 :y 10)
    (desktop-dispatch-event desktop (make-pointer-event :x 50 :y 50
                                                        :action ':press
                                                        :button ':left))
    (test-assert (null (desktop-menu desktop))
                 "a press outside dismisses the menu")
    (test-assert (null chosen))
    (desktop-open-menu desktop items :x 10 :y 10)
    (desktop-dispatch-event desktop (make-pointer-event :x 12 :y 14
                                                        :action ':press
                                                        :button ':left))
    (test-assert (eq ':aa chosen) "a press inside chooses that entry")
    (test-assert (null (desktop-menu desktop)))))


(-> test-exec-menu () t)
(defun test-exec-menu ()
  "A right press on an exec presentation opens its context menu."
  (let* ((*system-font* *fixed-font*)
         (desktop (make-desktop :width 400 :height 300))
         (exec-window (make-exec-window :world nil
                                        :package (find-package '#:lispbsd)
                                        :x 4 :y 4 :width 120 :height 80))
         (window (exec-window-window exec-window)))
    (desktop-attach-window desktop window)
    (loop for character across "(list 1 2)"
          do (desktop-dispatch-event desktop
                                     (make-key-event :key ':character
                                                     :character character)))
    (desktop-dispatch-event desktop (make-key-event :key ':return))
    (desktop-dispatch-event desktop (make-pointer-event :x 8 :y 27
                                                        :action ':press
                                                        :button ':right))
    (test-assert (desktop-menu desktop)
                 "a right press over a presentation opens a menu")
    (test-assert (= 1 (length (desktop-windows desktop)))
                 "the menu opens before anything is inspected")
    (test-assert (null (desktop-pointer-grab desktop))
                 "opening a menu releases the pointer grab")
    (desktop-dispatch-event desktop (make-pointer-event :x 8 :y 27
                                                        :action ':release
                                                        :button ':right))
    (desktop-dispatch-event desktop (make-key-event :key ':return))
    (test-assert (null (desktop-menu desktop)))
    (test-assert (= 2 (length (desktop-windows desktop)))
                 "choosing Inspect opens the inspector")
    (test-assert (typep (window-application (desktop-focus desktop))
                        'inspector-window))))


(-> test-system-menu () t)
(defun test-system-menu ()
  "A right press on the bare desktop offers system operations."
  (let* ((*system-font* *fixed-font*)
         (desktop (make-desktop :width 400 :height 300)))
    (desktop-install-system-menu desktop)
    (desktop-dispatch-event desktop (make-pointer-event :x 200 :y 200
                                                        :action ':press
                                                        :button ':right))
    (let ((menu (desktop-menu desktop)))
      (test-assert menu "a background right press opens the system menu")
      (test-assert (equal '(:new-exec :inspect-world)
                          (mapcar #'menu-item-value (menu-items menu)))))
    (desktop-dispatch-event desktop (make-pointer-event :x 200 :y 200
                                                        :action ':release
                                                        :button ':right))
    (desktop-dispatch-event desktop (make-key-event :key ':return))
    (test-assert (= 1 (length (desktop-windows desktop)))
                 "choosing New Exec attaches a window")
    (test-assert (typep (window-application (first (desktop-windows desktop)))
                        'exec-window))
    (desktop-dispatch-event desktop (make-pointer-event :x 4 :y 290
                                                        :action ':press
                                                        :button ':right))
    (desktop-dispatch-event desktop (make-pointer-event :x 4 :y 290
                                                        :action ':release
                                                        :button ':right))
    (desktop-dispatch-event desktop (make-key-event :key ':down))
    (desktop-dispatch-event desktop (make-key-event :key ':return))
    (test-assert (= 2 (length (desktop-windows desktop))))
    (test-assert (typep (window-application (desktop-focus desktop))
                        'inspector-window)
                 "choosing Inspect World attaches an inspector")
    (desktop-dispatch-event desktop (make-pointer-event :x 200 :y 100
                                                        :action ':press
                                                        :button ':left))
    (test-assert (null (desktop-menu desktop))
                 "a left press on the background opens nothing")))


(-> test-window-resize () t)
(defun test-window-resize ()
  "Resizing replaces the content bitmap and repaints the application."
  (let* ((*system-font* *fixed-font*)
         (desktop (make-desktop :width 200 :height 200))
         (exec-window (make-exec-window :world nil
                                        :package (find-package '#:lispbsd)
                                        :x 4 :y 4 :width 120 :height 80))
         (window (exec-window-window exec-window)))
    (desktop-attach-window desktop window)
    (loop for character across "(+ 1 2)"
          do (desktop-dispatch-event desktop
                                     (make-key-event :key ':character
                                                     :character character)))
    (desktop-dispatch-event desktop (make-key-event :key ':return))
    (window-resize window :width 160 :height 100)
    (test-assert (= 160 (window-width window)))
    (test-assert (= 100 (window-height window)))
    (let ((content (window-content-bitmap window)))
      (test-assert (= 158 (bitmap-width content)))
      (test-assert (= 87 (bitmap-height content)))
      (test-assert (= 1 (bitmap-pixel content 4 2))
                   "the application repainted its prompt after the resize"))
    (test-assert (window-presentations window)
                 "repainting re-recorded the input line presentations")
    (handler-case
        (progn
          (window-resize window :width 5 :height 5)
          (test-assert nil "a too-small resize should signal"))
      (invalid-window-geometry ()
        (test-assert t)))))


(-> test-window-menu () t)
(defun test-window-menu ()
  "A right press on a title bar offers Close and Hide."
  (let* ((*system-font* *fixed-font*)
         (desktop (make-desktop :width 64 :height 64))
         (window (make-window :title "w" :x 2 :y 2 :width 40 :height 30)))
    (desktop-attach-window desktop window)
    (desktop-dispatch-event desktop (make-pointer-event :x 10 :y 8
                                                        :action ':press
                                                        :button ':right))
    (let ((menu (desktop-menu desktop)))
      (test-assert menu "a right press on the title bar opens the menu")
      (test-assert (equal '(:close :hide)
                          (mapcar #'menu-item-value (menu-items menu)))))
    (test-assert (null (desktop-window-drag desktop))
                 "a right press does not start a drag")
    (desktop-dispatch-event desktop (make-pointer-event :x 10 :y 8
                                                        :action ':release
                                                        :button ':right))
    (desktop-dispatch-event desktop (make-key-event :key ':down))
    (desktop-dispatch-event desktop (make-key-event :key ':return))
    (test-assert (not (window-visible-p window))
                 "choosing Hide hides the window")
    (window-show window)
    (desktop-dispatch-event desktop (make-pointer-event :x 10 :y 8
                                                        :action ':press
                                                        :button ':right))
    (desktop-dispatch-event desktop (make-key-event :key ':return))
    (test-assert (null (desktop-windows desktop))
                 "choosing Close detaches the window")))


(-> run-tests () t)
(defun run-tests ()
  "Run the LispBSD test suite and signal an error on failure."
  (setf *test-failures* nil)
  (setf *test-count* 0)
  (test-object-id)
  (test-hosted-world)
  (test-authority)
  (test-generation-roundtrip)
  (test-journal)
  (test-world-mutation)
  (test-network-control)
  (test-event-queries)
  (test-activity-mailbox)
  (test-activity-failure)
  (test-exec)
  (test-inspector-and-definitions)
  (test-runtime-identity)
  (test-bitmap)
  (test-font)
  (test-truetype-font)
  (test-system-font)
  (test-window)
  (test-input)
  (test-bitmap-io)
  (test-exec-window)
  (test-inspector-window)
  (test-presentation)
  (test-exec-inspect)
  (test-menu)
  (test-exec-menu)
  (test-system-menu)
  (test-window-shadow)
  (test-window-close-box)
  (test-window-resize)
  (test-window-menu)
  (if *test-failures*
      (error "~D assertion~:P failed of ~D:~%~{  ~A~%~}"
             (length *test-failures*)
             *test-count*
             (nreverse *test-failures*))
      (progn
        (format t "~D assertions passed.~%" *test-count*)
        t)))
