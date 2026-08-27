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


(-> test-window () t)
(defun test-window ()
  "Windows stack, take focus, hit test, and compose onto the screen."
  (let* ((desktop (make-desktop :width 32 :height 32))
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
      (test-assert (= 1 (bitmap-pixel screen 0 0)) "stipple ink at origin")
      (test-assert (= 0 (bitmap-pixel screen 1 0)) "stipple paper beside origin")
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
  (let* ((desktop  (make-desktop :width 32 :height 32))
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
  (let ((desktop (make-desktop :width 8 :height 8)))
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
           (let ((desktop (make-desktop :width 24 :height 24)))
             (desktop-attach-window desktop (make-window :title "s" :x 2 :y 2
                                                         :width 20 :height 16))
             (desktop-screenshot desktop path)
             (let ((screen (bitmap-read-pbm path)))
               (test-assert (= 24 (bitmap-width screen)))
               (test-assert (= 1 (bitmap-pixel screen 0 0))
                            "screenshot keeps the background stipple")
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
  (let* ((desktop (make-desktop :width 160 :height 120))
         (exec-window (make-exec-window :world nil
                                        :package (find-package '#:lispbsd)
                                        :title "Exec"
                                        :x 4 :y 4 :width 120 :height 80))
         (window (exec-window-window exec-window)))
    (desktop-attach-window desktop window)
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


(-> run-tests () t)
(defun run-tests ()
  "Run the LispBSD test suite and signal an error on failure."
  (setf *test-failures* nil)
  (setf *test-count* 0)
  (test-object-id)
  (test-hosted-world)
  (test-authority)
  (test-generation-roundtrip)
  (test-activity-mailbox)
  (test-activity-failure)
  (test-exec)
  (test-inspector-and-definitions)
  (test-runtime-identity)
  (test-bitmap)
  (test-font)
  (test-window)
  (test-input)
  (test-bitmap-io)
  (test-exec-window)
  (if *test-failures*
      (error "~D assertion~:P failed of ~D:~%~{  ~A~%~}"
             (length *test-failures*)
             *test-count*
             (nreverse *test-failures*))
      (progn
        (format t "~D assertions passed.~%" *test-count*)
        t)))
