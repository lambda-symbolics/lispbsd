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
                (bad (exec-evaluate exec "(error \"nope\")")))
           (test-assert (equal (exec-entry-values ok) '(3)))
           (test-assert (null (exec-entry-condition ok)))
           (test-assert (exec-entry-condition bad))
           (test-assert (= 2 (length (exec-history exec)))))
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
  (if *test-failures*
      (error "~D assertion~:P failed of ~D:~%~{  ~A~%~}"
             (length *test-failures*)
             *test-count*
             (nreverse *test-failures*))
      (progn
        (format t "~D assertions passed.~%" *test-count*)
        t)))
