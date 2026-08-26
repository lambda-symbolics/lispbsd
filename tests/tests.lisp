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
  (if *test-failures*
      (error "~D assertion~:P failed of ~D:~%~{  ~A~%~}"
             (length *test-failures*)
             *test-count*
             (nreverse *test-failures*))
      (progn
        (format t "~D assertions passed.~%" *test-count*)
        t)))
