(in-package #:lispbsd)

;;;; -- Lisp Exec --

(defclass exec-entry ()
  ((exec-entry-form
    :initarg :form
    :reader exec-entry-form
    :documentation "Form that was evaluated.")
   (exec-entry-values
    :initarg :values
    :initform nil
    :reader exec-entry-values
    :documentation "Multiple values produced by the form.")
   (exec-entry-condition
    :initarg :condition
    :initform nil
    :reader exec-entry-condition
    :documentation "Condition signaled during evaluation, if any.")
   (exec-entry-timestamp
    :initarg :timestamp
    :initform (current-timestamp)
    :reader exec-entry-timestamp
    :type timestamp
    :documentation "Universal time of this evaluation."))
  (:documentation "One evaluation recorded by a Lisp Exec."))


(defclass exec ()
  ((exec-world
    :initarg :world
    :reader exec-world
    :documentation "World this Exec evaluates against.")
   (exec-package
    :initarg :package
    :initform (find-package '#:lispbsd)
    :accessor exec-package
    :documentation "Package used when reading Exec input.")
   (exec-history
    :initform nil
    :accessor exec-history
    :documentation "Evaluations in chronological order."))
  (:documentation
   "Primary interactive evaluation surface of the Machine."))


(-> make-exec (&key (:world world) (:package package)) exec)
(defun make-exec (&key (world *world*) (package *package*))
  "Return a new Exec bound to WORLD."
  (make-instance 'exec :world world :package package))


(-> exec--read (exec (or string cons)) t)
(defun exec--read (exec input)
  "Read INPUT as a form in EXEC's package."
  (if (stringp input)
      (let ((*package* (exec-package exec))
            (*read-eval* nil))
        (read-from-string input))
      input))


(-> exec-evaluate (exec (or string cons)) exec-entry)
(defun exec-evaluate (exec input)
  "Evaluate INPUT in EXEC and record the result."
  (let* ((form (exec--read exec input))
         (world (exec-world exec))
         (entry
           (handler-case
               (let ((*world* world)
                     (*package* (exec-package exec)))
                 (make-instance 'exec-entry
                                :form form
                                :values (multiple-value-list (eval form))))
             (error (condition)
               (make-instance 'exec-entry
                              :form form
                              :condition condition)))))
    (setf (exec-history exec)
          (nconc (exec-history exec) (list entry)))
    (when world
      (emit-event (world-history world)
                  (if (exec-entry-condition entry)
                      ':exec-error
                      ':exec-evaluated)
                  :source exec
                  :payload (list :form form)))
    entry))
