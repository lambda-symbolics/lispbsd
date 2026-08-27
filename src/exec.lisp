(in-package #:lispbsd)

;;;; -- Lisp Exec --

(defclass exec-entry ()
  ((exec-entry-input
    :initarg :input
    :initform nil
    :reader exec-entry-input
    :documentation "Exact input text this entry came from, if textual.")
   (exec-entry-form
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


(-> make-exec (&key (:world (option world)) (:package package)) exec)
(defun make-exec (&key (world *world*) (package *package*))
  "Return a new Exec bound to WORLD, or to no world when WORLD is NIL."
  (make-instance 'exec :world world :package package))


(-> exec--read (exec (or string cons)) t)
(defun exec--read (exec input)
  "Read INPUT as a form in EXEC's package."
  (if (stringp input)
      (let ((*package* (exec-package exec))
            (*read-eval* nil))
        (read-from-string input))
      input))


(-> exec--evaluate-entry (exec (or string cons) t) exec-entry)
(defun exec--evaluate-entry (exec input world)
  "Read and evaluate INPUT for EXEC and return the resulting entry.

A reader failure records the raw INPUT as the entry's form; an
evaluation failure records the successfully read form."
  (let ((input-text (if (stringp input)
                        input
                        (prin1-to-string input))))
    (block nil
      (let ((form (handler-case
                      (exec--read exec input)
                    (error (condition)
                      (return (make-instance 'exec-entry
                                             :input input-text
                                             :form input
                                             :condition condition))))))
        (handler-case
            (let ((*world* world)
                  (*package* (exec-package exec)))
              (make-instance 'exec-entry
                             :input input-text
                             :form form
                             :values (multiple-value-list (eval form))))
          (error (condition)
            (make-instance 'exec-entry
                           :input input-text
                           :form form
                           :condition condition)))))))


(-> exec-evaluate (exec (or string cons)) exec-entry)
(defun exec-evaluate (exec input)
  "Evaluate INPUT in EXEC and record the result.

Reader failures and evaluation failures are both recorded on the
entry. When INPUT cannot be read, the entry's form is the raw input."
  (let* ((world (exec-world exec))
         (entry (exec--evaluate-entry exec input world)))
    (setf (exec-history exec)
          (nconc (exec-history exec) (list entry)))
    (when world
      (emit-event (world-history world)
                  (if (exec-entry-condition entry)
                      ':exec-error
                      ':exec-evaluated)
                  :source exec
                  :payload (list :form (exec-entry-form entry))))
    entry))
