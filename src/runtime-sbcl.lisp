(in-package #:lispbsd)

;;;; -- SBCL Runtime Adapter --

(defclass sbcl-runtime (runtime)
  ()
  (:default-initargs
   :name "SBCL"
   :version (lisp-implementation-version))
  (:documentation "Runtime adapter for Steel Bank Common Lisp."))


(-> make-sbcl-runtime () sbcl-runtime)
(defun make-sbcl-runtime ()
  "Return a new SBCL runtime adapter."
  (make-instance 'sbcl-runtime))


(defmethod runtime-identity ((runtime sbcl-runtime))
  (list :name (runtime-name runtime)
        :version (runtime-version runtime)
        :machine-type (machine-type)
        :machine-instance (machine-instance)
        :software-type (software-type)
        :software-version (software-version)
        :features (copy-list *features*)))


(defmethod runtime-start-activity ((runtime sbcl-runtime) function &key name)
  (declare (ignore runtime))
  (make-thread function :name (or name "lispbsd-activity")))


(defmethod runtime-interrupt-activity ((runtime sbcl-runtime) thread function)
  (declare (ignore runtime))
  (interrupt-thread thread function)
  t)


(defmethod runtime-stack ((runtime sbcl-runtime) thread)
  (declare (ignore runtime))
  (if (eq thread (current-thread))
      (sb-debug:list-backtrace)
      (let ((frames nil)
            (done nil)
            (lock (make-lock "stack-sample"))
            (cvar (make-condition-variable)))
        (with-lock-held (lock)
          (interrupt-thread
           thread
           (lambda ()
             (let ((sample (sb-debug:list-backtrace)))
               (with-lock-held (lock)
                 (setf frames sample)
                 (setf done t)
                 (condition-notify cvar)))))
          (loop until done
                do (condition-wait cvar lock))
          frames))))


(defmethod runtime-compile-definition ((runtime sbcl-runtime) form)
  (declare (ignore runtime))
  (compile nil form))


(defmethod runtime-install-definition ((runtime sbcl-runtime) form)
  (declare (ignore runtime))
  (eval form))


(defmethod runtime-gc-information ((runtime sbcl-runtime))
  (declare (ignore runtime))
  (list :dynamic-space-size (sb-ext:dynamic-space-size)
        :bytes-consed (sb-ext:get-bytes-consed)))


(defmethod runtime-save-image ((runtime sbcl-runtime) destination &key entry-point)
  (declare (ignore runtime))
  (sb-ext:save-lisp-and-die destination :toplevel entry-point :executable nil))
