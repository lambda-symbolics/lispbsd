(in-package #:lispbsd)

;;;; -- Threads --

(defun make-lock (&optional name)
  "Return a mutex, optionally named NAME."
  (sb-thread:make-mutex :name name))

(defun current-thread ()
  "Return the calling thread."
    sb-thread:*current-thread*)

(defmacro with-lock-held ((lock) &body body)
  "Evaluate BODY while holding LOCK."
  `(sb-thread:with-mutex (,lock) ,@body))

(defun make-condition-variable (&optional name)
  "Return a waitqueue, optionally named NAME."
  (sb-thread:make-waitqueue :name name))

(defun condition-wait (waitqueue mutex)
  "Wait on WAITQUEUE, releasing MUTEX until notified."
  (sb-thread:condition-wait waitqueue mutex))

(defun condition-notify (waitqueue)
  "Wake one thread waiting on WAITQUEUE."
  (sb-thread:condition-notify waitqueue))
