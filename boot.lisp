;;;; Native console boot: load the world and leave a LispBSD listener.

(require "asdf")

;; A crash can leave stale or truncated fasls in the cache. Recompiling
;; is always safe at boot, so take ASDF's TRY-RECOMPILING restart
;; whenever a load fails and one is offered.
(let ((root (make-pathname :name nil :type nil :defaults *load-truename*)))
  (asdf:load-asd (merge-pathnames "lispbsd.asd" root))
  (handler-bind ((error
                   (lambda (condition)
                     (declare (ignore condition))
                     (let ((restart (find-if
                                     (lambda (restart)
                                       (let ((name (restart-name restart)))
                                         (and name
                                              (string= (symbol-name name)
                                                       "TRY-RECOMPILING"))))
                                     (compute-restarts))))
                       (when restart
                         (invoke-restart restart))))))
    (asdf:load-system "lispbsd")))

(in-package #:lispbsd)

(unless (and *world* (eq (world-phase *world*) ':operational))
  (make-hosted-world :name "lispbsd"))

(setf *package* (find-package '#:lispbsd))
(format t "~%LispBSD ~A~%world ~A  phase ~S~%~%"
        (lisp-implementation-version)
        (world-id *world*)
        (world-phase *world*))
