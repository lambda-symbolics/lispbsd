;;;; Native console boot: load the world and leave a LispBSD listener.

(require "asdf")

(let* ((root (make-pathname :name nil :type nil :defaults *load-truename*))
       (asd (merge-pathnames "lispbsd.asd" root)))
  (setf (uiop:getenv "HOME") (namestring root))
  (asdf:initialize-source-registry
   `(:source-registry (:directory ,root) :ignore-inherited-configuration))
  (asdf:disable-output-translations)
  (asdf:load-asd asd)
  (asdf:load-system :lispbsd))

(in-package #:lispbsd)

(unless (and *world* (eq (world-phase *world*) ':operational))
  (make-hosted-world :name "lispbsd"))

(setf *package* (find-package '#:lispbsd))
(format t "~%LispBSD ~A~%world ~A  phase ~S~%~%"
        (lisp-implementation-version)
        (world-id *world*)
        (world-phase *world*))
