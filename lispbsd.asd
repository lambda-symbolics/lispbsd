(defsystem #:lispbsd
  :description "A live Common Lisp operating environment on the NetBSD kernel."
  :author "Lukáš Hozda"
  :license "ISC"
  :version "0.0.1"
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "types")
                             (:file "threads")
                             (:file "conditions")
                             (:file "event")
                             (:file "authority")
                             (:file "runtime")
                             (:file "runtime-sbcl")
                             (:file "resource")
                             (:file "machine")
                             (:file "activity")
                             (:file "generation")
                             (:file "world")
                             (:file "definition")
                             (:file "inspector")
                             (:file "exec"))))
  :in-order-to ((test-op (test-op #:lispbsd/tests))))


(defsystem #:lispbsd/tests
  :description "Tests for LispBSD."
  :author "Lukáš Hozda"
  :license "ISC"
  :depends-on (#:lispbsd)
  :pathname "tests"
  :serial t
  :components ((:file "tests"))
  :perform (test-op (operation component)
             (uiop:symbol-call '#:lispbsd '#:run-tests)))
