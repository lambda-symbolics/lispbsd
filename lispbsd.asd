;; Vendored third-party systems live under vendor/; see the dependency
;; notes in docs/architecture.org.
(dolist (vendored '("vendor/zpb-ttf/" "vendor/cl-vectors/"))
  (pushnew (merge-pathnames vendored
                            (make-pathname :name nil :type nil
                                           :defaults *load-truename*))
           asdf:*central-registry*
           :test #'equal))


(defsystem #:lispbsd
  :description "A live Common Lisp operating environment on the NetBSD kernel."
  :author "Lukáš Hozda"
  :license "ISC"
  :version "0.0.1"
  :depends-on (#:zpb-ttf
               #:cl-vectors
               #:cl-paths-ttf)
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "types")
                             (:file "threads")
                             (:file "conditions")
                             (:file "bitmap")
                             (:file "font")
                             (:file "truetype")
                             (:file "window")
                             (:file "menu")
                             (:file "input")
                             (:file "presentation")
                             (:file "bitmap-io")
                             (:file "event")
                             (:file "journal")
                             (:file "authority")
                             (:file "runtime")
                             (:file "runtime-sbcl")
                             (:file "resource")
                             (:file "machine")
                             (:file "network")
                             (:file "storage")
                             (:file "activity")
                             (:file "supervisor")
                             (:file "generation")
                             (:file "world")
                             (:file "mutation")
                             (:file "checkpoint")
                             (:file "image")
                             (:file "definition")
                             (:file "inspector")
                             (:file "exec")
                             (:file "inspector-window")
                             (:file "exec-window")
                             (:file "event-window")
                             (:file "resource-window")
                             (:file "system-menu"))))
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
