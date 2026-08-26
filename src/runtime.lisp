(in-package #:lispbsd)

;;;; -- Runtime Protocol --

(defclass runtime ()
  ((runtime-name
    :initarg :name
    :reader runtime-name
    :type string
    :documentation "Implementation name, for example SBCL.")
   (runtime-version
    :initarg :version
    :reader runtime-version
    :type string
    :documentation "Implementation version string."))
  (:documentation
   "A Common Lisp runtime adapter for the live world."))


(defgeneric runtime-identity (runtime)
  (:documentation
   "Return a property list identifying RUNTIME and its host."))

(defgeneric runtime-start-activity (runtime function &key name)
  (:documentation
   "Start FUNCTION as a native thread and return that thread."))

(defgeneric runtime-interrupt-activity (runtime thread function)
  (:documentation
   "Run FUNCTION on THREAD at the next interruption point."))

(defgeneric runtime-stack (runtime thread)
  (:documentation
   "Return a list of stack-frame descriptions for THREAD."))

(defgeneric runtime-compile-definition (runtime form)
  (:documentation
   "Compile FORM in RUNTIME and return the resulting function."))

(defgeneric runtime-install-definition (runtime form)
  (:documentation
   "Evaluate FORM in RUNTIME, installing any definitions it contains."))

(defgeneric runtime-gc-information (runtime)
  (:documentation
   "Return a property list of collector statistics for RUNTIME."))

(defgeneric runtime-save-image (runtime destination &key entry-point)
  (:documentation
   "Save the current image to DESTINATION. This does not return on success."))
