(in-package #:lispbsd)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'sb-introspect))

;;;; -- Definitions --

(defclass definition ()
  ((definition-name
    :initarg :name
    :reader definition-name
    :type symbol
    :documentation "Name of the defined entity.")
   (definition-kind
    :initarg :kind
    :reader definition-kind
    :type keyword
    :documentation "Kind of definition, for example :function or :class.")
   (definition-package
    :initarg :package
    :reader definition-package
    :type package
    :documentation "Package of the defined name.")
   (definition-source
    :initarg :source
    :initform nil
    :reader definition-source
    :documentation "Source form, if it can be recovered.")
   (definition-source-location
    :initarg :source-location
    :initform nil
    :reader definition-source-location
    :documentation "Implementation source location object, if available."))
  (:documentation
   "A source-level named program definition with provenance."))


(-> definition--source-location (symbol keyword) t)
(defun definition--source-location (name kind)
  "Return SBCL's source location for NAME of KIND, or NIL."
  (handler-case
      (sb-introspect:find-definition-source
       (ecase kind
         (:function (and (fboundp name) (fdefinition name)))
         (:macro (and (macro-function name) (macro-function name)))
         (:variable name)
         (:class (find-class name nil))))
    (error ()
      nil)))


(-> find-definition (symbol &key (:kind keyword)) (option definition))
(defun find-definition (name &key (kind ':function))
  "Return the definition of NAME of KIND, or NIL if it does not exist."
  (let ((present-p
          (ecase kind
            (:function (and (fboundp name) (not (macro-function name))))
            (:macro (and (fboundp name) (macro-function name)))
            (:variable (boundp name))
            (:class (find-class name nil)))))
    (when present-p
      (make-instance 'definition
                     :name name
                     :kind kind
                     :package (symbol-package name)
                     :source (ignore-errors (function-lambda-expression
                                             (if (eq kind ':macro)
                                                 (macro-function name)
                                                 (and (fboundp name)
                                                      (fdefinition name)))))
                     :source-location (definition--source-location name kind)))))


(-> list-definitions (&key (:package package) (:kind keyword)) list)
(defun list-definitions (&key (package *package*) (kind ':function))
  "Return definitions of KIND present in PACKAGE."
  (let ((definitions nil))
    (do-symbols (symbol package)
      (when (eq (symbol-package symbol) package)
        (let ((definition (find-definition symbol :kind kind)))
          (when definition
            (push definition definitions)))))
    (nreverse definitions)))
