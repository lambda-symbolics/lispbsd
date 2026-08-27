(in-package #:lispbsd)

;;;; -- Resources --

(defclass resource ()
  ((resource-id
    :initarg :id
    :initform (make-object-id)
    :reader resource-id
    :type object-id
    :documentation "Stable identifier of this resource.")
   (resource-kind
    :initarg :kind
    :reader resource-kind
    :type keyword
    :documentation "Kind of resource, for example :network-interface.")
   (resource-name
    :initarg :name
    :reader resource-name
    :type string
    :documentation "Human-readable name of the resource.")
   (resource-live-p
    :initarg :live-p
    :initform t
    :accessor resource-live-p
    :type boolean
    :documentation "True while the substrate still presents the resource."))
  (:documentation
   "A typed object representing a finite or externally anchored thing."))


(defclass operation ()
  ((operation-name
    :initarg :name
    :reader operation-name
    :documentation "Keyword naming the semantic operation.")
   (operation-label
    :initarg :label
    :reader operation-label
    :type string
    :documentation "Human-readable label for menus and prompts.")
   (operation-function
    :initarg :function
    :reader operation-function
    :documentation "Function of the target object performing the operation."))
  (:documentation
   "A semantic operation on an object.

One declaration serves human menus, listener commands, and future
agent projections alike."))


(-> make-operation (&key (:name keyword) (:label string)
                        (:function function))
    operation)
(defun make-operation (&key name label function)
  "Return an operation named NAME, shown as LABEL, performing FUNCTION."
  (make-instance 'operation :name name :label label :function function))


(defgeneric resource-operations (resource)
  (:documentation
   "Return the operations available on RESOURCE, most useful first.")
  (:method ((resource t))
    nil))


(defclass network-interface (resource)
  ((network-interface-address
    :initarg :address
    :initform nil
    :reader network-interface-address
    :type (option string)
    :documentation "Hardware address of the interface, if known.")
   (network-interface-operstate
    :initarg :operstate
    :initform nil
    :reader network-interface-operstate
    :type (option string)
    :documentation "Substrate operational state, if known."))
  (:default-initargs :kind ':network-interface)
  (:documentation "A network interface resource."))
