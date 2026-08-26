(in-package #:lispbsd)

;;;; -- Conditions --

(define-condition lispbsd-error (error)
  ()
  (:documentation "Base condition for LispBSD failures."))


(define-condition world-error (lispbsd-error)
  ((world-error-world
    :initarg :world
    :reader world-error-world
    :documentation "The world in which the failure occurred."))
  (:documentation "A failure involving a live world."))

(define-condition world-phase-error (world-error)
  ((world-phase-error-expected
    :initarg :expected
    :reader world-phase-error-expected
    :documentation "The world phase required by the operation.")
   (world-phase-error-actual
    :initarg :actual
    :reader world-phase-error-actual
    :documentation "The world phase observed when the operation failed."))
  (:report (lambda (condition stream)
             (format stream "World ~A is in phase ~S, expected ~S."
                     (world-id (world-error-world condition))
                     (world-phase-error-actual condition)
                     (world-phase-error-expected condition))))
  (:documentation "A world operation was attempted in an illegal phase."))


(define-condition authority-denied (lispbsd-error)
  ((authority-denied-subject
    :initarg :subject
    :reader authority-denied-subject
    :documentation "The subject that requested the operation.")
   (authority-denied-operation
    :initarg :operation
    :reader authority-denied-operation
    :documentation "The operation that was denied.")
   (authority-denied-target
    :initarg :target
    :reader authority-denied-target
    :documentation "The object the operation would have acted on."))
  (:report (lambda (condition stream)
             (format stream "Authority denied: ~S cannot ~S ~S."
                     (authority-denied-subject condition)
                     (authority-denied-operation condition)
                     (authority-denied-target condition))))
  (:documentation "A requested operation is not granted by any live authority."))


(define-condition activity-error (lispbsd-error)
  ((activity-error-activity
    :initarg :activity
    :reader activity-error-activity
    :documentation "The activity involved in the failure."))
  (:documentation "A failure involving a schedulable activity."))

(define-condition activity-stopped (activity-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Activity ~A was stopped."
                     (activity-id (activity-error-activity condition)))))
  (:documentation "An activity was stopped by supervisory request."))

(define-condition activity-failed (activity-error)
  ((activity-failed-condition
    :initarg :condition
    :reader activity-failed-condition
    :documentation "The unhandled condition that failed the activity."))
  (:report (lambda (condition stream)
             (format stream "Activity ~A failed: ~A"
                     (activity-id (activity-error-activity condition))
                     (activity-failed-condition condition))))
  (:documentation "An activity terminated because of an unhandled condition."))


(define-condition generation-error (lispbsd-error)
  ((generation-error-path
    :initarg :path
    :reader generation-error-path
    :documentation "The generation pathname that failed to validate."))
  (:report (lambda (condition stream)
             (format stream "Invalid generation at ~A."
                     (generation-error-path condition))))
  (:documentation "A generation manifest is missing, unreadable, or invalid."))


(define-condition runtime-error (lispbsd-error)
  ((runtime-error-runtime
    :initarg :runtime
    :reader runtime-error-runtime
    :documentation "The runtime adapter that failed."))
  (:documentation "A failure in the Common Lisp runtime adapter."))
