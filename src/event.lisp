(in-package #:lispbsd)

;;;; -- Structured Events --

(defclass event ()
  ((event-id
    :initarg :id
    :initform (make-object-id)
    :reader event-id
    :type object-id
    :documentation "Stable identifier of this event.")
   (event-timestamp
    :initarg :timestamp
    :initform (current-timestamp)
    :reader event-timestamp
    :type timestamp
    :documentation "Universal time at which the event was emitted.")
   (event-kind
    :initarg :kind
    :reader event-kind
    :type keyword
    :documentation "Typed kind of the event, for example :world-started.")
   (event-source
    :initarg :source
    :initform nil
    :reader event-source
    :documentation "Object that emitted the event, if one is known.")
   (event-payload
    :initarg :payload
    :initform nil
    :reader event-payload
    :documentation "Property list of typed event fields."))
  (:documentation "A structured historical record emitted by the system."))


(defclass event-log ()
  ((event-log-events
    :initform nil
    :accessor event-log-events
    :documentation "Events in chronological order.")
   (event-log-lock
    :initform (make-lock "event-log")
    :reader event-log-lock
    :documentation "Lock protecting the event list."))
  (:documentation "An append-only sequence of structured events."))


(-> make-event-log () event-log)
(defun make-event-log ()
  "Return a new empty event log."
  (make-instance 'event-log))


(-> emit-event (event-log keyword &key (:source t) (:payload list)) event)
(defun emit-event (log kind &key source payload)
  "Append an event of KIND to LOG and return it."
  (let ((event (make-instance 'event
                              :kind kind
                              :source source
                              :payload payload)))
    (with-lock-held ((event-log-lock log))
      (setf (event-log-events log)
            (nconc (event-log-events log) (list event))))
    event))
