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


(-> events-of-kind (event-log keyword) list)
(defun events-of-kind (log kind)
  "Return LOG's events of KIND in chronological order."
  (with-lock-held ((event-log-lock log))
    (remove-if-not (lambda (event)
                     (eq (event-kind event) kind))
                   (event-log-events log))))


(-> events-involving (event-log t) list)
(defun events-involving (log object)
  "Return LOG's events whose source or payload mentions OBJECT.

Payload property values are compared with EQL, so stable identifiers
and live objects both work."
  (with-lock-held ((event-log-lock log))
    (loop for event in (event-log-events log)
          when (or (eql (event-source event) object)
                   (loop for (nil value) on (event-payload event) by #'cddr
                         thereis (eql value object)))
            collect event)))


(-> events-since (event-log timestamp) list)
(defun events-since (log timestamp)
  "Return LOG's events emitted at or after TIMESTAMP."
  (with-lock-held ((event-log-lock log))
    (remove-if (lambda (event)
                 (< (event-timestamp event) timestamp))
               (event-log-events log))))
