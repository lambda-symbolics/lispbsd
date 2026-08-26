(in-package #:lispbsd)

;;;; -- Activities --

(defclass activity ()
  ((activity-id
    :initarg :id
    :initform (make-object-id)
    :reader activity-id
    :type object-id
    :documentation "Stable identifier of this activity.")
   (activity-name
    :initarg :name
    :reader activity-name
    :type string
    :documentation "Human-readable name of this activity.")
   (activity-world
    :initarg :world
    :accessor activity-world
    :documentation "World this activity belongs to.")
   (activity-function
    :initarg :function
    :reader activity-function
    :documentation "Thunk started as the activity body.")
   (activity-state
    :initarg :state
    :initform ':new
    :accessor activity-state
    :type activity-state
    :documentation "Current lifecycle state.")
   (activity-parent
    :initarg :parent
    :initform nil
    :accessor activity-parent
    :documentation "Supervisory parent activity, if any.")
   (activity-children
    :initform nil
    :accessor activity-children
    :documentation "Activities supervised by this activity.")
   (activity-authorities
    :initform nil
    :accessor activity-authorities
    :documentation "Authority grants held by this activity.")
   (activity-mailbox
    :initform nil
    :accessor activity-mailbox
    :documentation "Messages sent to this activity and not yet received.")
   (activity-thread
    :initform nil
    :accessor activity-thread
    :documentation "Native thread backing this activity, if running.")
   (activity-condition
    :initform nil
    :accessor activity-condition
    :documentation "Unhandled condition that failed this activity, if any.")
   (activity-lock
    :initform (make-lock "activity")
    :reader activity-lock
    :documentation "Lock protecting activity state.")
   (activity-suspend-cvar
    :initform (make-condition-variable)
    :reader activity-suspend-cvar
    :documentation "Condition variable used to wait out suspension."))
  (:documentation "A schedulable computation in the Lisp world."))


(-> make-activity (string function &key (:world t) (:parent (option activity)))
    activity)
(defun make-activity (name function &key world parent)
  "Return a new activity named NAME that will run FUNCTION."
  (let ((activity (make-instance 'activity
                                 :name name
                                 :function function
                                 :world world
                                 :parent parent)))
    (when parent
      (push activity (activity-children parent)))
    activity))


(-> activity--set-state (activity activity-state) activity-state)
(defun activity--set-state (activity state)
  "Set ACTIVITY's state to STATE and return STATE."
  (setf (activity-state activity) state))


(-> activity--run (activity) t)
(defun activity--run (activity)
  "Run ACTIVITY's body, recording failure or a clean stop."
  (handler-case
      (progn
        (activity--set-state activity ':runnable)
        (funcall (activity-function activity) activity)
        (activity--set-state activity ':stopped))
    (activity-stopped ()
      (activity--set-state activity ':stopped))
    (error (condition)
      (setf (activity-condition activity) condition)
      (activity--set-state activity ':failed)
      (let ((world (activity-world activity)))
        (when world
          (emit-event (world-history world)
                      ':activity-failed
                      :source activity
                      :payload (list :condition condition)))))))


(-> start-activity (activity) activity)
(defun start-activity (activity)
  "Start ACTIVITY on the world runtime and return ACTIVITY."
  (with-lock-held ((activity-lock activity))
    (unless (member (activity-state activity) '(:new :stopped :failed))
      (error 'activity-error :activity activity))
    (let* ((world (activity-world activity))
           (runtime (if world (world-runtime world) (make-sbcl-runtime)))
           (thread (runtime-start-activity
                    runtime
                    (lambda ()
                      (activity--run activity))
                    :name (activity-name activity))))
      (setf (activity-thread activity) thread)
      (when world
        (world-register-activity world activity)
        (emit-event (world-history world)
                    ':activity-started
                    :source activity))))
  activity)


(-> stop-activity (activity) activity)
(defun stop-activity (activity)
  "Request ACTIVITY to stop and return ACTIVITY."
  (let ((thread (activity-thread activity))
        (world (activity-world activity)))
    (when (and thread (thread-alive-p thread))
      (let ((runtime (if world (world-runtime world) (make-sbcl-runtime))))
        (runtime-interrupt-activity
         runtime
         thread
         (lambda ()
           (error 'activity-stopped :activity activity)))))
    (activity--set-state activity ':quiescing)
    (when world
      (emit-event (world-history world)
                  ':activity-stop-requested
                  :source activity)))
  activity)


(-> interrupt-activity (activity function) activity)
(defun interrupt-activity (activity function)
  "Run FUNCTION on ACTIVITY's thread and return ACTIVITY."
  (let ((thread (activity-thread activity))
        (world (activity-world activity)))
    (unless (and thread (thread-alive-p thread))
      (error 'activity-error :activity activity))
    (runtime-interrupt-activity (if world
                                    (world-runtime world)
                                    (make-sbcl-runtime))
                                thread
                                function))
  activity)


(-> suspend-activity (activity) activity)
(defun suspend-activity (activity)
  "Suspend ACTIVITY at the next interruption point and return it."
  (interrupt-activity
   activity
   (lambda ()
     (with-lock-held ((activity-lock activity))
       (activity--set-state activity ':suspended)
       (loop while (eq (activity-state activity) ':suspended)
             do (condition-wait (activity-suspend-cvar activity)
                                (activity-lock activity)))
       (activity--set-state activity ':runnable))))
  activity)


(-> resume-activity (activity) activity)
(defun resume-activity (activity)
  "Resume a suspended ACTIVITY and return it."
  (with-lock-held ((activity-lock activity))
    (when (eq (activity-state activity) ':suspended)
      (activity--set-state activity ':runnable)
      (condition-notify (activity-suspend-cvar activity))))
  activity)


(-> activity--join (activity &key (:timeout real)) boolean)
(defun activity--join (activity &key (timeout 2))
  "Wait up to TIMEOUT seconds for ACTIVITY's thread to finish."
  (let ((thread (activity-thread activity))
        (deadline (+ (get-internal-real-time)
                     (floor (* timeout internal-time-units-per-second)))))
    (when thread
      (loop while (thread-alive-p thread)
            do (when (>= (get-internal-real-time) deadline)
                 (return-from activity--join nil))
               (sleep 0.01))
      (ignore-errors (join-thread thread)))
    t))


(-> restart-activity (activity) activity)
(defun restart-activity (activity)
  "Stop ACTIVITY if it is live, then start it again."
  (let ((thread (activity-thread activity)))
    (when (and thread (thread-alive-p thread))
      (stop-activity activity)
      (activity--join activity :timeout 2)))
  (activity--set-state activity ':new)
  (setf (activity-condition activity) nil)
  (start-activity activity))


(-> debug-activity (activity) list)
(defun debug-activity (activity)
  "Return a stack sample for ACTIVITY and mark it debugging."
  (let* ((world (activity-world activity))
         (thread (activity-thread activity))
         (runtime (if world (world-runtime world) (make-sbcl-runtime))))
    (unless (and thread (thread-alive-p thread))
      (error 'activity-error :activity activity))
    (activity--set-state activity ':debugging)
    (runtime-stack runtime thread)))


(-> send (activity t) t)
(defun send (activity message)
  "Enqueue MESSAGE for ACTIVITY and return MESSAGE."
  (with-lock-held ((activity-lock activity))
    (setf (activity-mailbox activity)
          (nconc (activity-mailbox activity) (list message))))
  message)


(-> receive (activity &key (:timeout (option real))) t)
(defun receive (activity &key timeout)
  "Dequeue the next message for ACTIVITY, waiting up to TIMEOUT seconds."
  (let ((deadline (and timeout (+ (get-internal-real-time)
                                  (* timeout internal-time-units-per-second)))))
    (loop
      (with-lock-held ((activity-lock activity))
        (let ((mailbox (activity-mailbox activity)))
          (when mailbox
            (setf (activity-mailbox activity) (rest mailbox))
            (return (first mailbox)))
          (activity--set-state activity ':waiting)))
      (when (and deadline (>= (get-internal-real-time) deadline))
        (activity--set-state activity ':runnable)
        (return nil))
      (sleep 0.01))))
