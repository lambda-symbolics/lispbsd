(in-package #:lispbsd)

;;;; -- Activity Supervision --

(defclass supervision-policy ()
  ((supervision-policy-restart-p
    :initarg :restart-p
    :initform t
    :reader supervision-policy-restart-p
    :documentation "Whether failed children are restarted at all.")
   (supervision-policy-maximum-restarts
    :initarg :maximum-restarts
    :initform 3
    :reader supervision-policy-maximum-restarts
    :type integer
    :documentation "Restarts allowed for one child within the window.")
   (supervision-policy-window
    :initarg :window
    :initform 60
    :reader supervision-policy-window
    :type (integer 1)
    :documentation "Seconds over which restarts are counted."))
  (:documentation
   "Restart policy applied by a supervisor to its failing children."))


(defclass supervisor ()
  ((supervisor-policy
    :initarg :policy
    :reader supervisor-policy
    :documentation "The supervision policy applied to children.")
   (supervisor-children
    :initform nil
    :accessor supervisor-children
    :documentation "Activities this supervisor is responsible for.")
   (supervisor-restarts
    :initform (make-hash-table :test 'eq)
    :reader supervisor-restarts
    :documentation "Recent restart times per child, newest first.")
   (supervisor-lock
    :initform (make-lock "supervisor")
    :reader supervisor-lock
    :documentation "Lock protecting supervision bookkeeping."))
  (:documentation
   "Restarts failing activities according to a policy.

Supervision contains accidental damage; it is not a security
boundary."))


(-> make-supervision-policy (&key (:restart-p boolean)
                                 (:maximum-restarts integer)
                                 (:window (integer 1)))
    supervision-policy)
(defun make-supervision-policy (&key (restart-p t) (maximum-restarts 3)
                                (window 60))
  "Return a supervision policy."
  (make-instance 'supervision-policy
                 :restart-p restart-p
                 :maximum-restarts maximum-restarts
                 :window window))


(-> make-supervisor (&key (:policy (option supervision-policy))) supervisor)
(defun make-supervisor (&key policy)
  "Return a supervisor applying POLICY, which defaults sensibly."
  (make-instance 'supervisor
                 :policy (or policy (make-supervision-policy))))


(-> supervisor-adopt (supervisor activity) activity)
(defun supervisor-adopt (supervisor activity)
  "Place ACTIVITY under SUPERVISOR and return ACTIVITY."
  (with-lock-held ((supervisor-lock supervisor))
    (pushnew activity (supervisor-children supervisor)))
  (setf (activity-supervisor activity) supervisor)
  activity)


(defmethod activity-failed-hook ((supervisor supervisor) activity)
  (if (supervisor--restart-allowed-p supervisor activity)
      (progn
        (supervisor--record-restart supervisor activity)
        (supervisor--emit supervisor activity ':supervision-restarted)
        (start-activity activity))
      (supervisor--emit supervisor activity ':supervision-gave-up))
  activity)


(-> supervisor--restart-allowed-p (supervisor activity) boolean)
(defun supervisor--restart-allowed-p (supervisor activity)
  "Return true when the policy permits restarting ACTIVITY now."
  (let ((policy (supervisor-policy supervisor)))
    (and (supervision-policy-restart-p policy)
         (< (supervisor--recent-restart-count supervisor activity)
            (supervision-policy-maximum-restarts policy))
         t)))


(-> supervisor--recent-restart-count (supervisor activity) integer)
(defun supervisor--recent-restart-count (supervisor activity)
  "Return how many restarts ACTIVITY had within the policy window."
  (let* ((policy (supervisor-policy supervisor))
         (horizon (- (get-internal-real-time)
                     (* (supervision-policy-window policy)
                        internal-time-units-per-second))))
    (with-lock-held ((supervisor-lock supervisor))
      (count-if (lambda (time)
                  (>= time horizon))
                (gethash activity (supervisor-restarts supervisor))))))


(-> supervisor--record-restart (supervisor activity) t)
(defun supervisor--record-restart (supervisor activity)
  "Note that ACTIVITY is being restarted now."
  (with-lock-held ((supervisor-lock supervisor))
    (push (get-internal-real-time)
          (gethash activity (supervisor-restarts supervisor))))
  nil)


(-> supervisor--emit (supervisor activity keyword) t)
(defun supervisor--emit (supervisor activity kind)
  "Emit a supervision event of KIND about ACTIVITY."
  (let ((world (activity-world activity)))
    (when world
      (emit-event (world-history world)
                  kind
                  :source supervisor
                  :payload (list :activity-id (activity-id activity)
                                 :activity activity))))
  nil)
