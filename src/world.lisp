(in-package #:lispbsd)

;;;; -- World --

(defvar *world* nil
  "The current live world, or NIL when none is established.")


(defclass world ()
  ((world-id
    :initarg :id
    :initform (make-object-id)
    :reader world-id
    :type object-id
    :documentation "Logical identity of this world, surviving checkpoint.")
   (world-name
    :initarg :name
    :reader world-name
    :type string
    :documentation "Human-readable name of this world.")
   (world-generation
    :initarg :generation
    :initform nil
    :accessor world-generation
    :type (option generation)
    :documentation "Current generation metadata, if recorded.")
   (world-runtime
    :initarg :runtime
    :reader world-runtime
    :documentation "Runtime adapter hosting this world.")
   (world-machine
    :initarg :machine
    :initform nil
    :accessor world-machine
    :documentation "Machine inventory attached to this world.")
   (world-history
    :initarg :history
    :initform (make-event-log)
    :reader world-history
    :documentation "Structured event log for this world.")
   (world-journal
    :initarg :journal
    :initform nil
    :reader world-journal
    :documentation "Durable mutation journal, or NIL for an ephemeral world.")
   (world-break-handler
    :initform nil
    :accessor world-break-handler
    :documentation "Function of (activity condition) deciding breaks, or NIL.")
   (world-authority-root
    :initarg :authority-root
    :initform (make-authority-set)
    :reader world-authority-root
    :documentation "Root authority set for this world.")
   (world-phase
    :initarg :phase
    :initform ':unborn
    :accessor world-phase
    :type world-phase
    :documentation "Current startup or shutdown phase.")
   (world-activities
    :initform nil
    :accessor world-activities
    :documentation "Activities registered in this world.")
   (world-resources
    :initform nil
    :accessor world-resources
    :documentation "Resources attached to this world.")
   (world-lock
    :initform (make-lock "world")
    :reader world-lock
    :documentation "Lock protecting world tables."))
  (:documentation "The coherent live environment of the Machine."))


(-> world-events (world) list)
(defun world-events (world)
  "Return the events recorded in WORLD, in chronological order."
  (event-log-events (world-history world)))


(-> world-register-activity (world activity) activity)
(defun world-register-activity (world activity)
  "Register ACTIVITY in WORLD and return ACTIVITY."
  (with-lock-held ((world-lock world))
    (pushnew activity (world-activities world)))
  activity)


(-> world--set-phase (world world-phase) world-phase)
(defun world--set-phase (world phase)
  "Advance WORLD to PHASE and emit a phase event."
  (setf (world-phase world) phase)
  (emit-event (world-history world)
              ':world-phase
              :source world
              :payload (list :phase phase))
  phase)


(-> world--require-phase (world world-phase) t)
(defun world--require-phase (world expected)
  "Signal WORLD-PHASE-ERROR unless WORLD is in EXPECTED."
  (let ((actual (world-phase world)))
    (unless (eq actual expected)
      (error 'world-phase-error
             :world world
             :expected expected
             :actual actual)))
  t)


(-> world-start (world) world)
(defun world-start (world)
  "Walk WORLD through hosted startup phases and mark it operational."
  (world--require-phase world ':unborn)
  (world--set-phase world ':runtime)
  (let ((generation (or (world-generation world)
                        (make-generation (world-id world)
                                         (world-runtime world)))))
    (setf (world-generation world) generation)
    (world--set-phase world ':generation-validated)
    (world--set-phase world ':stores-attached)
    (world--set-phase world ':world-loaded)
    (let ((machine (or (world-machine world) (probe-hosted-machine))))
      (setf (world-machine world) machine)
      (setf (world-resources world)
            (append (machine-resources machine) (probe-storage-volumes)))
      (setf *network-substrate* (probe-network-substrate))
      (world--set-phase world ':machine-attached))
    (grant-authority (world-authority-root world)
                     world
                     ':all
                     t
                     :delegable-p t)
    (world--set-phase world ':activities-started)
    (world--set-phase world ':session-established)
    (world--set-phase world ':operational)
    (emit-event (world-history world)
                ':world-started
                :source world))
  world)


(-> world-shutdown (world) world)
(defun world-shutdown (world)
  "Quiesce WORLD activities, record a generation, and mark it stopped."
  (world--set-phase world ':shutting-down)
  (emit-event (world-history world)
              ':world-shutdown
              :source world)
  (dolist (activity (copy-list (world-activities world)))
    (ignore-errors (stop-activity activity)))
  (dolist (activity (copy-list (world-activities world)))
    (activity--join activity :timeout 2))
  (setf (world-generation world)
        (make-generation (world-id world)
                         (world-runtime world)
                         :parent-id (and (world-generation world)
                                         (generation-id (world-generation world)))))
  (world--set-phase world ':stopped)
  world)


(-> make-hosted-world (&key (:name string)
                           (:journal-path (option (or pathname string))))
    world)
(defun make-hosted-world (&key (name "hosted") journal-path)
  "Create and start a hosted world on this Common Lisp image.

When JOURNAL-PATH is supplied, the world keeps a durable mutation
journal there; without it the world is ephemeral and refuses durable
mutations."
  (let* ((runtime (make-sbcl-runtime))
         (world (make-instance 'world
                               :name name
                               :runtime runtime
                               :journal (and journal-path
                                             (make-journal :path journal-path)))))
    (world-start world)
    (setf *world* world)
    world))
