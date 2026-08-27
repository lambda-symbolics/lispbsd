(in-package #:lispbsd)

;;;; -- Display Backends and Desktop Sessions --

(defgeneric display-backend-size (backend)
  (:documentation
   "Return the backend's display size as (values width height)."))

(defgeneric display-backend-present (backend bitmap)
  (:documentation
   "Show the composed 1-bit BITMAP on the backend's display."))

(defgeneric display-backend-poll-events (backend)
  (:documentation
   "Return pending input events from the backend, oldest first."))

(defgeneric display-backend-close (backend)
  (:documentation
   "Release the backend's display and input resources."))


(defclass headless-backend ()
  ((headless-backend-width
    :initarg :width
    :reader headless-backend-width
    :type (integer 1)
    :documentation "Logical display width in pixels.")
   (headless-backend-height
    :initarg :height
    :reader headless-backend-height
    :type (integer 1)
    :documentation "Logical display height in pixels.")
   (headless-backend-frame
    :initform nil
    :accessor headless-backend-frame
    :documentation "Copy of the most recently presented frame, or NIL.")
   (headless-backend-frame-count
    :initform 0
    :accessor headless-backend-frame-count
    :type integer
    :documentation "How many frames have been presented.")
   (headless-backend-pending
    :initform nil
    :accessor headless-backend-pending
    :documentation "Queued input events, oldest first.")
   (headless-backend-lock
    :initform (make-lock "headless-backend")
    :reader headless-backend-lock
    :documentation "Lock guarding the event queue and frame."))
  (:documentation
   "A display backend with no physical display.

Used for tests and remote or noninteractive operation: frames are
retained for inspection and input events are injected
programmatically."))


(-> make-headless-backend (&key (:width (integer 1)) (:height (integer 1)))
    headless-backend)
(defun make-headless-backend (&key (width 640) (height 400))
  "Return a headless display backend of WIDTH by HEIGHT."
  (make-instance 'headless-backend :width width :height height))


(defmethod display-backend-size ((backend headless-backend))
  (values (headless-backend-width backend)
          (headless-backend-height backend)))

(defmethod display-backend-present ((backend headless-backend) bitmap)
  (with-lock-held ((headless-backend-lock backend))
    (setf (headless-backend-frame backend) (bitmap-copy bitmap))
    (incf (headless-backend-frame-count backend)))
  bitmap)

(defmethod display-backend-poll-events ((backend headless-backend))
  (with-lock-held ((headless-backend-lock backend))
    (let ((events (headless-backend-pending backend)))
      (setf (headless-backend-pending backend) nil)
      events)))

(defmethod display-backend-close ((backend headless-backend))
  backend)


(-> headless-backend-inject (headless-backend input-event) input-event)
(defun headless-backend-inject (backend event)
  "Queue EVENT as backend input for the next poll."
  (with-lock-held ((headless-backend-lock backend))
    (setf (headless-backend-pending backend)
          (nconc (headless-backend-pending backend) (list event))))
  event)


(defclass desktop-session ()
  ((desktop-session-desktop
    :initarg :desktop
    :reader desktop-session-desktop
    :documentation "The desktop being presented.")
   (desktop-session-backend
    :initarg :backend
    :reader desktop-session-backend
    :documentation "The display backend showing the desktop.")
   (desktop-session-activity
    :initarg :activity
    :reader desktop-session-activity
    :documentation "The activity running the session loop.")
   (desktop-session-stop-p
    :initform nil
    :accessor desktop-session-stop-p
    :documentation "True once the session has been asked to stop."))
  (:documentation
   "A running interactive session: input, dispatch, compose, present."))


(-> start-desktop-session (&key (:desktop (option desktop))
                               (:backend t) (:world t)
                               (:interval real))
    desktop-session)
(defun start-desktop-session (&key desktop
                              (backend (make-headless-backend))
                              (world *world*)
                              (interval 1/30))
  "Start the interactive loop presenting a desktop on BACKEND.

Without DESKTOP a fresh one matching the backend size is created, with
the system menu installed and break windows wired to WORLD when one is
present. The loop polls input, dispatches it, and presents composed
frames roughly every INTERVAL seconds. Returns the session."
  (let* ((desktop (or desktop
                      (multiple-value-bind (width height)
                          (display-backend-size backend)
                        (let ((desktop (make-desktop :width width
                                                     :height height)))
                          (desktop-install-system-menu desktop)
                          (when world
                            (desktop-install-break-handler desktop
                                                           :world world))
                          desktop))))
         (session (make-instance 'desktop-session
                                 :desktop desktop
                                 :backend backend
                                 :activity nil)))
    (let ((activity (make-activity
                     "desktop-session"
                     (lambda (self)
                       (declare (ignore self))
                       (desktop-session--loop session interval))
                     :world world
                     :breakable-p nil)))
      (setf (slot-value session 'desktop-session-activity) activity)
      (start-activity activity))
    session))


(-> stop-desktop-session (desktop-session) desktop-session)
(defun stop-desktop-session (session)
  "Ask SESSION's loop to stop and release its backend."
  (setf (desktop-session-stop-p session) t)
  (activity--join (desktop-session-activity session) :timeout 2)
  (display-backend-close (desktop-session-backend session))
  session)


(-> desktop-session--loop (desktop-session real) t)
(defun desktop-session--loop (session interval)
  "Run SESSION's input, dispatch, compose, present cycle until stopped."
  (let ((desktop (desktop-session-desktop session))
        (backend (desktop-session-backend session)))
    (loop until (desktop-session-stop-p session)
          do (dolist (event (display-backend-poll-events backend))
               (desktop-dispatch-event desktop event))
             (display-backend-present backend (desktop-compose desktop))
             (sleep interval)))
  nil)
