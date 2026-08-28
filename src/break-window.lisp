(in-package #:lispbsd)

;;;; -- Break Windows --

(defclass break-context ()
  ((break-context-activity
    :initarg :activity
    :reader break-context-activity
    :documentation "The suspended activity.")
   (break-context-condition
    :initarg :condition
    :reader break-context-condition
    :documentation "The unhandled condition that opened the break.")
   (break-context-backtrace
    :initarg :backtrace
    :initform nil
    :reader break-context-backtrace
    :documentation "Backtrace lines captured at the break.")
   (break-context-choice
    :initform nil
    :accessor break-context-choice
    :documentation "The chosen restart, ':retry or ':abort, or NIL.")
   (break-context-lock
    :initform (make-lock "break")
    :reader break-context-lock
    :documentation "Lock guarding the choice.")
   (break-context-cvar
    :initform (make-condition-variable "break")
    :reader break-context-cvar
    :documentation "Signaled when a choice is made."))
  (:documentation
   "A suspended computation waiting for a debugging decision."))


(-> make-break-context (activity condition) break-context)
(defun make-break-context (activity condition)
  "Return a break context for ACTIVITY suspended on CONDITION."
  (make-instance 'break-context
                 :activity activity
                 :condition condition
                 :backtrace (break--capture-backtrace)))


(-> break-context-wait (break-context) keyword)
(defun break-context-wait (context)
  "Block the suspended thread until a choice is made, then return it."
  (with-lock-held ((break-context-lock context))
    (loop until (break-context-choice context)
          do (condition-wait (break-context-cvar context)
                             (break-context-lock context)))
    (break-context-choice context)))


(-> break-context-choose (break-context keyword) keyword)
(defun break-context-choose (context choice)
  "Resume the suspended thread with CHOICE, ':retry or ':abort."
  (with-lock-held ((break-context-lock context))
    (setf (break-context-choice context) choice)
    (condition-notify (break-context-cvar context)))
  choice)


(-> break--capture-backtrace () list)
(defun break--capture-backtrace ()
  "Return the current backtrace as bounded strings, innermost first."
  (handler-case
      (let ((*print-length* *window-print-length*)
            (*print-level* 2))
        (loop for frame in (sb-debug:list-backtrace :count 12)
              collect (prin1-to-string frame)))
    (error ()
      nil)))


(defclass break-window ()
  ((break-window-window
    :initarg :window
    :reader break-window-window
    :documentation "The desktop window presenting the break.")
   (break-window-context
    :initarg :context
    :reader break-window-context
    :documentation "The suspended computation being debugged.")
   (break-window-selection
    :initform 0
    :accessor break-window-selection
    :type integer
    :documentation "Index of the selected restart."))
  (:documentation
   "An interactive break presented as a desktop window."))


(defparameter *break-window-restarts*
  (list (cons ':retry "Retry the activity")
        (cons ':abort "Abort the activity"))
  "Restart choices offered by a break window, as (choice . label).")


(defmethod application-repaint ((application break-window))
  (break-window-repaint application))


(defgeneric break-window-handle-event (break-window event)
  (:documentation
   "React to an input EVENT routed to BREAK-WINDOW's window.

Up and Down select a restart and Return invokes it, resuming the
suspended activity and closing the window. Returns BREAK-WINDOW."))


(defmethod break-window-handle-event ((break-window break-window)
                                      (event input-event))
  break-window)


(defmethod break-window-handle-event ((break-window break-window)
                                      (event key-event))
  (when (eq (key-event-action event) ':press)
    (case (key-event-key event)
      (:up
       (setf (break-window-selection break-window)
             (max 0 (1- (break-window-selection break-window)))))
      (:down
       (setf (break-window-selection break-window)
             (min (1- (length *break-window-restarts*))
                  (1+ (break-window-selection break-window)))))
      (:return
       (break-window-invoke break-window)))
    (when (window-desktop (break-window-window break-window))
      (break-window-repaint break-window)))
  break-window)


(-> make-break-window (&key (:context break-context) (:x integer)
                           (:y integer) (:width integer) (:height integer))
    break-window)
(defun make-break-window (&key context (x 40) (y 40)
                          (width 420) (height 220))
  "Return a break window over CONTEXT in a fresh detached window."
  (let* ((window (make-window :title (format nil "Break: ~A"
                                             (activity-name
                                              (break-context-activity
                                               context)))
                              :x x
                              :y y
                              :width width
                              :height height))
         (break-window (make-instance 'break-window
                                      :window window
                                      :context context)))
    (setf (window-application window) break-window)
    (setf (window-event-handler window)
          (lambda (window event)
            (declare (ignore window))
            (break-window-handle-event break-window event)))
    (break-window-repaint break-window)
    break-window))


(-> break-window-invoke (break-window) break-window)
(defun break-window-invoke (break-window)
  "Invoke the selected restart and close the break window."
  (let ((context (break-window-context break-window))
        (window (break-window-window break-window)))
    (break-context-choose context
                          (first (nth (break-window-selection break-window)
                                      *break-window-restarts*)))
    (when (window-desktop window)
      (desktop-detach-window (window-desktop window) window)))
  break-window)


(-> break-window-repaint (break-window) break-window)
(defun break-window-repaint (break-window)
  "Redraw the condition, restarts, and backtrace of the break."
  (let* ((window (break-window-window break-window))
         (content (window-content-bitmap window))
         (font *system-font*)
         (context (break-window-context break-window))
         (row 0))
    (bitmap-clear content)
    (window-clear-presentations window)
    (flet ((line (text &key inverted-p)
             (let ((line-y (window-line-y row)))
               (when inverted-p
                 (bitmap-fill content :x 0
                                      :y line-y
                                      :width (bitmap-width content)
                                      :height (font-height font)))
               (bitmap-draw-text content font text
                                 :x *window-text-margin*
                                 :y line-y
                                 :shade (if inverted-p 0 255)))
             (incf row)))
      (let ((*print-length* *window-print-length*)
            (*print-level* *window-print-level*))
        (line (format nil "~A" (break-context-condition context)))
        (window-present window (break-context-condition context)
                        :type ':condition
                        :x 0
                        :y (window-line-y 0)
                        :width (bitmap-width content)
                        :height (font-height font))
        (line "")
        (loop for (choice . label) in *break-window-restarts*
              for index from 0
              do (progn choice)
                 (line label
                       :inverted-p (= index
                                      (break-window-selection break-window))))
        (line "")
        (dolist (frame (break-context-backtrace context))
          (line frame)))))
  break-window)


(-> desktop-install-break-handler (desktop &key (:world (option world)))
    desktop)
(defun desktop-install-break-handler (desktop &key (world *world*))
  "Open break windows on DESKTOP for WORLD's unhandled activity conditions.

The failing thread attaches a break window and blocks until a restart
is chosen there."
  (setf (world-break-handler world)
        (lambda (activity condition)
          (let ((context (make-break-context activity condition)))
            (desktop-attach-window desktop
                                   (break-window-window
                                    (make-break-window :context context)))
            (break-context-wait context))))
  desktop)
