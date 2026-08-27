(in-package #:lispbsd)

;;;; -- Input Events --

(deftype pointer-button ()
  "A pointer button in the three-button PARC tradition."
  '(member :left :middle :right))

(deftype pointer-action ()
  "What a pointer event reports."
  '(member :move :press :release))

(deftype key-action ()
  "What a key event reports."
  '(member :press :release))


(defclass input-event ()
  ((input-event-timestamp
    :initarg :timestamp
    :reader input-event-timestamp
    :type timestamp
    :documentation "Universal time at which the event occurred."))
  (:documentation
   "An occurrence of user input routed through the desktop."))


(defclass pointer-event (input-event)
  ((pointer-event-x
    :initarg :x
    :reader pointer-event-x
    :type integer
    :documentation "Pointer X position in desktop coordinates.")
   (pointer-event-y
    :initarg :y
    :reader pointer-event-y
    :type integer
    :documentation "Pointer Y position in desktop coordinates.")
   (pointer-event-action
    :initarg :action
    :reader pointer-event-action
    :documentation "One of ':move, ':press, or ':release.")
   (pointer-event-button
    :initarg :button
    :reader pointer-event-button
    :documentation "The button involved, or NIL for plain motion."))
  (:documentation
   "Pointer motion or a button transition at a desktop position."))


(defclass key-event (input-event)
  ((key-event-action
    :initarg :action
    :reader key-event-action
    :documentation "One of ':press or ':release.")
   (key-event-key
    :initarg :key
    :reader key-event-key
    :documentation "Symbolic key name, for example ':a or ':return.")
   (key-event-character
    :initarg :character
    :reader key-event-character
    :documentation "The character produced by the key, or NIL.")
   (key-event-modifiers
    :initarg :modifiers
    :reader key-event-modifiers
    :documentation "List of held modifiers such as ':control and ':meta."))
  (:documentation
   "A keyboard key transition delivered to the focused window."))


(defgeneric desktop-dispatch-event (desktop event)
  (:documentation
   "Route EVENT to a window on DESKTOP and return that window or NIL.

A pointer press focuses, raises, and grabs the window under the
pointer; subsequent pointer events go to the grabbed window until the
release. Key events go to the focused window."))


(defmethod desktop-dispatch-event ((desktop desktop) (event pointer-event))
  (let ((window (or (desktop-pointer-grab desktop)
                    (desktop-window-at desktop
                                       (pointer-event-x event)
                                       (pointer-event-y event)))))
    (ecase (pointer-event-action event)
      (:press
       (when window
         (desktop-focus-window desktop window)
         (window-raise window)
         (setf (desktop-pointer-grab desktop) window)))
      (:release
       (setf (desktop-pointer-grab desktop) nil))
      (:move
       nil))
    (when window
      (window--deliver-event window event))
    window))


(defmethod desktop-dispatch-event ((desktop desktop) (event key-event))
  (let ((window (desktop-focus desktop)))
    (when window
      (window--deliver-event window event))
    window))


(-> make-pointer-event (&key (:x integer) (:y integer) (:action pointer-action)
                            (:button (option pointer-button))
                            (:timestamp timestamp))
    pointer-event)
(defun make-pointer-event (&key (x 0) (y 0) (action ':move) button
                           (timestamp (current-timestamp)))
  "Return a pointer event at desktop position (X, Y)."
  (make-instance 'pointer-event
                 :x x
                 :y y
                 :action action
                 :button button
                 :timestamp timestamp))


(-> make-key-event (&key (:key t) (:character (option character))
                        (:action key-action) (:modifiers list)
                        (:timestamp timestamp))
    key-event)
(defun make-key-event (&key key character (action ':press) modifiers
                       (timestamp (current-timestamp)))
  "Return a key event for the symbolic KEY, optionally producing CHARACTER."
  (make-instance 'key-event
                 :key key
                 :character character
                 :action action
                 :modifiers modifiers
                 :timestamp timestamp))


(-> window--deliver-event (window input-event) window)
(defun window--deliver-event (window event)
  "Call WINDOW's event handler with EVENT when one is installed."
  (let ((handler (window-event-handler window)))
    (when handler
      (funcall handler window event)))
  window)
