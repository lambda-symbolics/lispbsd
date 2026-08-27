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
release. A press on a title bar starts a drag that moves the window
with the pointer, and a press on the close box detaches the window.
Key events go to the focused window."))


(defmethod desktop-dispatch-event ((desktop desktop) (event pointer-event))
  (let ((menu (desktop-menu desktop)))
    (when menu
      (desktop--menu-pointer desktop menu event)
      (return-from desktop-dispatch-event nil)))
  (let ((x (pointer-event-x event))
        (y (pointer-event-y event)))
    (let ((window (or (desktop-pointer-grab desktop)
                      (desktop-window-at desktop x y))))
      (ecase (pointer-event-action event)
        (:press
         (cond (window
                (when (eq (window-region-at window x y) ':close-box)
                  (desktop-detach-window desktop window)
                  (return-from desktop-dispatch-event window))
                (desktop-focus-window desktop window)
                (window-raise window)
                (when (and (eq (window-region-at window x y) ':title-bar)
                           (eq (pointer-event-button event) ':right))
                  (desktop-open-menu desktop
                                     (desktop--window-menu-items desktop
                                                                 window)
                                     :x x
                                     :y y)
                  (return-from desktop-dispatch-event window))
                (setf (desktop-pointer-grab desktop) window)
                (when (eq (window-region-at window x y) ':title-bar)
                  (setf (desktop-window-drag desktop)
                        (list window
                              (- x (window-x window))
                              (- y (window-y window))))))
               ((and (eq (pointer-event-button event) ':right)
                     (desktop-menu-items-function desktop))
                (desktop-open-menu desktop
                                   (funcall (desktop-menu-items-function
                                             desktop)
                                            desktop x y)
                                   :x x
                                   :y y))))
        (:release
         (setf (desktop-pointer-grab desktop) nil)
         (setf (desktop-window-drag desktop) nil))
        (:move
         (let ((drag (desktop-window-drag desktop)))
           (when drag
             (window-move (first drag)
                          :x (- x (second drag))
                          :y (- y (third drag)))))))
      (when window
        (window--deliver-event window event))
      window)))


(defmethod desktop-dispatch-event ((desktop desktop) (event key-event))
  (let ((menu (desktop-menu desktop)))
    (when menu
      (desktop--menu-key desktop menu event)
      (return-from desktop-dispatch-event nil)))
  (let ((window (desktop-focus desktop)))
    (when window
      (window--deliver-event window event))
    window))


(-> desktop--window-menu-items (desktop window) list)
(defun desktop--window-menu-items (desktop window)
  "Return the title bar context menu entries for WINDOW."
  (list (make-menu-item :label "Close"
                        :value ':close
                        :action (lambda ()
                                  (desktop-detach-window desktop window)))
        (make-menu-item :label "Hide"
                        :value ':hide
                        :action (lambda ()
                                  (window-hide window)))))


(-> desktop--menu-pointer (desktop menu pointer-event) t)
(defun desktop--menu-pointer (desktop menu event)
  "Route a pointer EVENT to the open MENU.

Motion highlights the entry under the pointer; a press inside chooses
that entry, and a press outside dismisses the menu."
  (let ((x (pointer-event-x event))
        (y (pointer-event-y event)))
    (ecase (pointer-event-action event)
      (:move
       (let ((index (menu-item-at menu x y)))
         (when index
           (menu-select menu index))))
      (:press
       (let ((index (menu-item-at menu x y)))
         (desktop-close-menu desktop)
         (when index
           (menu-select menu index)
           (menu-choose menu))))
      (:release
       nil)))
  nil)


(-> desktop--menu-key (desktop menu key-event) t)
(defun desktop--menu-key (desktop menu event)
  "Route a key EVENT to the open MENU.

Up and Down move the highlight, Return chooses, Escape dismisses."
  (when (eq (key-event-action event) ':press)
    (case (key-event-key event)
      (:up
       (menu-select menu (1- (menu-selection menu))))
      (:down
       (menu-select menu (1+ (menu-selection menu))))
      (:return
       (desktop-close-menu desktop)
       (menu-choose menu))
      (:escape
       (desktop-close-menu desktop))))
  nil)


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
