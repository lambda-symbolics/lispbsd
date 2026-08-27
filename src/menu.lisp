(in-package #:lispbsd)

;;;; -- Pop-up Menus --

(defclass menu-item ()
  ((menu-item-label
    :initarg :label
    :reader menu-item-label
    :type string
    :documentation "Text shown for this entry.")
   (menu-item-value
    :initarg :value
    :initform nil
    :reader menu-item-value
    :documentation "Semantic value of this entry.")
   (menu-item-action
    :initarg :action
    :initform nil
    :reader menu-item-action
    :documentation "Function of no arguments invoked when chosen, or NIL."))
  (:documentation "One choosable entry of a pop-up menu."))


(defclass menu ()
  ((menu-items
    :initarg :items
    :reader menu-items
    :documentation "The menu-item objects, top to bottom.")
   (menu-x
    :initarg :x
    :reader menu-x
    :type integer
    :documentation "Left edge in desktop coordinates.")
   (menu-y
    :initarg :y
    :reader menu-y
    :type integer
    :documentation "Top edge in desktop coordinates.")
   (menu-width
    :initarg :width
    :reader menu-width
    :type (integer 1)
    :documentation "Total width in pixels.")
   (menu-height
    :initarg :height
    :reader menu-height
    :type (integer 1)
    :documentation "Total height in pixels.")
   (menu-selection
    :initform 0
    :accessor menu-selection
    :type integer
    :documentation "Index of the highlighted entry.")
   (menu-chosen
    :initform nil
    :accessor menu-chosen
    :documentation "The chosen menu-item after a choice, or NIL."))
  (:documentation
   "A transient pop-up menu presented above all windows."))


(-> make-menu-item (&key (:label string) (:value t)
                        (:action (option function)))
    menu-item)
(defun make-menu-item (&key label value action)
  "Return a menu entry labeled LABEL carrying VALUE and ACTION."
  (make-instance 'menu-item :label label :value value :action action))


(-> make-menu (list &key (:x integer) (:y integer)) menu)
(defun make-menu (items &key (x 0) (y 0))
  "Return a pop-up menu of ITEMS positioned at desktop point (X, Y)."
  (let ((width (+ 2 (* 2 *window-text-margin*)
                  (loop for item in items
                        maximize (font-text-width *system-font*
                                                  (menu-item-label item)))))
        (height (+ 4 (* (length items) (window-line-height)))))
    (make-instance 'menu
                   :items items
                   :x x
                   :y y
                   :width width
                   :height height)))


(-> desktop-open-menu (desktop list &key (:x integer) (:y integer)) menu)
(defun desktop-open-menu (desktop items &key (x 0) (y 0))
  "Open a pop-up menu of ITEMS on DESKTOP at (X, Y) and return it.

An already open menu is replaced. The menu receives all input until a
choice is made or the pointer is pressed outside it."
  (let ((menu (make-menu items :x x :y y)))
    (setf (desktop-pointer-grab desktop) nil)
    (setf (desktop-window-drag desktop) nil)
    (setf (desktop-menu desktop) menu)
    menu))


(-> desktop-close-menu (desktop) desktop)
(defun desktop-close-menu (desktop)
  "Close DESKTOP's pop-up menu, if one is open."
  (setf (desktop-menu desktop) nil)
  desktop)


(-> menu-contains-point-p (menu integer integer) boolean)
(defun menu-contains-point-p (menu x y)
  "Return true when desktop point (X, Y) lies inside MENU."
  (and (>= x (menu-x menu))
       (>= y (menu-y menu))
       (< x (+ (menu-x menu) (menu-width menu)))
       (< y (+ (menu-y menu) (menu-height menu)))
       t))


(-> menu-item-at (menu integer integer) (option integer))
(defun menu-item-at (menu x y)
  "Return the entry index under desktop point (X, Y), or NIL."
  (block nil
    (unless (menu-contains-point-p menu x y)
      (return nil))
    (let ((index (floor (- y (menu-y menu) 2) (window-line-height))))
      (when (and (>= (- y (menu-y menu)) 2)
                 (< index (length (menu-items menu))))
        index))))


(-> menu-select (menu integer) menu)
(defun menu-select (menu index)
  "Highlight the entry at INDEX, clamped to the menu's entries."
  (setf (menu-selection menu)
        (max 0 (min (1- (length (menu-items menu))) index)))
  menu)


(-> menu-choose (menu) menu-item)
(defun menu-choose (menu)
  "Choose the highlighted entry, run its action, and return it."
  (let ((item (nth (menu-selection menu) (menu-items menu))))
    (setf (menu-chosen menu) item)
    (when (menu-item-action item)
      (funcall (menu-item-action item)))
    item))


(-> menu-draw (menu bitmap) bitmap)
(defun menu-draw (menu screen)
  "Draw MENU onto SCREEN with the highlighted entry inverted."
  (let ((x (menu-x menu))
        (y (menu-y menu))
        (font *system-font*))
    (bitmap-fill screen :x x
                        :y y
                        :width (menu-width menu)
                        :height (menu-height menu)
                        :bit 0)
    (bitmap-draw-rectangle screen :x x
                                  :y y
                                  :width (menu-width menu)
                                  :height (menu-height menu))
    (loop for item in (menu-items menu)
          for index from 0
          for line-y = (+ y 2 (* index (window-line-height)))
          do (when (= index (menu-selection menu))
               (bitmap-fill screen :x (1+ x)
                                   :y line-y
                                   :width (- (menu-width menu) 2)
                                   :height (window-line-height)))
             (bitmap-draw-text screen font (menu-item-label item)
                               :x (+ x 1 *window-text-margin*)
                               :y line-y
                               :bit (if (= index (menu-selection menu))
                                        0
                                        1)))
    screen))
