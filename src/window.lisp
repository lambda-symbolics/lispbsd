(in-package #:lispbsd)

;;;; -- Windows and Desktop --

(defparameter *window-title-bar-height*
  (+ (font-height *fixed-font*) 2)
  "Pixel height of a window title bar, excluding its separator line.")

(defparameter *window-text-margin*
  2
  "Pixel margin between window content edges and drawn text.")

(defparameter *window-shadow-offset*
  4
  "Pixel offset of the dithered drop shadow right of and below a window.")

(defparameter *window-print-length*
  16
  "Bound to *PRINT-LENGTH* when printing objects into window content.")

(defparameter *window-print-level*
  4
  "Bound to *PRINT-LEVEL* when printing objects into window content.")


(-> window-line-height () integer)
(defun window-line-height ()
  "Return the pixel height of one text line in window content."
  (1+ (font-height *fixed-font*)))


(-> window-line-y (integer) integer)
(defun window-line-y (index)
  "Return the content Y coordinate of text line INDEX."
  (+ 1 (* index (window-line-height))))


(deftype window-region ()
  "A semantic region of a window's exterior rectangle."
  '(member :border :title-bar :close-box :content))


(define-condition window-error (lispbsd-error)
  ((window-error-window
    :initarg :window
    :initform nil
    :reader window-error-window
    :documentation "The window involved in the failure, if any."))
  (:documentation "A failure involving a desktop window."))


(define-condition invalid-window-geometry (window-error)
  ((invalid-window-geometry-width
    :initarg :width
    :reader invalid-window-geometry-width
    :documentation "The requested exterior width.")
   (invalid-window-geometry-height
    :initarg :height
    :reader invalid-window-geometry-height
    :documentation "The requested exterior height."))
  (:report (lambda (condition stream)
             (format stream "Invalid window geometry ~Ax~A: too small for chrome."
                     (invalid-window-geometry-width condition)
                     (invalid-window-geometry-height condition))))
  (:documentation "A window was requested too small to hold its chrome."))


(define-condition window-not-attached (window-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Window ~S is not attached to the desktop."
                     (window-error-window condition))))
  (:documentation "An operation required a window attached to a desktop."))


(define-condition window-already-attached (window-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Window ~S is already attached to a desktop."
                     (window-error-window condition))))
  (:documentation "A window was attached while it belonged to a desktop."))


(defclass window ()
  ((window-id
    :initarg :id
    :reader window-id
    :type object-id
    :documentation "Stable identity of this window.")
   (window-title
    :initarg :title
    :accessor window-title
    :type string
    :documentation "Title drawn in the window's title bar.")
   (window-x
    :initarg :x
    :accessor window-x
    :type integer
    :documentation "Exterior left edge in desktop coordinates.")
   (window-y
    :initarg :y
    :accessor window-y
    :type integer
    :documentation "Exterior top edge in desktop coordinates.")
   (window-width
    :initarg :width
    :reader window-width
    :type (integer 1)
    :documentation "Exterior width in pixels, including chrome.")
   (window-height
    :initarg :height
    :reader window-height
    :type (integer 1)
    :documentation "Exterior height in pixels, including chrome.")
   (window-visible-p
    :initarg :visible-p
    :accessor window-visible-p
    :documentation "True when the window participates in composition.")
   (window-content-bitmap
    :initarg :content-bitmap
    :reader window-content-bitmap
    :documentation "1-bit bitmap applications draw into.")
   (window-event-handler
    :initarg :event-handler
    :accessor window-event-handler
    :documentation "Function of (window input-event) receiving routed input, or NIL.")
   (window-application
    :initform nil
    :accessor window-application
    :documentation "The application object presenting this window, or NIL.")
   (window-presentations
    :initform nil
    :accessor window-presentations
    :documentation "Presentations over the content bitmap, newest first.")
   (window-desktop
    :initform nil
    :accessor window-desktop
    :documentation "The desktop this window is attached to, or NIL."))
  (:documentation
   "An overlapping desktop window with a 1-bit content bitmap."))


(defclass desktop ()
  ((desktop-id
    :initarg :id
    :reader desktop-id
    :type object-id
    :documentation "Stable identity of this desktop.")
   (desktop-screen
    :initarg :screen
    :reader desktop-screen
    :documentation "The 1-bit bitmap windows are composed onto.")
   (desktop-windows
    :initform nil
    :accessor desktop-windows
    :documentation "Attached windows ordered bottom to top.")
   (desktop-focus
    :initform nil
    :accessor desktop-focus
    :documentation "The window holding input focus, or NIL.")
   (desktop-pointer-grab
    :initform nil
    :accessor desktop-pointer-grab
    :documentation "The window receiving all pointer events during a press, or NIL.")
   (desktop-window-drag
    :initform nil
    :accessor desktop-window-drag
    :documentation "Active title-bar drag as (window offset-x offset-y), or NIL."))
  (:documentation
   "A monochrome desktop owning window stacking, focus, and composition."))


(-> window--content-width (integer) integer)
(defun window--content-width (width)
  "Return the content width inside a window of exterior WIDTH."
  (- width 2))


(-> window--content-height (integer) integer)
(defun window--content-height (height)
  "Return the content height inside a window of exterior HEIGHT."
  (- height 3 *window-title-bar-height*))


(-> window--close-box-geometry (window)
    (values (option integer) (option integer) (option integer)))
(defun window--close-box-geometry (window)
  "Return the local X, Y, and size of WINDOW's close box.

The box sits at the right end of the title bar. Returns three NILs
when the window is too narrow to carry one."
  (let* ((size (font-height *fixed-font*))
         (box-x (- (window-width window) size 3))
         (box-y (+ 1 (floor (- *window-title-bar-height* size) 2))))
    (if (plusp box-x)
        (values box-x box-y size)
        (values nil nil nil))))


(-> make-window (&key (:title string) (:x integer) (:y integer)
                     (:width integer) (:height integer)
                     (:event-handler (option function)))
    window)
(defun make-window (&key (title "Untitled") (x 0) (y 0) (width 64) (height 64)
                    event-handler)
  "Return a detached window with a fresh content bitmap.

WIDTH and HEIGHT are exterior sizes; the content bitmap is smaller by
the border, title bar, and separator line. EVENT-HANDLER is a function
of (window input-event) called for input routed to the window."
  (let ((content-width (window--content-width width))
        (content-height (window--content-height height)))
    (unless (and (plusp content-width) (plusp content-height))
      (error 'invalid-window-geometry :width width :height height))
    (make-instance 'window
                   :id (make-object-id)
                   :title title
                   :x x
                   :y y
                   :width width
                   :height height
                   :visible-p t
                   :event-handler event-handler
                   :content-bitmap (make-bitmap content-width content-height))))


(-> make-desktop (&key (:width integer) (:height integer)) desktop)
(defun make-desktop (&key (width 1024) (height 768))
  "Return a desktop with a WIDTH by HEIGHT 1-bit screen."
  (make-instance 'desktop
                 :id (make-object-id)
                 :screen (make-bitmap width height)))


(-> desktop-attach-window (desktop window) window)
(defun desktop-attach-window (desktop window)
  "Attach WINDOW on top of DESKTOP's stack and give it focus."
  (when (window-desktop window)
    (error 'window-already-attached :window window))
  (setf (window-desktop window) desktop)
  (setf (desktop-windows desktop)
        (append (desktop-windows desktop) (list window)))
  (setf (desktop-focus desktop) window)
  window)


(-> desktop-detach-window (desktop window) window)
(defun desktop-detach-window (desktop window)
  "Remove WINDOW from DESKTOP. Focus falls to the topmost visible window."
  (unless (eq (window-desktop window) desktop)
    (error 'window-not-attached :window window))
  (setf (desktop-windows desktop)
        (remove window (desktop-windows desktop)))
  (setf (window-desktop window) nil)
  (when (eq (desktop-focus desktop) window)
    (setf (desktop-focus desktop)
          (desktop--topmost-visible desktop)))
  window)


(-> desktop--topmost-visible (desktop) (option window))
(defun desktop--topmost-visible (desktop)
  "Return DESKTOP's topmost visible window, or NIL."
  (find-if #'window-visible-p (reverse (desktop-windows desktop))))


(-> window-raise (window) window)
(defun window-raise (window)
  "Move WINDOW to the top of its desktop's stack."
  (let ((desktop (window-desktop window)))
    (unless desktop
      (error 'window-not-attached :window window))
    (setf (desktop-windows desktop)
          (append (remove window (desktop-windows desktop)) (list window))))
  window)


(-> window-lower (window) window)
(defun window-lower (window)
  "Move WINDOW to the bottom of its desktop's stack."
  (let ((desktop (window-desktop window)))
    (unless desktop
      (error 'window-not-attached :window window))
    (setf (desktop-windows desktop)
          (cons window (remove window (desktop-windows desktop)))))
  window)


(-> desktop-focus-window (desktop window) window)
(defun desktop-focus-window (desktop window)
  "Give WINDOW input focus on DESKTOP, showing it first when hidden."
  (unless (eq (window-desktop window) desktop)
    (error 'window-not-attached :window window))
  (window-show window)
  (setf (desktop-focus desktop) window)
  window)


(-> window-show (window) window)
(defun window-show (window)
  "Make WINDOW visible. Stacking and focus are unchanged."
  (setf (window-visible-p window) t)
  window)


(-> window-hide (window) window)
(defun window-hide (window)
  "Hide WINDOW. Focus falls to the topmost remaining visible window."
  (setf (window-visible-p window) nil)
  (let ((desktop (window-desktop window)))
    (when (and desktop (eq (desktop-focus desktop) window))
      (setf (desktop-focus desktop)
            (desktop--topmost-visible desktop))))
  window)


(-> window-move (window &key (:x integer) (:y integer)) window)
(defun window-move (window &key (x (window-x window)) (y (window-y window)))
  "Move WINDOW's exterior top-left corner to (X, Y)."
  (setf (window-x window) x)
  (setf (window-y window) y)
  window)


(-> window-contains-point-p (window integer integer) boolean)
(defun window-contains-point-p (window x y)
  "Return true when desktop point (X, Y) lies inside WINDOW's exterior."
  (and (>= x (window-x window))
       (>= y (window-y window))
       (< x (+ (window-x window) (window-width window)))
       (< y (+ (window-y window) (window-height window)))
       t))


(-> window-region-at (window integer integer) (option window-region))
(defun window-region-at (window x y)
  "Return the window region under desktop point (X, Y), or NIL outside WINDOW.

The border is the outermost one-pixel ring. The close box sits at the
right end of the title bar, which includes its separator line.
Everything else is content."
  (block nil
    (unless (window-contains-point-p window x y)
      (return nil))
    (let ((local-x (- x (window-x window)))
          (local-y (- y (window-y window))))
      (when (or (zerop local-x)
                (zerop local-y)
                (= local-x (1- (window-width window)))
                (= local-y (1- (window-height window))))
        (return ':border))
      (multiple-value-bind (box-x box-y box-size)
          (window--close-box-geometry window)
        (when (and box-x
                   (>= local-x box-x)
                   (< local-x (+ box-x box-size))
                   (>= local-y box-y)
                   (< local-y (+ box-y box-size)))
          (return ':close-box)))
      (if (<= local-y (1+ *window-title-bar-height*))
          ':title-bar
          ':content))))


(-> window-point->content (window integer integer)
    (values (option integer) (option integer)))
(defun window-point->content (window x y)
  "Translate desktop point (X, Y) into WINDOW content coordinates.

Returns two values, the content X and Y, or NIL and NIL when the point
lies outside the content region."
  (if (eq (window-region-at window x y) ':content)
      (values (- x (window-x window) 1)
              (- y (window-y window) 2 *window-title-bar-height*))
      (values nil nil)))


(-> desktop-window-at (desktop integer integer) (option window))
(defun desktop-window-at (desktop x y)
  "Return the topmost visible window at desktop point (X, Y), or NIL."
  (find-if (lambda (window)
             (and (window-visible-p window)
                  (window-contains-point-p window x y)))
           (reverse (desktop-windows desktop))))


(-> desktop-compose (desktop) bitmap)
(defun desktop-compose (desktop)
  "Render the background and all visible windows onto DESKTOP's screen.

Windows are drawn bottom to top over a white background, each casting
a dithered drop shadow that never falls on another window. The focused
window's title bar is inverted. Returns the screen bitmap."
  (let ((screen (desktop-screen desktop)))
    (bitmap-clear screen)
    (dolist (window (desktop-windows desktop))
      (when (window-visible-p window)
        (desktop--draw-window-shadow desktop window)
        (window--draw window screen (eq window (desktop-focus desktop)))))
    screen))


(-> desktop--draw-window-shadow (desktop window) bitmap)
(defun desktop--draw-window-shadow (desktop window)
  "Draw WINDOW's dithered drop shadow onto the screen background.

The shadow is a 50 percent dither offset right of and below the
window. Pixels covered by any other visible window are left alone."
  (let* ((screen (desktop-screen desktop))
         (offset *window-shadow-offset*)
         (x (window-x window))
         (y (window-y window))
         (width (window-width window))
         (height (window-height window)))
    (flet ((shadow-pixel (shadow-x shadow-y)
             (when (and (evenp (+ shadow-x shadow-y))
                        (notany (lambda (other)
                                  (and (not (eq other window))
                                       (window-visible-p other)
                                       (window-contains-point-p other
                                                                shadow-x
                                                                shadow-y)))
                                (desktop-windows desktop)))
               (setf (bitmap-pixel screen shadow-x shadow-y) 1))))
      (loop for shadow-y from (+ y offset) below (+ y height offset)
            do (loop for shadow-x from (+ x width) below (+ x width offset)
                     do (shadow-pixel shadow-x shadow-y)))
      (loop for shadow-y from (+ y height) below (+ y height offset)
            do (loop for shadow-x from (+ x offset) below (+ x width)
                     do (shadow-pixel shadow-x shadow-y))))
    screen))


(-> window--draw (window bitmap boolean) window)
(defun window--draw (window screen focused-p)
  "Draw WINDOW's chrome and content onto SCREEN.

The title bar is drawn ink-on-paper, or inverted when FOCUSED-P."
  (let* ((x (window-x window))
         (y (window-y window))
         (width (window-width window))
         (height (window-height window))
         (title-height *window-title-bar-height*)
         (title-bar (make-bitmap (window--content-width width) title-height
                                 :initial-element (if focused-p 1 0))))
    (bitmap-draw-text title-bar *fixed-font* (window-title window)
                      :x 1 :y 1 :bit (if focused-p 0 1))
    (bitblt title-bar screen :dx (+ x 1) :dy (+ y 1))
    (multiple-value-bind (box-x box-y box-size)
        (window--close-box-geometry window)
      (when box-x
        (let ((box-bit (if focused-p 0 1)))
          (bitmap-draw-rectangle screen :x (+ x box-x)
                                        :y (+ y box-y)
                                        :width box-size
                                        :height box-size
                                        :bit box-bit)
          (bitmap-fill screen :x (+ x box-x 3)
                              :y (+ y box-y 3)
                              :width 2
                              :height 2
                              :bit box-bit))))
    (bitmap-draw-line screen
                      :x0 (+ x 1) :y0 (+ y 1 title-height)
                      :x1 (+ x width -2) :y1 (+ y 1 title-height))
    (bitblt (window-content-bitmap window) screen
            :dx (+ x 1) :dy (+ y 2 title-height))
    (bitmap-draw-rectangle screen :x x :y y :width width :height height))
  window)
