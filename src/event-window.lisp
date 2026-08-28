(in-package #:lispbsd)

;;;; -- Event Browser Window --

(defclass event-window ()
  ((event-window-window
    :initarg :window
    :reader event-window-window
    :documentation "The desktop window presenting the event log.")
   (event-window-log
    :initarg :log
    :reader event-window-log
    :documentation "The event log being browsed.")
   (event-window-selection
    :initform 0
    :accessor event-window-selection
    :type integer
    :documentation "Index of the selected event, oldest first."))
  (:documentation
   "A browser over a structured event log."))


(defmethod application-repaint ((application event-window))
  (event-window-repaint application))


(defgeneric event-window-handle-event (event-window event)
  (:documentation
   "React to an input EVENT routed to EVENT-WINDOW's window.

Up and Down move the selection and Return inspects the selected
event. A pointer press selects the event under the pointer. Returns
EVENT-WINDOW."))


(defmethod event-window-handle-event ((event-window event-window)
                                      (event input-event))
  event-window)


(defmethod event-window-handle-event ((event-window event-window)
                                      (event key-event))
  (when (eq (key-event-action event) ':press)
    (case (key-event-key event)
      (:up
       (event-window--move-selection event-window -1))
      (:down
       (event-window--move-selection event-window 1))
      (:return
       (event-window-inspect event-window)))
    (event-window-repaint event-window))
  event-window)


(defmethod event-window-handle-event ((event-window event-window)
                                      (event pointer-event))
  (when (eq (pointer-event-action event) ':press)
    (let ((index (event-window--index-at event-window
                                         (pointer-event-x event)
                                         (pointer-event-y event))))
      (when index
        (setf (event-window-selection event-window) index)
        (event-window-repaint event-window))))
  event-window)


(-> make-event-window (&key (:log (option event-log)) (:world t)
                           (:title string) (:x integer) (:y integer)
                           (:width integer) (:height integer))
    event-window)
(defun make-event-window (&key log (world *world*) (title "Events")
                          (x 0) (y 0) (width 360) (height 200))
  "Return a browser over LOG presented in a fresh detached window.

LOG defaults to WORLD's history."
  (let* ((window (make-window :title title
                              :x x
                              :y y
                              :width width
                              :height height))
         (event-window (make-instance 'event-window
                                      :window window
                                      :log (or log (world-history world)))))
    (setf (window-application window) event-window)
    (setf (window-event-handler window)
          (lambda (window event)
            (declare (ignore window))
            (event-window-handle-event event-window event)))
    (event-window-repaint event-window)
    event-window))


(-> event-window-selected-event (event-window) (option event))
(defun event-window-selected-event (event-window)
  "Return the currently selected event, or NIL when the log is empty."
  (nth (event-window-selection event-window)
       (event-log-events (event-window-log event-window))))


(-> event-window-inspect (event-window) event-window)
(defun event-window-inspect (event-window)
  "Open an inspector on the selected event beside the browser."
  (let ((selected (event-window-selected-event event-window))
        (window (event-window-window event-window)))
    (when (and selected (window-desktop window))
      (desktop-attach-window
       (window-desktop window)
       (inspector-window-window
        (make-inspector-window :object selected
                               :x (+ (window-x window) 24)
                               :y (+ (window-y window) 24))))))
  event-window)


(-> event-window-repaint (event-window) event-window)
(defun event-window-repaint (event-window)
  "Redraw the event lines with the selection inverted and presented.

Events scroll to keep the selection visible; each visible line is
recorded as a presentation of its event."
  (let* ((window (event-window-window event-window))
         (content (window-content-bitmap window))
         (font *system-font*)
         (events (event-log-events (event-window-log event-window)))
         (selection (min (event-window-selection event-window)
                         (max 0 (1- (length events)))))
         (visible-count (event-window--visible-count event-window))
         (first-visible (max 0 (- (1+ selection) visible-count))))
    (setf (event-window-selection event-window) selection)
    (bitmap-clear content)
    (window-clear-presentations window)
    (let ((*print-length* *window-print-length*)
          (*print-level* *window-print-level*)
          (*package* (find-package '#:lispbsd)))
      (loop for index from first-visible
              below (min (length events) (+ first-visible visible-count))
            for row from 0
            for event = (nth index events)
            for line-y = (window-line-y row)
            for text = (format nil "~A  ~S"
                               (event-kind event)
                               (event-source event))
            do (when (= index selection)
                 (bitmap-fill content :x 0
                                      :y line-y
                                      :width (bitmap-width content)
                                      :height (font-height font)))
               (bitmap-draw-text content font text
                                 :x *window-text-margin*
                                 :y line-y
                                 :shade (if (= index selection)
                                            0
                                            255))
               (window-present window event
                               :type ':event
                               :x *window-text-margin*
                               :y line-y
                               :width (font-text-width font text)
                               :height (font-height font)))))
  event-window)


(-> event-window--move-selection (event-window integer) event-window)
(defun event-window--move-selection (event-window delta)
  "Move the selection by DELTA, clamped to the log's events."
  (let ((events (event-log-events (event-window-log event-window))))
    (setf (event-window-selection event-window)
          (max 0 (min (max 0 (1- (length events)))
                      (+ (event-window-selection event-window) delta)))))
  event-window)


(-> event-window--visible-count (event-window) integer)
(defun event-window--visible-count (event-window)
  "Return how many event lines fit in the content area."
  (let ((content (window-content-bitmap (event-window-window event-window))))
    (max 1 (floor (1- (bitmap-height content)) (window-line-height)))))


(-> event-window--index-at (event-window integer integer) (option integer))
(defun event-window--index-at (event-window x y)
  "Return the event index under desktop point (X, Y), or NIL."
  (multiple-value-bind (content-x content-y)
      (window-point->content (event-window-window event-window) x y)
    (when content-x
      (let* ((visible-count (event-window--visible-count event-window))
             (selection (event-window-selection event-window))
             (first-visible (max 0 (- (1+ selection) visible-count)))
             (index (+ first-visible
                       (floor (- content-y 1) (window-line-height)))))
        (when (< index (length (event-log-events
                                (event-window-log event-window))))
          index)))))
