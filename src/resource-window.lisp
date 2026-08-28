(in-package #:lispbsd)

;;;; -- Resource Browser Window --

(defclass resource-window ()
  ((resource-window-window
    :initarg :window
    :reader resource-window-window
    :documentation "The desktop window presenting the resources.")
   (resource-window-world
    :initarg :world
    :reader resource-window-world
    :documentation "World whose resources are browsed.")
   (resource-window-selection
    :initform 0
    :accessor resource-window-selection
    :type integer
    :documentation "Index of the selected resource."))
  (:documentation
   "A browser over a world's resources with their operations."))


(defmethod application-repaint ((application resource-window))
  (resource-window-repaint application))


(defgeneric resource-window-handle-event (resource-window event)
  (:documentation
   "React to an input EVENT routed to RESOURCE-WINDOW's window.

Up and Down move the selection, Return inspects the selected resource,
and a right press opens the resource's operation menu. Returns
RESOURCE-WINDOW."))


(defmethod resource-window-handle-event ((resource-window resource-window)
                                         (event input-event))
  resource-window)


(defmethod resource-window-handle-event ((resource-window resource-window)
                                         (event key-event))
  (when (eq (key-event-action event) ':press)
    (case (key-event-key event)
      (:up
       (resource-window--move-selection resource-window -1))
      (:down
       (resource-window--move-selection resource-window 1))
      (:return
       (resource-window-inspect resource-window)))
    (resource-window-repaint resource-window))
  resource-window)


(defmethod resource-window-handle-event ((resource-window resource-window)
                                         (event pointer-event))
  (when (eq (pointer-event-action event) ':press)
    (let ((index (resource-window--index-at resource-window
                                            (pointer-event-x event)
                                            (pointer-event-y event))))
      (when index
        (setf (resource-window-selection resource-window) index)
        (resource-window-repaint resource-window)
        (when (eq (pointer-event-button event) ':right)
          (resource-window-open-menu resource-window
                                     (pointer-event-x event)
                                     (pointer-event-y event))))))
  resource-window)


(-> make-resource-window (&key (:world t) (:title string) (:x integer)
                              (:y integer) (:width integer)
                              (:height integer))
    resource-window)
(defun make-resource-window (&key (world *world*) (title "Resources")
                             (x 0) (y 0) (width 360) (height 200))
  "Return a browser over WORLD's resources in a fresh detached window."
  (let* ((window (make-window :title title
                              :x x
                              :y y
                              :width width
                              :height height))
         (resource-window (make-instance 'resource-window
                                         :window window
                                         :world world)))
    (setf (window-application window) resource-window)
    (setf (window-event-handler window)
          (lambda (window event)
            (declare (ignore window))
            (resource-window-handle-event resource-window event)))
    (resource-window-repaint resource-window)
    resource-window))


(-> resource-window-resources (resource-window) list)
(defun resource-window-resources (resource-window)
  "Return the resources currently shown by RESOURCE-WINDOW."
  (world-resources (resource-window-world resource-window)))


(-> resource-window-selected-resource (resource-window) (option resource))
(defun resource-window-selected-resource (resource-window)
  "Return the selected resource, or NIL when there are none."
  (nth (resource-window-selection resource-window)
       (resource-window-resources resource-window)))


(-> resource-window-inspect (resource-window) resource-window)
(defun resource-window-inspect (resource-window)
  "Open an inspector on the selected resource beside the browser."
  (let ((selected (resource-window-selected-resource resource-window))
        (window (resource-window-window resource-window)))
    (when (and selected (window-desktop window))
      (desktop-attach-window
       (window-desktop window)
       (inspector-window-window
        (make-inspector-window :object selected
                               :x (+ (window-x window) 24)
                               :y (+ (window-y window) 24))))))
  resource-window)


(-> resource-window-open-menu (resource-window integer integer)
    resource-window)
(defun resource-window-open-menu (resource-window x y)
  "Open the operation menu for the selected resource at (X, Y).

The menu offers inspection plus the resource's declared operations."
  (let ((selected (resource-window-selected-resource resource-window))
        (desktop (window-desktop (resource-window-window resource-window))))
    (when (and selected desktop)
      (desktop-open-menu
       desktop
       (cons (make-menu-item :label "Inspect"
                             :value ':inspect
                             :action (lambda ()
                                       (resource-window-inspect
                                        resource-window)))
             (loop for operation in (resource-operations selected)
                   collect (let ((operation operation))
                             (make-menu-item
                              :label (operation-label operation)
                              :value (operation-name operation)
                              :action (lambda ()
                                        (funcall (operation-function operation)
                                                 selected)
                                        (resource-window-repaint
                                         resource-window))))))
       :x x
       :y y)))
  resource-window)


(-> resource-window-repaint (resource-window) resource-window)
(defun resource-window-repaint (resource-window)
  "Redraw the resource lines with the selection inverted and presented."
  (let* ((window (resource-window-window resource-window))
         (content (window-content-bitmap window))
         (font *system-font*)
         (resources (resource-window-resources resource-window))
         (selection (min (resource-window-selection resource-window)
                         (max 0 (1- (length resources)))))
         (visible-count (resource-window--visible-count resource-window))
         (first-visible (max 0 (- (1+ selection) visible-count))))
    (setf (resource-window-selection resource-window) selection)
    (bitmap-clear content)
    (window-clear-presentations window)
    (loop for index from first-visible
            below (min (length resources) (+ first-visible visible-count))
          for row from 0
          for resource = (nth index resources)
          for line-y = (window-line-y row)
          for text = (format nil "~A  ~A"
                             (resource-kind resource)
                             (resource-name resource))
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
             (window-present window resource
                             :type ':resource
                             :x *window-text-margin*
                             :y line-y
                             :width (font-text-width font text)
                             :height (font-height font))))
  resource-window)


(-> resource-window--move-selection (resource-window integer) resource-window)
(defun resource-window--move-selection (resource-window delta)
  "Move the selection by DELTA, clamped to the resources."
  (let ((resources (resource-window-resources resource-window)))
    (setf (resource-window-selection resource-window)
          (max 0 (min (max 0 (1- (length resources)))
                      (+ (resource-window-selection resource-window)
                         delta)))))
  resource-window)


(-> resource-window--visible-count (resource-window) integer)
(defun resource-window--visible-count (resource-window)
  "Return how many resource lines fit in the content area."
  (let ((content (window-content-bitmap
                  (resource-window-window resource-window))))
    (max 1 (floor (1- (bitmap-height content)) (window-line-height)))))


(-> resource-window--index-at (resource-window integer integer)
    (option integer))
(defun resource-window--index-at (resource-window x y)
  "Return the resource index under desktop point (X, Y), or NIL."
  (multiple-value-bind (content-x content-y)
      (window-point->content (resource-window-window resource-window) x y)
    (when content-x
      (let* ((visible-count (resource-window--visible-count resource-window))
             (selection (resource-window-selection resource-window))
             (first-visible (max 0 (- (1+ selection) visible-count)))
             (index (+ first-visible
                       (floor (- content-y 1) (window-line-height)))))
        (when (< index (length (resource-window-resources resource-window)))
          index)))))
