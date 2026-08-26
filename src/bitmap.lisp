(in-package #:lispbsd)

;;;; -- 1-bit Bitmap --

(deftype raster-operation ()
  "A 1-bit raster operation combining source and destination pixels."
  '(member :src :ior :xor :and :not-src :clear :set :not))


(define-condition bitmap-error (lispbsd-error)
  ((bitmap-error-bitmap
    :initarg :bitmap
    :initform nil
    :reader bitmap-error-bitmap
    :documentation "The bitmap involved in the failure, if any."))
  (:documentation "A failure involving a 1-bit bitmap."))


(define-condition invalid-bitmap-size (bitmap-error)
  ((invalid-bitmap-size-width
    :initarg :width
    :reader invalid-bitmap-size-width
    :documentation "The requested width.")
   (invalid-bitmap-size-height
    :initarg :height
    :reader invalid-bitmap-size-height
    :documentation "The requested height."))
  (:report (lambda (condition stream)
             (format stream "Invalid bitmap size ~Ax~A."
                     (invalid-bitmap-size-width condition)
                     (invalid-bitmap-size-height condition))))
  (:documentation "A bitmap was requested with a non-positive size."))


(define-condition unknown-raster-operation (bitmap-error)
  ((unknown-raster-operation-operation
    :initarg :operation
    :reader unknown-raster-operation-operation
    :documentation "The raster operation that was not recognized."))
  (:report (lambda (condition stream)
             (format stream "Unknown raster operation ~S."
                     (unknown-raster-operation-operation condition))))
  (:documentation "A bitblt or fill was given an unknown raster operation."))


(defclass bitmap ()
  ((bitmap-width
    :initarg :width
    :reader bitmap-width
    :type (integer 1)
    :documentation "Width in pixels.")
   (bitmap-height
    :initarg :height
    :reader bitmap-height
    :type (integer 1)
    :documentation "Height in pixels.")
   (bitmap-bits
    :initarg :bits
    :reader bitmap-bits
    :documentation "Row-major 2D bit array, origin at the top left."))
  (:documentation
   "A canonical 1-bit image. Paper is 0, ink is 1."))


(-> make-bitmap (integer integer &key (:initial-element bit)) bitmap)
(defun make-bitmap (width height &key (initial-element 0))
  "Return a WIDTH by HEIGHT 1-bit bitmap filled with INITIAL-ELEMENT."
  (unless (and (plusp width) (plusp height))
    (error 'invalid-bitmap-size :width width :height height))
  (make-instance 'bitmap
                 :width width
                 :height height
                 :bits (make-array (list height width)
                                   :element-type 'bit
                                   :initial-element initial-element)))


(-> bitmap-contains-point-p (bitmap integer integer) boolean)
(defun bitmap-contains-point-p (bitmap x y)
  "Return true when (X, Y) lies inside BITMAP."
  (and (>= x 0)
       (>= y 0)
       (< x (bitmap-width bitmap))
       (< y (bitmap-height bitmap))
       t))


(-> bitmap-pixel (bitmap integer integer) bit)
(defun bitmap-pixel (bitmap x y)
  "Return the bit at (X, Y), or 0 when the point is outside BITMAP."
  (if (bitmap-contains-point-p bitmap x y)
      (aref (bitmap-bits bitmap) y x)
      0))


(defun (setf bitmap-pixel) (bit bitmap x y)
  "Store BIT at (X, Y) when the point lies inside BITMAP."
  (check-type bit bit)
  (when (bitmap-contains-point-p bitmap x y)
    (setf (aref (bitmap-bits bitmap) y x) bit))
  bit)


(-> raster-op (t bit bit) bit)
(defun raster-op (operation source destination)
  "Combine SOURCE and DESTINATION bits under OPERATION."
  (ecase operation
    (:src source)
    (:ior (logior source destination))
    (:xor (logxor source destination))
    (:and (logand source destination))
    (:not-src (logxor source 1))
    (:clear 0)
    (:set 1)
    (:not (logxor destination 1))))


(-> clip-span (&key (:start integer) (:length integer) (:origin integer) (:size integer))
    (values integer integer))
(defun clip-span (&key start length origin size)
  "Clip a 1-D span against [ORIGIN, ORIGIN+SIZE) and return start and length."
  (let* ((lo (max start origin))
         (hi (min (+ start length) (+ origin size))))
    (if (> hi lo)
        (values lo (- hi lo))
        (values 0 0))))


(-> bitblt (bitmap bitmap &key (:sx integer) (:sy integer) (:dx integer)
                   (:dy integer) (:width (option integer))
                   (:height (option integer)) (:operation t))
    bitmap)
(defun bitblt (source destination &key (sx 0) (sy 0) (dx 0) (dy 0)
               width height (operation ':src))
  "Copy a rectangle from SOURCE to DESTINATION under OPERATION.

WIDTH and HEIGHT default to the remaining source size from SX and SY.
The transfer is clipped to both bitmaps. Returns DESTINATION."
  (unless (typep operation 'raster-operation)
    (error 'unknown-raster-operation :operation operation
                                     :bitmap destination))
  (let* ((copy-width (or width (- (bitmap-width source) sx)))
         (copy-height (or height (- (bitmap-height source) sy))))
    (multiple-value-bind (clipped-dx clipped-width)
        (clip-span :start dx :length copy-width
                   :origin 0 :size (bitmap-width destination))
      (multiple-value-bind (clipped-dy clipped-height)
          (clip-span :start dy :length copy-height
                     :origin 0 :size (bitmap-height destination))
        (let ((shifted-sx (+ sx (- clipped-dx dx)))
              (shifted-sy (+ sy (- clipped-dy dy))))
          (multiple-value-bind (final-sx final-width)
              (clip-span :start shifted-sx :length clipped-width
                         :origin 0 :size (bitmap-width source))
            (multiple-value-bind (final-sy final-height)
                (clip-span :start shifted-sy :length clipped-height
                           :origin 0 :size (bitmap-height source))
              (bitmap--copy-rect source destination
                                 :operation operation
                                 :sx final-sx
                                 :sy final-sy
                                 :dx (+ clipped-dx (- final-sx shifted-sx))
                                 :dy (+ clipped-dy (- final-sy shifted-sy))
                                 :width final-width
                                 :height final-height)))))))
  destination)


(-> bitmap--copy-rect (bitmap bitmap &key (:operation t) (:sx integer)
                             (:sy integer) (:dx integer) (:dy integer)
                             (:width integer) (:height integer))
    bitmap)
(defun bitmap--copy-rect (source destination &key operation sx sy dx dy width height)
  "Copy WIDTH by HEIGHT pixels from SOURCE at SX,SY to DESTINATION at DX,DY."
  (when (and (plusp width) (plusp height))
    (let ((src-bits (bitmap-bits source))
          (dst-bits (bitmap-bits destination)))
      (dotimes (row height)
        (dotimes (column width)
          (setf (aref dst-bits (+ dy row) (+ dx column))
                (raster-op operation
                           (aref src-bits (+ sy row) (+ sx column))
                           (aref dst-bits (+ dy row) (+ dx column))))))))
  destination)


(-> bitmap-fill (bitmap &key (:x integer) (:y integer) (:width (option integer))
                            (:height (option integer)) (:bit bit)
                            (:operation t))
    bitmap)
(defun bitmap-fill (bitmap &key (x 0) (y 0) width height (bit 1)
                    (operation ':src))
  "Fill a rectangle of BITMAP with BIT under OPERATION. Returns BITMAP."
  (unless (typep operation 'raster-operation)
    (error 'unknown-raster-operation :operation operation :bitmap bitmap))
  (let ((fill-width (or width (bitmap-width bitmap)))
        (fill-height (or height (bitmap-height bitmap)))
        (bits (bitmap-bits bitmap)))
    (multiple-value-bind (clipped-x clipped-width)
        (clip-span :start x :length fill-width
                   :origin 0 :size (bitmap-width bitmap))
      (multiple-value-bind (clipped-y clipped-height)
          (clip-span :start y :length fill-height
                     :origin 0 :size (bitmap-height bitmap))
        (when (and (plusp clipped-width) (plusp clipped-height))
          (dotimes (row clipped-height)
            (dotimes (column clipped-width)
              (let ((dest-x (+ clipped-x column))
                    (dest-y (+ clipped-y row)))
                  (setf (aref bits dest-y dest-x)
                        (raster-op operation bit (aref bits dest-y dest-x))))))))))
    bitmap)


(-> bitmap-clear (bitmap) bitmap)
(defun bitmap-clear (bitmap)
  "Fill BITMAP with paper and return it."
  (bitmap-fill bitmap :bit 0 :operation ':src))


(-> bitmap-draw-line (bitmap &key (:x0 integer) (:y0 integer) (:x1 integer)
                                 (:y1 integer) (:bit bit))
    bitmap)
(defun bitmap-draw-line (bitmap &key (x0 0) (y0 0) (x1 0) (y1 0) (bit 1))
  "Draw a 1-pixel-thick line from (X0, Y0) to (X1, Y1) in BIT. Returns BITMAP."
  (let* ((dx (abs (- x1 x0)))
         (dy (abs (- y1 y0)))
         (sx (if (< x0 x1) 1 -1))
         (sy (if (< y0 y1) 1 -1))
         (err (- dx dy))
         (x x0)
         (y y0))
    (loop
      (setf (bitmap-pixel bitmap x y) bit)
      (when (and (= x x1) (= y y1))
        (return))
      (let ((e2 (* 2 err)))
        (when (> e2 (- dy))
          (decf err dy)
          (incf x sx))
        (when (< e2 dx)
          (incf err dx)
          (incf y sy)))))
  bitmap)


(-> bitmap-draw-rectangle (bitmap &key (:x integer) (:y integer)
                                      (:width integer) (:height integer)
                                      (:bit bit))
    bitmap)
(defun bitmap-draw-rectangle (bitmap &key (x 0) (y 0) (width 1) (height 1)
                             (bit 1))
  "Draw a 1-pixel outline rectangle. Returns BITMAP."
  (when (and (plusp width) (plusp height))
    (let ((x1 (+ x width -1))
          (y1 (+ y height -1)))
      (bitmap-draw-line bitmap :x0 x :y0 y :x1 x1 :y1 y :bit bit)
      (bitmap-draw-line bitmap :x0 x :y0 y1 :x1 x1 :y1 y1 :bit bit)
      (bitmap-draw-line bitmap :x0 x :y0 y :x1 x :y1 y1 :bit bit)
      (bitmap-draw-line bitmap :x0 x1 :y0 y :x1 x1 :y1 y1 :bit bit)))
  bitmap)


(-> bitmap-copy (bitmap) bitmap)
(defun bitmap-copy (bitmap)
  "Return a pixel-for-pixel copy of BITMAP."
  (let ((copy (make-bitmap (bitmap-width bitmap) (bitmap-height bitmap))))
    (bitblt bitmap copy)
    copy))


(-> bitmap-row-string (bitmap integer) string)
(defun bitmap-row-string (bitmap y)
  "Return row Y of BITMAP as a string of # (ink) and . (paper)."
  (let* ((width (bitmap-width bitmap))
         (string (make-string width)))
    (dotimes (x width)
      (setf (char string x)
            (if (plusp (bitmap-pixel bitmap x y)) #\# #\.)))
    string))


(-> bitmap-ascii (bitmap &key (:x integer) (:y integer)
                             (:width (option integer))
                             (:height (option integer)))
    list)
(defun bitmap-ascii (bitmap &key (x 0) (y 0) width height)
  "Return a list of ASCII rows for a rectangle of BITMAP."
  (let ((region-width (or width (- (bitmap-width bitmap) x)))
        (region-height (or height (- (bitmap-height bitmap) y))))
    (loop for row from y below (+ y region-height)
          collect (let ((string (make-string region-width)))
                    (dotimes (column region-width)
                      (setf (char string column)
                            (if (plusp (bitmap-pixel bitmap (+ x column) row))
                                #\#
                                #\.)))
                    string))))
