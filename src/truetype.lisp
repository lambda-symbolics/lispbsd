(in-package #:lispbsd)

;;;; -- TrueType Fonts --

(defclass truetype-font ()
  ((truetype-font-name
    :initarg :name
    :reader truetype-font-name
    :type string
    :documentation "Full name of the font face.")
   (truetype-font-loader
    :initarg :loader
    :reader truetype-font-loader
    :documentation "The zpb-ttf font loader behind this face.")
   (truetype-font-size
    :initarg :size
    :reader truetype-font-size
    :type (integer 1)
    :documentation "Requested pixel size of one em.")
   (truetype-font-scale
    :initarg :scale
    :reader truetype-font-scale
    :documentation "Pixels per font unit.")
   (truetype-font-height
    :initarg :height
    :reader truetype-font-height
    :type (integer 1)
    :documentation "Pixel height of the line box.")
   (truetype-font-ascent
    :initarg :ascent
    :reader truetype-font-ascent
    :type integer
    :documentation "Pixels from the line top to the baseline.")
   (truetype-font-glyphs
    :initform (make-hash-table :test 'eql)
    :reader truetype-font-glyphs
    :documentation "Cache of rendered glyphs keyed by character."))
  (:documentation
   "A TrueType face rasterized on demand onto the 1-bit display."))


(-> make-truetype-font (&key (:path (or pathname string)) (:size (integer 4)))
    truetype-font)
(defun make-truetype-font (&key path (size 14))
  "Load the TrueType font at PATH scaled to SIZE pixels per em.

Glyphs are rasterized lazily and cached. The loader stays open for the
life of the font."
  (let* ((loader (open-font-loader path))
         (scale (/ (float size 1.0d0) (units/em loader)))
         (ascent (ceiling (* (ascender loader) scale)))
         (descent (ceiling (* (- (descender loader)) scale))))
    (make-instance 'truetype-font
                   :name (or (ignore-errors (full-name loader))
                             (file-namestring path))
                   :loader loader
                   :size size
                   :scale scale
                   :ascent ascent
                   :height (+ ascent descent))))


(defmethod font-name ((font truetype-font))
  (truetype-font-name font))

(defmethod font-height ((font truetype-font))
  (truetype-font-height font))

(defmethod font-ascent ((font truetype-font))
  (truetype-font-ascent font))

(defmethod font-glyph ((font truetype-font) character)
  (or (gethash character (truetype-font-glyphs font))
      (setf (gethash character (truetype-font-glyphs font))
            (truetype--render-glyph font character))))

(defmethod font-kerning ((font truetype-font) left-character right-character)
  (round (* (truetype-font-scale font)
            (kerning-offset left-character right-character
                            (truetype-font-loader font)))))


(-> truetype--render-glyph (truetype-font character) glyph)
(defun truetype--render-glyph (font character)
  "Rasterize CHARACTER from FONT's outlines into a 1-bit glyph.

Coverage of at least half a pixel becomes ink. Characters outside the
font's character map render as the font's notdef glyph."
  (let* ((loader (truetype-font-loader font))
         (outline (find-glyph character loader))
         (scale (truetype-font-scale font))
         (advance (round (* (advance-width outline) scale)))
         (left (floor (* (xmin outline) scale)))
         (right (ceiling (* (xmax outline) scale)))
         (width (max 1 (1+ (- right left))))
         (height (truetype-font-height font))
         (bitmap (make-bitmap width height))
         (bits (bitmap-bits bitmap))
         (state (make-state)))
    (update-state state
                  (paths-from-glyph outline
                                    :offset (make-point (- left)
                                                        (truetype-font-ascent font))
                                    :scale-x scale
                                    :scale-y (- scale)))
    (cells-sweep state
                 (lambda (x y alpha)
                   (when (and (<= 0 x) (< x width)
                              (<= 0 y) (< y height)
                              (>= (min 255 (abs alpha)) 128))
                     (setf (aref bits y x) 1))))
    (make-instance 'glyph
                   :bitmap bitmap
                   :advance advance
                   :offset-x left)))
