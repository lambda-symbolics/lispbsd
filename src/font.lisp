(in-package #:lispbsd)

;;;; -- Fonts and Glyphs --

(defclass glyph ()
  ((glyph-bitmap
    :initarg :bitmap
    :reader glyph-bitmap
    :documentation "1-bit image of the glyph, aligned to the line top.")
   (glyph-advance
    :initarg :advance
    :reader glyph-advance
    :type integer
    :documentation "Horizontal pen movement after drawing this glyph.")
   (glyph-offset-x
    :initarg :offset-x
    :initform 0
    :reader glyph-offset-x
    :type integer
    :documentation "Horizontal offset of the bitmap from the pen position."))
  (:documentation
   "One renderable character image with its metrics."))


(defclass bitmap-font ()
  ((bitmap-font-name
    :initarg :name
    :reader bitmap-font-name
    :type string
    :documentation "Human-readable name of this font.")
   (bitmap-font-width
    :initarg :width
    :reader bitmap-font-width
    :type (integer 1)
    :documentation "Nominal glyph cell width in pixels.")
   (bitmap-font-height
    :initarg :height
    :reader bitmap-font-height
    :type (integer 1)
    :documentation "Nominal glyph cell height in pixels.")
   (bitmap-font-ascent
    :initarg :ascent
    :reader bitmap-font-ascent
    :type integer
    :documentation "Pixels from the top of the cell to the baseline.")
   (bitmap-font-glyphs
    :initarg :glyphs
    :reader bitmap-font-glyphs
    :documentation "Hash table mapping characters to glyph objects.")
   (bitmap-font-missing
    :initarg :missing
    :reader bitmap-font-missing
    :documentation "Glyph drawn for characters with no defined image."))
  (:documentation
   "A fixed-cell bitmap font used by the 1-bit display."))


(defgeneric font-name (font)
  (:documentation "Return the human-readable name of FONT."))

(defgeneric font-height (font)
  (:documentation "Return the pixel height of FONT's line box."))

(defgeneric font-ascent (font)
  (:documentation "Return the pixels from FONT's line top to its baseline."))

(defgeneric font-glyph (font character)
  (:documentation "Return the glyph for CHARACTER in FONT.

Characters without a defined image map to the font's missing glyph."))

(defgeneric font-kerning (font left-character right-character)
  (:documentation
   "Return the pen adjustment between LEFT-CHARACTER and RIGHT-CHARACTER."))


(defmethod font-name ((font bitmap-font))
  (bitmap-font-name font))

(defmethod font-height ((font bitmap-font))
  (bitmap-font-height font))

(defmethod font-ascent ((font bitmap-font))
  (bitmap-font-ascent font))

(defmethod font-glyph ((font bitmap-font) character)
  (or (gethash character (bitmap-font-glyphs font))
      (bitmap-font-missing font)))

(defmethod font-kerning ((font t) left-character right-character)
  (declare (ignore left-character right-character))
  0)


(-> art->glyph (list) bitmap)
(defun art->glyph (rows)
  "Return an 8x8 glyph bitmap from ROWS of # (ink) and other (paper) characters."
  (let* ((height (length rows))
         (width (if rows (length (first rows)) 0))
         (bitmap (make-bitmap (max 8 width) (max 8 height))))
    (loop for y from 0
          for row in rows
          do (loop for x from 0 below (length row)
                   when (char= (char row x) #\#)
                     do (setf (bitmap-pixel bitmap x y) 1)))
    bitmap))


(-> make-missing-glyph (integer integer) bitmap)
(defun make-missing-glyph (width height)
  "Return a hollow box glyph bitmap of WIDTH by HEIGHT."
  (let ((bitmap (make-bitmap width height)))
    (bitmap-draw-rectangle bitmap :x 0 :y 0 :width width :height height)
    (setf (bitmap-pixel bitmap (floor width 2) (floor height 2)) 1)
    bitmap))


(-> make-bitmap-font (&key (:name string) (:width (integer 1)) (:height (integer 1))
                          (:ascent integer) (:glyphs hash-table)
                          (:missing bitmap))
    bitmap-font)
(defun make-bitmap-font (&key (name "fixed") (width 8) (height 8)
                         (ascent 7) glyphs missing)
  "Return a bitmap font with the supplied GLYPHS table.

MISSING is a bitmap drawn for undefined characters; it defaults to a
hollow box."
  (make-instance 'bitmap-font
                 :name name
                 :width width
                 :height height
                 :ascent ascent
                 :glyphs (or glyphs (make-hash-table :test 'eql))
                 :missing (make-instance 'glyph
                                         :bitmap (or missing
                                                     (make-missing-glyph width height))
                                         :advance width)))


(-> font-text-width (t string) integer)
(defun font-text-width (font string)
  "Return the pixel width of STRING in FONT, including kerning."
  (loop for index from 0 below (length string)
        for character = (char string index)
        sum (glyph-advance (font-glyph font character))
        when (< (1+ index) (length string))
          sum (font-kerning font character (char string (1+ index)))))


(-> bitmap-draw-text (bitmap t string &key (:x integer) (:y integer)
                            (:bit bit))
    bitmap)
(defun bitmap-draw-text (bitmap font string &key (x 0) (y 0) (bit 1))
  "Draw STRING in FONT onto BITMAP with the line top at (X, Y).

Glyphs are clipped. BIT 1 draws ink; BIT 0 draws the glyphs in paper.
Returns BITMAP."
  (let ((pen x))
    (loop for index from 0 below (length string)
          for character = (char string index)
          for glyph = (font-glyph font character)
          for glyph-x = (+ pen (glyph-offset-x glyph))
          do (if (eql bit 1)
                 (bitblt (glyph-bitmap glyph) bitmap
                         :dx glyph-x :dy y :operation ':ior)
                 (let ((inverted (bitmap-copy (glyph-bitmap glyph))))
                   (bitmap-fill inverted :operation ':not)
                   (bitblt inverted bitmap
                           :dx glyph-x :dy y :operation ':and)))
             (incf pen (glyph-advance glyph))
             (when (< (1+ index) (length string))
               (incf pen (font-kerning font character
                                       (char string (1+ index)))))))
  bitmap)


(-> install-glyph (bitmap-font character list) glyph)
(defun install-glyph (font character rows)
  "Define CHARACTER in FONT from ASCII-art ROWS and return the glyph."
  (let ((glyph (make-instance 'glyph
                              :bitmap (art->glyph rows)
                              :advance (bitmap-font-width font))))
    (setf (gethash character (bitmap-font-glyphs font)) glyph)
    glyph))


(defparameter *fixed-font-art*
  '((#\Space "        " "        " "        " "        "
             "        " "        " "        " "        ")
    (#\! "  ##    " "  ##    " "  ##    " "  ##    "
         "  ##    " "        " "  ##    " "        ")
    (#\" " ## ##  " " ## ##  " "  #  #  " "        "
         "        " "        " "        " "        ")
    (#\# " ## ##  " " ## ##  " "####### " " ## ##  "
         "####### " " ## ##  " " ## ##  " "        ")
    (#\$ "  ##    " " #####  " "##      " " ####   "
         "    ##  " "#####   " "  ##    " "        ")
    (#\% "##   #  " "##  #   " "    #   " "   #    "
         "  #     " " #  ##  " "#   ##  " "        ")
    (#\& " ##     " "#  #    " "#  #    " " ##     "
         "#  # ## " "#   #   " " ### ## " "        ")
    (#\' "  ##    " "  ##    " "  #     " "        "
         "        " "        " "        " "        ")
    (#\( "   ##   " "  #     " " #      " " #      "
         " #      " "  #     " "   ##   " "        ")
    (#\) " ##     " "   #    " "    #   " "    #   "
         "    #   " "   #    " " ##     " "        ")
    (#\* "        " "  #  #  " "   ##   " " #####  "
         "   ##   " "  #  #  " "        " "        ")
    (#\+ "        " "   #    " "   #    " " #####  "
         "   #    " "   #    " "        " "        ")
    (#\, "        " "        " "        " "        "
         "  ##    " "  ##    " "  #     " "        ")
    (#\- "        " "        " "        " " #####  "
         "        " "        " "        " "        ")
    (#\. "        " "        " "        " "        "
         "        " "  ##    " "  ##    " "        ")
    (#\/ "      # " "     #  " "    #   " "   #    "
         "  #     " " #      " "#       " "        ")
    (#\0 "  ###   " " #   #  " " #  ##  " " # # #  "
         " ##  #  " " #   #  " "  ###   " "        ")
    (#\1 "   #    " "  ##    " "   #    " "   #    "
         "   #    " "   #    " " #####  " "        ")
    (#\2 "  ###   " " #   #  " "     #  " "   ##   "
         "  #     " " #      " " #####  " "        ")
    (#\3 "  ###   " " #   #  " "     #  " "   ##   "
         "     #  " " #   #  " "  ###   " "        ")
    (#\4 "    ##  " "   # #  " "  #  #  " " #   #  "
         " ###### " "     #  " "     #  " "        ")
    (#\5 " #####  " " #      " " ####   " "     #  "
         "     #  " " #   #  " "  ###   " "        ")
    (#\6 "  ###   " " #      " " #      " " ####   "
         " #   #  " " #   #  " "  ###   " "        ")
    (#\7 " #####  " "     #  " "    #   " "   #    "
         "  #     " "  #     " "  #     " "        ")
    (#\8 "  ###   " " #   #  " " #   #  " "  ###   "
         " #   #  " " #   #  " "  ###   " "        ")
    (#\9 "  ###   " " #   #  " " #   #  " "  ####  "
         "     #  " "     #  " "  ###   " "        ")
    (#\: "        " "  ##    " "  ##    " "        "
         "  ##    " "  ##    " "        " "        ")
    (#\; "        " "  ##    " "  ##    " "        "
         "  ##    " "  ##    " "  #     " "        ")
    (#\< "    #   " "   #    " "  #     " " #      "
         "  #     " "   #    " "    #   " "        ")
    (#\= "        " "        " " #####  " "        "
         " #####  " "        " "        " "        ")
    (#\> " #      " "  #     " "   #    " "    #   "
         "   #    " "  #     " " #      " "        ")
    (#\? "  ###   " " #   #  " "     #  " "   ##   "
         "   #    " "        " "   #    " "        ")
    (#\@ "  ###   " " #   #  " " # ###  " " # # #  "
         " # ###  " " #      " "  ####  " "        ")
    (#\A "  ###   " " #   #  " " #   #  " " #####  "
         " #   #  " " #   #  " " #   #  " "        ")
    (#\B " ####   " " #   #  " " #   #  " " ####   "
         " #   #  " " #   #  " " ####   " "        ")
    (#\C "  ###   " " #   #  " " #      " " #      "
         " #      " " #   #  " "  ###   " "        ")
    (#\D " ####   " " #   #  " " #   #  " " #   #  "
         " #   #  " " #   #  " " ####   " "        ")
    (#\E " #####  " " #      " " #      " " ####   "
         " #      " " #      " " #####  " "        ")
    (#\F " #####  " " #      " " #      " " ####   "
         " #      " " #      " " #      " "        ")
    (#\G "  ###   " " #   #  " " #      " " # ###  "
         " #   #  " " #   #  " "  ###   " "        ")
    (#\H " #   #  " " #   #  " " #   #  " " #####  "
         " #   #  " " #   #  " " #   #  " "        ")
    (#\I " #####  " "   #    " "   #    " "   #    "
         "   #    " "   #    " " #####  " "        ")
    (#\J "  ####  " "    #   " "    #   " "    #   "
         "    #   " " #  #   " "  ##    " "        ")
    (#\K " #   #  " " #  #   " " # #    " " ##     "
         " # #    " " #  #   " " #   #  " "        ")
    (#\L " #      " " #      " " #      " " #      "
         " #      " " #      " " #####  " "        ")
    (#\M " #   #  " " ## ##  " " # # #  " " # # #  "
         " #   #  " " #   #  " " #   #  " "        ")
    (#\N " #   #  " " ##  #  " " ##  #  " " # # #  "
         " #  ##  " " #  ##  " " #   #  " "        ")
    (#\O "  ###   " " #   #  " " #   #  " " #   #  "
         " #   #  " " #   #  " "  ###   " "        ")
    (#\P " ####   " " #   #  " " #   #  " " ####   "
         " #      " " #      " " #      " "        ")
    (#\Q "  ###   " " #   #  " " #   #  " " #   #  "
         " # # #  " " #  #   " "  ## #  " "        ")
    (#\R " ####   " " #   #  " " #   #  " " ####   "
         " # #    " " #  #   " " #   #  " "        ")
    (#\S "  ###   " " #   #  " " #      " "  ###   "
         "     #  " " #   #  " "  ###   " "        ")
    (#\T " #####  " "   #    " "   #    " "   #    "
         "   #    " "   #    " "   #    " "        ")
    (#\U " #   #  " " #   #  " " #   #  " " #   #  "
         " #   #  " " #   #  " "  ###   " "        ")
    (#\V " #   #  " " #   #  " " #   #  " " #   #  "
         " #   #  " "  # #   " "   #    " "        ")
    (#\W " #   #  " " #   #  " " #   #  " " # # #  "
         " # # #  " " ## ##  " " #   #  " "        ")
    (#\X " #   #  " " #   #  " "  # #   " "   #    "
         "  # #   " " #   #  " " #   #  " "        ")
    (#\Y " #   #  " " #   #  " "  # #   " "   #    "
         "   #    " "   #    " "   #    " "        ")
    (#\Z " #####  " "     #  " "    #   " "   #    "
         "  #     " " #      " " #####  " "        ")
    (#\[ "  ###   " "  #     " "  #     " "  #     "
         "  #     " "  #     " "  ###   " "        ")
    (#\\ "#       " " #      " "  #     " "   #    "
         "    #   " "     #  " "      # " "        ")
    (#\] "  ###   " "    #   " "    #   " "    #   "
         "    #   " "    #   " "  ###   " "        ")
    (#\^ "   #    " "  # #   " " #   #  " "        "
         "        " "        " "        " "        ")
    (#\_ "        " "        " "        " "        "
         "        " "        " " #####  " "        ")
    (#\` " ##     " "  ##    " "   #    " "        "
         "        " "        " "        " "        ")
    (#\a "        " "        " "  ###   " "     #  "
         "  ####  " " #   #  " "  ####  " "        ")
    (#\b " #      " " #      " " ####   " " #   #  "
         " #   #  " " #   #  " " ####   " "        ")
    (#\c "        " "        " "  ###   " " #      "
         " #      " " #      " "  ###   " "        ")
    (#\d "     #  " "     #  " "  ####  " " #   #  "
         " #   #  " " #   #  " "  ####  " "        ")
    (#\e "        " "        " "  ###   " " #   #  "
         " #####  " " #      " "  ###   " "        ")
    (#\f "   ##   " "  #     " "  #     " " ####   "
         "  #     " "  #     " "  #     " "        ")
    (#\g "        " "        " "  ####  " " #   #  "
         " #   #  " "  ####  " "     #  " "  ###   ")
    (#\h " #      " " #      " " ####   " " #   #  "
         " #   #  " " #   #  " " #   #  " "        ")
    (#\i "   #    " "        " "  ##    " "   #    "
         "   #    " "   #    " " #####  " "        ")
    (#\j "    #   " "        " "   ##   " "    #   "
         "    #   " "    #   " " #  #   " "  ##    ")
    (#\k " #      " " #      " " #  #   " " # #    "
         " ##     " " # #    " " #  #   " "        ")
    (#\l "  ##    " "   #    " "   #    " "   #    "
         "   #    " "   #    " " #####  " "        ")
    (#\m "        " "        " " ## #   " " # # #  "
         " # # #  " " # # #  " " # # #  " "        ")
    (#\n "        " "        " " ####   " " #   #  "
         " #   #  " " #   #  " " #   #  " "        ")
    (#\o "        " "        " "  ###   " " #   #  "
         " #   #  " " #   #  " "  ###   " "        ")
    (#\p "        " "        " " ####   " " #   #  "
         " #   #  " " ####   " " #      " " #      ")
    (#\q "        " "        " "  ####  " " #   #  "
         " #   #  " "  ####  " "     #  " "     #  ")
    (#\r "        " "        " " # ##   " " ##     "
         " #      " " #      " " #      " "        ")
    (#\s "        " "        " "  ###   " " #      "
         "  ###   " "     #  " " ####   " "        ")
    (#\t "  #     " "  #     " " ####   " "  #     "
         "  #     " "  #  #  " "   ##   " "        ")
    (#\u "        " "        " " #   #  " " #   #  "
         " #   #  " " #   #  " "  ####  " "        ")
    (#\v "        " "        " " #   #  " " #   #  "
         " #   #  " "  # #   " "   #    " "        ")
    (#\w "        " "        " " #   #  " " #   #  "
         " # # #  " " # # #  " "  # #   " "        ")
    (#\x "        " "        " " #   #  " "  # #   "
         "   #    " "  # #   " " #   #  " "        ")
    (#\y "        " "        " " #   #  " " #   #  "
         " #   #  " "  ####  " "     #  " "  ###   ")
    (#\z "        " "        " " #####  " "    #   "
         "   #    " "  #     " " #####  " "        ")
    (#\{ "   ##   " "  #     " "  #     " " #      "
         "  #     " "  #     " "   ##   " "        ")
    (#\| "   #    " "   #    " "   #    " "   #    "
         "   #    " "   #    " "   #    " "        ")
    (#\} " ##     " "   #    " "   #    " "    #   "
         "   #    " "   #    " " ##     " "        ")
    (#\~ "        " " ##  #  " "#  ##   " "        "
         "        " "        " "        " "        "))
  "ASCII-art source for the built-in 8x8 programming font.")


(-> make-fixed-font () bitmap-font)
(defun make-fixed-font ()
  "Return the built-in 8x8 programming font covering printable ASCII."
  (let ((font (make-bitmap-font :name "fixed-8x8" :width 8 :height 8 :ascent 7)))
    (dolist (entry *fixed-font-art*)
      (install-glyph font (first entry) (rest entry)))
    font))


(defparameter *fixed-font* (make-fixed-font)
  "The default 8x8 programming font.")
