(in-package #:lispbsd)

;;;; -- Bitmap Fonts --

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
    :documentation "Hash table mapping characters to glyph bitmaps.")
   (bitmap-font-missing
    :initarg :missing
    :reader bitmap-font-missing
    :documentation "Glyph drawn for characters with no defined bitmap."))
  (:documentation
   "An inspectable bitmap font used by the 1-bit display."))


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
  "Return a hollow box glyph of WIDTH by HEIGHT."
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
  "Return a bitmap font with the supplied GLYPHS table."
  (make-instance 'bitmap-font
                 :name name
                 :width width
                 :height height
                 :ascent ascent
                 :glyphs (or glyphs (make-hash-table :test 'eql))
                 :missing (or missing (make-missing-glyph width height))))


(-> font-glyph (bitmap-font character) bitmap)
(defun font-glyph (font character)
  "Return the glyph bitmap for CHARACTER in FONT."
  (or (gethash character (bitmap-font-glyphs font))
      (bitmap-font-missing font)))


(-> font-text-width (bitmap-font string) integer)
(defun font-text-width (font string)
  "Return the pixel width of STRING in FONT."
  (* (length string) (bitmap-font-width font)))


(-> bitmap-draw-text (bitmap bitmap-font string &key (:x integer) (:y integer)
                            (:bit bit))
    bitmap)
(defun bitmap-draw-text (bitmap font string &key (x 0) (y 0) (bit 1))
  "Draw STRING in FONT onto BITMAP at (X, Y). Glyphs are clipped. Returns BITMAP."
  (let ((cell-width (bitmap-font-width font)))
    (loop for index from 0 below (length string)
          for character = (char string index)
          for glyph = (font-glyph font character)
          for dest-x = (+ x (* index cell-width))
          do (if (eql bit 1)
                 (bitblt glyph bitmap :dx dest-x :dy y :operation ':ior)
                 (let ((inverted (bitmap-copy glyph)))
                   (bitmap-fill inverted :operation ':not)
                   (bitblt inverted bitmap :dx dest-x :dy y :operation ':and)))))
  bitmap)


(-> install-glyph (bitmap-font character list) bitmap)
(defun install-glyph (font character rows)
  "Define CHARACTER in FONT from ASCII-art ROWS and return the glyph."
  (let ((glyph (art->glyph rows)))
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
