(in-package #:lispbsd)

;;;; -- Bitmap Files and Screenshots --

(define-condition invalid-bitmap-file (bitmap-error)
  ((invalid-bitmap-file-path
    :initarg :path
    :reader invalid-bitmap-file-path
    :documentation "The file that could not be read.")
   (invalid-bitmap-file-reason
    :initarg :reason
    :reader invalid-bitmap-file-reason
    :documentation "Human-readable description of the defect."))
  (:report (lambda (condition stream)
             (format stream "Invalid bitmap file ~A: ~A."
                     (invalid-bitmap-file-path condition)
                     (invalid-bitmap-file-reason condition))))
  (:documentation "A bitmap file was malformed or truncated."))


(-> bitmap-write-pbm (bitmap (or pathname string)) pathname)
(defun bitmap-write-pbm (bitmap path)
  "Write BITMAP to PATH as a binary PBM (P4) image. Returns the true pathname.

Shades of at least half ink become 1. Rows are packed most significant
bit first and padded to whole bytes."
  (with-open-file (stream path :direction ':output
                               :element-type '(unsigned-byte 8)
                               :if-exists ':supersede
                               :if-does-not-exist ':create)
    (let* ((width (bitmap-width bitmap))
           (height (bitmap-height bitmap))
           (header (format nil "P4~%~A ~A~%" width height))
           (row-bytes (ceiling width 8))
           (bits (bitmap-bits bitmap)))
      (loop for character across header
            do (write-byte (char-code character) stream))
      (dotimes (y height)
        (dotimes (byte-index row-bytes)
          (let ((byte 0))
            (dotimes (bit-index 8)
              (let ((x (+ (* byte-index 8) bit-index)))
                (when (and (< x width)
                           (>= (aref bits y x) 128))
                  (setf byte (logior byte (ash 1 (- 7 bit-index)))))))
            (write-byte byte stream))))))
  (truename path))


(-> bitmap-read-pbm ((or pathname string)) bitmap)
(defun bitmap-read-pbm (path)
  "Read a binary PBM (P4) image from PATH into a fresh bitmap.

Header comments and whitespace are handled per the PBM format. Signals
INVALID-BITMAP-FILE for malformed or truncated files."
  (with-open-file (stream path :direction ':input
                               :element-type '(unsigned-byte 8))
    (labels ((malformed (reason)
               (error 'invalid-bitmap-file :path path :reason reason))

             (next-byte ()
               (let ((byte (read-byte stream nil nil)))
                 (unless byte
                   (malformed "truncated file"))
                 byte))

             (whitespace-byte-p (byte)
               (and (member byte '(9 10 13 32)) t))

             (skip-to-token ()
               (loop for byte = (next-byte)
                     do (cond ((whitespace-byte-p byte)
                               nil)
                              ((= byte (char-code #\#))
                               (loop for comment-byte = (next-byte)
                                     until (= comment-byte 10)))
                              (t
                               (return byte)))))

             (read-number ()
               (let ((byte (skip-to-token))
                     (value 0))
                 (unless (<= 48 byte 57)
                   (malformed "expected a decimal number"))
                 (loop while (<= 48 byte 57)
                       do (setf value (+ (* value 10) (- byte 48)))
                          (setf byte (next-byte)))
                 (unless (whitespace-byte-p byte)
                   (malformed "expected whitespace after a number"))
                 value)))
      (unless (and (= (next-byte) (char-code #\P))
                   (= (next-byte) (char-code #\4)))
        (malformed "not a binary PBM (P4) file"))
      (let* ((width (read-number))
             (height (read-number))
             (bitmap (make-bitmap width height))
             (bits (bitmap-bits bitmap))
             (row-bytes (ceiling width 8)))
        (dotimes (y height)
          (dotimes (byte-index row-bytes)
            (let ((byte (next-byte)))
              (dotimes (bit-index 8)
                (let ((x (+ (* byte-index 8) bit-index)))
                  (when (< x width)
                    (setf (aref bits y x)
                          (* 255 (ldb (byte 1 (- 7 bit-index)) byte)))))))))
        bitmap))))


(-> bitmap-write-pgm (bitmap (or pathname string)) pathname)
(defun bitmap-write-pgm (bitmap path)
  "Write BITMAP to PATH as a binary PGM (P5) image with full shades.

PGM's 255 is white, so shades are inverted on the way out: ink writes
as 0 and paper as 255. Returns the true pathname."
  (with-open-file (stream path :direction ':output
                               :element-type '(unsigned-byte 8)
                               :if-exists ':supersede
                               :if-does-not-exist ':create)
    (let* ((width (bitmap-width bitmap))
           (height (bitmap-height bitmap))
           (header (format nil "P5~%~A ~A~%255~%" width height))
           (bits (bitmap-bits bitmap)))
      (loop for character across header
            do (write-byte (char-code character) stream))
      (dotimes (y height)
        (dotimes (x width)
          (write-byte (- 255 (aref bits y x)) stream)))))
  (truename path))


(-> bitmap-read-pgm ((or pathname string)) bitmap)
(defun bitmap-read-pgm (path)
  "Read a binary PGM (P5) image from PATH into a fresh bitmap.

Gray values are inverted back into shades: 0 reads as full ink."
  (with-open-file (stream path :direction ':input
                               :element-type '(unsigned-byte 8))
    (labels ((malformed (reason)
               (error 'invalid-bitmap-file :path path :reason reason))

             (next-byte ()
               (let ((byte (read-byte stream nil nil)))
                 (unless byte
                   (malformed "truncated file"))
                 byte))

             (whitespace-byte-p (byte)
               (and (member byte '(9 10 13 32)) t))

             (read-number ()
               (let ((byte (next-byte))
                     (value 0))
                 (loop while (whitespace-byte-p byte)
                       do (setf byte (next-byte)))
                 (unless (<= 48 byte 57)
                   (malformed "expected a decimal number"))
                 (loop while (<= 48 byte 57)
                       do (setf value (+ (* value 10) (- byte 48)))
                          (setf byte (next-byte)))
                 (unless (whitespace-byte-p byte)
                   (malformed "expected whitespace after a number"))
                 value)))
      (unless (and (= (next-byte) (char-code #\P))
                   (= (next-byte) (char-code #\5)))
        (malformed "not a binary PGM (P5) file"))
      (let* ((width (read-number))
             (height (read-number))
             (maximum (read-number))
             (bitmap (make-bitmap width height))
             (bits (bitmap-bits bitmap)))
        (unless (= maximum 255)
          (malformed "unsupported gray range"))
        (dotimes (y height)
          (dotimes (x width)
            (setf (aref bits y x) (- 255 (next-byte)))))
        bitmap))))


(-> desktop-screenshot (desktop (or pathname string)) pathname)
(defun desktop-screenshot (desktop path)
  "Compose DESKTOP and write its screen to PATH as a PGM image.

PGM carries the full shade range, so antialiased edges survive."
  (desktop-compose desktop)
  (bitmap-write-pgm (desktop-screen desktop) path))
