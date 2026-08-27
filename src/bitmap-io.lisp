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

PBM marks ink as 1, matching the bitmap convention. Rows are packed
most significant bit first and padded to whole bytes."
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
                           (plusp (aref bits y x)))
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
                          (ldb (byte 1 (- 7 bit-index)) byte))))))))
        bitmap))))


(-> desktop-screenshot (desktop (or pathname string)) pathname)
(defun desktop-screenshot (desktop path)
  "Compose DESKTOP and write its screen to PATH as a PBM image."
  (desktop-compose desktop)
  (bitmap-write-pbm (desktop-screen desktop) path))
