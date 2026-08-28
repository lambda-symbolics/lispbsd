(in-package #:lispbsd)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'sb-posix))

;;;; -- NetBSD Framebuffer Display --

;; Request codes from sys/dev/wscons/wsconsio.h, encoded per NetBSD's
;; _IOR and _IOW for group W.
(defparameter *wsdisplayio-ginfo*
  (logior #x40000000 (ash 16 16) (ash (char-code #\W) 8) 65)
  "WSDISPLAYIO_GINFO: read a struct wsdisplay_fbinfo.")

(defparameter *wsdisplayio-linebytes*
  (logior #x40000000 (ash 4 16) (ash (char-code #\W) 8) 95)
  "WSDISPLAYIO_LINEBYTES: read the framebuffer row stride.")

(defparameter *wsdisplayio-smode*
  (logior #x80000000 (ash 4 16) (ash (char-code #\W) 8) 76)
  "WSDISPLAYIO_SMODE: set the display mode.")

(defparameter *wsdisplayio-mode-emul* 0
  "Text emulation display mode.")

(defparameter *wsdisplayio-mode-dumbfb* 2
  "Mapped dumb framebuffer display mode.")


(define-condition display-error (lispbsd-error)
  ((display-error-device
    :initarg :device
    :initform nil
    :reader display-error-device
    :documentation "The display device involved in the failure.")
   (display-error-detail
    :initarg :detail
    :initform nil
    :reader display-error-detail
    :documentation "Substrate error detail, if any."))
  (:report (lambda (condition stream)
             (format stream "Display failure on ~A~@[: ~A~]."
                     (display-error-device condition)
                     (display-error-detail condition))))
  (:documentation "A failure involving a physical display."))


(sb-alien:define-alien-routine ("ioctl" wsdisplay--ioctl) sb-alien:int
  (file-descriptor sb-alien:int)
  (request sb-alien:unsigned-long)
  (argument (* t)))


(defclass wsdisplay-backend ()
  ((wsdisplay-backend-device
    :initarg :device
    :reader wsdisplay-backend-device
    :type string
    :documentation "Path of the wsdisplay terminal device.")
   (wsdisplay-backend-descriptor
    :initarg :descriptor
    :reader wsdisplay-backend-descriptor
    :documentation "Open file descriptor of the device.")
   (wsdisplay-backend-width
    :initarg :width
    :reader wsdisplay-backend-width
    :documentation "Framebuffer width in pixels.")
   (wsdisplay-backend-height
    :initarg :height
    :reader wsdisplay-backend-height
    :documentation "Framebuffer height in pixels.")
   (wsdisplay-backend-depth
    :initarg :depth
    :reader wsdisplay-backend-depth
    :documentation "Framebuffer depth in bits per pixel.")
   (wsdisplay-backend-line-bytes
    :initarg :line-bytes
    :reader wsdisplay-backend-line-bytes
    :documentation "Bytes per framebuffer row.")
   (wsdisplay-backend-framebuffer
    :initarg :framebuffer
    :reader wsdisplay-backend-framebuffer
    :documentation "System area pointer to the mapped framebuffer."))
  (:documentation
   "The native NetBSD display: a mapped wsdisplay framebuffer.

The canonical 1-bit desktop is expanded to the framebuffer depth on
present; ink is black and paper is white."))


(-> make-wsdisplay-backend (&key (:device string)) wsdisplay-backend)
(defun make-wsdisplay-backend (&key (device "/dev/ttyE0"))
  "Map the framebuffer behind DEVICE and return a display backend.

The display is switched to dumb framebuffer mode. Only 32 bits per
pixel framebuffers are supported; anything else signals
DISPLAY-ERROR."
  (let ((descriptor (handler-case
                        (wsdisplay--open device)
                      (error (condition)
                        (error 'display-error
                               :device device
                               :detail (princ-to-string condition))))))
    (wsdisplay--set-mode descriptor device *wsdisplayio-mode-dumbfb*)
    (multiple-value-bind (width height depth)
        (wsdisplay--framebuffer-info descriptor device)
      (unless (= depth 32)
        (wsdisplay--set-mode descriptor device *wsdisplayio-mode-emul*)
        (error 'display-error
               :device device
               :detail (format nil "unsupported depth ~A" depth)))
      (let* ((line-bytes (wsdisplay--line-bytes descriptor device))
             (framebuffer
               (handler-case
                   (sb-posix:mmap nil
                                  (* line-bytes height)
                                  (logior sb-posix:prot-read
                                          sb-posix:prot-write)
                                  sb-posix:map-shared
                                  descriptor
                                  0)
                 (error (condition)
                   (wsdisplay--set-mode descriptor device
                                        *wsdisplayio-mode-emul*)
                   (error 'display-error
                          :device device
                          :detail (princ-to-string condition))))))
        (make-instance 'wsdisplay-backend
                       :device device
                       :descriptor descriptor
                       :width width
                       :height height
                       :depth depth
                       :line-bytes line-bytes
                       :framebuffer framebuffer)))))


(defmethod display-backend-size ((backend wsdisplay-backend))
  (values (wsdisplay-backend-width backend)
          (wsdisplay-backend-height backend)))

(defmethod display-backend-present ((backend wsdisplay-backend) bitmap)
  (let ((framebuffer (wsdisplay-backend-framebuffer backend))
        (line-bytes (wsdisplay-backend-line-bytes backend))
        (bits (bitmap-bits bitmap))
        (width (min (bitmap-width bitmap)
                    (wsdisplay-backend-width backend)))
        (height (min (bitmap-height bitmap)
                     (wsdisplay-backend-height backend))))
    (dotimes (y height)
      (let ((row-offset (* y line-bytes)))
        (dotimes (x width)
          (setf (sb-sys:sap-ref-32 framebuffer (+ row-offset (* x 4)))
                (if (plusp (aref bits y x))
                    #x00000000
                    #x00FFFFFF)))))
    bitmap))

(defmethod display-backend-poll-events ((backend wsdisplay-backend))
  nil)

(defmethod display-backend-close ((backend wsdisplay-backend))
  (ignore-errors
    (sb-posix:munmap (wsdisplay-backend-framebuffer backend)
                     (* (wsdisplay-backend-line-bytes backend)
                        (wsdisplay-backend-height backend))))
  (ignore-errors
    (wsdisplay--set-mode (wsdisplay-backend-descriptor backend)
                         (wsdisplay-backend-device backend)
                         *wsdisplayio-mode-emul*))
  (ignore-errors
    (sb-posix:close (wsdisplay-backend-descriptor backend)))
  backend)


(-> wsdisplay--open (string) integer)
(defun wsdisplay--open (device)
  "Open DEVICE, configuring its wscons screen first when needed.

With a serial console no virtual screens exist until wsconscfg creates
one, so a failed open is retried once after configuring screen 0."
  (handler-case
      (sb-posix:open device sb-posix:o-rdwr)
    (sb-posix:syscall-error ()
      (sb-ext:run-program "/usr/sbin/wsconscfg" (list "0")
                          :output nil
                          :error nil
                          :wait t)
      (sb-posix:open device sb-posix:o-rdwr))))


(-> wsdisplay--set-mode (integer string integer) integer)
(defun wsdisplay--set-mode (descriptor device mode)
  "Set the wsdisplay MODE on DESCRIPTOR."
  (sb-alien:with-alien ((value sb-alien:unsigned-int))
    (setf value mode)
    (when (minusp (wsdisplay--ioctl descriptor *wsdisplayio-smode*
                                    (sb-alien:addr value)))
      (error 'display-error
             :device device
             :detail "WSDISPLAYIO_SMODE failed")))
  mode)


(-> wsdisplay--framebuffer-info (integer string)
    (values integer integer integer))
(defun wsdisplay--framebuffer-info (descriptor device)
  "Return the width, height, and depth reported by DESCRIPTOR."
  (sb-alien:with-alien ((info (sb-alien:array sb-alien:unsigned-int 4)))
    (when (minusp (wsdisplay--ioctl descriptor *wsdisplayio-ginfo*
                                    (sb-alien:addr info)))
      (error 'display-error
             :device device
             :detail "WSDISPLAYIO_GINFO failed"))
    ;; struct wsdisplay_fbinfo: height, width, depth, cmsize.
    (values (sb-alien:deref info 1)
            (sb-alien:deref info 0)
            (sb-alien:deref info 2))))


(-> wsdisplay--line-bytes (integer string) integer)
(defun wsdisplay--line-bytes (descriptor device)
  "Return the framebuffer row stride reported by DESCRIPTOR."
  (sb-alien:with-alien ((value sb-alien:unsigned-int))
    (when (minusp (wsdisplay--ioctl descriptor *wsdisplayio-linebytes*
                                    (sb-alien:addr value)))
      (error 'display-error
             :device device
             :detail "WSDISPLAYIO_LINEBYTES failed"))
    value))
