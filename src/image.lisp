(in-package #:lispbsd)

;;;; -- Heap Image Building --

(define-condition image-error (lispbsd-error)
  ((image-error-core-path
    :initarg :core-path
    :initform nil
    :reader image-error-core-path
    :documentation "The core file involved in the failure, if any."))
  (:documentation "A failure involving heap image construction."))


(define-condition image-build-failed (image-error)
  ((image-build-failed-output
    :initarg :output
    :reader image-build-failed-output
    :documentation "Output of the failed child build."))
  (:report (lambda (condition stream)
             (format stream "Heap image build failed:~%~A"
                     (image-build-failed-output condition))))
  (:documentation "The child Lisp could not build the requested image."))


(-> build-world-image (&key (:core-path (or pathname string))
                           (:source-root (or pathname string))
                           (:journal-path (option (or pathname string))))
    pathname)
(defun build-world-image (&key core-path
                          (source-root (asdf:system-source-directory
                                        '#:lispbsd))
                          journal-path)
  "Build a bootable heap image at CORE-PATH without disturbing this image.

A fresh child Lisp loads the system from SOURCE-ROOT, replays the
durable mutations of the journal at JOURNAL-PATH when one is given,
and saves its heap. Returns the true pathname of the saved core."
  (let ((script-path (merge-pathnames
                      (format nil "lispbsd-image-~A.lisp" (make-object-id))
                      (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (image--write-build-script script-path
                                      :core-path core-path
                                      :source-root source-root
                                      :journal-path journal-path)
           (multiple-value-bind (output error-output exit-code)
               (uiop:run-program (list (namestring sb-ext:*runtime-pathname*)
                                       "--non-interactive"
                                       "--no-sysinit"
                                       "--no-userinit"
                                       "--load" (namestring script-path))
                                 :output ':string
                                 :error-output ':output
                                 :ignore-error-status t)
             (declare (ignore error-output))
             (unless (and (zerop exit-code) (probe-file core-path))
               (error 'image-build-failed
                      :core-path core-path
                      :output output))))
      (ignore-errors (delete-file script-path)))
    (truename core-path)))


(-> image--write-build-script (pathname &key (:core-path (or pathname string))
                                            (:source-root (or pathname string))
                                            (:journal-path
                                             (option (or pathname string))))
    pathname)
(defun image--write-build-script (script-path &key core-path source-root
                                  journal-path)
  "Write the child build script for BUILD-WORLD-IMAGE to SCRIPT-PATH."
  (with-open-file (out script-path :direction ':output
                                   :if-exists ':supersede
                                   :if-does-not-exist ':create)
    ;; The child reads in CL-USER, so print with the keyword package
    ;; current to force full package qualification on every symbol.
    (let ((*package* (find-package '#:keyword))
          (*print-readably* t))
      (dolist (form
               (append
                (list '(require "asdf")
                      `(asdf:load-asd ,(namestring
                                        (merge-pathnames "lispbsd.asd"
                                                         (uiop:ensure-directory-pathname
                                                          source-root))))
                      '(asdf:load-system "lispbsd"))
                (when journal-path
                  (list `(journal-replay
                          (make-journal :path ,(namestring journal-path))
                          (make-sbcl-runtime))))
                (list `(sb-ext:save-lisp-and-die ,(namestring core-path)))))
        (prin1 form out)
        (terpri out))))
  script-path)
