;;;; Host-side driver for a LispBSD QEMU guest serial console.
;;;;
;;;; Usage: script/guest command...
;;;;        script/guest -c 'shell snippet'

(require "asdf")
(require "sb-bsd-sockets")

(defparameter *guest-prompt* "LISPBSD# "
  "Prompt installed after login so command output can be framed.")

(defparameter *guest-serial-log* nil
  "Stream receiving a raw copy of everything read from the serial console.")

(defparameter *default-kernel*
  "/root/common-lisp/refs/netbsd-obj/sys/arch/amd64/compile/LISPBSD/netbsd"
  "Default Multiboot kernel built by script/netbsd-build.")

(define-condition guest-error (error)
  ((guest-error-message
    :initarg :message
    :reader guest-error-message))
  (:report (lambda (condition stream)
             (write-string (guest-error-message condition) stream))))

(defun guest-argv ()
  "Return user arguments after sbcl --script guest.lisp."
  (let ((args sb-ext:*posix-argv*))
    (when (and args (search "sbcl" (first args)))
      (setf args (rest args)))
    (when (and args (string= (first args) "--script"))
      (setf args (rest args)))
    (when (and args (search "guest.lisp" (first args)))
      (setf args (rest args)))
    args))

(defun guest-repository-root ()
  "Return the LispBSD repository root containing this script."
  (let ((script (or *load-truename* *load-pathname*)))
    (unless script
      (error 'guest-error :message "cannot locate guest.lisp"))
    (truename (merge-pathnames "../" (make-pathname :name nil :type nil
                                                    :defaults script)))))

(defun guest-getenv (name &optional default)
  "Return environment variable NAME, or DEFAULT when unset or empty."
  (let ((value (uiop:getenv name)))
    (if (and value (plusp (length value)))
        value
        default)))

(defun guest-parse-arguments (arguments)
  "Parse ARGUMENTS into a guest command string."
  (cond
    ((null arguments)
     (let ((text (string-trim '(#\Space #\Tab #\Newline)
                              (uiop:slurp-stream-string *standard-input*))))
       (when (zerop (length text))
         (error 'guest-error :message "usage: script/guest command..."))
       text))
    ((string= (first arguments) "-c")
     (unless (rest arguments)
       (error 'guest-error :message "usage: script/guest -c command"))
     (format nil "~{~A~^ ~}" (rest arguments)))
    ((string= (first arguments) "--send")
     (let ((path (second arguments)))
       (unless (and path (probe-file path))
         (error 'guest-error :message "usage: script/guest --send file"))
       (uiop:read-file-string path)))
    (t
     (format nil "~{~A~^ ~}" arguments))))

(defun guest-host-address (host)
  "Return the IPv4 address vector for HOST."
  (sb-bsd-sockets:host-ent-address (sb-bsd-sockets:get-host-by-name host)))

(defun guest-open-serial (host port &key (attempts 40))
  "Connect to the QEMU serial TCP port, retrying until it listens."
  (loop for attempt from 1 to attempts do
    (handler-case
        (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                                     :type :stream :protocol :tcp)))
          (sb-bsd-sockets:socket-connect socket (guest-host-address host) port)
          (return (sb-bsd-sockets:socket-make-stream socket
                                                     :input t
                                                     :output t
                                                     :element-type 'character
                                                     :buffering :none)))
      (error (condition)
        (when (= attempt attempts)
          (error 'guest-error
                 :message (format nil "cannot connect to ~A:~A: ~A"
                                  host port condition)))
        (sleep 0.5)))))

(defun guest-send (stream string)
  "Write STRING to STREAM line by line, each ended by a carriage return.

Lines are paced so multi-line commands never overrun the guest
terminal's input buffer."
  (let ((start 0))
    (loop
      (let ((newline (position #\Newline string :start start)))
        (write-string string stream :start start
                                    :end (or newline (length string)))
        (write-char #\Return stream)
        (force-output stream)
        (unless newline
          (return))
        (setf start (1+ newline))
        (sleep 0.1)))))

(defun guest-read (stream timeout)
  "Read available characters from STREAM, waiting up to TIMEOUT seconds."
  (let ((fd (sb-sys:fd-stream-fd stream)))
    (when (sb-sys:wait-until-fd-usable fd :input timeout)
      (loop while (listen stream)
            for character = (read-char-no-hang stream nil nil)
            while character
            collect character into characters
            finally (let ((chunk (coerce characters 'string)))
                      (when *guest-serial-log*
                        (write-string chunk *guest-serial-log*)
                        (force-output *guest-serial-log*))
                      (return chunk))))))

(defun guest-buffer-ends-with-p (buffer pattern)
  "Return true when BUFFER ends with PATTERN."
  (let ((buffer-length (length buffer))
        (pattern-length (length pattern)))
    (and (>= buffer-length pattern-length)
         (string= buffer pattern :start1 (- buffer-length pattern-length)))))

(defun guest-wait-for-any (stream patterns timeout)
  "Accumulate STREAM output until it ends quietly with one of PATTERNS.

A prompt only counts when it is the suffix of the stream and no more
output follows shortly, so prompt-like text inside boot noise or
command echo cannot satisfy the wait."
  (let ((buffer (make-array 0 :element-type 'character :adjustable t :fill-pointer 0))
        (deadline (+ (get-internal-real-time)
                     (floor (* timeout internal-time-units-per-second)))))
    (flet ((append-chunk (chunk)
             (loop for character across chunk do
               (vector-push-extend character buffer))))
      (loop
        (let ((remaining (/ (- deadline (get-internal-real-time))
                            internal-time-units-per-second)))
          (when (<= remaining 0)
            (error 'guest-error
                   :message (format nil "timed out waiting for ~S; saw:~%~A"
                                    patterns buffer)))
          (let ((chunk (guest-read stream (max 0.1 (min 1 remaining)))))
            (when (and chunk (plusp (length chunk)))
              (append-chunk chunk))
            (when (find-if (lambda (pattern)
                             (guest-buffer-ends-with-p buffer pattern))
                           patterns)
              (let ((more (guest-read stream 0.3)))
                (if (and more (plusp (length more)))
                    (append-chunk more)
                    (return (copy-seq buffer)))))))))))

(defun guest-wait-for (stream pattern timeout)
  "Accumulate STREAM output until PATTERN appears."
  (guest-wait-for-any stream (list pattern) timeout))

(defun guest-start-qemu (image kernel memory host port)
  "Start QEMU for IMAGE and return the process."
  (let* ((root (guest-repository-root))
         (vm-run (merge-pathnames "script/vm-run" root))
         (pkg-disk (guest-getenv "PKG_DISK"))
         (environment (append (list (format nil "MEMORY=~A" memory)
                                    (format nil "SERIAL=tcp:~A:~A,server,nowait"
                                            host port))
                              (when (and kernel (probe-file kernel))
                                (list (format nil "KERNEL=~A"
                                              (namestring (truename kernel)))))
                              ;; Values resolved here override the inherited
                              ;; environment; KERNEL=disk must not leak into
                              ;; vm-run as a kernel path.
                              (remove-if (lambda (entry)
                                           (or (uiop:string-prefix-p "MEMORY=" entry)
                                               (uiop:string-prefix-p "SERIAL=" entry)
                                               (uiop:string-prefix-p "KERNEL=" entry)))
                                         (sb-ext:posix-environ))))
         (command (append (list (namestring vm-run) (namestring image))
                          (when (and pkg-disk (probe-file pkg-disk))
                            (list "-drive"
                                  (format nil "file=~A,format=raw,index=1"
                                          (namestring (truename pkg-disk))))))))
    (uiop:launch-program command
                         :environment environment
                         :output (merge-pathnames "vm/qemu-guest.log" root)
                         :error-output :output)))

(defun guest-lisp-prompt-p (text)
  "Return true when TEXT ends at a Lisp REPL or debugger prompt."
  (or (guest-buffer-ends-with-p text "* ")
      (guest-buffer-ends-with-p text "] ")))

(defun guest-login (stream)
  "Reach a root shell or Lisp listener on STREAM. Return :unix or :lisp."
  (let ((text (guest-wait-for-any stream (list "login: " "* " "] " "/bin/sh: ") 120)))
    (cond
      ((and (guest-lisp-prompt-p text)
            (not (search "login:" text))
            (not (search "pathname of shell" text)))
       :lisp)
      ((search "pathname of shell" text)
       (guest-send stream "")
       (let ((after (guest-wait-for-any stream (list "# " "$ " "* " "] ") 60)))
         (if (guest-lisp-prompt-p after)
             :lisp
             (progn
               (guest-send stream (format nil "export PS1='~A'" *guest-prompt*))
               (guest-wait-for stream *guest-prompt* 15)
               :unix))))
      (t
       (guest-send stream "root")
       (let ((after (guest-wait-for-any stream (list "Password:" "# " "$ " "* " "] ") 600)))
         (when (search "Password:" after)
           (guest-send stream "")
           (setf after (guest-wait-for-any stream (list "# " "$ " "* " "] ") 600)))
         (cond
           ((guest-lisp-prompt-p after)
            :lisp)
           (t
            (guest-send stream (format nil "export PS1='~A'" *guest-prompt*))
            (guest-wait-for stream *guest-prompt* 15)
              :unix)))))))

(defun guest-run-command (stream command &key (timeout 300) (console :unix))
  "Run COMMAND at the guest prompt and return framed output."
  (if (eq console :lisp)
      (progn
        (guest-send stream command)
        (let* ((text (guest-wait-for-any stream (list "* " "] ") timeout))
               (echo-end (let ((cr (position #\Return text)))
                           (if cr (1+ cr) 0)))
               (prompt-start (position #\Newline text :from-end t)))
          (string-trim '(#\Space #\Tab #\Return #\Newline)
                       (subseq text echo-end (or prompt-start (length text))))))
      (progn
        (guest-send stream command)
        (let* ((text (guest-wait-for stream *guest-prompt* timeout))
               (echo-end (let ((cr (position #\Return text)))
                           (if cr (1+ cr) 0)))
               (prompt-start (search *guest-prompt* text :from-end t)))
          (string-trim '(#\Space #\Tab #\Return #\Newline)
                       (subseq text echo-end (or prompt-start (length text))))))))

(defun guest-shutdown (stream process &key (console :unix))
  "Halt the guest and wait for QEMU to exit."
  (ignore-errors
    (if (eq console :lisp)
        (guest-send stream "(sb-ext:exit :abort t)")
        (guest-send stream "shutdown -p now")))
  (let ((deadline (+ (get-internal-real-time)
                     (* 30 internal-time-units-per-second))))
    (loop until (not (uiop:process-alive-p process))
          do (when (>= (get-internal-real-time) deadline)
               (uiop:terminate-process process :urgent t)
               (return))
             (sleep 0.5)))
  (ignore-errors (uiop:wait-process process))
  t)

(defun guest-main (&optional (arguments (guest-argv)))
  "Boot the guest, run the requested command, and halt."
  (let* ((command (guest-parse-arguments arguments))
         (root (guest-repository-root))
         (image (pathname (guest-getenv "GUEST_IMAGE"
                                        (namestring (merge-pathnames
                                                     "vm/lispbsd-live.img"
                                                     root)))))
         (kernel (let ((value (guest-getenv "KERNEL" *default-kernel*)))
                   ;; KERNEL=disk boots through the disk's own bootloader,
                   ;; which can set a VESA framebuffer mode.
                   (if (string= value "disk")
                       nil
                       value)))
         (memory (guest-getenv "MEMORY" "2048"))
         (host (guest-getenv "SERIAL_HOST" "127.0.0.1"))
         (port (parse-integer (guest-getenv "SERIAL_PORT" "4555")))
         (process nil)
         (stream nil))
    (unless (probe-file image)
      (error 'guest-error :message (format nil "missing image ~A" image)))
    (with-open-file (*guest-serial-log* (merge-pathnames "vm/guest-serial.log"
                                                         root)
                                        :direction :output
                                        :if-exists :supersede
                                        :if-does-not-exist :create)
      (unwind-protect
           (progn
             (setf process (guest-start-qemu image kernel memory host port))
             (setf stream (guest-open-serial host port))
             (let* ((console (guest-login stream))
                    (output (guest-run-command stream command
                                               :timeout 600
                                               :console console)))
               (write-string output)
               (terpri)
               (guest-shutdown stream process :console console)
               (setf process nil)
               output))
        (when stream
          (ignore-errors (close stream)))
        (when (and process (uiop:process-alive-p process))
          (uiop:terminate-process process :urgent t)
          (ignore-errors (uiop:wait-process process)))))))

(handler-case
    (guest-main)
  (guest-error (condition)
    (format *error-output* "~A~%" condition)
    (uiop:quit 1))
  (error (condition)
    (format *error-output* "guest failed: ~A~%" condition)
    (uiop:quit 1)))
