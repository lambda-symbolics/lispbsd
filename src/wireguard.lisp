(in-package #:lispbsd)

;;;; -- WireGuard Tunnels --

(defclass wireguard-tunnel (network-interface)
  ()
  (:default-initargs :kind ':wireguard-tunnel)
  (:documentation
   "A WireGuard tunnel interface.

Key material lives only in files; the heap holds paths and public
keys, never private keys."))


(defclass wireguard-peer ()
  ((wireguard-peer-name
    :initarg :name
    :reader wireguard-peer-name
    :type string
    :documentation "Peer name as configured on the tunnel.")
   (wireguard-peer-public-key
    :initarg :public-key
    :initform nil
    :reader wireguard-peer-public-key
    :type (option string)
    :documentation "Base64 public key of the peer, if reported.")
   (wireguard-peer-endpoint
    :initarg :endpoint
    :initform nil
    :reader wireguard-peer-endpoint
    :type (option string)
    :documentation "Remote address and port, if configured.")
   (wireguard-peer-allowed-ips
    :initarg :allowed-ips
    :initform nil
    :reader wireguard-peer-allowed-ips
    :documentation "List of allowed CIDR strings.")
   (wireguard-peer-latest-handshake
    :initarg :latest-handshake
    :initform nil
    :reader wireguard-peer-latest-handshake
    :type (option string)
    :documentation "Time of the newest handshake, or NIL for never."))
  (:documentation "One configured peer of a WireGuard tunnel."))


(defmethod resource-operations ((resource wireguard-tunnel))
  (append (call-next-method)
          (list (make-operation :name ':wireguard-destroy
                                :label "Destroy"
                                :function #'wireguard-destroy))))


(-> wireguard-create (string) wireguard-tunnel)
(defun wireguard-create (name)
  "Create the WireGuard tunnel interface NAME and return its resource."
  (wireguard--require-substrate ':wireguard-create)
  (netbsd-network--ifconfig (list name "create") ':wireguard-create nil)
  (let ((tunnel (make-instance 'wireguard-tunnel :name name)))
    (network--record tunnel ':wireguard-create)
    tunnel))


(-> wireguard-destroy (wireguard-tunnel) wireguard-tunnel)
(defun wireguard-destroy (tunnel)
  "Destroy TUNNEL's interface. The resource stops being live."
  (wireguard--require-substrate ':wireguard-destroy)
  (netbsd-network--ifconfig (list (resource-name tunnel) "destroy")
                            ':wireguard-destroy tunnel)
  (setf (resource-live-p tunnel) nil)
  (network--record tunnel ':wireguard-destroy)
  tunnel)


(-> wireguard-generate-key ((or pathname string)) pathname)
(defun wireguard-generate-key (path)
  "Generate a private key into the file at PATH and return its pathname.

The key never enters the heap; only the path is handled."
  (wireguard--require-substrate ':wireguard-generate-key)
  (with-open-file (out path :direction ':output
                            :if-exists ':supersede
                            :if-does-not-exist ':create)
    (sb-ext:run-program "/usr/sbin/wg-keygen" nil
                        :output out
                        :wait t))
  (sb-ext:run-program "/bin/chmod" (list "600" (namestring path))
                      :wait t)
  (pathname path))


(-> wireguard-set-private-key (wireguard-tunnel (or pathname string))
    wireguard-tunnel)
(defun wireguard-set-private-key (tunnel key-path)
  "Give TUNNEL the private key stored in the file at KEY-PATH."
  (wireguard--wgconfig tunnel
                       (list "set" "private-key" (namestring key-path))
                       ':wireguard-set-private-key)
  (network--record tunnel ':wireguard-set-private-key)
  tunnel)


(-> wireguard-set-listen-port (wireguard-tunnel (integer 1 65535))
    wireguard-tunnel)
(defun wireguard-set-listen-port (tunnel port)
  "Make TUNNEL listen on PORT."
  (wireguard--wgconfig tunnel
                       (list "set" "listen-port"
                             (format nil "~A" port))
                       ':wireguard-set-listen-port)
  (network--record tunnel ':wireguard-set-listen-port)
  tunnel)


(-> wireguard-add-peer (wireguard-tunnel &key (:name string)
                                             (:public-key string)
                                             (:endpoint (option string))
                                             (:allowed-ips list)
                                             (:preshared-key-path
                                              (option (or pathname string))))
    wireguard-tunnel)
(defun wireguard-add-peer (tunnel &key name public-key endpoint allowed-ips
                           preshared-key-path)
  "Add a peer to TUNNEL.

ALLOWED-IPS is a list of CIDR strings. PRESHARED-KEY-PATH names a file
holding the preshared key, when one is used."
  (wireguard--wgconfig
   tunnel
   (append (list "add" "peer" name public-key)
           (when preshared-key-path
             (list (format nil "--preshared-key=~A"
                           (namestring preshared-key-path))))
           (when endpoint
             (list (format nil "--endpoint=~A" endpoint)))
           (when allowed-ips
             (list (format nil "--allowed-ips=~{~A~^,~}" allowed-ips))))
   ':wireguard-add-peer)
  (network--record tunnel ':wireguard-add-peer)
  tunnel)


(-> wireguard-delete-peer (wireguard-tunnel string) wireguard-tunnel)
(defun wireguard-delete-peer (tunnel name)
  "Remove the peer called NAME from TUNNEL."
  (wireguard--wgconfig tunnel (list "delete" "peer" name)
                       ':wireguard-delete-peer)
  (network--record tunnel ':wireguard-delete-peer)
  tunnel)


(-> wireguard-listen-port (wireguard-tunnel) (option integer))
(defun wireguard-listen-port (tunnel)
  "Return TUNNEL's listen port, or NIL when none is set."
  (nth-value 0 (wireguard--parse-show (wireguard--show tunnel))))


(-> wireguard-peers (wireguard-tunnel) list)
(defun wireguard-peers (tunnel)
  "Return TUNNEL's configured peers as wireguard-peer objects."
  (nth-value 1 (wireguard--parse-show (wireguard--show tunnel))))


(-> wireguard--show (wireguard-tunnel) string)
(defun wireguard--show (tunnel)
  "Return the wgconfig report for TUNNEL."
  (wireguard--wgconfig tunnel (list "show" "all") ':wireguard-show))


(-> wireguard--require-substrate (keyword) t)
(defun wireguard--require-substrate (operation)
  "Signal NETWORK-OPERATION-UNSUPPORTED off the NetBSD substrate."
  (unless (machine--netbsd-p)
    (error 'network-operation-unsupported :operation operation))
  nil)


(-> wireguard--wgconfig (wireguard-tunnel list keyword) string)
(defun wireguard--wgconfig (tunnel arguments operation)
  "Run wgconfig on TUNNEL with ARGUMENTS and return its output."
  (wireguard--require-substrate operation)
  (let ((output (make-string-output-stream))
        (errors (make-string-output-stream)))
    (let ((exit-code (sb-ext:process-exit-code
                      (sb-ext:run-program
                       "/usr/sbin/wgconfig"
                       (cons (resource-name tunnel) arguments)
                       :output output
                       :error errors
                       :wait t))))
      (unless (zerop exit-code)
        (error 'network-operation-failed
               :interface tunnel
               :operation operation
               :detail (string-trim '(#\Space #\Newline)
                                    (get-output-stream-string errors))))
      (get-output-stream-string output))))


(-> wireguard--parse-show (string) (values (option integer) list))
(defun wireguard--parse-show (text)
  "Parse a wgconfig show report into a listen port and peer objects."
  (let ((listen-port nil)
        (peers nil)
        (current nil))
    (flet ((finish-peer ()
             (when current
               (push (make-instance 'wireguard-peer
                                    :name (getf current ':name)
                                    :public-key (getf current ':public-key)
                                    :endpoint (getf current ':endpoint)
                                    :allowed-ips (getf current ':allowed-ips)
                                    :latest-handshake (getf current
                                                            ':latest-handshake))
                     peers)
               (setf current nil))))
      (dolist (line (uiop:split-string text :separator '(#\Newline)))
        (multiple-value-bind (key value) (wireguard--parse-report-line line)
          (cond ((null key)
                 nil)
                ((string= key "peer")
                 (finish-peer)
                 (setf current (list ':name value)))
                ((null current)
                 (when (string= key "listen-port")
                   (setf listen-port (parse-integer value :junk-allowed t))))
                ((string= key "public-key")
                 (setf (getf current ':public-key) value))
                ((string= key "endpoint")
                 (setf (getf current ':endpoint) value))
                ((string= key "allowed-ips")
                 (setf (getf current ':allowed-ips)
                       (uiop:split-string value :separator '(#\,))))
                ((string= key "latest-handshake")
                 (unless (string= value "(never)")
                   (setf (getf current ':latest-handshake) value))))))
      (finish-peer))
    (values listen-port (nreverse peers))))


(-> wireguard--parse-report-line (string) (values (option string)
                                                  (option string)))
(defun wireguard--parse-report-line (line)
  "Split a wgconfig report LINE into a key and value, or NILs."
  (let* ((trimmed (string-trim '(#\Space #\Tab) line))
         (colon (search ": " trimmed)))
    (if colon
        (values (subseq trimmed 0 colon)
                (string-trim '(#\Space) (subseq trimmed (+ colon 2))))
        (values nil nil))))
