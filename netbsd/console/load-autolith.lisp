(handler-case
    (progn
      (sb-ext:run-program "/sbin/mount" '("-uw" "/") :wait t)
      (setf (uiop:getenv "HOME") "/tmp")
      (ensure-directories-exist #p"/tmp/fasl/")
      (let ((env '("PATH=/bin:/sbin:/usr/bin:/usr/sbin" "HOME=/tmp")))
        (ignore-errors
          (sb-ext:run-program "/sbin/mknod" '("/dev/vio9p0" "c" "356" "0")
                              :wait t :environment env))
        (sb-ext:run-program "/bin/mkdir" '("-p" "/host")
                            :wait t :environment env)
        (sb-ext:run-program "/usr/sbin/mount_9p" '("-cu" "/dev/vio9p0" "/host")
                            :wait nil :environment env)
        (sleep 3))
      (asdf:initialize-source-registry
       `(:source-registry
         ,@(loop for name in '("frob"
                               "cl-colorist" "cl-exec-sandbox" "cl-jobpond"
                               "cl-llm-provider-api" "cl-skills" "cl-termdown"
                               "clifff" "clinedi" "colorlisp" "colordiff"
                               "idsmall" "mcparen" "org-templater" "parenchek"
                               "sbcl-generations" "sbcl-workers"
                               "sexp-config" "sexp-store" "structlisp")
                 collect (list :directory
                               (pathname (format nil "/host/common-lisp/~A/" name))))
         (:tree #p"/host/quicklisp/dists/quicklisp/software/")
         :ignore-inherited-configuration))
      (asdf:initialize-output-translations
       '(:output-translations (t #p"/tmp/fasl/") :ignore-inherited-configuration))
      (asdf:load-system :autolith)
      :loaded)
  (error (condition)
    (princ-to-string condition)))
