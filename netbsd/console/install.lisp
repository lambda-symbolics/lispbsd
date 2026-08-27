(progn
  (sb-ext:run-program "/sbin/mount" '("-uw" "/") :wait t)
  (let ((env '("PATH=/bin:/sbin:/usr/bin:/usr/sbin"))
        (log (make-string-output-stream)))
    (sb-ext:run-program "/bin/mkdir" '("-p" "/lispbsd" "/mnt/autolith")
                        :wait t :environment env)
    (sb-ext:run-program "/bin/tar" '("xf" "/dev/wd1d" "-C" "/lispbsd")
                        :wait t :output log :error log :environment env)
    (sb-ext:run-program "/bin/cp"
                        '("/lispbsd/netbsd/console/profile" "/.profile")
                        :wait t :environment env :output log :error log)
    (sb-ext:run-program "/bin/cp"
                        '("/lispbsd/netbsd/console/rc.conf" "/etc/rc.conf")
                        :wait t :environment env :output log :error log)
    (sb-ext:run-program "/bin/sh" '("MAKEDEV" "vio9p0")
                        :wait t :directory #p"/dev/" :environment env
                        :output log :error log)
    (sb-ext:run-program "/bin/sync" nil :wait t)
    (get-output-stream-string log)))
