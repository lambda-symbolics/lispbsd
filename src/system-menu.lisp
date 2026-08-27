(in-package #:lispbsd)

;;;; -- Desktop System Menu --

(-> desktop-install-system-menu (desktop) desktop)
(defun desktop-install-system-menu (desktop)
  "Give DESKTOP the default background context menu.

A right press on the bare desktop then offers a fresh Exec and an
inspector on the current world, both opening at the pointer."
  (setf (desktop-menu-items-function desktop)
        (lambda (desktop x y)
          (desktop--system-menu-items desktop x y)))
  desktop)


(-> desktop--system-menu-items (desktop integer integer) list)
(defun desktop--system-menu-items (desktop x y)
  "Return the system menu entries for desktop point (X, Y)."
  (list (make-menu-item :label "New Exec"
                        :value ':new-exec
                        :action (lambda ()
                                  (desktop-attach-window
                                   desktop
                                   (exec-window-window
                                    (make-exec-window :x x :y y)))))
        (make-menu-item :label "Inspect World"
                        :value ':inspect-world
                        :action (lambda ()
                                  (desktop-attach-window
                                   desktop
                                   (inspector-window-window
                                    (make-inspector-window :object *world*
                                                           :x x
                                                           :y y)))))
        (make-menu-item :label "World Events"
                        :value ':world-events
                        :action (lambda ()
                                  (desktop-attach-window
                                   desktop
                                   (event-window-window
                                    (make-event-window :x x :y y)))))))
