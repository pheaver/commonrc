;; ----------------------------------------
;; my notification and timer settings

(require 'phil-darwin)

(if darwin-system
    (defun notify (title msg &optional sticky)
      (shell-command
       (concat "growlnotify"
               (if sticky " -s " " ")
               "-a emacs -t \"" title "\" -m \"" msg "\"")
       nil nil)
      )
  (defun notify (title msg &optional sticky)
    (message msg)
    ))

(defun notify-timer (time msg)
  "At time TIME, notify user with message MSG"
  (interactive "sRun at time: \nsMessage: ")
  (run-at-time time nil 'notify "Emacs Timer" msg t))

(defun tea-timer (time)
  (interactive "sRun at time: ")
  (notify-timer time "Tea is ready!"))

;; phil's itimer mode
(autoload 'itimer-list-timers "itimer" "itimer" t)

(global-set-key (kbd "C-c L") 'itimer-list-timers)

;; ----------------------------------------

(provide 'phil-notify-timer)
