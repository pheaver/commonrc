;; ----------------------------------------
;; my notification and timer settings

(defun notify (title msg &optional sticky)
  (message (concat title ": " msg))
  (when (string= system-type "darwin")
    (shell-command
     (concat "growlnotify"
             (if sticky " -s " " ")
             "-a emacs -t \"" title "\" -m \"" msg "\"")
     nil nil)
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
