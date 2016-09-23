;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; miscellaneous utility functions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-timer (title &rest forms)
  "Run the given FORMS, counting the elapsed time.
A message including the given TITLE and the corresponding elapsed
time is displayed."
  (declare (indent 1))
  (let ((nowvar (make-symbol "now"))
        (body   `(progn ,@forms)))
    `(let ((,nowvar (current-time)))
       (message "%s..." ,title)
       (prog1 ,body
         (let ((elapsed
                (float-time (time-subtract (current-time) ,nowvar))))
           (message "%s... done (%.3fs)" ,title elapsed))))))

;;;; parent dirs
(defun phil/parent-dir (path)
  (interactive)
  (if path
      (file-name-directory (directory-file-name path))))

(defun phil/parent-dirs (path)
  (interactive)
  (let ((parent (phil/parent-dir path)))
    (cond
     ((null parent) ())
     ((string= parent path) ())
     (t (cons parent (phil/parent-dirs parent))))))

;;;; notifications
(defvar growlnotify-path "growlnotify")
(defvar gntp-send-path "gntp-send")
(defvar notify-send-path "notify-send")
(defvar terminal-notifier-path "terminal-notifier")

(defun notify (title msg &optional sticky)
  (interactive "sTitle: \nsMessage: ")
  (message (concat title ": " msg))
  (phil-notify title msg sticky))

(defun phil-notify (title msg &optional sticky)
  (interactive "sTitle: \nsMessage: ")
  (or (notify-terminal-notifier title msg)
      (notify-notify-send title msg)
      (notify-growlnotify title msg sticky)
      (notify-gntp-send title msg sticky)))

(defun notify-terminal-notifier (title msg &optional sticky)
  (when (executable-find terminal-notifier-path)
    (call-process terminal-notifier-path nil nil nil "-title" title "-message" msg)))

;; zzz gntp-send doesn't seem to support a sticky flag
(defun notify-gntp-send (title msg &optional sticky)
  (when (executable-find gntp-send-path)
    (call-process gntp-send-path nil nil nil "-u" title msg)))

(defun notify-growlnotify (title msg &optional sticky)
  (let ((args (cond
               ((eq system-type 'darwin)
                (let ((args0 (list "-a" "emacs" "-t" title "-m" msg)))
                  (if sticky (cons "-s" args0) args0)))
               ((memq system-type '(cygwin windows-nt))
                (list (if sticky "/s:true" "/s:false") (concat "/t:" title) msg)))))

    (when (and args (executable-find growlnotify-path))
      (apply 'call-process growlnotify-path nil nil nil args))))

(defun notify-notify-send (title msg)
  (when (executable-find notify-send-path)
    (call-process notify-send-path nil nil nil title msg)))

(defun notify-timer (time msg)
  "At time TIME, notify user with message MSG"
  (interactive "sRun at time: \nsMessage: ")
  (run-at-time time nil 'notify "Emacs Timer" msg t))

(defun tea-timer (time)
  (interactive "sRun at time: ")
  (notify-timer time "Tea is ready!"))

;;; other
(defun phil/switch-to-buffer (buffer)
  (let ((w (get-buffer-window buffer)))
    (if w (select-window w)
      (switch-to-buffer buffer))))

(defun phil/shell-command-on-file
  (command &optional filename output-buffer error-buffer)
  (interactive
   (list (read-shell-command "Shell command: " nil nil)
         nil
         current-prefix-arg
         shell-command-default-error-buffer))
  (let ((n (or filename (buffer-file-name))))
    (if (null n)
        (message (concat "Not a file: " (buffer-name)))
      (shell-command (concat command " " n) output-buffer error-buffer))))

(defun phil/macro-query (arg)
  "Prompt for input using minibuffer during kbd macro execution.
With prefix argument, allows you to select what prompt string to use.
If the input is non-empty, it is inserted at point."
  (interactive "P")
  (let* ((prompt (if arg (read-from-minibuffer "PROMPT: ") "Input: "))
          (input (minibuffer-with-setup-hook (lambda () (kbd-macro-query t))
                     (read-from-minibuffer prompt))))
    (unless (string= "" input) (insert input))))

;;;; cleanup files - remove trailing whitespace and tabs
(defun phil/cleanup (file)
  (let* ((buffer0 (find-buffer-visiting file))
         (buffer1 (or buffer0 (find-file file))))
    (with-current-buffer buffer1
      (delete-trailing-whitespace)
      (untabify (point-min) (point-max)))
    (when (not buffer0)
      (save-buffer buffer1)
      (kill-buffer buffer1))))

(defun phil/dired-cleanup-marked-files ()
  (interactive)
  (eval-when-compile (require 'dired))
  (let ((files (dired-get-marked-files nil nil)))
    (mapc 'phil/cleanup files)))

(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
     "python -mjson.tool" (current-buffer) t)))

;;;; my tweak to the split-window functions
(defun phil/split-window-vertically (&optional size)
  (interactive "P")
  (with-selected-window (split-window-vertically size)
      (switch-to-buffer (other-buffer)))
  (balance-windows))

(defun phil/split-window-horizontally (&optional size)
  (interactive "P")
  (with-selected-window (split-window-horizontally size)
      (switch-to-buffer (other-buffer)))
  (balance-windows))

(defun revert-buffer-noconfirm (&optional ignore-auto preserve-modes)
  (interactive "P")
  (revert-buffer ignore-auto t preserve-modes))

(defun phil/bury-buffer (&optional arg buffer-or-name)
  (interactive "P")
  (bury-buffer buffer-or-name)
  (when arg (other-window 1)))

(provide 'phil-utils)
