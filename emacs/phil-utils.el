;; ----------------------------------------
;; miscellaneous utility functions.
;; these should be functions that are not needed to load emacs,
;; but are only loaded on demand, such as by an autoload.

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

(defun phil/mark-end-of-line (arg)
  "Put mark at end of line.  Arg works as in `forward-line'.
If this command is repeated, it marks the next ARG lines after
the ones already marked.  Identical to `mark-end-of-sentence',
except uses `forward-line' instead of `forward-sentence'."
  (interactive "p")
  (push-mark
   (save-excursion
     (if (and (eq last-command this-command) (mark t))
         (goto-char (mark)))
     (forward-line arg)
     (point))
   nil t))

(defun phil/mark-end-of-line-previous (arg)
  (interactive "p")
  (phil/mark-end-of-line (- 0 arg)))

(defun phil/isearch-occur ()
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string
             (regexp-quote isearch-string)))))

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

;; meant to be called from command line.
;; only works for absolute paths
;; TODO: use ido to make it easy to use interactively.
(defun phil/find-file-sudo (filename)
  (interactive)
  (find-file (concat "/sudo::" filename)))

;; ----------------------------------------

(provide 'phil-utils)
