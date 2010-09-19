;; ----------------------------------------
;; miscellaneous utility functions.
;; these should be functions that are not needed to load emacs,
;; but are only loaded on demand, such as by an autoload.

;;;###autoload
(defun phil/switch-to-buffer (buffer)
  (let ((w (get-buffer-window buffer)))
    (if w (select-window w)
      (switch-to-buffer buffer))))

;;;###autoload
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

;;;###autoload
(defun phil/macro-query (arg)
  "Prompt for input using minibuffer during kbd macro execution.
With prefix argument, allows you to select what prompt string to use.
If the input is non-empty, it is inserted at point."
  (interactive "P")
  (let* ((prompt (if arg (read-from-minibuffer "PROMPT: ") "Input: "))
          (input (minibuffer-with-setup-hook (lambda () (kbd-macro-query t))
                     (read-from-minibuffer prompt))))
    (unless (string= "" input) (insert input))))

;;;###autoload
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

;;;###autoload
(defun phil/mark-end-of-line-previous (arg)
  (interactive "p")
  (phil/mark-end-of-line (- 0 arg)))

;;;###autoload
(defun phil/isearch-occur ()
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string
             (regexp-quote isearch-string)))))

;;;###autoload
(defun phil/cleanup (file)
  (let* ((buffer0 (find-buffer-visiting file))
         (buffer1 (or buffer0 (find-file file))))
    (with-current-buffer buffer1
      (delete-trailing-whitespace)
      (untabify (point-min) (point-max)))
    (when (not buffer0)
      (save-buffer buffer1)
      (kill-buffer buffer1))))

;;;###autoload
(defun phil/dired-cleanup-marked-files ()
  (interactive)
  (eval-when-compile (require 'dired))
  (let ((files (dired-get-marked-files nil nil)))
    (mapc 'phil/cleanup files)))

;; ----------------------------------------

(provide 'phil-utils)
