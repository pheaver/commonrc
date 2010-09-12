;; ----------------------------------------

(defvar darwin-system (string= system-type "darwin"))

;;;; not used anywhere but here, so for now this is where it's defined
(defun shell-command-on-file (command) (interactive)
  (let ((n (buffer-file-name)))
    (if (null n)
        (message (concat "Not a file: " (buffer-name)))
        (shell-command (concat command " " n)))))

(when darwin-system
  (setq ns-command-modifier 'super)
  (setq ns-option-modifier 'meta)
  (global-unset-key (kbd "s-q"))
  (global-unset-key (kbd "s-w"))

  (set-cursor-color "orange")
  (add-to-list 'default-frame-alist
               '(cursor-color . "orange"))

  (defun ns-raise-emacs ()
    (ns-do-applescript "tell application \"Emacs\" to activate"))

  ;;;; on os x, open a file using the "open" command
  ; TODO: use (dired-get-file-for-visit) to call "open" on a file
  (defun open-file () (interactive) (shell-command-on-file "open"))

  ;;(add-hook 'server-switch-hook 'raise-emacs-on-aqua)
  )

(provide 'phil-darwin)

;; ----------------------------------------
