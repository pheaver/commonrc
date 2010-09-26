;; ----------------------------------------

(when (eq system-type 'darwin)
  (setq ns-command-modifier 'super)
  (setq ns-option-modifier 'meta)
  (global-unset-key (kbd "s-q"))
  (global-unset-key (kbd "s-w"))

  (set-cursor-color "orange")
  (add-to-list 'default-frame-alist
               '(cursor-color . "orange"))

  ;;;; on os x, open a file using the "open" command
  ; TODO: use (dired-get-file-for-visit) to call "open" on a file
  (defun phil/open-file ()
    (interactive)
    (require 'phil-utils)
    (phil/shell-command-on-file "open"))

  ;;(add-hook 'server-switch-hook 'raise-emacs-on-aqua)
  )

(provide 'phil-darwin)

;; ----------------------------------------
