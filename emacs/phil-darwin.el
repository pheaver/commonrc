;; ----------------------------------------

(defvar darwin-system (string= system-type "darwin"))

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

  ;;(add-hook 'server-switch-hook 'raise-emacs-on-aqua)
  )

(provide 'phil-darwin)

;; ----------------------------------------
