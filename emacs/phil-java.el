(defun phil/java-mode-hook ()
  (c-set-offset 'inexpr-class 0)
  (c-set-offset 'arglist-intro 4)
  (c-set-offset 'arglist-close '+)
  (c-set-offset 'arglist-cont-nonempty 4)
  (c-set-offset 'case-label '+)
  )

(add-hook 'java-mode-hook 'phil/java-mode-hook)

(with-eval-after-load 'eclim
  (setq eclim-auto-save t)
  (setq eclimd-autostart-with-default-workspace nil)
  (setq eclimd-autostart nil)

  (global-eclim-mode)

  (define-key eclim-mode-map (kbd "M-n") 'eclim-problems-next-same-window)
  (define-key eclim-mode-map (kbd "M-p") 'eclim-problems-previous-same-window)
  (define-key eclim-mode-map (kbd "M-c") 'eclim-problems-correct)
  (define-key eclim-mode-map (kbd "M-.") 'eclim-java-find-declaration)
  (require 'company-emacs-eclim nil t))

(with-eval-after-load 'company-emacs-eclim
  (company-emacs-eclim-setup))

;; (defun load-eclim ()
;;   (interactive)
;;   (message "Loading eclim...")
;;   (require 'eclim)
;;   (message "Loading eclim... done"))

;; (global-set-key (kbd "C-c C-e") 'load-eclim)

(provide 'phil-java)
