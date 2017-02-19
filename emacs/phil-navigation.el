;;;; recentf
(setq recentf-max-saved-items 500)
(setq recentf-max-menu-items 60)
(if (fboundp 'helm-recentf)
    (global-set-key (kbd "C-c f") 'helm-recentf)
  (global-set-key (kbd "C-c f") 'recentf-open-files))

;;;; ibuffer
;; emacs should have ibuffer set to autoload, so this will bind C-x C-b when
;; ibuffer is available, even if it's not yet loaded.
(when (fboundp 'ibuffer)
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (setq ibuffer-default-sorting-mode 'major-mode)
  (setq ibuffer-always-compile-formats t)
  (setq ibuffer-use-other-window nil))


;;;; ido
(use-package ido
  :defer 1
  :init
  (setq ido-default-buffer-method 'selected-window)
  (setq ido-default-file-method 'selected-window)
  (setq ido-ignore-buffers
        '("^ " "*Buffer" "*Help*" "*Messages" "*Shell Command Output" "*Completions"))
  (setq ido-max-prospects 30)
  (setq ido-max-window-height 2) ;; nil means use max-mini-window-height
  (setq ido-decorations '("" "" "," " ..." "[" "]" " [No match]" " [Matched]"))

  :config
  (ido-mode t)
  (ido-everywhere t)
  (defalias 'read-buffer 'ido-read-buffer)
  (defalias 'read-directory-name 'ido-read-directory-name)
  (defalias 'read-file-name 'ido-read-file-name)
  (setq magit-completing-read-function 'magit-ido-completing-read)
  (setq gnus-completing-read-function 'gnus-ido-completing-read)
  )

(use-package ido-ubiquitous
  :defer 3
  :config
  (ido-ubiquitous-mode 1))

(use-package smex
  :defer 3
  :config
  (global-set-key (kbd "M-X") #'smex-major-mode-commands)
  (global-set-key [remap execute-extended-command] #'smex)
  )


;;;; helm
(use-package helm
  :defer
  :init
  (defun load-helm ()
    (interactive)
    (message "Loading helm")
    (require 'helm))
  (global-set-key (kbd "C-c h") 'load-helm)

  :config
  (setq helm-full-frame nil)
  (setq helm-split-window-in-side-p nil)
  (setq helm-always-two-windows nil)
  (setq helm-split-window-default-side 'other)
  (setq helm-candidate-number-limit 250)
  (setq helm-follow-mode-persistent t)

  (require 'helm-config)
  (helm-mode 1)

  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-unset-key (kbd "C-x c"))

  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
  (global-set-key (kbd "C-c h @") 'helm-list-elisp-packages-no-fetch)

  ;; undo some ido stuff
  (with-eval-after-load 'ido
    (ido-everywhere nil)
    (setq magit-completing-read-function 'magit-builtin-completing-read)
    (setq gnus-completing-read-function 'gnus-ido-completing-read))
  )

(use-package helm-ag
  :defer
  :config (setq helm-ag-insert-at-point 'symbol)
  :bind (( "C-c C-." . helm-ag )))

(use-package helm-projectile
  :defer
  :config
  (helm-projectile-on)
  (global-set-key (kbd "C-c C-.") 'helm-projectile-ag)
  )

(use-package helm-swoop
  :defer
  :init
  ;; (global-set-key (kbd "M-i") 'helm-swoop)
  ;; (global-set-key (kbd "C-c M-i") 'helm-multi-swoop-current-mode)
  ;; (global-set-key (kbd "C-c M-I") 'helm-multi-swoop-all)

  :bind (
         ( "C-c o" . helm-swoop )
         ( "C-c O" . helm-swoop-current-mode )
         ( "C-c M-o" . helm-mutli-swoop-all )
         :map isearch-mode-map
         ( "M-i" . helm-sweoop-from-isearch )
         ( "C-c o" . helm-sweoop-from-isearch )
         :map helm-swoop-map
         ( "M-i" . helm-multi-swoop-current-mode-from-helm-swoop )
         ( "M-I" . helm-multi-swoop-all-from-helm-swoop )
         :map helm-multi-swoop-map
         ( "M-i" . helm-swoop-all-from-helm-swoop )
         )
  )

;;;; projectile
(use-package projectile
  :defer
  :init
  (global-set-key (kbd "C-c p") (lambda () (interactive) (message "loading projectile") (require 'projectile)))

  :config
  (projectile-global-mode)
  (define-key projectile-command-map (kbd "ESC") nil)
  (define-key projectile-command-map (kbd "M-i") 'helm-multi-swoop-projectile)
  (with-eval-after-load 'helm
    (setq projectile-completion-system 'helm))
  (require 'helm-projectile nil 'noerror)
  )

(provide 'phil-navigation)
