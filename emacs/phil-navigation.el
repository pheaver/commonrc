;;;; recentf
(use-package recentf
  :init
  (setq recentf-max-saved-items 500)
  (setq recentf-max-menu-items 60)
  :config
  (recentf-mode 1)
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
  (setq helm-candidate-number-limit 250)
  (setq helm-follow-mode-persistent t)
  (setq helm-buffer-max-length 35)
  (require 'helm-config)
  (helm-mode t)
  (helm-adaptive-mode t)

  ;; each of these 4 takes precedence over the others below it
  (setq helm-full-frame nil)
  (setq helm-split-window-in-side-p nil)
  (setq helm-always-two-windows nil)
  (setq helm-split-window-default-side 'other)

  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
  (global-unset-key (kbd "C-x c"))
  (global-set-key (kbd "C-c h") 'helm-command-prefix)
  (global-set-key (kbd "C-c h T") 'helm-timers)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x r l") 'helm-filtered-bookmarks)
  (global-set-key (kbd "C-c f") 'helm-recentf)
  (define-key helm-command-map (kbd "@") 'helm-list-elisp-packages-no-fetch)

  ;; https://github.com/emacs-helm/helm/issues/1492
  (defun helm-buffers-sort-transformer@donot-sort (_ candidates _)
    candidates)
  (advice-add 'helm-buffers-sort-transformer :around 'helm-buffers-sort-transformer@donot-sort)

  ;; undo some ido stuff
  (with-eval-after-load 'ido
    (ido-everywhere nil)
    (setq magit-completing-read-function 'magit-builtin-completing-read)
    (setq gnus-completing-read-function 'gnus-ido-completing-read))
  )

(use-package helm-projectile
  :defer
  :config
  (helm-projectile-on)
  )

(use-package helm-swoop
  :defer
  :init
  :bind (
         ( "C-c o" . helm-swoop )
         ( "C-c O" . helm-multi-swoop-current-mode )
         ( "C-c M-o" . helm-multi-swoop-all )
         :map isearch-mode-map
         ( "C-c o" . helm-swoop-from-isearch )
         :map helm-swoop-map
         ( "M-i" . helm-multi-swoop-current-mode-from-helm-swoop )
         ( "M-I" . helm-multi-swoop-all-from-helm-swoop )
         :map helm-multi-swoop-map
         ( "M-i" . helm-multi-swoop-all-from-helm-swoop )
         )
  )

;; ivy / swiper
(use-package swiper
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-height 16)
  (setq ivy-count-format "(%d/%d) ")
  (setq projectile-completion-system 'ivy)
  ;; (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (define-key ctl-x-map (kbd "C-f") 'counsel-find-file)
  ;; (global-set-key (kbd "M-x") 'counsel-M-x)
  (define-key ivy-minibuffer-map (kbd "C-SPC") 'ivy-restrict-to-matches)

  ;; (global-set-key (kbd "C-s") 'swiper)
  (define-key isearch-mode-map (kbd "C-o") 'swiper-from-isearch)
  (global-set-key (kbd "M-s o") 'swiper)
  )


(defun phil/switch-buffer ()
  (interactive)
  (cond ((featurep 'ivy) (ivy-switch-buffer))
        ((featurep 'helm) (helm-buffers-list))
        ((featurep 'ido) (ido-switch-buffer))
        (t (switch-to-buffer))))

(define-key ctl-x-map (kbd "b") 'phil/switch-buffer)

;;;; projectile
(use-package projectile
  :defer
  :init
  (global-set-key (kbd "C-c p") (lambda () (interactive) (message "loading projectile") (require 'projectile)))

  :config
  (projectile-global-mode)
  (define-key projectile-command-map (kbd "ESC") nil)
  (define-key projectile-command-map (kbd "s") 'helm-projectile-ag)
  (add-to-list 'projectile-other-file-alist '("java" "ui.xml"))
  (add-to-list 'projectile-other-file-alist '("ui.xml" "java"))
  (with-eval-after-load 'helm
    (setq projectile-completion-system 'helm))
  (require 'helm-projectile nil 'noerror)
  )

(provide 'phil-navigation)
