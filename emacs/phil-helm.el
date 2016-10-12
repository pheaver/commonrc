(setq helm-full-frame nil)
(setq helm-split-window-in-side-p nil)
(setq helm-always-two-windows nil)
(setq helm-split-window-default-side 'other)
(setq helm-candidate-number-limit 250)

;; search for library in `require' and `declare-function' sexp.
;; (setq helm-ff-search-library-in-sexp t)

;; scroll 8 lines other window using M-<next>/M-<prior>
;; (setq helm-scroll-amount 8)

;; (setq helm-ff-file-name-history-use-recentf t)

;; (setq helm-M-x-fuzzy-match t) ;; optional fuzzy matching for helm-M-x

(defun load-helm ()
  (interactive)
  (message "Loading helm...")
  (require 'helm)
  (message "Loading helm... done"))

(global-set-key (kbd "C-c h") 'load-helm)
(global-set-key (kbd "M-X") 'helm-M-x)

(with-eval-after-load 'helm
  (require 'helm-config)
  (helm-mode 1)
  (require 'helm-swoop nil 'noerror)
  (require 'helm-projectile nil 'noerror)

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
    (setq gnus-completing-read-function 'gnus-ido-completing-read)
    (set-variable 'ido-enable-replace-completing-read nil))
  )

;; helm-swoop
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop-current-mode)
(global-set-key (kbd "C-c M-I") 'helm-multi-swoop-all)

(with-eval-after-load 'helm-swoop
  (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
  (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-current-mode-from-helm-swoop)
  (define-key helm-swoop-map (kbd "M-I") 'helm-multi-swoop-all-from-helm-swoop)
  (define-key helm-multi-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop))

;;;; helm + ag
(setq helm-ag-insert-at-point 'symbol)
(global-set-key (kbd "C-c C-.") 'helm-ag)
(with-eval-after-load 'helm-projectile
  (global-set-key (kbd "C-c C-.") 'helm-projectile-ag))


;;;; helm + company
(with-eval-after-load 'company
  (define-key company-mode-map (kbd "C-:") 'helm-company)
  (define-key company-active-map (kbd "C-:") 'helm-company))

(provide 'phil-helm)
