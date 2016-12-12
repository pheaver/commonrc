(require 'phil-paths)
(require 'phil-init)

(setq vc-handled-backends/original vc-handled-backends)
(setq vc-handled-backends nil)
(phil/eval-at-init-level 3
  '(setq vc-handled-backends vc-handled-backends/original))

(setq magit-bury-buffer-function 'magit-mode-quit-window)

(setq magit-popup-show-common-commands nil)

(setq magit-sha1-abbrev-length 8)

(setq magit-omit-untracked-dir-contents t)

(setq magit-repo-dirs-depth 1)

(setq magit-repository-directories
      (list "~/src" "~/local/src" "~/commonrc" user-emacs-directory "~/work"))

;; for older versions of magit
(setq magit-repo-dirs magit-repository-directories)

;; over ssh on CentOS, C-/ behaves as C-_
(global-set-key (kbd "C-x C-/") 'magit-status)
(global-set-key (kbd "C-x C-_") 'magit-status)
(global-set-key (kbd "C-x C-.") 'magit-blame)

(provide 'phil-vc)
