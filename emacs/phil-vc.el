(require 'phil-paths)

(setq magit-repository-directories
      (list "~" "~/src" "~/local/src" "~/commonrc" user-emacs-directory (concat user-emacs-directory "el-get") "~/work"))
(setq magit-repository-directories-depth 1)

(setq magit-bury-buffer-function 'magit-mode-quit-window)
(setq magit-popup-show-common-commands nil)
(setq magit-sha1-abbrev-length 8)
(setq magit-auto-revert-immediately nil)
(setq magit-omit-untracked-dir-contents t)

;; for older versions of magit
(setq magit-repo-dirs magit-repository-directories)
(setq magit-repo-dirs-depth magit-repository-directories-depth)


(defun phil/magit-status ()
  (interactive)
  (let ((helm-split-window-in-side-p t)
        (helm-display-buffer-default-height 30))
    (call-interactively 'magit-status)))

(with-eval-after-load 'magit
  (remove-hook 'server-switch-hook 'magit-commit-diff)

  (setq vc-handled-backends/original vc-handled-backends)
  (setq vc-handled-backends nil)
  ;; (setq vc-handled-backends (delq 'Git vc-handled-backends))

  (global-set-key (kbd "C-x v m") #'magit-merge-popup)
  (global-set-key (kbd "C-x v l") #'magit-log-popup)
  (global-set-key (kbd "C-x v =") #'magit-diff-popup)
  (global-set-key (kbd "C-x v d") #'magit-diff-popup)
  (global-set-key (kbd "C-x v t") #'magit-tag-popup)
  (global-set-key (kbd "C-x v b") #'magit-branch-popup)
  (global-set-key (kbd "C-x v p") #'magit-push-popup)
  (global-set-key (kbd "C-x v f") #'magit-fetch-popup)

  nil
  )

;; over ssh on CentOS, C-/ behaves as C-_
(global-set-key (kbd "C-x C-/") #'phil/magit-status)
(global-set-key (kbd "C-x C-_") #'phil/magit-status)
(global-set-key (kbd "C-x C-.") 'magit-blame)

(provide 'phil-vc)
