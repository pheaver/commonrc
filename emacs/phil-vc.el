(require 'phil-paths)

(defun phil/magit-status ()
  (interactive)
  (let ((helm-split-window-in-side-p t)
        (helm-display-buffer-default-height 30))
    (call-interactively 'magit-status)))

(use-package magit
  :init
  (setq magit-repository-directories
        `(("~/src" . 1) ("~/local/src" . 1) ("~/commonrc" . 0) (,user-emacs-directory . 1) ("~/work" . 1)))

  (setq magit-bury-buffer-function 'magit-mode-quit-window)
  (setq magit-popup-show-common-commands nil)
  (setq magit-sha1-abbrev-length 8)
  (setq magit-auto-revert-immediately nil)
  (setq magit-omit-untracked-dir-contents t)

  :config
  (remove-hook 'server-switch-hook 'magit-commit-diff)
  (setq vc-handled-backends/original vc-handled-backends)
  (setq vc-handled-backends nil)

  :bind (
         ( "C-x v m" . magit-merge-popup )
         ( "C-x v l" . magit-log-popup )
         ( "C-x v =" . magit-diff-popup )
         ( "C-x v d" . magit-diff-popup )
         ( "C-x v t" . magit-tag-popup )
         ( "C-x v b" . magit-branch-popup )
         ( "C-x v p" . magit-push-popup )
         ( "C-x v f" . magit-fetch-popup )
         ;; over ssh on CentOS, C-/ behaves as C-_
         ( "C-x C-/" . phil/magit-status )
         ( "C-x C-_" . phil/magit-status )
         ( "C-x C-." . magit-blame )
         )
  )

(provide 'phil-vc)
