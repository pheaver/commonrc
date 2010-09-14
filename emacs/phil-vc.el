;; ----------------------------------------
;; version control settings

(require 'phil-paths)

(unless (daemonp)
  (setq vc-handled-backends nil))

(defun diff-cur-buffer ()
  (interactive)
  (diff-buffer-with-file (current-buffer)))

;(defun gitk ()
;  (interactive)
;  (shell-command "gitk --all &"))

; broken
;(global-set-key (kbd "C-x v s") 'git-status)
;(add-load-path (commonrc "git-emacs"))
;; (if (require 'vc-git nil t)
;;     (require 'git-emacs nil t))

;(require 'dvc-autoloads "dvc-autoloads" t)
(autoload 'magit-status "magit" "magit" t)
(autoload 'egg-status "egg" "egg" t)

(defun phil/magit-log-edit (arg)
  (interactive "P")
  (magit-log-edit)
  (when arg (magit-log-edit-toggle-amending)))

(eval-after-load 'magit
  '(define-key magit-mode-map (kbd "c") 'phil/magit-log-edit))

(setq magit-omit-untracked-dir-contents t)

(setq magit-repo-dirs-depth 1)
(eval-after-load "magit"
  '(progn
     (require 'ido nil t)
     (when (fboundp 'ido-completing-read)
       (setq magit-completing-read 'ido-completing-read))))

(setq magit-repo-dirs
  (list "~" (commonrc) user-emacs-directory "~/signali"))

;; over ssh on CentOS, C-/ behaves as C-_
(global-set-key (kbd "C-x C-/") 'magit-status)
(global-set-key (kbd "C-x C-_") 'magit-status)

;; ----------------------------------------

(provide 'phil-vc)
