;; ----------------------------------------
;; version control settings

(require 'phil-paths)

(setq vc-handled-backends/original vc-handled-backends)
(setq vc-handled-backends nil)
(phil/eval-at-init-level 3
  '(setq vc-handled-backends vc-handled-backends/original))

(defun diff-cur-buffer ()
  (interactive)
  (diff-buffer-with-file (current-buffer)))

;(defun gitk ()
;  (interactive)
;  (shell-command "gitk --all &"))

; broken
;(global-set-key (kbd "C-x v s") 'git-status)
;(add-load-path (commonrc-dir "git-emacs"))
;; (if (require 'vc-git nil t)
;;     (require 'git-emacs nil t))

;(require 'dvc-autoloads "dvc-autoloads" t)
(autoload 'magit-status "magit" "magit" t)
(autoload 'egg-status "egg" "egg" t)

;; (defun phil/magit-log-edit (arg)
;;   (interactive "P")
;;   (magit-log-edit)
;;   (when arg (magit-log-edit-toggle-amending)))

;; (eval-after-load 'magit
;;   '(define-key magit-mode-map (kbd "c") 'phil/magit-log-edit))

(setq magit-sha1-abbrev-length 8)

(setq magit-omit-untracked-dir-contents t)

(setq magit-repo-dirs-depth 1)
(eval-after-load "magit"
  '(progn
     (require 'ido nil t)
     (when (fboundp 'ido-completing-read)
       (setq magit-completing-read 'ido-completing-read))))

(setq magit-repository-directories
      (list "~" "~/local/src" (commonrc-dir) user-emacs-directory "~/rtk"))

;; for older versions of magit
(setq magit-repo-dirs magit-repository-directories)

(defun magit-quit-buffers ()
  (interactive)
  (magit-for-all-buffers 'bury-buffer))

(eval-after-load 'magit
  '(define-key magit-mode-map (kbd "Q") 'magit-quit-buffers))

;; over ssh on CentOS, C-/ behaves as C-_
(global-set-key (kbd "C-x C-/") 'magit-status)
(global-set-key (kbd "C-x C-_") 'magit-status)

;; ----------------------------------------

(provide 'phil-vc)
