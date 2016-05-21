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

(autoload 'magit-status "magit" "magit" t)
(autoload 'egg-status "egg" "egg" t)

(setq magit-sha1-abbrev-length 8)

(setq magit-omit-untracked-dir-contents t)

(setq magit-repo-dirs-depth 1)
(eval-after-load "magit"
  '(progn
     (require 'ido nil t)
     (when (fboundp 'ido-completing-read)
       (setq magit-completing-read 'ido-completing-read))))

(setq magit-repository-directories
      (list "~/src" "~/local/src" "~/commonrc" user-emacs-directory "~/work"))

;; for older versions of magit
(setq magit-repo-dirs magit-repository-directories)

;; over ssh on CentOS, C-/ behaves as C-_
(global-set-key (kbd "C-x C-/") 'magit-status)
(global-set-key (kbd "C-x C-_") 'magit-status)

;; ----------------------------------------

(provide 'phil-vc)
