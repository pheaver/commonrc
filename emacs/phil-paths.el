;; ----------------------------------------

(when (not (boundp 'user-emacs-directory))
  (setq user-emacs-directory "~/.emacs.d/"))

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(setq commonrc-dir (file-name-directory load-file-name))

(defun commonrc (&optional path)
  (if (null path)
      commonrc-dir
      (concat commonrc-dir path)))

(defun add-load-path (path)
  (let ((default-directory path))
    (add-to-list 'load-path (abbreviate-file-name path))
    (normal-top-level-add-subdirs-to-load-path)))

(add-load-path "~/local/share/emacs/site-lisp")
(add-load-path commonrc-dir)
(add-load-path user-emacs-directory)

(provide 'phil-paths)

;; ----------------------------------------
