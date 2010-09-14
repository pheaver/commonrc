;; ----------------------------------------

(when (not (boundp 'user-emacs-directory))
  (setq user-emacs-directory "~/.emacs.d/"))

(when (not (boundp 'user-init-file))
  (setq user-init-file (concat user-emacs-directory "init.el")))

(defvar local-rc-file
  "~/.localrc"
  "Path to local shell file.")

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(defvar commonrc-dir
  (file-name-directory load-file-name))

(defvar common-init-file
  (concat commonrc-dir "emacs.el")
  "Path to my master shared emacs init file.  Should be set in that file.")

(defun commonrc (&optional path)
  (concat commonrc-dir path))

(defun add-load-path (path)
  (let ((default-directory path))
    (add-to-list 'load-path (abbreviate-file-name path))
    (normal-top-level-add-subdirs-to-load-path)))

(add-load-path "~/local/share/emacs/site-lisp")
(add-load-path commonrc-dir)
(add-load-path user-emacs-directory)

(provide 'phil-paths)

;; ----------------------------------------
