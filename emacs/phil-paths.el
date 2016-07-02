(when (not (boundp 'user-emacs-directory))
  (setq user-emacs-directory "~/.emacs.d/"))

(when (not (boundp 'user-init-file))
  (setq user-init-file (concat user-emacs-directory "init.el")))

(defvar documents-dir "~/Documents/")

(defvar dropbox-dir "~/Dropbox/")

(defvar local-rc-file
  "~/.localrc"
  "Path to local shell file.")

(defvar commonrc-dir
  (file-name-directory load-file-name))

(defvar common-init-file
  (abbreviate-file-name (concat commonrc-dir "emacs.el"))
  "Path to my master shared emacs init file.  Should be set in that file.")

(defun concatpaths (paths)
  (mapconcat 'identity paths "/"))

(defun commonrc-dir (&rest paths)
  (concat commonrc-dir (concatpaths paths)))

(defun dropbox-dir (&rest paths)
  (concat dropbox-dir (concatpaths paths)))

(provide 'phil-paths)
