(require 'phil-paths)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ))

(unless (require 'package nil 'noerror)
  ;; handle case where package.el is not available (e.g. on emacs < 24)
  ;; package-legacy.el is just package.el, renamed so that it only loads when I
  ;; manually reference it here, and downloaded from here:
  ;; http://git.savannah.gnu.org/gitweb/?p=emacs.git;a=blob_plain;hb=ba08b24186711eaeb3748f3d1f23e2c2d9ed0d09;f=lisp/emacs-lisp/package.el
  (load (commonrc-dir "package-legacy.el"))
  (require 'package))

;; if package list does not exist yet, download it (should only happen once)
(unless (file-exists-p package-user-dir)
  (message "refreshing package contents")
  (package-refresh-contents))

(package-initialize)

;;;; el-get
(add-to-list 'load-path (concat user-emacs-directory "el-get" "/" "el-get"))

(eval-after-load "el-get"
  '(add-to-list 'el-get-recipe-path (commonrc-dir "el-get-recipes")))

(when (require 'el-get nil 'noerror)
  (el-get 'sync))

;;;; packages to auto install
(defvar my-packages
  '(
    paredit
    browse-kill-ring
    org
    magit
    )
  )

(defvar my-el-get-packages '(itimer))

(defun phil/auto-install-all ()
  (interactive)
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p)))
  ;; cannot use package-installed-p (or use-package or req-package) because el-get
  ;; self-updates and uninstalls the elpa package.
  (unless (require 'el-get nil 'noerror)
    (package-install 'el-get)
    (require 'el-get))

  (el-get 'sync my-el-get-packages))

(provide 'phil-packages)
