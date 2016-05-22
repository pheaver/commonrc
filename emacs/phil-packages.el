(require 'phil-paths)

(defvar phil/auto-install-packages nil
  "Automatically install package managers (e.g. package.el, el-get) and any packages that might be specified.

This is set in commonrc/emacs/Makefile.")

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ))

(unless (require 'package nil 'noerror)
  (load (commonrc-dir "package-legacy.el"))
  (require 'package))

;; if package list does not exist yet, download it (should only happen once)
(unless (file-exists-p package-user-dir)
  (message "refreshing package contents")
  (package-refresh-contents))

(package-initialize)

;; --------------------
;; el-get

(add-to-list 'load-path (concat user-emacs-directory "el-get" "/" "el-get"))

(eval-after-load "el-get"
  '(add-to-list 'el-get-recipe-path (commonrc-dir "el-get-recipes")))

(defun install-el-get ()
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(unless (require 'el-get nil 'noerror)
  (when phil/auto-install-packages (install-el-get)))

(when (require 'el-get nil 'noerror)
  (el-get 'sync))

;; it doesn't work to use package-install, use-package or req-package; el-get
;; will always self-update and install itself so that package.el doesn't know
;; about it anymore
;; (unless (require 'el-get nil 'noerror)
;;   (package-install 'el-get)
;;   (require 'el-get))
;; (el-get 'sync)

;; ----------------------------------------
;; define which packages I use

(setq my-packages
      '(
        paredit
        browse-kill-ring
        org
        magit
        auto-complete
        markdown-mode
        )
      )

;; use el-get only for my little itimer package.
(setq el-get-sources '((:name itimer)))

(defun phil/install-all ()
  (interactive)
  (if (require 'el-get)
      (el-get nil (mapcar 'el-get-source-name el-get-sources)))
  (dolist (package my-packages)
    (package-install package)))

(when phil/auto-install-packages (phil/install-all))

(provide 'phil-packages)
