;; ----------------------------------------
;; initialize ELPA

(when (require 'package nil t)
  (package-initialize)

  ;; marmalade and melpa are super similar.
  ;; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

  ;; has lots of snapshot buids, yuck
  ;; (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net") t)

  ;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
)

;; ----------------------------------------
;; initialize el-get

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(eval-after-load "el-get"
  '(add-to-list 'el-get-recipe-path (commonrc-dir "el-get-recipes")))

(if (require 'el-get nil t)
    (el-get))

;; ----------------------------------------
;; define which packages I use

(setq my-packages
      '(
        el-get
        magit
        browse-kill-ring
        auto-complete
        markdown-mode
        maxframe
        org
        paredit
        ;; tail
        ;; haskell-mode
        ;; haskell-mode-exts
        ;; anything
        ;; anything-config
        ;; auto-complete-etags
        ;; calfw
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

(provide 'phil-packages)

