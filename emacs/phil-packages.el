;; ----------------------------------------
;; initialize ELPA

(if (require 'package nil t)
    (progn
      ;; Emacs 24+ includes ELPA, but requires some extra setup
      ;; to use the (better) tromey repo
      (if (>= emacs-major-version 24)
          (setq package-archives
                (cons '("tromey" . "http://tromey.com/elpa/")
                package-archives)))
      (package-initialize)))


;; ----------------------------------------
;; define which packages I use

(setq el-get-sources
      '(
        (:name browse-kill-ring)
        (:name popup)
        (:name auto-complete)
        (:name auto-complete-etags)
        ;; not working:
        ;; (:name anything :checkout "v1.3.9")
        ;; (:name anything-config)
        (:name calfw)
        (:name ghc-mod :checkout "v2.1.1")
        (:name haskell-mode :checkout "v13.06")
        (:name haskell-mode-exts)
        (:name itimer)
        ;; (:name magit :checkout "origin/master")
        (:name magit :checkout "1.2.0")
        (:name markdown-mode)
        (:name maxframe)
        (:name multi-term)
        (:name org-mode :checkout "release_8.2.4")
        (:name paredit)
        (:name tail)
        ;; these two have an error:
        ;; (:name g-client)
        ;; (:name google-weather)
      ))

;; ----------------------------------------
;; initialize el-get

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(eval-after-load "el-get"
  '(add-to-list 'el-get-recipe-path (commonrc-dir "el-get-recipes")))

(if (require 'el-get nil t)
    (el-get))

(defun el-get-install-all ()
  (interactive)
  (require 'el-get)
  (el-get nil (mapcar 'el-get-source-name el-get-sources)))

;; ----------------------------------------

(provide 'phil-packages)

