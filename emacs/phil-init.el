(require 'phil-paths)

;; http://cachestocaches.com/2015/8/getting-started-use-package/
(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;;;; el-get
(add-to-list 'load-path (concat user-emacs-directory "el-get" "/" "el-get"))

(eval-after-load "el-get"
  '(add-to-list 'el-get-recipe-path (commonrc-dir "el-get-recipes")))

(when (require 'el-get nil 'noerror)
  (el-get 'sync))


(setq my-favorite-packages
      '(helm helm-ag helm-swoop projectile helm-projectile eclim haskell-mode magit company company-emacs-eclim intero))

(defun install-my-favorite-packages ()
  (interactive)
  (mapcar 'package-install my-favorite-packages))

(provide 'phil-init)
