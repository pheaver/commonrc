(require 'phil-paths)

;; http://cachestocaches.com/2015/8/getting-started-use-package/
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
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


(provide 'phil-init)
