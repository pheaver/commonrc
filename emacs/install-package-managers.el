;; ----------------------------------------
;; install elpa and el-get
;; http://bytes.inso.cc/2011/08/13/auto-installing-packages-in-emacs-with-elpa-and-el-get/
;; http://www.emacswiki.org/emacs/el-get

; derived from ELPA installation
; http://tromey.com/elpa/install.html
(defun eval-url (url)
  (let ((buffer (url-retrieve-synchronously url)))
  (save-excursion
    (set-buffer buffer)
    (goto-char (point-min))
    (re-search-forward "^$" nil 'move)
    (eval-region (point) (point-max))
    (kill-buffer (current-buffer)))))

(add-to-list 'load-path "~/.emacs.d/elpa")
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(defun install-elpa ()
  (eval-url "http://tromey.com/elpa/package-install.el"))

(defun install-el-get ()
  (eval-url "https://github.com/dimitri/el-get/raw/master/el-get-install.el"))

(if (require 'package nil t)
    (message "elpa is already installed")
  (install-elpa))

(if (require 'el-get nil t)
    (message "el-get is already installed")
  (install-el-get))

;; ----------------------------------------

(provide 'install-package-managers)
