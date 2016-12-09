;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ~/commonrc/emacs/emacs.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path (file-name-directory load-file-name))

(require 'phil-paths)
(require 'phil-utils)
(require 'phil-packages)
(require 'phil-buffers)
(require 'phil-completion)
(require 'phil-frames)
(require 'phil-haskell)
(require 'phil-helm)
(require 'phil-java)
(require 'phil-org)
(require 'phil-scala)
(require 'phil-tags)
(require 'phil-term)
(require 'phil-vc)
(require 'phil-wspace)

(when (and (equal system-type 'windows-nt) (executable-find "cygpath"))
  (require 'phil-file-modes)
  (add-hook 'find-file-hook 'phil/file-modes-check)
  (add-hook 'after-save-hook 'phil/file-modes-restore))

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; autoloads for cl-* files
(load "cl-loaddefs")

;;;; registers
(set-register ?i (cons 'file user-init-file))
(set-register ?e (cons 'file common-init-file))
(set-register ?E (cons 'file (phil/parent-dir common-init-file)))
(set-register ?D (cons 'file (dropbox-dir "emacs")))
(set-register ?l (cons 'file local-rc-file))
(set-register ?n (cons 'file (dropbox-dir "org" "notes.org")))
(set-register ?I (cons 'file (dropbox-dir "org" "inbox.org")))
(set-register ?w (cons 'file (dropbox-dir "org" "work-notes.org")))
(set-register ?s (cons 'file (dropbox-dir "emacs/stuff.el")))
(set-register ?a (cons 'file (file-truename "~/.config/awesome/rc.lua")))

(global-set-key (kbd "C-x r v") 'view-register)
(global-set-key (kbd "C-x r L") 'list-registers)
(global-set-key (kbd "C-x /") 'point-to-register)  ;; also bound to C-x r SPC
(global-set-key (kbd "C-x j") 'register-to-point)  ;; also bound to C-x r j
(global-set-key (kbd "C-x F") 'find-file-at-point)

;;;; miscellaneous keybindings
(global-set-key (kbd "M-g") 'goto-line) ;; default is M-g Mg
(global-set-key (kbd "C-z") 'repeat)
(global-set-key (kbd "C-x a r") 'align-regexp)
(global-set-key (kbd "C-x a t") 'untabify)

;;;; more keybindings, for functions defined in phil-utils
(define-key ctl-x-map "2" 'phil/split-window-vertically)
(define-key ctl-x-map "3" 'phil/split-window-horizontally)

(global-set-key (kbd "C-c C-x t") 'notify-timer)
(global-set-key "\C-xQ" 'phil/macro-query)
(global-set-key (kbd "C-M-]") 'phil/bury-buffer)
(global-set-key (kbd "C-x g") 'revert-buffer-noconfirm)

;; (eval-after-load "dired"
;;   '(define-key dired-mode-map (kbd "C-c C-.") 'phil/dired-cleanup-marked-files))

;;;; set some minor modes
(when (functionp 'column-number-mode) (column-number-mode 1))
(when (functionp 'display-time-mode) (display-time-mode 0))
(when (functionp 'tool-bar-mode) (tool-bar-mode 0))
(when (functionp 'scroll-bar-mode) (scroll-bar-mode 0))
(when (functionp 'show-paren-mode) (show-paren-mode 1))
(when (functionp 'size-indication-mode) (size-indication-mode 0))
(when (functionp 'transient-mark-mode) (transient-mark-mode 1))
(when (functionp 'savehist-mode) (savehist-mode 1))
(when (functionp 'global-font-lock-mode) (global-font-lock-mode 1))
(when (functionp 'menu-bar-mode) (menu-bar-mode 0))
(when (functionp 'async-bytecomp-package-mode) (async-bytecomp-package-mode 1))

;;;; miscellaneous
(setq inhibit-startup-screen 1)
(setq initial-scratch-message nil)
(setq help-window-select t)         ; always select help window
(setq kill-whole-line t)
(setq require-final-newline t)
(setq set-mark-command-repeat-pop t)
(setq split-height-threshold nil)
(setq split-width-threshold 200)
(setq x-stretch-cursor t)           ; draw block cursor as wide as the glyph under it
(setq-default fill-column 120)
(setq-default indent-tabs-mode nil) ; use spaces instead of tabs
(setq show-paren-delay 0)

;;;; os x
(when (eq system-type 'darwin)
  (setq ns-command-modifier 'super)
  (setq ns-option-modifier 'meta)
  (global-unset-key (kbd "s-q"))
  (global-unset-key (kbd "s-w"))
  (global-unset-key (kbd "s-t"))
  (global-unset-key (kbd "s-p")))

;;;; make the mark visible
;(when (require 'visible-mark nil 'noerror)
;  (global-visible-mark-mode 1))

;;;; do not blink the cursor
(blink-cursor-mode (- (*) (*) (*)))

;;;; enable some stuff that is normally disabled
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;;;; recentf
(setq recentf-max-saved-items 500)
(setq recentf-max-menu-items 60)
(if (fboundp 'helm-recentf)
    (global-set-key (kbd "C-c f") 'helm-recentf)
  (global-set-key (kbd "C-c f") 'recentf-open-files))

;;;; paredit
(defun load-paredit-mode ()
  (if (require 'paredit-mode "paredit-mode" t)
      (paredit-mode +1)))

(add-hook 'emacs-lisp-mode-hook       'load-paredit-mode)
(add-hook 'lisp-mode-hook             'load-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'load-paredit-mode)
(add-hook 'scheme-mode-hook           'load-paredit-mode)

;;;; projectile
(defun init-projectile ()
  (interactive)
  (require 'projectile)
  (projectile-global-mode)
  (define-key projectile-command-map (kbd "ESC") nil)
  (define-key projectile-command-map (kbd "M-i") 'helm-multi-swoop-projectile)
  (with-eval-after-load 'helm
    (setq projectile-completion-system 'helm))
  (with-eval-after-load 'helm-projectile
    (helm-projectile-on))
  (require 'helm-projectile nil 'noerror)
  )

(global-set-key (kbd "C-c p") 'init-projectile)

;;;; undo-tree
(when (require 'undo-tree "undo-tree" 'noerror)
  (global-undo-tree-mode t))
(setq undo-tree-visualizer-quit-action 'save-and-restore)

;;;; browse-kill-ring
(setq browse-kill-ring-quit-action 'save-and-restore)
(autoload 'browse-kill-ring "browse-kill-ring")
(global-set-key (kbd "C-c k") 'browse-kill-ring)
(eval-after-load "browse-kill-ring"
  '(browse-kill-ring-default-keybindings))

(setq kill-ring-max 200)

;;; phil's itimer mode
(autoload 'itimer-list-timers "itimer" "itimer" t)
(global-set-key (kbd "C-c T") 'itimer-list-timers)

;;;; tail
(autoload 'tail-file "tail" "tail" t)

;;;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;;;; saveplace
(require 'saveplace)
(setq-default save-place t)

;;;; save backup files in ~/.emacs_backup instead of current directory
(setq backup-directory-alist
      `((,(expand-file-name "~/work") . "~/work/.emacs_backups")
        ("." . "~/.emacs_backups")))

;;;; make sure the scratch buffer always exists
(run-with-idle-timer 1 t
    '(lambda () (get-buffer-create "*scratch*")))

;;;; use aspell instead of ispell
(setq-default ispell-program-name "aspell")
(setq ispell-extra-args '("--sug-mode=ultra"))

;;;; C and c++ modes
(setq-default c-basic-offset 2)

(defun my-c-mode-common-hook ()
  (c-set-offset 'substatement-open 0))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;;;; javascript/json
(setq js-indent-level 2)

;;;; shell stuff
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

; don't echo passwords
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

; make scripts executable when they're saved
(add-hook 'after-save-hook
   'executable-make-buffer-file-executable-if-script-p)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")
