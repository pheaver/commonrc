;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ~/commonrc/emacs/emacs.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path (file-name-directory load-file-name))

(require 'phil-init)
(require 'phil-paths)
(require 'phil-utils)
(require 'phil-navigation)
(require 'phil-completion)
(require 'phil-frames)
(require 'phil-haskell)
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

;; In case I accidentally override a register (yeah it happens a lot).
(defun phil/set-registers ()
  (interactive)
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
  (set-register ?c (cons 'file (file-truename "~/.i3/config")))
  )

(defun phil/reset-registers ()
  (interactive)
  (setq register-alist nil)
  (phil/set-registers))

(phil/set-registers)

(global-unset-key (kbd "C-x C-c")) ;; never again

(global-set-key (kbd "C-x r v") 'view-register)
(global-set-key (kbd "C-x r L") 'list-registers)
(global-set-key (kbd "C-x /") 'point-to-register)  ;; also bound to C-x r SPC
(global-set-key (kbd "C-x j") 'register-to-point)  ;; also bound to C-x r j
(global-set-key (kbd "C-x F") 'find-file-at-point)

(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)

;;;; miscellaneous keybindings
(global-set-key (kbd "C-z") 'repeat)
(global-set-key (kbd "C-x a r") 'align-regexp)
(global-set-key (kbd "C-x a t") 'untabify)

(global-set-key (kbd "M-o") 'mode-line-other-buffer)
(global-set-key (kbd "C-c m") 'mc/edit-lines)

;;;; more keybindings, for functions defined in phil-utils
(define-key ctl-x-map "2" 'phil/split-window-vertically)
(define-key ctl-x-map "3" 'phil/split-window-horizontally)

(global-set-key (kbd "C-c C-x t") 'notify-timer)
(global-set-key "\C-xQ" 'phil/macro-query)
(global-set-key (kbd "C-M-]") 'phil/bury-buffer)
(global-set-key (kbd "C-x g") 'revert-buffer-noconfirm)

(set-default 'truncate-lines t)

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
(setq use-dialog-box nil)
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

;;;; paredit
(defun load-paredit-mode ()
  (if (require 'paredit-mode "paredit-mode" t)
      (paredit-mode +1)))

(add-hook 'emacs-lisp-mode-hook       'load-paredit-mode)
(add-hook 'lisp-mode-hook             'load-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'load-paredit-mode)
(add-hook 'scheme-mode-hook           'load-paredit-mode)

;;;; undo-tree
(use-package undo-tree
  :init
  (setq undo-tree-visualizer-quit-action 'save-and-restore)
  :config
  (global-undo-tree-mode t))

;;;; browse-kill-ring
(use-package browse-kill-ring
  :defer 3
  :init
  (setq browse-kill-ring-quit-action 'save-and-restore)
  :config
  (browse-kill-ring-default-keybindings)
  :bind
  ("C-c k" . browse-kill-ring)
  )

(setq kill-ring-max 200)

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

;;;; indent levels
(setq-default c-basic-offset 2)
(setq-default sh-basic-offset 2)
(setq-default js-indent-level 2)

;;;; C and c++ modes
(defun my-c-mode-common-hook ()
  (c-set-offset 'substatement-open 0))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

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
