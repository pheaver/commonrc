;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ~/commonrc/emacs/emacs.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-startup-screen 1)
(setq initial-scratch-message nil)

;;;; some stuff that I'd prefer to have in phil-paths.el,
;;;; but there's a chicken-and-the-egg problem.
(setq common-init-file (abbreviate-file-name load-file-name))
(setq commonrc-dir (file-name-directory common-init-file))

(load-file (concat commonrc-dir "phil-paths.el"))
(require 'phil-init)

;; this is pretty important, so execute it at all run levels
(when (and (equal system-type 'windows-nt) (executable-find "cygpath"))
  (require 'phil-file-modes)
  (add-hook 'find-file-hook 'phil/file-modes-check)
  (add-hook 'after-save-hook 'phil/file-modes-restore))

(phil/eval-at-init-level 1 '(progn

;;;; load my other files
(require 'phil-packages)
(require 'phil-parent-dirs)
(require 'phil-anything)
(require 'phil-auto-complete)
(require 'phil-breadcrumb)
(require 'phil-buffers)
(require 'phil-darwin)
(require 'phil-flymake)
(require 'phil-haskell)
(require 'phil-scala)
(require 'phil-hippie-expand)
(require 'phil-org)
(require 'phil-frames)
(require 'phil-tags)
(require 'phil-term)
(require 'phil-utils)
(require 'phil-vc)
(require 'phil-wspace)
(require 'phil-recentf)

;;;; disabled stuff that I don't use anymore
;; (require 'phil-erc)
;; (require 'phil-mew)

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
(global-unset-key (kbd "s-p")) ;; so that i do not accidentally print

;;; always select help window
(setq help-window-select t)

;;;;;;;; paredit
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)

(defun load-paredit-mode ()
  (if (require 'paredit-mode "paredit-mode" t)
      (paredit-mode +1)))

(add-hook 'emacs-lisp-mode-hook       'load-paredit-mode)
(add-hook 'lisp-mode-hook             'load-paredit-mode)
(add-hook 'lisp-interaction-mode-hook 'load-paredit-mode)
(add-hook 'scheme-mode-hook           'load-paredit-mode)

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
      `((,(expand-file-name "~/signali") . "~/signali/.emacs_backups")
        ("." . "~/.emacs_backups")))

;;;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;;; allow windows to be split horizontally
;;; default is 160
(setq split-width-threshold 150)

;;;; make sure the scratch buffer always exists
(run-with-idle-timer 1 t
    '(lambda () (get-buffer-create "*scratch*")))

;;;; miscellaneous
(setq-default fill-column 80)
(setq set-mark-command-repeat-pop t)
(setq require-final-newline t)
(setq kill-whole-line t)

;;;; draw block cursor as wide as the glyph under it
(setq x-stretch-cursor t)

;;;; make the mark visible
;(when (require 'visible-mark nil 'noerror)
;  (global-visible-mark-mode 1))

;;;; do not blink the cursor
(blink-cursor-mode (- (*) (*) (*)))

;;;; use aspell instead of ispell
(setq-default ispell-program-name "aspell")
(setq ispell-extra-args '("--sug-mode=ultra"))

;;;; enable some stuff that is normally disabled
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;;;; wikipedia mode
(autoload 'wikipedia-mode "wikipedia-mode.el"
"Major mode for editing documents in Wikipedia markup." t)

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

;;;; my tweak to the split-window functions
(defun phil/split-window-vertically (&optional size)
  (interactive "P")
  (with-selected-window (split-window-vertically size)
      (switch-to-buffer (other-buffer)))
  (balance-windows))

(defun phil/split-window-horizontally (&optional size)
  (interactive "P")
  (with-selected-window (split-window-horizontally size)
      (switch-to-buffer (other-buffer)))
  (balance-windows))

(define-key ctl-x-map "2" 'phil/split-window-vertically)
(define-key ctl-x-map "3" 'phil/split-window-horizontally)

;; ---------------------------------------------
(defun beautify-json ()
  (interactive)
  (let ((b (if mark-active (min (point) (mark)) (point-min)))
        (e (if mark-active (max (point) (mark)) (point-max))))
    (shell-command-on-region b e
     "python -mjson.tool" (current-buffer) t)))

;; ---------------------------------------------
;; media files

(autoload 'list-media-files "media-files" nil t)
(eval-after-load 'media-files
  '(load "media-files-config"))

;; ---------------------------------------------
;; markdown mode

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(push '("\\.md" . markdown-mode) auto-mode-alist)
(push '("\\.markdown" . markdown-mode) auto-mode-alist)

;; ---------------------------------------------
;; C and c++ modes

(setq-default c-basic-offset 2)

(defun my-c-mode-common-hook ()
  (c-set-offset 'substatement-open 0))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

(push '("\\.c$" . c-mode) auto-mode-alist)
(push '("\\.h$" . c-mode) auto-mode-alist)
(push '("\\.cc$" . c++-mode) auto-mode-alist)
(push '("\\.icc$" . c++-mode) auto-mode-alist)
(push '("\\.C$" . c++-mode) auto-mode-alist)
(push '("\\.hh$" . c++-mode) auto-mode-alist)

;; ---------------------------------------------
;; javascript/json

(setq js-indent-level 2)

;; ---------------------------------------------
;; miscellaneous auto-mode-alist settings

(push '("\\.zsh$" . sh-mode) auto-mode-alist)
(push '("\\.txt$" . text-mode) auto-mode-alist)
(push '("\\.a$" . ada-mode) auto-mode-alist)
(push '("\\.vhdl$" . vhdl-mode) auto-mode-alist)
(push '("\\.vhd$" . vhdl-mode) auto-mode-alist)
(push '("\\.html$" . html-helper-mode) auto-mode-alist)

;; -----------------------------
;; auctex, latex, tex

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook #'LaTeX-install-toolbar)

; for AUCTeX LaTeX mode
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'outline-minor-mode)

; for Emacs latex mode
(add-hook 'latex-mode-hook 'turn-on-reftex)
(add-hook 'latex-mode-hook 'outline-minor-mode)

;; ---------------------------------------------
;; set keybindings for functions in phil-utils

(global-set-key (kbd "C-c C-x t") 'notify-timer)
(global-set-key "\C-xQ" 'phil/macro-query)
(global-set-key (kbd "M-N") 'phil/mark-end-of-line)
(global-set-key (kbd "M-P") 'phil/mark-end-of-line-previous)

(when (< emacs-major-version 23)
  (define-key isearch-mode-map (kbd "M-s o") 'phil/isearch-occur))

(eval-after-load "dired"
  '(define-key dired-mode-map (kbd "C-c C-.") 'phil/dired-cleanup-marked-files))

;; ---------------------------------------------
;; shell stuff

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

; don't echo passwords
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

;;;; make scripts executable when they're saved
(add-hook 'after-save-hook
   'executable-make-buffer-file-executable-if-script-p)

;; (autoload 'shell-toggle "shell-toggle"
;;   "Toggles between the *shell* buffer and whatever buffer you are editing."  t)
;; (autoload 'shell-toggle-cd "shell-toggle"
;;  "Pops up a shell-buffer and insert a \"cd <file-dir>\" command." t)

;; ;; TODO: shell-toggle current window instead of other window?
;; (global-set-key (kbd "C-c C-;") 'shell-toggle)
;; ;(global-set-key (kbd "C-c :") 'shell-toggle-cd) ; bound in many modes
;; (global-set-key (kbd "C-c M-;") 'shell-toggle-cd)
;; (global-set-key (kbd "C-c s")
;;  (lambda () (interactive) (shell "*pshell*")))
;; (global-set-key (kbd "C-c S")
;;  (lambda () (interactive) (shell (generate-new-buffer-name "*pshell*"))))

;; create shell in new buffer with unique name

;(setenv "TERM" "emacs")

;; ---------------------------------------------

))
