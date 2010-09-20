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

;;;; load my other files
(require 'phil-auto-complete)
(require 'phil-breadcrumb)
(require 'phil-buffers)
(require 'phil-darwin)
;; (require 'phil-erc) ;; not used
(require 'phil-flymake)
(require 'phil-haskell)
(require 'phil-hippie-expand)
;; (require 'phil-mew) ;; disabled because I don't use it
(require 'phil-org)
(require 'phil-quattro)
;; (require 'phil-recentf) ;; increases startup time, and i don't even use it
(require 'phil-frames)
(require 'phil-tags)
(require 'phil-term)
(require 'phil-vc)
(require 'phil-wspace)

;; autoloads for cl-* files
(load "cl-loaddefs")

;;;; registers
(set-register ?i (cons 'file user-init-file))
(set-register ?e (cons 'file common-init-file))
(set-register ?l (cons 'file local-rc-file))

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

;;; phil's itimer mode
(autoload 'itimer-list-timers "itimer" "itimer" t)
(global-set-key (kbd "C-c L") 'itimer-list-timers)

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

;;;; miscellaneous
(setq-default fill-column 80)
(setq set-mark-command-repeat-pop t)
(setq require-final-newline t)
;;(setq kill-whole-line t)

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
(when (functionp 'transient-mark-mode) (transient-mark-mode 0))
(when (functionp 'savehist-mode) (savehist-mode 1))
(when (functionp 'global-font-lock-mode) (global-font-lock-mode 1))
;; (when (functionp 'menu-bar-mode) (menu-bar-mode 0))

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
;; miscellaneous stuff from phil-utils

(autoload 'phil/find-file-sudo             "phil-utils" nil t)
(autoload 'phil/macro-query                "phil-utils" nil t)
(autoload 'phil/mark-end-of-line           "phil-utils" nil t)
(autoload 'phil/mark-end-of-line-previous  "phil-utils" nil t)
(autoload 'phil/isearch-occur              "phil-utils" nil t)
(autoload 'phil/dired-cleanup-marked-files "phil-utils" nil t)

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
