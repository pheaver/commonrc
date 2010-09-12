;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ~/commonrc/emacs/emacs.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-startup-screen 1)
(setq initial-scratch-message nil)

(load-file (concat (file-name-directory load-file-name) "/" "phil-paths.el"))

;;;; load my other files
(require 'phil-auto-complete)
(require 'phil-buffers)
(require 'phil-darwin)
;; (require 'phil-erc) ;; not used
(require 'phil-haskell)
(require 'phil-hippie-expand)
(require 'phil-notify-timer)
(require 'phil-org)
(require 'phil-quattro)
;; (require 'phil-recentf) ;; increases startup time, and i don't even use it
(require 'phil-frames)
(require 'phil-tags)
(require 'phil-term)
(require 'phil-vc)
(require 'phil-wspace)

;;;; registers
(setq common-init-file (abbreviate-file-name load-file-name))

(set-register ?i (cons 'file user-init-file))
(set-register ?e (cons 'file common-init-file))
(set-register ?l (cons 'file "~/.localrc"))

(global-set-key (kbd "C-x r v") 'view-register)
(global-set-key (kbd "C-x r L") 'list-registers)
(global-set-key (kbd "C-x /") 'point-to-register)  ;; also bound to C-x r SPC
(global-set-key (kbd "C-x j") 'register-to-point)  ;; also bound to C-x r j
(global-set-key (kbd "C-x F") 'find-file-at-point)

;;;; miscellaneous keybindings
(global-set-key (kbd "M-g") 'goto-line) ;; default is M-g Mg
(global-set-key [wheel-up] (lambda () (interactive) (scroll-down 10)))
(global-set-key [wheel-down] (lambda () (interactive) (scroll-up 10)))
(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 10)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 10)))
(global-set-key (kbd "C-z") 'repeat)
(global-set-key (kbd "C-x a r") 'align-regexp)
(global-set-key (kbd "C-x a t") 'untabify)

;;;; undo-tree
(when (require 'undo-tree "undo-tree" 'noerror)
  (global-undo-tree-mode t))
(setq undo-tree-visualizer-quit-action 'save-and-restore)

;;;; browse-kill-ring
(when (require 'browse-kill-ring nil 'noerror)
  (browse-kill-ring-default-keybindings)
  (setq browse-kill-ring-quit-action 'save-and-restore))

;;;; server-edit
(add-hook 'server-switch-hook
  (lambda () (local-set-key (kbd "C-c k") 'server-edit)))

;;;; flymake
(setq flymake-no-changes-timeout nil)
(setq flymake-start-syntax-check-on-newline nil)

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
(when (functionp 'size-indication-mode) (size-indication-mode 1))
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
;; mew mail

(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)
(global-set-key "\C-xm" 'mew)
(setq mew-rc-file (commonrc "mewrc.el"))

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

;; -------------------
;; open file as sudo

;; meant to be called from command line.
;; only works for absolute paths
;; TODO: use ido to make it easy to use interactively.
(defun find-file-sudo (filename)
  (interactive)
  (find-file (concat "/sudo::" filename)))

;; ---------------------------------------------
;; macro query

(defun my-macro-query (arg)
  "Prompt for input using minibuffer during kbd macro execution.
With prefix argument, allows you to select what prompt string to use.
If the input is non-empty, it is inserted at point."
  (interactive "P")
  (let* ((prompt (if arg (read-from-minibuffer "PROMPT: ") "Input: "))
          (input (minibuffer-with-setup-hook (lambda () (kbd-macro-query t))
                     (read-from-minibuffer prompt))))
    (unless (string= "" input) (insert input))))

(global-set-key "\C-xQ" 'my-macro-query)

;; ---------------------------------------------

(defun mark-end-of-line (arg)
  "Put mark at end of line.  Arg works as in `forward-line'.
If this command is repeated, it marks the next ARG lines after
the ones already marked.  Identical to `mark-end-of-sentence',
except uses `forward-line' instead of `forward-sentence'."
  (interactive "p")
  (push-mark
   (save-excursion
     (if (and (eq last-command this-command) (mark t))
         (goto-char (mark)))
     (forward-line arg)
     (point))
   nil t))

;; ---------------------------------------------
;; isearch show all occurrancs

(define-key isearch-mode-map (kbd "M-o")
  (lambda ()
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string
               (regexp-quote isearch-string))))))

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
