;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ~/commonrc/emacs/emacs.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-startup-screen 1)
(setq initial-scratch-message nil)

;; required by a lot of things
(require 'cc-mode)
(require 'cl)
(require 'phil-auto-complete)
;; (require 'phil-erc)
(require 'phil-haskell)
(require 'phil-hippie-expand)
(require 'phil-notify-timer)
(require 'phil-org)
(require 'phil-paths)
(require 'phil-quattro)
(require 'phil-recentf)
(require 'phil-resize-frame)
(require 'phil-tags)
(require 'phil-term)
(require 'phil-vc)
(require 'phil-wspace)

;; ---------------------------------------------
;; registers

(setq common-init-file (abbreviate-file-name load-file-name))

(set-register ?i (cons 'file user-init-file))
(set-register ?e (cons 'file common-init-file))
(set-register ?l (cons 'file "~/.localrc"))

(global-set-key (kbd "C-x r v") 'view-register)
(global-set-key (kbd "C-x r L") 'list-registers)
(global-set-key (kbd "C-x /") 'point-to-register)  ;; also bound to C-x r SPC
(global-set-key (kbd "C-x j") 'register-to-point)  ;; also bound to C-x r j

;; ---------------------------------------------
;; miscellaneous keybindings

(global-set-key (kbd "M-g") 'goto-line) ;; default is M-g Mg
(global-set-key [wheel-up] (lambda () (interactive) (scroll-down 10)))
(global-set-key [wheel-down] (lambda () (interactive) (scroll-up 10)))
(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 10)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 10)))
(global-set-key (kbd "C-z") 'repeat)
(global-set-key (kbd "C-x a r") 'align-regexp)
(global-set-key (kbd "C-x a t") 'untabify)

;; ---------------------------------------------

(when (require 'undo-tree "undo-tree" 'noerror)
  (global-undo-tree-mode t))

(setq undo-tree-visualizer-quit-action 'save-and-restore)

;; ---------------------------------------------
;; browse-kill-ring

(when (require 'browse-kill-ring nil 'noerror)
  (browse-kill-ring-default-keybindings)
  (setq browse-kill-ring-quit-action 'save-and-restore))

;; ---------------------------------------------

(add-hook 'server-switch-hook
  (lambda () (local-set-key (kbd "C-c k") 'server-edit)))

;; ---------------------------------------------
;; miscellaneous

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

;; save backup files in ~/.emacs_backup instead of current directory
(setq backup-directory-alist
      `((,(expand-file-name "~/signali") . "~/signali/.emacs_backups")
        ("." . "~/.emacs_backups")))

(setq-default fill-column 80)

(setq require-final-newline t)

;;(setq kill-whole-line t)

;; tail
(autoload 'tail-file "tail" "tail" t)

;; use aspell instead of ispell
(setq-default ispell-program-name "aspell")
(setq ispell-extra-args '("--sug-mode=ultra"))

;; enable some stuff that is normally disabled
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)

;; -------------------

(autoload 'wikipedia-mode "wikipedia-mode.el"
"Major mode for editing documents in Wikipedia markup." t)

;; -------------------
;; open file as sudo

;; meant to be called from command line.
;; only works for absolute paths
;; TODO: use ido to make it easy to use interactively.
(defun find-file-sudo (filename)
  (interactive)
  (find-file (concat "/sudo::" filename)))

;; ---------------------------------------------
;; appearance - cursor, highlighting, colors

;; draw block cursor as wide as the glyph under it
(setq x-stretch-cursor t)

;; make the mark visible
;(when (require 'visible-mark nil 'noerror)
;  (global-visible-mark-mode 1))

;; do not blink the cursor
(blink-cursor-mode (- (*) (*) (*)))

(setq set-mark-command-repeat-pop t)

;; enable hl-line-mode for all major modes except these:
;; (defvar hl-line-disabled-mode-list '()
;;   "List of major modes in which `hl-line-mode' should be disabled.
;; In all other modes, `hl-line-mode' is enabled")

;; (setq hl-line-disabled-mode-list '(phil-term-mode magit-mode term-mode))
;; (setq hl-line-sticky-flag nil)

;; (defun maybe-hl-line-mode (&optional buffer)
;;   "Enable or disable `hl-line-mode' in BUFFER, based on the
;; whether BUFFER's major-mode is in `hl-line-disabled-mode-list'.
;; If BUFFER is nil, use current buffer"
;;   (interactive)
;;   (let ((mode (buffer-local-value 'major-mode (or buffer (current-buffer)))))
;;     (if (member mode hl-line-disabled-mode-list)
;;         (hl-line-mode 0)
;;       (hl-line-mode 1))))

;; ;; setup hl-line
;; (when (require 'hl-line)
;;   (global-hl-line-mode 1)
;;   (set-face-background 'hl-line "khaki1")
;;   (add-hook 'after-change-major-mode-hook 'maybe-hl-line-mode))

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
;; ibuffer, iswitch, ido

(if (require 'ibuffer nil t)
    (global-set-key (kbd "C-x C-b") 'ibuffer))

(if (require 'ido nil t)
    (ido-mode t)
  (if (require 'iswitchb nil t)
      (iswitchb-mode t)))

(eval-after-load "ido"
  '(progn
     (ido-everywhere t)
     (defalias 'read-buffer 'ido-read-buffer)
     (defalias 'read-directory-name 'ido-read-directory-name)
     (defalias 'read-file-name 'ido-read-file-name)))

(defvar ido-enable-replace-completing-read t
  "If t, use ido-completing-read instead of completing-read if possible.

Set it to nil using let in around-advice for functions where the
original completing-read is required.  For example, if a function
foo absolutely must use the original completing-read, define some
advice like this:

(defadvice foo (around original-completing-read-only activate)
  (let (ido-enable-replace-completing-read) ad-do-it))")

;; Replace completing-read wherever possible, unless directed otherwise
(defadvice completing-read
  (around use-ido-when-possible activate)
  (if (or (not ido-enable-replace-completing-read) ; Manual override disable ido
          (boundp 'ido-cur-list)) ; Avoid infinite loop from ido calling this
      ad-do-it
    (let ((allcomp (all-completions "" collection predicate)))
      (if allcomp
          (setq ad-return-value
                (ido-completing-read prompt
                               allcomp
                               nil require-match initial-input hist def))
        ad-do-it))))

(setq ido-execute-command-cache nil)

(defun ido-execute-command ()
  (interactive)
  (call-interactively
   (intern
    (ido-completing-read
     "M-x "
     (progn
       (unless ido-execute-command-cache
         (mapatoms (lambda (s)
                     (when (commandp s)
                       (setq ido-execute-command-cache
                             (cons (format "%S" s) ido-execute-command-cache))))))
       ido-execute-command-cache)))))

(global-set-key (kbd "M-X") 'ido-execute-command)

;(setq ido-enable-flex-matching nil)

;; (when (and darwin-system window-system)
;;     (add-hook 'ido-setup-hook
;;            '(lambda ()
;;               (set-face-attribute 'ido-first-match '() :foreground "blue4" :weight 'bold)
;;               (set-face-attribute 'ido-subdir '() :foreground "khaki")
;;               ))
;; )

;(setq iswitchb-buffer-ignore
;      '("^ " "*Buffer" "*Help*" "*Messages" "*Shell Command Output" "*Completions"))
(setq ido-ignore-buffers
      '("^ " "*Buffer" "*Help*" "*Messages" "*Shell Command Output" "*Completions"))

(setq ido-max-prospects 30)
(setq ido-max-window-height 2) ;; nil means use max-mini-window-height

(setq ido-decorations '("" "" "," " ..." "[" "]" " [No match]" " [Matched]"))

;; kinda neat:
;; (setq ido-decorations '("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))

;; ---------------------------------------------
;; buffer stuff

;; TODO: swap buffers
;; TODO: scroll other window?
;; TODO: kill current buffer, kill other buffer?

(global-set-key (kbd "C-M-;")
 (lambda () (interactive) (switch-to-buffer (other-buffer)))) ;; nil means (other-buffer)
(global-set-key (kbd "C-M-'")
 (lambda () (interactive) (display-buffer (other-buffer) 'not-this-window)))
;; (lambda () (interactive) (set-window-buffer (next-window) (other-buffer))))

;(global-set-key (kbd "C-M-;")
; (lambda () (interactive) (display-buffer (other-buffer))))

; this one is insane; call it three times to swap
; (lambda () (interactive) (switch-to-buffer-other-window (other-buffer))))

;; (global-set-key [(control tab)]
;;  (lambda () (interactive) (other-window 1)))
;; (global-set-key [(control shift tab)]
;;  (lambda () (interactive) (other-window -1)))

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)

(defun revert-buffer-noconfirm (&optional ignore-auto preserve-modes)
  (interactive "P")
  (revert-buffer ignore-auto t preserve-modes))

(global-set-key (kbd "C-M-]") 'bury-buffer)
(global-set-key (kbd "C-x g") 'revert-buffer)
(global-set-key (kbd "C-x G") 'revert-buffer-noconfirm)

; TODO: use (dired-get-file-for-visit) to call "open" on a file
(defun open-file () (interactive) (shell-command-on-file "open"))

(defun shell-command-on-file (command) (interactive)
  (let ((n (buffer-file-name)))
    (if (null n)
        (message (concat "Not a file: " (buffer-name)))
        (shell-command (concat command " " n)))))

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

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

; don't echo passwords
(add-hook 'comint-output-filter-functions
          'comint-watch-for-password-prompt)

;; ---------------------------------------------
;; markdown mode

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(push '("\\.md" . markdown-mode) auto-mode-alist)
(push '("\\.markdown" . markdown-mode) auto-mode-alist)

;; ---------------------------------------------
;; C and c++ modes

(defun my-c-mode-common-hook ()
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0)
  )
;;(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

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

;; ---------------------------------------------
;; Activate modes based on file extension

(push '("\\.zsh$" . sh-mode) auto-mode-alist)
(push '("\\.c$" . c-mode) auto-mode-alist)
(push '("\\.h$" . c-mode) auto-mode-alist)
(push '("\\.cc$" . c++-mode) auto-mode-alist)
(push '("\\.icc$" . c++-mode) auto-mode-alist)
(push '("\\.C$" . c++-mode) auto-mode-alist)
(push '("\\.hh$" . c++-mode) auto-mode-alist)
(push '("\\.txt$" . text-mode) auto-mode-alist)
(push '("\\.a$" . ada-mode) auto-mode-alist)
(push '("\\.vhdl$" . vhdl-mode) auto-mode-alist)
(push '("\\.vhd$" . vhdl-mode) auto-mode-alist)
(push '("\\.[hg]s$" . haskell-mode) auto-mode-alist)
(push '("\\.hi$" . haskell-mode) auto-mode-alist)
(push '("\\.l[hg]s$" . literate-haskell-mode) auto-mode-alist)
(push '("\\.html$" . html-helper-mode) auto-mode-alist)
(push '("\\.lang" . haskell-mode) auto-mode-alist)
(push '("\\.monad" . haskell-mode) auto-mode-alist)
(push '("\\.org\\'" . org-mode) auto-mode-alist)

;; ----------------------------
;; set some minor modes

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
