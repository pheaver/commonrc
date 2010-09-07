;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ~/commonrc/emacs/emacs.el
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-startup-screen 1)
(setq initial-scratch-message nil)

;; required by a lot of things
(require 'cc-mode)
(require 'cl)

(defvar darwin-system (string= system-type "darwin"))

(if darwin-system
    (defun notify (title msg &optional sticky)
      (shell-command
       (concat "growlnotify"
               (if sticky " -s " " ")
               "-a emacs -t \"" title "\" -m \"" msg "\"")
       nil nil)
      )
  (defun notify (title msg &optional sticky)
    (message msg)
    ))

(setq common-init-file (abbreviate-file-name load-file-name))

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; utility functions
(defun parent-dir (path)
  (interactive)
  (file-name-directory (directory-file-name path)))

(defun parent-dirs (path)
  (interactive)
  (let ((parent (parent-dir path)))
    (cond
     ((null parent) ())
     ((string= parent path) ())
     (t (cons parent (parent-dirs parent))))))

(when darwin-system
  (setq ns-command-modifier 'super)
  (setq ns-option-modifier 'meta))

;; ---------------------------------------------
;; load paths

(setq commonrc-dir (file-name-directory common-init-file))

(defun commonrc (&optional path)
  (if (null path)
      commonrc-dir
      (concat commonrc-dir path)))

(defun add-load-path (path)
  (let ((default-directory path))
    (add-to-list 'load-path (abbreviate-file-name path))
    (normal-top-level-add-subdirs-to-load-path)))

(add-load-path "~/local/share/emacs/site-lisp")
(add-load-path commonrc-dir)
(add-load-path user-emacs-directory)

;(global-set-key (kbd "C-c C-r")
;  (lambda () (interactive) (load "~/.emacs")))

;; ---------------------------------------------

(when (require 'undo-tree "undo-tree" 'noerror)
  (global-undo-tree-mode t))

(setq undo-tree-visualizer-quit-action 'save-and-restore)

;; ---------------------------------------------
;; timer

(defun notify-timer (time msg)
  "At time TIME, notify user with message MSG"
  (interactive "sRun at time: \nsMessage: ")
  (run-at-time time nil 'notify "Emacs Timer" msg t))

(defun tea-timer (time)
  (interactive "sRun at time: ")
  (notify-timer time "Tea is ready!"))

;; ---------------------------------------------
;; quattro mode

(load "cryptol-site-file" 'noerror)

;; ---------------------------------------------
;; compilation and quattro

(when darwin-system
  (add-hook 'compilation-finish-functions
    (lambda (buffer str) (notify "Compile" str))))

(defvar quattro-source-dir nil)

(defun set-quattro-source-dir ()
  (interactive)
  (or quattro-source-dir
      (let ((dirs '("~/Quattro" "~/signali/Quattro")))
        (while (and dirs (not quattro-source-dir))
          (when (file-directory-p (concat (car dirs) "/base"))
            (setq quattro-source-dir (car dirs)))
          (pop dirs))
        quattro-source-dir)))

(set-quattro-source-dir)

(defun compile-quattro ()
  (interactive)
  (if (set-quattro-source-dir)
      (compile (concat "cd " quattro-source-dir " && make -k") t)
    (error "quattro-source-dir is not set")))

(global-set-key (kbd "C-x M") 'compile-quattro)

;; ---------------------------------------------
;; recentf

;;recentf
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 500)
(setq recentf-max-menu-items 60)
;;(global-set-key (kbd "C-c r") 'recentf-open-files)
(global-set-key (kbd "C-c r") 'recentf-choose-file)
(global-set-key (kbd "C-c R") 'recentf-open-most-recent-file)

(defun recentf-choose-file ()
  "Select a recently opened file from the `recentf-list'"
  (interactive)
  (find-file
   (completing-read "Recentf open: "
                    (mapcar 'abbreviate-file-name recentf-list)
                    nil t)))

;; ---------------------------------------------
;; miscellaneous

(global-set-key (kbd "C-z") 'repeat)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

(global-set-key (kbd "C-x a r") 'align-regexp)
(global-set-key (kbd "C-x a t") 'untabify)

(when darwin-system
 (global-unset-key (kbd "s-q"))
 (global-unset-key (kbd "s-w")))

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

;; phil's itimer mode
(autoload 'itimer-list-timers "itimer" "itimer" t)

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

(when darwin-system
  (set-cursor-color "orange")
  (add-to-list 'default-frame-alist
               '(cursor-color . "orange")))

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
;; tags and expansion

(setq tags-table-list
  (list (parent-dir (commonrc))))

(add-hook 'find-file-hook 'find-tags-table)

(setq tags-revert-without-query t)

(defun find-tags-table (&optional filename)
  (interactive)
  (require 'etags)
  (let* ((filename1 (or filename (buffer-file-name)))
         (mode-name (substring (symbol-name major-mode) 0 -5))
         (tagsfile (get-tags-table mode-name (abbreviate-file-name filename1))))
    (when tagsfile
      (add-to-list (make-local-variable 'tags-table-list) tagsfile))))

(defun get-tags-table (mode-name dir)
  (interactive)
  (let* ((ext (concat "." mode-name))
         (dirs (parent-dirs dir))
         (suffixes (list "" ext)))
    (locate-file "TAGS" dirs suffixes)))

(global-set-key "\M-]" 'my-find-tag)
(global-set-key "\M-[" 'pop-tag-mark)

(defun my-find-tag ()
  (interactive)
  (find-tag (funcall
             (or find-tag-default-function
                 (get major-mode 'find-tag-default-function)
                 'find-tag-default))))

(when (require 'hippie-exp "hippie-exp" 'noerror)
  (eval-after-load "dabbrev" '(defalias 'dabbrev-expand 'hippie-expand))
  (add-to-list 'hippie-expand-try-functions-list 'try-expand-tag 'append)
  ;;(global-set-key (kbd "M-?") 'hippie-expand)
  )

(defun he-tag-beg ()
  (let ((p
         (save-excursion
           (backward-word 1)
           (point))))
    p))

(defun try-expand-tag (old)
  (unless  old
    (he-init-string (he-tag-beg) (point))
    (setq he-expand-list (sort
                          (all-completions he-search-string 'tags-complete-tag) 'string-lessp)))
  (while (and he-expand-list
              (he-string-member (car he-expand-list) he-tried-table))
              (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn
        (when old (he-reset-string))
        ())
    (he-substitute-string (car he-expand-list))
    (setq he-expand-list (cdr he-expand-list))
    t))

(defun tags-complete-tag (string predicate what)
  (require 'etags)
  (save-excursion
    ;; If we need to ask for the tag table, allow that.
    (if what
        (all-completions string (tags-completion-table) predicate)
      (try-completion string (tags-completion-table) predicate))))


;; ---------------------------------------------
;; auto-complete

(setq my-default-ac-sources
  '(ac-source-abbrev
    ac-source-words-in-buffer
    ac-source-files-in-current-dir
    ac-source-filename))

(when (require 'auto-complete nil t)
  (set-default 'ac-sources my-default-ac-sources)

  (when (and (require 'etags nil t) (require 'auto-complete-etags nil t))
    (add-to-list 'my-default-ac-sources 'ac-source-etags 'append)
    (set-face-foreground 'ac-etags-candidate-face "orange")
    )

  (eval-after-load "verilog"
    (when (require 'auto-complete-verilog nil t)
      (add-hook 'verilog-mode-hook
                '(lambda () (add-to-list 'ac-sources 'ac-source-verilog)))))

  (setq ac-auto-start 2)
  (setq ac-delay 1.0)

  (add-hook 'lisp-interaction-mode
            '(lambda () (add-to-list 'ac-sources 'ac-source-symbols)))
  (add-hook 'emacs-lisp-mode-hook
            '(lambda () (add-to-list 'ac-sources 'ac-source-symbols)))

  (add-to-list 'ac-modes 'verilog-mode)
  (add-to-list 'ac-modes 'haskell-mode)

  ;; defaults, not sure if i like them
  (define-key ac-complete-mode-map (kbd "TAB") 'ac-expand)
  (define-key ac-complete-mode-map (kbd "RET") 'ac-complete)

  (define-key ac-complete-mode-map "\M-n" 'ac-next)
  (define-key ac-complete-mode-map "\M-p" 'ac-previous)
  ;;(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)

  (ac-set-trigger-key "M-TAB")

  (global-auto-complete-mode t)
  )

;; ---------------------------------------------
;; erc

(autoload 'erc "erc" "erc mode" t)

(setq erc-autojoin-channels-alist
      '(("docserver" "#galois")
        ("docserver.galois.com" "#galois")))

;(defun erc-osd-notice (str msg buffer sender)
;  (osd-echo (concat str "; " msg "; " buffer "; " sender))
;  t)

;(setq erc-auto-query 'window-noselect)

(setq erc-notify t)

(defun erc-docserver () (interactive)
  (erc :server "docserver.galois.com" :port 6667 :nick "phil"))

;(defun joel-erc-notify (proc parsed) ())
(defun joel-erc-notify (proc parsed)
  (let* ((nick (car (erc-parse-user (erc-response.sender parsed))))
         (target (car (erc-response.command-args parsed)))
         (msg (erc-response.contents parsed))
         (query (if (not erc-query-on-unjoined-chan-privmsg)
                     nick
                   (if (erc-current-nick-p target)
                       nick
                     target))))
    (and (not (erc-ignored-user-p (erc-response.sender parsed)))
         (not (eq (erc-get-buffer query proc) (current-buffer)))
         ;; this portion of the predicate filters out channel activity and
         ;; restricts to just private messages
         (or erc-query-on-unjoined-chan-privmsg
             (string= target (erc-current-nick)))
         (not (erc-is-message-ctcp-and-not-action-p msg))
         (if erc-notify (notify nick msg))
         nil)))

(add-hook 'erc-after-connect
 (lambda (server nick)
   (add-hook 'erc-server-PRIVMSG-functions 'joel-erc-notify)))

;(add-hook 'erc-server-NOTICE-hook 'erc-auto-query)))

;(add-hook 'erc-server-PRIVMSG-functions 'erc-auto-query)

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
;; browse-kill-ring

(when (require 'browse-kill-ring nil 'noerror)
  (browse-kill-ring-default-keybindings)
  (setq browse-kill-ring-quit-action 'save-and-restore))

;; ---------------------------------------------
;; navigation, marking

(global-set-key (kbd "M-g") 'goto-line) ;; default is M-g Mg
(global-set-key [wheel-up] (lambda () (interactive) (scroll-down 10)))
(global-set-key [wheel-down] (lambda () (interactive) (scroll-up 10)))
(global-set-key [mouse-4] (lambda () (interactive) (scroll-down 10)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up 10)))

(set-register ?i (cons 'file user-init-file))
(set-register ?e (cons 'file common-init-file))
(set-register ?l (cons 'file "~/.localrc"))

(global-set-key (kbd "C-x r v") 'view-register)
(global-set-key (kbd "C-x r L") 'list-registers)
(global-set-key (kbd "C-x /") 'point-to-register)  ;; also bound to C-x r SPC
(global-set-key (kbd "C-x j") 'register-to-point)  ;; also bound to C-x r j

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

(require 'phil-wspace "phil-wspace" 'noerror)

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)

(defun revert-buffer-noconfirm (&optional ignore-auto preserve-modes)
  (interactive "P")
  (revert-buffer ignore-auto t preserve-modes))

(global-set-key (kbd "C-M-]") 'bury-buffer)
(global-set-key (kbd "C-x g") 'revert-buffer)
(global-set-key (kbd "C-x G") 'revert-buffer-noconfirm)

;; shells/terminals
(require 'phil-term)

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
;; organizer, planner, todo list

(when (require 'org-install "org-install" t)
  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
  (add-hook 'org-mode-hook 'turn-on-font-lock)  ; Org buffers only
  )

(setq org-agenda-files '("~/Documents"))

(setq org-export-with-archived-trees nil) ;; nil, t, headline

(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-agenda-dim-blocked-tasks t) ;; default
(setq org-goto-auto-isearch nil)
(setq org-archive-default-command 'org-archive-to-archive-sibling)

(setq org-special-ctrl-a/e 'reversed)
(setq org-special-ctrl-k t)
(setq org-cycle-global-at-bob t)
(setq org-use-speed-commands t)

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(defun find-org-files () (interactive)
  (require 'org)
  (let* ((alist (mapcar (lambda (x) (cons (file-name-nondirectory x) x)) (org-agenda-files)))
         (name (ido-completing-read "Org file: " (mapcar 'car alist)))
         (file-name (cdr (assoc-string name alist)))
         (org-buffer (org-get-agenda-file-buffer file-name)))
    (switch-to-buffer org-buffer))
  )

(global-set-key (kbd "C-c C-'") 'find-org-files)

; "C-cl" is bound in tex/latex mode, so we use C-s S-l also
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c S-l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)

;(setq org-log-done t)

(add-hook 'diary-display-hook 'fancy-diary-display)

;(defun todo-entry-timestamp ()
;  "Prepend timestamp (but no initials!) to the head of a TODO entry."
;  (let ((time-stamp-format todo-time-string-format))
;    (format "(%s) " (time-stamp-string))))

;(global-set-key (kbd "C-c t") 'todo-show)
;(global-set-key (kbd "C-c i") 'todo-insert-item)

;(global-set-key (kbd "C-c P")
;   (lambda () (interactive) (find-file "~/plans")))

;; ---------------------------------------------
;; version control

(defun diff-cur-buffer ()
  (interactive)
  (diff-buffer-with-file (current-buffer)))

;(defun gitk ()
;  (interactive)
;  (shell-command "gitk --all &"))

; broken
;(global-set-key (kbd "C-x v s") 'git-status)
;(add-load-path (commonrc "git-emacs"))
;; (if (require 'vc-git nil t)
;;     (require 'git-emacs nil t))

;(require 'dvc-autoloads "dvc-autoloads" t)
(autoload 'magit-status "magit" "magit" t)
(autoload 'egg-status "egg" "egg" t)

(defun phil/magit-log-edit (arg)
  (interactive "P")
  (magit-log-edit)
  (when arg (magit-log-edit-toggle-amending)))

(eval-after-load 'magit
  '(define-key magit-mode-map (kbd "c") 'phil/magit-log-edit))

(setq magit-omit-untracked-dir-contents t)

(setq magit-repo-dirs-depth 1)
(eval-after-load "magit"
  '(when (fboundp 'ido-completing-read)
     (setq magit-completing-read 'ido-completing-read)))

(setq magit-repo-dirs
  (list "~" (commonrc) user-emacs-directory "~/signali"))

;; over ssh on CentOS, C-/ behaves as C-_
(global-set-key (kbd "C-x C-/") 'magit-status)
(global-set-key (kbd "C-x C-_") 'magit-status)

;; ---------------------------------------------
;; C and c++ modes

(defun my-c-mode-common-hook ()
  (setq c-basic-offset 4)
  (c-set-offset 'substatement-open 0)
  )
;;(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; ---------------------------------------------
;; haskell stuff

(when (load "haskell-site-file" 'noerror)

  (add-hook 'haskell-mode-hook 'turn-on-font-lock)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  ;(add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)

  ;; choose one of these indentation modes:
  ;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  ;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  (setq haskell-indent-offset 2)

  (global-set-key (kbd "C-c h") 'haskell-hoogle)

  ;(setq haskell-literate-default 'latex)
)

; hpaste
(autoload 'hpaste-paste-buffer "hpaste" "hpaste" t)
(autoload 'hpaste-paste-region "hpaste" "hpaste" t)

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

;; ---------------------------------------------
;; for auto-resizing windowed emacs

;; (defun fullscreen () (interactive)
;;   (set-frame-parameter nil 'fullscreen
;;                        (if (frame-parameter nil 'fullscreen)
;;                            nil
;;                          'fullboth)))

(defun resize-frame (frame) (interactive)
  (progn
    (set-frame-position frame 0 0)
    (mf-set-frame-pixel-size frame
                             1400
                             (- (display-pixel-height) 50))))

(defun resize-current-frame () (interactive)
  (resize-frame (selected-frame)))

(add-hook 'after-make-frame-functions 'phil-new-frame-hook)
;;(add-hook 'window-setup-hook 'maximize-frame t)

(defun phil-new-frame-hook (frame)
  (interactive)
  (case (framep frame)
    ('ns (when (require 'maxframe "maxframe" 'noerror)
           (select-frame frame)
           (ns-raise-emacs)
           (sleep-for 0 10)
           (maximize-frame)))
    ('x (raise-frame frame))
    ))

(when darwin-system
  (global-set-key (kbd "<s-return>") 'ns-toggle-fullscreen))
;;  (global-set-key (kbd "<C-M-return>") 'maximize-frame)

(defun ns-raise-emacs ()
  (ns-do-applescript "tell application \"Emacs\" to activate"))

;;(add-hook 'server-switch-hook 'raise-emacs-on-aqua)
(add-hook 'server-switch-hook
  (lambda () (local-set-key (kbd "C-c k") 'server-edit)))

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

