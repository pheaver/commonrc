;; ----------------------------------------
;; my auto-complete settings

(require 'phil-paths)

(defvar my-default-ac-sources
  '(ac-source-abbrev
    ;; ac-source-words-in-buffer
    ac-source-words-in-same-mode-buffers
    ac-source-dictionary
    ac-source-files-in-current-dir
    ))

(autoload 'auto-complete "auto-complete" nil t)
(global-set-key (kbd "C-c TAB") 'auto-complete)

(defun ac-etags-setup ()
  (add-to-list 'ac-sources 'ac-source-etags 'append))

(defun ac-verilog-setup ()
  (add-to-list 'ac-sources 'ac-source-verilog))

(defun ac-haskell-setup ()
  (when (featurep 'ghc)
    (add-to-list 'ac-sources 'ac-source-ghc-mod)))

(defun phil-auto-complete-setup ()
  (interactive)
  (require 'auto-complete)
  (require 'auto-complete-config)

  (add-to-list 'ac-dictionary-directories (commonrc-dir "auto-complete.git/dict"))
  (add-to-list 'ac-modes 'makefile-gmake-mode)

  (setq-default ac-sources my-default-ac-sources)

  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  (when (memq major-mode '(emacs-lisp-mode lisp-interaction-mode inferior-emacs-lisp-mode))
    (ac-emacs-lisp-mode-setup))

  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (when (equal major-mode 'c-mode) (ac-cc-mode-setup))

  (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  (when (equal major-mode 'ruby-mode) (ac-ruby-mode-setup))

  (add-hook 'css-mode-hook 'ac-css-mode-setup)
  (when (equal major-mode 'css-mode) (ac-css-mode-setup))

  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (ac-common-setup)

  (when (and (require 'etags nil t) (require 'auto-complete-etags nil t))
    (set-face-foreground 'ac-etags-candidate-face "orange")
    (setq ac-etags-ignore-empty-table-list t)
    (add-hook 'auto-complete-mode-hook 'ac-etags-setup)
    (ac-etags-setup))

  (add-hook 'haskell-mode-hook 'ac-haskell-setup)
  (when (equal major-mode 'haskell-mode) (ac-haskell-setup))

  (eval-after-load 'verilog-mode
    (when (require 'auto-complete-verilog nil t)
      (add-hook 'verilog-mode-hook 'ac-verilog-setup)
      (when (equal major-mode 'verilog-mode) (ac-verilog-setup))))

  ;; this is default.  it makes TAB cycle through menu elements
  ;; (define-key ac-completing-map (kbd "TAB") 'ac-expand)

  ;; can be helpful when ac-delay is high and the menu doesn't update.
  ;; this will start the auto-complete over on whatever you've typed,
  ;; which has the same effect as when ac-delay expires.
  ;; by default, M-TAB is bound to this, but that key isn't always available.
  (define-key ac-completing-map (kbd "<C-tab>") 'auto-complete)

  ;; never start showing completions automatically;
  ;; wait until I hit "TAB"
  (setq ac-auto-start nil)
  (ac-set-trigger-key "TAB")
  (setq ac-delay 0.1) ;; while auto-complete is active, this will
                      ;; cause the menu to update its results as you type.

  ;; when completions start, immediately popup the menu,
  ;; and support local keybindings in the menu (like C-n and C-p)
  (setq ac-auto-show-menu t)
  (setq ac-use-menu-map t)

  ;; enable fuzzy matching
  (setq ac-fuzzy-enable t)

  (global-auto-complete-mode t)
  )

(eval-after-load 'auto-complete
  '(phil-auto-complete-setup))

(phil/eval-at-init-level 3
  '(require 'auto-complete nil t))

;; ----------------------------------------

(provide 'phil-auto-complete)
