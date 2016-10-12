(load "haskell-mode-autoloads" 'noerror)

; hpaste
(autoload 'hpaste-paste-buffer "hpaste" "hpaste" t)
(autoload 'hpaste-paste-region "hpaste" "hpaste" t)

(autoload 'haskell-sort-imports "haskell-sort-imports" "haskell-sort-imports" t)
(autoload 'haskell-align-imports "haskell-align-imports" "haskell-align-imports" t)

(autoload 'ghc-init "ghc" nil t)

(defun my-haskell-cleanup-imports ()
  (interactive)
  (let ((col (current-column))) ;; Save the column.
    (haskell-align-imports)
    (haskell-sort-imports)
    (goto-char (+ (point) col)))) ;; Restore the column.

(defun my-haskell-mode-hook ()
  (make-variable-buffer-local 'after-save-hook)
  (remove-hook 'after-save-hook 'haskell-mode-after-save-handler)
  (add-hook 'after-save-hook 'phil/haskell-make-tags)
  (make-variable-buffer-local 'tags-case-fold-search)
  (setq tags-case-fold-search nil))

(with-eval-after-load 'haskell-mode
  ;; (define-key haskell-mode-map (kbd "C-c h") 'haskell-hoogle)
  (define-key haskell-mode-map (kbd "C-c .") 'my-haskell-cleanup-imports)
  (define-key haskell-mode-map (kbd "<f8>") 'haskell-navigate-imports)
  (define-key haskell-mode-map (kbd "C-<f8>") (lambda () (interactive) (haskell-navigate-imports t))))


(defun turn-on-ghc-mod ()
  (unwind-protect
      (progn
        (ghc-init)
        (remove-hook 'find-file-hook 'ghc-import-module)
        (flymake-mode)
        )))

(defun phil/haskell-make-tags ()
  (interactive)
  (let* ((cabal-file (haskell-cabal-find-file))
         (command (format "cd %s && %s | %s | %s"
                          (phil/parent-dir cabal-file)
                          "find . -name '*.hs*'"
                          "grep -v '#'" ; To avoid Emacs back-up files. Yeah.
                          "xargs hasktags -e -x")))
    (and cabal-file (start-process "maketags" "*haskell-make-tags*" shell-file-name shell-command-switch command))))

;; (phil/eval-at-init-level 3 '(add-hook 'haskell-mode-hook 'turn-on-ghc-mod))

(add-hook 'haskell-mode-hook 'turn-on-font-lock)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)

;; choose one of these indentation modes:
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(remove-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'my-haskell-mode-hook)

;; (setq haskell-indent-spaces 2)

;; haskell-indentation-mode:
;; (setq haskell-indentation-layout-offset 2)
;; (setq haskell-indentation-starter-offset 2)
;; (setq haskell-indentation-left-offset 2)
;; (setq haskell-indentation-where-pre-offset 2)
;; (setq haskell-indentation-where-post-offset 2)

;; haskell-indent-mode:
;; (setq haskell-indent-offset 4)
(setq haskell-indent-after-keywords
  '(("where" 2 0)
    ("of" 4)
    ("do" 4)
    ("in" 2 0)
    ("{" 2)
    ("=" 4)
    "if"
    "then"
    "else"
    "let"))

(setq haskell-tags-on-save nil)

;(setq haskell-literate-default 'latex)

;; ----------------------------------------

(provide 'phil-haskell)
