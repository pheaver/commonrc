;; ----------------------------------------

(load "haskell-site-file" 'noerror)

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
  (make-variable-buffer-local 'tags-case-fold-search)
  (setq tags-case-fold-search nil))

(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "C-c h") 'haskell-hoogle)
     (define-key haskell-mode-map (kbd "C-c .") 'my-haskell-cleanup-imports)))

(defun turn-on-ghc-mod ()
  (unwind-protect
      (progn
        (ghc-init)
        (remove-hook 'find-file-hook 'ghc-import-module)
        (flymake-mode)
        )))

;; (phil/eval-at-init-level 3 '(add-hook 'haskell-mode-hook 'turn-on-ghc-mod))

(add-hook 'haskell-mode-hook 'turn-on-font-lock)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;(add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)

;; choose one of these indentation modes:
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)

(setq haskell-indent-offset 4)

(setq haskell-indent-after-keywords
  '(("where" 2 0)
    ("of" 4)
    ("do" 4)
    ("in" 2 0)
    ("{" 2)
    "if"
    "then"
    "else"
    "let"))

;(setq haskell-literate-default 'latex)

;; ----------------------------------------

(provide 'phil-haskell)
