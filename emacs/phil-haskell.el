;; ----------------------------------------

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

(autoload 'haskell-sort-imports "haskell-sort-imports" "haskell-sort-imports" t)
(autoload 'haskell-align-imports "haskell-align-imports" "haskell-align-imports" t)

(define-key haskell-mode-map (kbd "C-c .")
  (lambda ()
    (interactive)
    (let ((col (current-column))) ;; Save the column.
      (haskell-align-imports)
      (haskell-sort-imports)
      (goto-char (+ (point) col))))) ;; Restore the column.

(push '("\\.[hg]s$" . haskell-mode) auto-mode-alist)
(push '("\\.hi$" . haskell-mode) auto-mode-alist)
(push '("\\.l[hg]s$" . literate-haskell-mode) auto-mode-alist)
(push '("\\.lang" . haskell-mode) auto-mode-alist)
(push '("\\.monad" . haskell-mode) auto-mode-alist)

; hpaste
(autoload 'hpaste-paste-buffer "hpaste" "hpaste" t)
(autoload 'hpaste-paste-region "hpaste" "hpaste" t)

;; ----------------------------------------

(provide 'phil-haskell)
