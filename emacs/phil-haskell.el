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

; hpaste
(autoload 'hpaste-paste-buffer "hpaste" "hpaste" t)
(autoload 'hpaste-paste-region "hpaste" "hpaste" t)

;; ----------------------------------------

(provide 'phil-haskell)
