(defun my-haskell-cleanup-imports ()
  (interactive)
  (let ((col (current-column))) ;; Save the column.
    (haskell-align-imports)
    (haskell-sort-imports)
    (goto-char (+ (point) col)))) ;; Restore the column.

(defun my-haskell-mode-hook ()
  (make-variable-buffer-local 'after-save-hook)
  (make-variable-buffer-local 'flycheck-check-syntax-automatically)
  (remove-hook 'after-save-hook 'haskell-mode-after-save-handler)
  ;; (add-hook 'after-save-hook 'phil/haskell-make-tags)
  (make-variable-buffer-local 'tags-case-fold-search)
  (setq tags-case-fold-search nil)
  (add-to-list 'company-backends 'dante-company)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  )

(defun phil/haskell-cleanup-imports-and-return ()
  (interactive)
  (my-haskell-cleanup-imports)
  (haskell-navigate-imports t)
  ;; (pop-to-mark-command)
  )

(defun phil/haskell-make-tags ()
  (interactive)
  (let* ((cabal-file (haskell-cabal-find-file))
         (command (format "cd %s && %s | %s | %s"
                          (phil/parent-dir cabal-file)
                          "find . -name '*.hs*'"
                          "grep -v '#'" ; To avoid Emacs back-up files. Yeah.
                          "xargs hasktags -e -x")))
    (and cabal-file (start-process "maketags" "*haskell-make-tags*" shell-file-name shell-command-switch command))))

(use-package haskell-mode
  :config
  (add-hook 'haskell-mode-hook 'turn-on-font-lock)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
  ;;(add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)

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
    '(("where" 2 0) ("of" 4) ("do" 4) ("in" 2 0) ("{" 2) ("=" 4) "if" "then" "else" "let"))

  (setq haskell-tags-on-save nil)

  ;(setq haskell-literate-default 'latex)

  :bind (
         :map haskell-mode-map
         ( "C-c ." . my-haskell-cleanup-imports )
         ( "<f8>" .  haskell-navigate-imports)
         ( "M-g i" .  haskell-navigate-imports)
         ( "C-<f8>" . phil/haskell-cleanup-imports-and-return)
         ))

(use-package dante
  ;; :ensure t
  :after haskell-mode
  :commands 'dante-mode
  :init
  (setq dante-repl-command-line-methods-alist
    `( (stack . ,(lambda (root) (dante-repl-by-file root '("stack.yaml") '("stack" "repl" dante-target)))) ))
  (add-hook 'haskell-mode-hook 'dante-mode)
  (add-hook 'haskell-mode-hook 'flycheck-mode))

(provide 'phil-haskell)
