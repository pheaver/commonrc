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

(defun phil-auto-complete-setup ()
  (interactive)
  (require 'auto-complete)
  (require 'auto-complete-config)

  (add-to-list 'ac-dictionary-directories (commonrc "auto-complete.git/dict"))

  (when (and (require 'etags nil t) (require 'auto-complete-etags nil t))
    (add-to-list 'my-default-ac-sources 'ac-source-etags 'append)
    (set-face-foreground 'ac-etags-candidate-face "orange")
    (setq ac-etags-ignore-empty-table-list t)
    )

  (ac-config-default)
  (setq-default ac-sources my-default-ac-sources)

  (add-hook 'haskell-mode-hook
     (lambda ()
       (when (featurep 'ghc) ;; (require 'ghc-mod nil t)
         (add-to-list 'ac-sources 'ac-source-ghc-mod))))

  (eval-after-load "verilog"
    (when (require 'auto-complete-verilog nil t)
      (add-hook 'verilog-mode-hook
                '(lambda () (add-to-list 'ac-sources 'ac-source-verilog)))))

  ;; (define-key ac-completing-map (kbd "TAB") 'ac-expand)
  ;; (define-key ac-completing-map (kbd "<C-tab>") 'auto-complete)

  ;; never start showing completions automatically;
  ;; wait until I hit "TAB"
  (setq ac-auto-start nil)
  (ac-set-trigger-key "TAB")
  ;; (setq ac-delay 0.1)

  ;; when completions start, immediately popup the menu,
  ;; and support local keybindings in the menu (like C-n and C-p)
  (setq ac-auto-show-menu t)
  (setq ac-use-menu-map t)

  ;; enable fuzzy matching
  (setq ac-fuzzy-enable t)
  )

(when (daemonp)
  (phil-auto-complete-setup))

;; ----------------------------------------

(provide 'phil-auto-complete)
