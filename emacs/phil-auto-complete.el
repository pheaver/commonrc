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

(when (and (require 'auto-complete nil t) (require 'auto-complete-config nil t))

  (add-to-list 'ac-dictionary-directories (commonrc "auto-complete.git/dict"))

  (when (and (require 'etags nil t) (require 'auto-complete-etags nil t))
    (add-to-list 'my-default-ac-sources 'ac-source-etags 'append)
    (set-face-foreground 'ac-etags-candidate-face "orange")
    (setq ac-etags-ignore-empty-table-list t)
    )

  (ac-config-default)
  (setq-default ac-sources my-default-ac-sources)

  (define-key ac-completing-map (kbd "TAB") 'auto-complete)

  (eval-after-load "verilog"
    (when (require 'auto-complete-verilog nil t)
      (add-hook 'verilog-mode-hook
                '(lambda () (add-to-list 'ac-sources 'ac-source-verilog)))))

  (setq ac-auto-start 2)
  (setq ac-delay 1.0)
  ;; (setq ac-auto-show-menu 0.8)
  (setq ac-fuzzy-enable t)
  (setq ac-use-menu-map t)
  (ac-set-trigger-key "TAB")
  )

;; ----------------------------------------

(provide 'phil-auto-complete)
