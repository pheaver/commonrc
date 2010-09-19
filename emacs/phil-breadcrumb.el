;; ----------------------------------------
;; my breadcrumb settings

(autoload 'bc-set            "breadcrumb" nil t)
(autoload 'bc-previous       "breadcrumb" nil t)
(autoload 'bc-next           "breadcrumb" nil t)
(autoload 'bc-local-previous "breadcrumb" nil t)
(autoload 'bc-local-next     "breadcrumb" nil t)
(autoload 'bc-goto-current   "breadcrumb" nil t)
(autoload 'bc-list           "breadcrumb" nil t)
(autoload 'bc-clear          "breadcrumb" nil t)

(global-set-key (kbd "C-c SPC") 'bc-set)
(global-set-key (kbd "C-c p"  ) 'bc-previous)
(global-set-key (kbd "C-c n"  ) 'bc-next)
(global-set-key (kbd "C-c P"  ) 'bc-local-previous)
(global-set-key (kbd "C-c N"  ) 'bc-local-next)
(global-set-key (kbd "C-c C-l") 'bc-list)

(autoload 'phil/switch-to-buffer "phil-utils" nil t)
(setq bc-switch-buffer-func 'phil/switch-to-buffer)

;; ----------------------------------------

(provide 'phil-breadcrumb)
