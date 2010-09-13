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

(setq bc-switch-buffer-func 'my-switch-to-buffer)

;; this is defined here because it's only used here.
;; maybe i should put this in phil-buffers.el.
(defun my-switch-to-buffer (buffer)
  (let ((w (get-buffer-window buffer)))
    (if w (select-window w)
      (switch-to-buffer buffer))))

;; ----------------------------------------

(provide 'phil-breadcrumb)
