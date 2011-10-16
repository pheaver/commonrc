;; ----------------------------------------
;; my breadcrumb settings

(load "breadcrumb-site-file" 'noerror)

(global-set-key (kbd "C-c SPC") 'bc-set)
(global-set-key (kbd "C-c p"  ) 'bc-previous)
(global-set-key (kbd "C-c n"  ) 'bc-next)
(global-set-key (kbd "C-c P"  ) 'bc-local-previous)
(global-set-key (kbd "C-c N"  ) 'bc-local-next)
(global-set-key (kbd "C-c M-l") 'bc-list)

(or (require 'phil-utils-loaddefs nil 'noerror)
    (require 'phil-utils))
(setq bc-switch-buffer-func 'phil/switch-to-buffer)

;; ----------------------------------------

(provide 'phil-breadcrumb)
