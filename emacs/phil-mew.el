;; ----------------------------------------
;; mew

(require 'phil-paths)

(autoload 'mew "mew" nil t)
(autoload 'mew-send "mew" nil t)
(global-set-key "\C-xm" 'mew)
(setq mew-rc-file (commonrc-dir "mewrc.el"))

;; ----------------------------------------

(provide 'phil-mew)
