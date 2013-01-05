;; ----------------------------------------

(add-to-list 'load-path "~/local/src/anything-config.git")

;; (require 'anything)
;; (require 'anything-config)

(autoload 'anything "anything" nil t)
(autoload 'anything-at-point "anything" nil t)
(autoload 'anything-other-buffer "anything" nil t)

(global-set-key (kbd "C-c C-_") 'anything-at-point)
(global-set-key (kbd "C-c C-/") 'anything-at-point)

(defun phil/anything-init ()
  (require 'anything-config)
  (setq anything-sources
        '(anything-c-source-buffers
          anything-c-source-recentf
          anything-c-source-files-in-current-dir+
          anything-c-source-imenu
          anything-c-source-occur
          anything-c-source-find-files
          anything-c-source-extended-command-history
          anything-c-source-emacs-commands
          anything-c-source-emacs-functions
          anything-c-source-emacs-functions-with-abbrevs
          anything-c-source-emacs-variables
          ;; anything-c-source-colors
          ;; anything-c-source-kill-ring
          anything-c-source-mark-ring
          anything-c-source-etags-select
          anything-c-source-ctags
          )))

;; 'anything-c-source-google-suggest

(global-set-key (kbd "C-;") 'anything-command-map)

(eval-after-load 'anything '( phil/anything-init))

;; ----------------------------------------

(provide 'phil-anything)
