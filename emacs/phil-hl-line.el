;; ----------------------------------------

;; enable hl-line-mode for all major modes except these:
;; (defvar hl-line-disabled-mode-list '()
;;   "List of major modes in which `hl-line-mode' should be disabled.
;; In all other modes, `hl-line-mode' is enabled")

;; (setq hl-line-disabled-mode-list '(phil-term-mode magit-mode term-mode))
;; (setq hl-line-sticky-flag nil)

;; (defun maybe-hl-line-mode (&optional buffer)
;;   "Enable or disable `hl-line-mode' in BUFFER, based on the
;; whether BUFFER's major-mode is in `hl-line-disabled-mode-list'.
;; If BUFFER is nil, use current buffer"
;;   (interactive)
;;   (let ((mode (buffer-local-value 'major-mode (or buffer (current-buffer)))))
;;     (if (member mode hl-line-disabled-mode-list)
;;         (hl-line-mode 0)
;;       (hl-line-mode 1))))

;; ;; setup hl-line
;; (when (require 'hl-line)
;;   (global-hl-line-mode 1)
;;   (set-face-background 'hl-line "khaki1")
;;   (add-hook 'after-change-major-mode-hook 'maybe-hl-line-mode))

;; ----------------------------------------

(provide 'phil-hl-line)
