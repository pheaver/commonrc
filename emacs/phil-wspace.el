(require 'whitespace)

;; ------------------------------
;; stuff from ethan-wspace.el
;; ------------------------------

(defvar buffer-whitespace-was-clean nil
  "Keep track, per-buffer, whether the whitespace was clean initially.

Used by clean-whitespace-tentative and show-ws.")
(make-variable-buffer-local 'buffer-whitespace-was-clean)

(defun buffer-whitespace-clean-p ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (not (or
         ;; (re-search-forward "\t" nil t)
         (re-search-forward "[ \t]+$" nil t)))
    ))

(defun clean-whitespace-check ()
  "Sets buffer-local variable buffer-whitespace-was-clean if there's nothing weird in the whitespace.

Used as a find-file-hook. (Seems to run after font-lock-mode hooks.)"
  ; FIXME: weird buffers, like if you open a binary file?
  ; FIXME: if interactive, report current status of ws
  (interactive)
  (setq buffer-whitespace-was-clean (buffer-whitespace-clean-p)))

;; ------------------------------

;; my preferred settings.
(setq whitespace-style
  '(face trailing tabs space-before-tab::tab space-before-tab::space))

(setq whitespace-action nil)

(defun phil-clean-whitespace (&optional force-trailing)
  (interactive "P")
  (let ((whitespace-style (if (or buffer-whitespace-was-clean force-trailing)
                              whitespace-style
                            (remove 'trailing whitespace-style))))
    (whitespace-cleanup)))

(global-whitespace-mode 1)

(add-hook 'find-file-hook 'clean-whitespace-check)
(add-hook 'before-save-hook 'phil-clean-whitespace)

;; whitespace-style
;;   tailing -- always, or only if buffer was clean to start with?  different face?
;;   tabs -- always
;;   space-before-tab::tab, space-before-tab::space

;; don't use:
;;   spaces
;;   lines
;;   lines-tail
;;   newline
;;   empty
;;   indentation::tab
;;   indentation::space
;;   indentation
;;   space-after-tab::tab
;;   space-after-tab::space
;;   space-after-tab
;;   space-before-tab
;;   space-before-tab
;;   space-mark
;;   tab-mark
;;   newline-mark

(provide 'phil-wspace)
