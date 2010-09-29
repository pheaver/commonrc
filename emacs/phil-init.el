;; ----------------------------------------

;; 0 -> bare minimum
;; 1 -> somewhere between 0 and 3
;; 2 -> somewhere between 0 and 3
;; 3 -> everything (default when daemon)
;; currently 0 through 2 are all the same, and 3 is the only different one

(defvar init-level
  (if (and (fboundp 'daemonp) (daemonp)) 3 2))
(defvar init-queue nil)

(defun phil/eval-at-init-level (level body)
  "Execute FORM when `init-level' becomes at least N, either
right now or when it is changed later by calling
`phil/switch-init-level'."
  (if (>= init-level level)
      (eval body)
    (phil/add-to-init-level level body)))

(defun phil/add-to-init-level (n form)
  (let ((entry (cdr (assq n init-queue))))
    (add-to-list 'entry form 'append)
    (setq init-queue (cons (cons n entry) (assq-delete-all n init-queue)))))

(defun phil/switch-init-level (new-level)
  (setq init-level new-level)
  (dolist (n (number-sequence 0 new-level))
    (dolist (x (assq n init-queue))
      (eval x))
    (setq init-queue (assq-delete-all n init-queue))))

;; ----------------------------------------

(provide 'phil-init)
