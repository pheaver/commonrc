;; hippie-expand
(when (fboundp 'hippie-expand)
  (eval-after-load "dabbrev" '(defalias 'dabbrev-expand 'hippie-expand))
  (add-to-list 'hippie-expand-try-functions-list 'try-expand-tag 'append)
  )

(defun he-tag-beg ()
  (let ((p
         (save-excursion
           (backward-word 1)
           (point))))
    p))

(defun try-expand-tag (old)
  (unless (or old (null tags-table-list))
    (he-init-string (he-tag-beg) (point))
    (setq he-expand-list (sort
                          (all-completions he-search-string 'tags-complete-tag) 'string-lessp)))
  (while (and he-expand-list
              (he-string-member (car he-expand-list) he-tried-table))
              (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn
        (when old (he-reset-string))
        ())
    (he-substitute-string (car he-expand-list))
    (setq he-expand-list (cdr he-expand-list))
    t))

(defun tags-complete-tag (string predicate what)
  (require 'etags)
  (save-excursion
    ;; If we need to ask for the tag table, allow that.
    (if what
        (all-completions string (tags-completion-table) predicate)
      (try-completion string (tags-completion-table) predicate))))

;; company-mode
(with-eval-after-load 'company
  (global-company-mode t)

  (setq company-minimum-prefix-length 2)
  (global-set-key (kbd "<M-tab>") 'company-complete)
  (global-set-key (kbd "C-M-i") 'company-complete)

  ;; (setq company-auto-complete t)

  ;; make TAB complete, then cycle
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "C-e") #'company-other-backend)
  (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)

  (setq company-backends
        '(
          company-files
          company-capf
          (company-dabbrev-code company-gtags company-etags company-keywords)
          company-dabbrev
          ))

  (setq completion-styles '(substring partial-completion emacs22))
  (setq company-dabbrev-downcase nil)
  (setq company-show-numbers nil)
  )

(require 'company nil 'noerror)

(provide 'phil-completion)
