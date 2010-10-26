;; ---------------------------------------------
;; organizer, diary, planner, todo list

(add-to-list 'load-path (commonrc-dir "org-mode.git/lisp/"))

(push '("\\.org\\'" . org-mode) auto-mode-alist)

(when (require 'org-install "org-install" t)
  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
  (add-hook 'org-mode-hook 'turn-on-font-lock)  ; Org buffers only
  )

(setq org-agenda-files
      (list documents-dir (dropbox-dir "emacs")))

(setq org-export-with-archived-trees nil) ;; nil, t, headline

(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-agenda-dim-blocked-tasks t) ;; default
(setq org-goto-auto-isearch nil)
(setq org-archive-default-command 'org-archive-to-archive-sibling)

;; 1st C-a goes to beginning of line, next goes after heading and todo items
;; 1st C-e goes in front of tags, next goes to end of line
(setq org-special-ctrl-a/e (cons 'reversed t))

(setq org-special-ctrl-k t)

;; cycle globally if cursor is at beginning of buffer before headlines
(setq org-cycle-global-at-bob t)

(setq org-use-speed-commands t)

(setq org-agenda-window-setup 'other-window)
(setq org-agenda-restore-windows-after-quit t)

(add-to-list 'auto-mode-alist
             '("diary$" . diary-mode))

(setq diary-display-function 'diary-fancy-display)
(add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
(add-hook 'diary-list-entries-hook 'diary-sort-entries)

(setq org-agenda-diary-file 'diary-file)
(setq diary-file (dropbox-dir "emacs" "diary"))
(setq org-agenda-include-diary t)

(setq org-completion-use-ido t)
(setq org-completion-use-iswitchb t)
(setq org-outline-path-complete-in-steps)

(defun org-summary-todo (n-done n-not-done)
  "Switch entry to DONE when all subentries are done, to TODO otherwise."
  (let (org-log-done org-log-states)   ; turn off logging
    (org-todo (if (= n-not-done 0) "DONE" "TODO"))))

(defun find-org-files () (interactive)
  (require 'org)
  (let* ((alist (mapcar (lambda (x) (cons (file-name-nondirectory x) x)) (org-agenda-files)))
         (name (completing-read "Org file: " (mapcar 'car alist)))
         (file-name (cdr (assoc-string name alist)))
         (org-buffer (org-get-agenda-file-buffer file-name)))
    (switch-to-buffer org-buffer))
  )

(global-set-key (kbd "C-c C-'") 'find-org-files)

; "C-cl" is bound in tex/latex mode, so we use C-s S-l also
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c L") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)

;(setq org-log-done t)

(add-hook 'diary-display-hook 'fancy-diary-display)

;(defun todo-entry-timestamp ()
;  "Prepend timestamp (but no initials!) to the head of a TODO entry."
;  (let ((time-stamp-format todo-time-string-format))
;    (format "(%s) " (time-stamp-string))))

;(global-set-key (kbd "C-c t") 'todo-show)
;(global-set-key (kbd "C-c i") 'todo-insert-item)

;(global-set-key (kbd "C-c P")
;   (lambda () (interactive) (find-file "~/plans")))

;; ----------------------------------------

(provide 'phil-org)
