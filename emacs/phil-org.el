(require 'phil-paths)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))

;;;; locations of org files
(setq org-directory (dropbox-dir "org"))

(defun org-dir (&rest paths)
  (concat org-directory "/" (concatpaths paths)))

(setq org-agenda-files
      (list documents-dir (dropbox-dir "Documents") org-directory))
(setq org-default-notes-file (org-dir "notes.org"))

;;;; miscellaneous org settings
(setq org-completion-use-ido t)
(setq org-export-with-archived-trees nil) ;; nil, t, headline
;; (setq org-archive-default-command 'org-archive-to-archive-sibling)
(setq org-archive-default-command 'org-archive-subtree)
(setq org-archive-location "%s_archive::") ;; default
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-goto-auto-isearch nil)
(setq org-outline-path-complete-in-steps)
(setq org-read-date-prefer-future nil)

(setq org-speed-commands-user
      '(("Custom")
        ("d" . org-decrypt-entry)))

;; 1st C-a goes to beginning of line, next goes after heading and todo items
;; 1st C-e goes in front of tags, next goes to end of line
(setq org-special-ctrl-a/e (cons 'reversed t))
(setq org-special-ctrl-k t)
(setq org-use-speed-commands t)

;; cycle globally if cursor is at beginning of buffer before headlines
(setq org-cycle-global-at-bob t)

;;;; capture
(setq org-capture-templates
      '(("t" "Task" entry (file+headline "" "Inbox") "** TODO %?\n%i")
        ("n" "Note" entry (file+headline "" "Inbox") "** %?\n%i")
        ("k" "Quick Note" entry (file+headline "" "Inbox") "** %^{Description}\n%i" :immediate-finish t)
        ("l" "Link Note" entry (file+headline "" "Inbox") "** %?\n%A")
        ;; ("j" "Journal" entry (file+datetree "") "* %?\nEntered on %U\n i\n%a")
        ;; ("x" "Test" entry (file+headline "" "Inbox") "* %?\n%^C")
        ;; ("x" "test prefix")
        ;; ("xx" "test" entry (file+headline "" "Tasks" "** %?"))
        ))

;;;; encryption
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance '("crypt"))
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
(setq org-crypt-key nil)
(setq org-crypt-disable-auto-save t)

;;;; custom functions
(defun find-org-files ()
  (interactive)
  (require 'org)
  (let* ((alist (mapcar (lambda (x) (cons (file-name-nondirectory x) x)) (org-agenda-files)))
         (name (completing-read "Org file: " (mapcar 'car alist)))
         (file-name (cdr (assoc-string name alist)))
         (org-buffer (org-get-agenda-file-buffer file-name)))
    (switch-to-buffer org-buffer)))

;;;; keybindings
(global-set-key (kbd "C-c C-'") 'find-org-files)
(global-set-key "\C-cb" 'org-ido-switchb)
(define-key global-map "\C-cc" 'org-capture)
;; "C-cl" is bound in tex/latex mode, so we use C-s S-l also
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c L") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)

(provide 'phil-org)
