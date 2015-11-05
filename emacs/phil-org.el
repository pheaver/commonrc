;; ---------------------------------------------
;; organizer, diary, planner, todo list

(autoload 'phil/import-calendars "phil-calendars")

(add-to-list 'load-path (commonrc-dir "org-mode.git/lisp/"))

(push '("\\.org\\'" . org-mode) auto-mode-alist)

(when (require 'org-install "org-install" t)
  (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
  (add-hook 'org-mode-hook 'turn-on-font-lock)  ; Org buffers only
  )

(eval-after-load 'org
  '(progn (add-to-list 'org-modules 'org-habit)
          (add-to-list 'org-modules 'org-mobile)))

;;;; locations of org files
(setq org-directory (dropbox-dir "org"))

(defun org-dir (&rest paths)
  (concat org-directory "/" (concatpaths paths)))

(setq org-agenda-files
      (list documents-dir (dropbox-dir "Documents") org-directory))
(setq org-default-notes-file (org-dir "notes.org"))

;;;; miscellaneous org settings
(setq org-completion-use-ido t)
(setq org-completion-use-iswitchb t)
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

;;;; agenda
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-use-time-grid nil)
(setq org-agenda-dim-blocked-tasks t) ;; default
(setq org-agenda-window-setup 'other-window)
(setq org-agenda-restore-windows-after-quit t)

;; agenda format
(setq org-agenda-prefix-format
      '((agenda . " %i %?-12t% s")
        ;; (agenda . " %i %-12:c%?-12t% s")
        (timeline . "  % s")
        (todo . " %i %-12:c")
        (tags . " %i %-12:c")
        (search . " %i %-12:c")))

;; google-weather in agenda
(setq org-google-weather-icon-directory (dropbox-dir "icons/"))
(setq org-google-weather-location "97217")
(eval-after-load "org"
  '(progn
     (require 'google-weather "google-weather" 'noerror)
     (require 'org-google-weather "org-google-weather" 'noerror)))

;;;; agenda and diary
(add-to-list 'auto-mode-alist
             '("diary$" . diary-mode))

(setq diary-display-function 'diary-fancy-display)
(add-hook 'diary-display-hook 'fancy-diary-display)
(add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
(add-hook 'diary-list-entries-hook 'diary-sort-entries)

(setq org-agenda-diary-file 'diary-file)
(setq diary-file (dropbox-dir "org" "diary"))
(setq org-agenda-include-diary t)

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

;;;; mobile
(setq org-mobile-directory (dropbox-dir "MobileOrg"))
(setq org-mobile-inbox-for-pull  (dropbox-dir "org/inbox.org"))

;;;; habits
; global STYLE property values for completion
(setq org-global-properties '(("STYLE_ALL" . "habit")))
; position the habit graph on the agenda to the right of the default
(setq org-habit-graph-column 55)
(setq org-habit-preceding-days 14) ; default
(setq org-habit-following-days 7)  ; default
(setq org-habit-show-habits-only-for-today t) ; default t
; make sure habits are turned on in the agenda each day
(run-at-time "06:00" 86400 '(lambda () (setq org-habit-show-habits t)))

;;;; encryption
(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance '("crypt"))
;; GPG key to use for encryption
;; Either the Key ID or set to nil to use symmetric encryption.
(setq org-crypt-key nil)
(setq org-crypt-disable-auto-save t)

;;;; custom functions
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

;;;; keybindings
(global-set-key (kbd "C-c C-'") 'find-org-files)
(global-set-key "\C-cb" 'org-ido-switchb)
(define-key global-map "\C-cc" 'org-capture)
; "C-cl" is bound in tex/latex mode, so we use C-s S-l also
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c L") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)

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
