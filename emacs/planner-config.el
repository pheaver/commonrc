;;; Sacha's configuration for planner.el
;; Sacha Chua <sacha@free.net.ph>

;;;_+ Loading

;; This directory contains all the latest emacs-wiki and planner files.
(add-to-list 'load-path "~/notebook/emacs/dev/planner")
(add-to-list 'load-path "~/notebook/emacs/dev/emacs-wiki")
(add-to-list 'load-path "~/notebook/emacs/dev/remember")

;; Separate file to make this config less scary
(require 'emacs-wiki-config)
(setq planner-backend 'emacs-wiki)
(require 'planner)

(planner-emacs-wiki-install)
(require 'remember-config)
;; Stock files

(load-library "planner")
(load-library "planner-accomplishments")
;;(require 'planner-appt)
;;(require 'planner-authz)
(load-library "planner-bbdb")
;;(require 'planner-bibtex)
(load-library "planner-bookmark")
;;(require 'planner-calendar)
(load-library "planner-cyclic")
(load-library "planner-deadline")
;(require 'planner-diary)
(load-library "planner-erc")
(load-library "planner-experimental")
;;(require 'planner-export-diary)
(load-library "planner-gnus")
;;(require 'planner-id)
;;(require 'planner-ledger)
(load-library "planner-lisp")
;;(require 'planner-log-edit)
;;(require 'planner-mhe)
(load-library "planner-multi")
(load-library "planner-notes-index")
(load-library "planner-report")
;;(require 'planner-rmail)
(load-library "planner-rss")
(load-library "planner-schedule")
;;(require 'planner-tasks-overview)
(load-library "planner-timeclock")
(load-library "planner-timeclock-summary")
;;(require 'planner-timeclock-summary-proj)
(load-library "planner-trunk")
;;(require 'planner-unix-mail)
;;(require 'planner-vm)
(load-library "planner-w3m")
;;(require 'planner-wl)
;;(require 'planner-xtla)

;;_+ Keybindings

;; This reminds me what I'm working on. C-u F9 F9 jumps to the task, too.
(global-set-key (kbd "<f9> p SPC") 'planner-goto-today)
(global-set-key (kbd "<f9> P SPC") 'planner-goto)
(global-set-key (kbd "<f9> r SPC") 'remember)
(global-set-key (kbd "<f9> R SPC") 'remember-region)
(global-set-key (kbd "<f9> t SPC") 'planner-create-task-from-buffer)
(global-set-key (kbd "<f9> T SPC") 'planner-create-task)
;; I use F9 p to go to today's page, anyway.
(define-key planner-mode-map (kbd "C-c C-n") 'planner-create-note-from-task)
(define-key planner-mode-map (kbd "C-c C-e") 'planner-edit-task-description)
;; I use an after-save-hook to publish, so I can remap C-c C-p
(define-key planner-mode-map (kbd "C-c C-p") 'planner-task-pending)

;;;_+ Basic setup

(setq planner-directory "/home/sacha/notebook/plans")
(setq planner-publishing-directory "/home/sacha/public_html/notebook/wiki")
(setq planner-carry-tasks-forward t)
(setq planner-expand-name-favor-future-p nil)
(setq planner-task-dates-favor-future-p t)
(setq planner-default-task-priority "B")
(setq planner-expand-name-default ".")
(setq emacs-wiki-recurse-directories t)
(setq planner-task-format "#%s%s %s %s%s")
;; I don't need my tasks renumbered.
(setq planner-renumber-tasks-automatically nil)
(setq planner-align-tasks-automatically nil)
(setq planner-renumber-notes-automatically nil)

;; Do not automatically add task IDs. I used to set this to non-nil,
;; but realized that I didn't edit my task descriptions that often. If
;; I want to edit a task, I can just add the task ID _before_ editing.
(setq planner-id-add-task-id-flag nil)

(add-to-list 'planner-markup-tags '("notes" nil nil nil planner-notes-tag))
(add-to-list 'planner-markup-tags '("example" t nil t emacs-wiki-example-tag))
(setq planner-custom-variables
      '((emacs-wiki-publishing-header . "<lisp>(my-publishing-header)</lisp>")
        (emacs-wiki-publishing-footer . "<lisp>(my-publishing-footer)</lisp>")
        (emacs-wiki-publishing-file-suffix . ".php")))

;; I don't mind having lots of open planner files
(setq planner-tasks-file-behavior nil)

;;;_+ planner-rss configuration

(setq planner-rss-base-url "http://sacha.free.net.ph/notebook/wiki/")
(setq planner-rss-category-feeds
      '(("ShortStories\\|flash" "/home/sacha/notebook/wiki/flash.rdf" "")
        ("planner" "/home/sacha/notebook/wiki/planner.rdf" "")
        ("education\\|teaching\\|students\\|graduate" "/home/sacha/notebook/wiki/education.rdf" "")
        ("cook" "/home/sacha/notebook/wiki/cook.rdf" "")
        ("emacs\\|planner" "/home/sacha/notebook/wiki/emacs.rdf" "")
        ("." "/home/sacha/public_html/notebook/wiki/blog-burn.rdf"
         "<?xml version=\"1.0\"?><rss version=\"2.0\"><channel>
<title>sachachua's blog</title>
<link>http://sacha.free.net.ph/notebook/wiki/today.php</link>
<description>Random notes</description>
</channel></rss>
")))
(setq planner-rss-feed-limits '(("." 20000 nil)))

;;;_+ Chronological notes on day pages and reverse-chronological on plan pages

(defun sacha/planner-twiddle-chronological-notes ()
  "Use chronological notes on day pages and reverse-chronological notes on plan pages.
People visit my site once a day, so chronologically-ordered notes
are easier for them to understand. People visit plan pages less
often, so new things should be closer to the top."
  (set (make-variable-buffer-local 'planner-reverse-chronological-notes)
       (not (string-match planner-date-regexp
                          (or (planner-page-name) "")))))
(add-hook 'planner-mode-hook 'sacha/planner-twiddle-chronological-notes)

;;;_+ Allowing =) smileys

;; Remove the =...= verbatim highlighter
(emacs-wiki-configure-highlighting 'emacs-wiki-highlight-markup
                                   (delete '("=[^[:blank:]=]" 61 emacs-wiki-highlight-verbatim)
					   emacs-wiki-highlight-markup))
(defadvice emacs-wiki-highlight-verbatim-tag (around sacha activate)
  "Do not do verbatim at all.")

;;;_+ Don't automatically highlight words

;; I like being able to use * and _ without worring about how they're
;; marked up in the published page
(setq emacs-wiki-publishing-markup
      (delq    ;; emphasized or literal text
       '["\\(^\\|[-[[:space:]<('`\"]\\)\\(=[^=[:space:]]\\|_[^_[:space:]]\\|\\*+[^*[:space:]]\\)"
         2 emacs-wiki-markup-word]
       emacs-wiki-publishing-markup))
(defadvice emacs-wiki-markup-word (around sacha activate)
  ;; Do nothing
  )

;;;_+ Compatibility for old pages or old code

;;; Compatibility, purely for old pages I'm too lazy to change.
;;; planner-diary is so much cooler.
(defun sacha/planner-get-diary-entries (date)
  "For DATE (yyyy.mm.dd), return a list of diary entries as a string."
  (require 'diary-lib)
  (when (string-match planner-date-regexp date)
    (let* ((diary-display-hook 'ignore)
           (entries (list-diary-entries
                     (list (string-to-number (match-string 2 date)) ; month
                           (string-to-number (match-string 3 date)) ; day
                           (string-to-number (match-string 1 date))) ; year
                     1))) ; Get entries for one day
      (if entries
          (mapconcat (lambda (item) (nth 1 item)) entries "\n")
        nil))))

(fset 'planner-get-diary-entries 'sacha/planner-get-diary-entries)

;;;_+ planner-diary

;;; Here we use planner-diary.
;(setq planner-diary-string "* ~/.diary schedule")
;(setq planner-diary-use-diary t)
;(planner-diary-insinuate)

;; Just in case?
;;(defadvice plan (after sacha)
;;  "Call `planner-diary-insert-diary'."
;;  (planner-diary-insert-diary))

(defun sacha/planner-diary-schedule-task (start end)
  "Add a diary entry for the current task from START to END."
  (interactive "MStart: \nMEnd: ")
  (save-window-excursion
    (save-excursion
      (save-restriction
        (let* ((info (planner-current-task-info))
               (original (planner-task-description info))
               main
               description)
          ;; TODO: Mark the task as scheduled for a particular time
          (setq description
                (cond
                 ((string-match "^\\(.+\\)\\s-+{{Schedule:\\([^-]+\\)-\\([^}]+\\)}}\\(.*\\)" original)
                  (setq main (match-string 1 original))
                  (save-excursion
                    (save-match-data
                      (goto-char (point-min))
                      (when (re-search-forward
                             (concat (match-string 2 original)
                                     " | "
                                     (match-string 3 original)
                                     " | "
                                     (match-string 1 original))
                             nil t)
                        (sacha/planner-diary-unschedule-entry))))
                  (concat (match-string 1 original)
                          " {{Schedule:"
                          start
                          "-"
                          end
                          "}}"
                          (match-string 4 original)))
                 ((string-match "\\(.*\\)\\(\\s-*\\)$" original)
                  (setq main (match-string 1 original))
                  (replace-match (concat " {{Schedule:" start "-" end "}}")
                                 t t original 2))))
          (planner-edit-task-description description)
          ;; Add the diary entry
          (sacha/planner-diary-add-entry
           (planner-task-date info)
           (concat start " | " end " | " main)))))))

(defun sacha/planner-diary-add-entry (date text &optional annotation)
  "Prompt for a diary entry to add to `diary-file'."
  (interactive
   (list
    (if (or current-prefix-arg
            (not (string-match planner-date-regexp (planner-page-name))))
        (planner-read-date)
      (planner-page-name))
    (read-string
     "Diary entry: ")))
  (save-excursion
    (save-window-excursion
      (let ((inhibit-read-only t))
        (make-diary-entry
         (concat
          (let ((cal-date (planner-filename-to-calendar-date date)))
            (calendar-date-string cal-date t t))
          " " text
          (or annotation
              (let ((annotation (run-hook-with-args-until-success
                                 'planner-annotation-functions)))
                (if annotation
                    (concat " " annotation)
                  ""))))))
      (planner-goto date)
      (planner-diary-insert-diary-maybe))))

(defun sacha/planner-diary-unschedule-entry ()
  "Unschedule the current entry."
  (interactive)
  (goto-char (line-beginning-position))
  (let ((id
         (if (re-search-forward "{{Tasks:\\([^}]+\\)}}" (line-end-position) t)
             (match-string 0)
           nil)))
    (sacha/planner-diary-delete-entry)
    (when id
      (planner-seek-to-first "Tasks")
      (re-search-forward id nil t))))

(defun sacha/planner-diary-delete-entry ()
  "Delete the current entry from `diary-file'."
  (interactive)
  (let ((cal-date (planner-filename-to-calendar-date (planner-page-name)))
        (text (buffer-substring (line-beginning-position)
                                (line-end-position)))
        (case-fold-search nil))
    (save-excursion
      (save-window-excursion
        (let ((inhibit-read-only t))
          (find-file diary-file)
          (save-excursion
            (save-restriction
              (widen)
              (goto-char (point-max))
              (when (re-search-backward
                     (concat "^"
                             (regexp-quote
                              (concat (calendar-date-string cal-date t t)
                                      " " text))))
                (delete-region (line-beginning-position)
                               (min (1+ (line-end-position)) (point-max))))
              (save-buffer))))
        (planner-diary-insert-diary-maybe t)))))

(define-key planner-mode-map (kbd "C-c C-s") 'sacha/planner-diary-schedule-task)
(define-key planner-mode-map (sacha/gnu-vs-x (kbd "C-c C-S-s")
                                             (kbd "C-c C-S"))
                                  'sacha/planner-diary-unschedule-entry)

;;;_+ Header and footer

;;(defvar planner-day-header-file "/home/sacha/notebook/wiki/.day.header"
;;  "The header file to include for planner day pages (ex: 2003.03.17)")
;;(defvar planner-day-footer-file "/home/sacha/notebook/wiki/.day.footer"
;;  "The footer file to include for planner day pages (ex: 2003.03.17)")
(defvar planner-header-file "/home/sacha/notebook/wiki/.header"
  "The header file to include for normal planner pages (ex: WelcomePage)")
(defvar planner-footer-file "/home/sacha/notebook/wiki/.footer"
  "The footer file to include for normal planner pages (ex: WelcomePage)")

(defvar sacha/planner-no-header-or-footer '("SideBar")
  "Pages that do not have header or footer.")

(defun my-publishing-header ()
  "Insert the header only if this file should have it."
  (cond
   ((member (emacs-wiki-page-name) sacha/planner-no-header-or-footer) "")
   ((file-readable-p planner-header-file) (ignore (insert-file-contents planner-header-file)))
   (t "this is the default header text, if the file can't be found\n")))
  
(defun my-publishing-footer ()
  "Insert the footer only if this file should have it."
  (cond
   ((member (emacs-wiki-page-name) sacha/planner-no-header-or-footer) "")
   ((file-readable-p planner-footer-file) 
    (ignore (insert-file-contents planner-footer-file)))
   (t "this is the default footer text, if the file can't be found\n")))

;;;_+ Emacspeak

(defadvice emacs-wiki-next-reference (after emacspeak pre act comp)
  "Provide additional feedback"
  (message "%s" (match-string 0)))
(defadvice emacs-wiki-previous-reference (after emacspeak pre act comp)
  "Provide additional feedback"
  (message "%s" (match-string 0)))

;;;_+ RSS blogging
(add-to-list 'remember-planner-append-hook 'planner-rss-add-note t)

(defadvice planner-rss-add-note (around sacha/absolute-urls activate)
  "Publish absolute URLs."
  (let ((sacha/emacs-wiki-use-absolute-url-flag t))
    (setq ad-return-value ad-do-it)))

(defadvice planner-rss-add-note (around sacha/norss activate)
  "Do not publish if note includes \"norss\""
  (save-restriction
    (when (planner-narrow-to-note)
      (goto-char (point-min))
      (unless (search-forward "norss" nil t)
        ad-do-it))))

(defun sacha/rss-delete-item ()
  (interactive)
  (delete-region
   (if (looking-at "<item>")
       (point)
     (when (re-search-backward "<item>" nil t)
       (match-beginning 0)))
   (when (re-search-forward "</item>" nil t)
     (match-end 0))))

(defun sacha/planner-update-note ()
  "Update this note in RSS and Planner."
  (interactive)
  (sacha/planner-rss-undo-this-note)
  (planner-update-note)
  (planner-rss-add-note))

(defun sacha/planner-rss-undo-this-note ()
  "Delete the current entry from the RDFs it matched."
  (interactive)
  (save-excursion
    (save-restriction
      (planner-narrow-to-note)
      (let* ((feeds planner-rss-category-feeds)
             (info (planner-current-note-info))
             (page
              (concat "<link>"
                      planner-rss-base-url
                      (emacs-wiki-published-name (planner-note-page info))
                      "#anchor-"
                      (planner-note-anchor info)
                      "</link>"))
             files)
        (while feeds
          (goto-char (point-min))
          (let ((criterion (car (car feeds)))
                (file (car (cdr (car feeds)))))
            (if (if (functionp criterion)
                    (funcall criterion)
                  (re-search-forward criterion nil t))
                (add-to-list 'files file))
            (setq feeds (cdr feeds))))
        (while files
          (with-current-buffer (find-file-noselect (car files))
            (goto-char (point-min))
            (when (re-search-forward page nil t)
              (sacha/rss-delete-item)
              (save-buffer)))
          (setq files (cdr files)))))))
        

;;;_+ Misc

(defun sacha/planner-replan-region (beg end &optional page)
  "Replan all tasks from BEG to END to PAGE."
  (interactive (list (point) (mark)
                     (planner-read-name (planner-file-alist) "Replan to: ")))
  (let ((start (if (< beg end) beg end))
        (finish (if (< beg end) end beg)))
    ;; Invoke planner-copy-or-move-task on each line in reverse
    (save-excursion
      (save-restriction
        (narrow-to-region
         (and (goto-char start) (line-beginning-position))
         (and (goto-char (1- finish)) (min (point-max)
                                      (1+ (line-end-position)))))
        (goto-char (point-min))
        (while (not (eobp))
	  (save-excursion (save-restriction (planner-replan-task page)))
	  (forward-line 1))))))

;;;_+ 20040504: Relative annotations
(setq planner-annotation-use-relative-file
      (lambda (filename)
        "Use relative filename if FILENAME is under my home directory."
        (save-match-data
          (or (string-match "^/home/sacha" filename)
              (string-match "^/mnt/data/home/sacha" filename)
              (string-match "^/mnt/media/home/sacha" filename)))))

;;;_+ Permalinks and comments

(defun sacha/planner-note-id (info)
  "Return the note identifier for commenting systems.
Prefers date pages."
  (planner-replace-regexp-in-string
   "[#\\.]" "-"
   (if (string-match planner-date-regexp (planner-note-page info))
       (concat (planner-note-page info) "#" (planner-note-anchor info))
     (let (found
           (pages (planner-multi-split (planner-note-link-text info))))
       (while pages
         (when (string-match planner-date-regexp (planner-link-base (car pages)))
          (setq found (planner-link-target (car pages))
                pages nil))
         (setq pages (cdr pages)))
       (or found
           (concat (planner-note-page info) "#" (planner-note-anchor info)))))))
  
;; I want notes preceded by a number so I know how to link to them.
(defun sacha/planner-markup-notes ()
  "Mark up notes neatly."
  (while (re-search-forward "^\\.#[0-9]" nil t)
    (save-restriction
      (planner-narrow-to-note)
      (let* ((info (planner-current-note-info t))
             (id (sacha/planner-note-id info)))
        (delete-region (point-min) (point-max))
        ;; Bound the entire thing in a div
        (insert "<div class=\"note\" id=\"anchor-" (planner-note-anchor info) "\">\n"
                "<h3>" (planner-note-anchor info) ". " (planner-note-title info) "</h3>\n"
                "<div class=\"note_info\">Categories: " (or (planner-note-link-text info) "None") " -- "
                (planner-make-link (concat (planner-page-name) "#" (planner-note-anchor info)) "Permalink")
                "</div><div class=\"note_body\">\n"
                (planner-note-body info)
                "\n</div><div class=\"note_info\">"
                (planner-make-link (concat (planner-page-name) "#" (planner-note-anchor info)) "Permalink")
                ", "
                (format "<span class=\"commentlink\"><?php $blog_id='%s';include (BK_PATH.\"/module/blogkomm_show_link.php\"); ?></span>" id)
                ", <a href=\"#feedback\">Mail me</a> -- <a href=\"#top\">Back to top</a></div></div>\n")))))

;;_+ Schedule next undated task from same project
;; For Jody Klymak
(defun sacha/planner-seek-next-unfinished-and-undated-task ()
  "Move point to the next unfinished task on this page.
Return nil if not found, the task if otherwise."
  (interactive)
  (let (task-info)
    (while (and (not task-info)
                (re-search-forward "^#[A-C][0-9]*\\s-+[^CX]\\s-+" nil t))
      (setq task-info (planner-current-task-info))
      (when (planner-task-date task-info) (setq task-info nil)))
    task-info))

(defun sacha/planner-queue-next-task (&optional task-info)
  "Schedule the next task for TASK-INFO or the current task for today."
  (interactive)
  (save-window-excursion
    (save-excursion
      (setq task-info (or task-info (planner-current-task-info)))
      (when (and task-info (planner-task-plan task-info))
        (planner-find-file (planner-task-plan task-info))
        (goto-char (point-min))
        (if (sacha/planner-seek-next-unfinished-and-undated-task)
            (planner-copy-or-move-task (planner-today))
          (message "No more unschedulefd tasks for %s."
                   (planner-task-plan task-info)))))))

(defun sacha/planner-schedule-next-task (old-status new-status)
  "Schedule next task if there are no other unfinished tasks for this project."
  (when (and (string= new-status "X")
             (not (string= old-status "X")))
    (let ((task-info (planner-current-task-info))
          (not-seen t))
      (when (and task-info
                 (planner-task-plan task-info)
                 (planner-task-date task-info))
        (save-window-excursion
          (save-excursion
            (when (string= (planner-task-plan task-info)
                           (planner-task-page task-info))
              (planner-jump-to-linked-task))
            (goto-char (point-min))
            (while (and not-seen
                        (re-search-forward "^#[A-C][0-9]*\\s-+[^CX]\\s-+" nil t))
              (let ((current (planner-current-task-info)))
                (when (string= (planner-task-plan task-info)
                               (planner-task-plan current))
                  (setq not-seen nil))))))
        (when not-seen
          (sacha/planner-queue-next-task task-info)))))
  t)
;(add-hook 'planner-mark-task-hook 'sacha/planner-schedule-next-task)

;;;_+ Keep track of what I'm supposed to be doing

;; I've bound sacha/planner-what-am-i-supposed-to-be-doing to F9 F9. I
;; start out by clocking into the task (use planner-timeclock.el and
;; C-c TAB to mark a task as in progress). Then, when I find myself
;; getting distracted, I hit F9 F9 to see my current task in the
;; minibuffer. C-u F9 F9 jumps back to the task so that I can either
;; mark it as postponed. M-x planner-task-pending (bound to C-c C-p in
;; my local config) and M-x planner-task-done (C-c C-x) both clock out
;; of the task. If I want to jump back to the previous window
;; configuration from that planner page, I can just hit F9 F9 again.

(defvar sacha/window-register "w"
  "Register for jumping back and forth between planner and wherever I am.")
(defvar sacha/planner-current-task nil
  "Current task info.")
(defadvice planner-task-in-progress (after sacha activate)
  "Keep track of the task info."
  (setq sacha/planner-current-task (planner-current-task-info)))

(defun sacha/planner-what-am-i-supposed-to-be-doing (&optional prefix)
  "Make it easy to keep track of what I'm supposed to be working on.
If PREFIX is non-nil, jump to the current task, else display it
in a message. If called from the plan page, jump back to whatever
I was looking at."
  (interactive "P")
  (if planner-timeclock-current-task
      (if (string= (planner-task-page sacha/planner-current-task)
                   (planner-page-name))
          (jump-to-register sacha/window-register)
        (if (null prefix)
            (message "%s" planner-timeclock-current-task)
          (frame-configuration-to-register sacha/window-register)
          (planner-find-file (planner-task-page sacha/planner-current-task))
          (planner-find-task sacha/planner-current-task)))
    (if prefix
        (planner-goto-today)
      (message "No current task. HEY!"))))

(global-set-key (kbd "<f9> <f9>") 'sacha/planner-what-am-i-supposed-to-be-doing)

;;;_+ Removing task numbers

(defun sacha/planner-strip-task-numbers ()
  (interactive)
  (while (re-search-forward "^#.\\([0-9]+\\)\\s-+.\\s-+" nil t)
    (replace-match "" t t nil 1))
  (planner-align-tasks))

;;;_+ Marking up IDs as images

(defun planner-id-image (id)
  "Return the image to mark up ID as, or nil if none."
  (save-match-data (when (string-match "Tasks" id) "~/notebook/pics/screen/id-small.png")))

(defun planner-id-highlight-images (beg end &optional verbose)
  "Highlight IDs as pictures from BEG to END.
VERBOSE is ignored."
  (goto-char beg)
  (while (re-search-forward "{{[^}]+}}" end t)
    (let ((image (planner-id-image (match-string 0))))
      (when image
        (emacs-wiki-inline-image (match-beginning 0)
                                 (match-end 0)
                                 image
                                 (match-string 0))))))

(add-hook 'planner-mode-hook
          (lambda () (add-hook 'emacs-wiki-highlight-buffer-hook
                               'planner-id-highlight-images)))

;;;_+ Fancy task sorting: idea and base code from johnsu01 on 2005.02.18.

;; This code allows you to sort your tasks based on regular expressions.
;; Try it out with
;;
;;    C-u M-x sacha/planner-score-sort-tasks RET some-regexp-matching-tasks-to-be-raised RET
;;
;; If you like the effects and want to keep a whole bunch of sorting
;; rules so that you can call M-x sacha/planner-score-sort-tasks
;; without any arguments, modify the sacha/planner-score-rules
;; variable.
;;
;; If you want this to become your default sorting algorithm,
;; (setq planner-sort-tasks-key-function 'sacha/planner-score-tasks-key)
;;
;; If you want it to trigger only on some pages but not on others, see
;; the `planner-sort-tasks-basic' function for inspiration.
;;
;; I hope this code shows how easy it is to tweak task sorting. =)
;; It's also handy for quickly pulling up certain tasks, as the regular
;; M-x planner-sort-tasks will leave some semblance of the old order in.

(defvar sacha/planner-score-rules '(("patch" . 100)
                                    ("bug" . 100))
  "*Alist of planner scoring rules of the form (regexp . score-value).
Tasks with higher scores are listed first.")

(defun sacha/planner-score-tasks-key ()
  "Sort tasks by the rules in `sacha/planner-score-rules'."
  (let ((score 0)
        (case-fold-search t)
        (line (buffer-substring-no-properties (line-beginning-position)
                                              (line-end-position))))
    (mapc
     (lambda (item)
       (when (string-match (car item) line)
         (setq score (- score (cdr item)))))
     sacha/planner-score-rules)
    score))

(defun sacha/planner-score-sort-tasks (&optional new-rule)
  "Sort tasks by `sacha/planner-score-rules' or NEW-RULE.
If called interactively, prompt for NEW-RULE. If NEW-RULE is
non-nil, tasks matching that regexp are raised. If not, tasks are
sorted according to `sacha/planner-score-rules'."
  (interactive (list (read-string "Task regexp: ")))
  (let ((planner-sort-tasks-key-function 'sacha/planner-score-tasks-key)
        (sacha/planner-score-rules
         (if new-rule
             (list (cons new-rule 1))
           sacha/planner-score-rules)))
    (planner-sort-tasks)))

;;;_+ 2005.03.14 Don't resolve e-mail addresses

(defun sacha/planner-bbdb-resolve-url (id)
  "Replace ID with the blog, web or e-mail address of the BBDB record."
  (save-match-data
    (when (string-match "^bbdb:/+" id)
      (setq id (replace-match "" t t id)))
    (let ((record (car (bbdb-search (bbdb-records) id id id))))
      (and record
           (or (bbdb-record-getprop record 'blog)
               (bbdb-record-getprop record 'web))))))

(defalias 'planner-bbdb-resolve-url 'sacha/planner-bbdb-resolve-url)

;;;_+ 2005.03.24 Random fortunes

(defvar sacha/fortune-file "/usr/share/games/fortunes/linuxcookie" "*Base file for fortune.")
(defvar sacha/fortune-command "/usr/games/fortune" "The fortune executable.")
  
(defun sacha/fortune (&optional file)
  "Return a fortune as a string."
  (interactive)
  (let ((line
         (shell-command-to-string
          (concat sacha/fortune-command " " (or file sacha/fortune-file)))))
    (kill-new line)
    (message line)
    line))

(defun sacha/planner-day-page-template ()
  "Day page template for Sacha."
  (let ((date (planner-filename-to-calendar-date (planner-page-name))))
    (insert "Headlines for " (calendar-day-name date) ":

<notes>

* Tasks


* Notes

* E-mail sent

")))

(setq planner-day-page-template 'sacha/planner-day-page-template)
;;;_+ No more line-breaking for tasks. Thanks to Keith Amidon

(add-hook 'planner-mode-hook 
           (lambda () 
             (setq auto-fill-inhibit-regexp "^#[ABC] +[_oX].*")
             (setq truncate-lines t)))

;;;_+ 2005.04.07 Livejournal

(defun sacha/planner-lj-browse (url)
  "Browse the LiveJournal user named by URL (lj:sachachua, for example)."
  (when (string-match "lj:\\(.+\\)" url)
    (browse-url (concat "http://www.livejournal.com/users/" (match-string 1 url)))))

(defun sacha/planner-lj-resolve (url)
"Browse the LiveJournal user named by URL (lj:sachachua, for example)."
  (when (string-match "lj:\\(.+\\)" url)
    (concat "http://www.livejournal.com/users/" (match-string 1 url))))

(planner-add-protocol "lj" 'sacha/planner-lj-browse 'sacha/planner-lj-resolve)

;;;_+ 2005.04.08 w3m mirror

(defun sacha/planner-w3m-annotation-from-mirror ()
  "Return an annotation to a mirror, if it exists."
  (when (and sacha/w3m-mirror-directory
             (eq major-mode 'w3m-mode))
    (let ((url w3m-current-url)
          file
          escaped)
      (when (string-match "^\\([^:]+\\):[^/]*//" url)
        (setq file (substring url (match-end 0)))
        (setq url (replace-match "mirror://" nil t url)))
      (if (file-exists-p
           (expand-file-name file sacha/w3m-mirror-directory))
          (concat (planner-make-link w3m-current-url w3m-current-title t)
                  " "
                  (planner-make-link url "mirror" t))
        (planner-make-link w3m-current-url w3m-current-title t)))))

(defun sacha/planner-w3m-mirror-browse-url (url)
  "Browse to the mirrored URL."
  (when (string-match "^mirror://" url)
    (setq url (replace-match
               (concat "file://"
                       (file-name-as-directory
                        (expand-file-name sacha/w3m-mirror-directory)))
               nil t url))
    (setq url (emacs-wiki-replace-regexp-in-string "\\?" "%3F" url))
    (let ((w3m-local-find-file-function nil))
      (browse-url url))))

(add-to-list 'planner-annotation-functions 'sacha/planner-w3m-annotation-from-mirror)
(planner-add-protocol "mirror" 'sacha/planner-w3m-mirror-browse-url nil)

;;;_+ Special markup for tasks

(defun sacha/planner-markup-tasks ()
  "Mark up tasks as a table."
  (while (re-search-forward planner-task-regexp nil t)
    (goto-char (line-beginning-position))
    (insert "<div class=\"tasks_explanation\">Priorities - A: high, B: medium, C: low; Status - _: unfinished, X: finished, C: cancelled, P: pending, o: in progress, &gt;: delegated. Covey quadrants - Q1 &amp; Q3: urgent, Q1 &amp; Q2: important</div>\n")
    (insert "<table class=\"tasks\">\n")
    (while (looking-at "^#\\([A-C]\\)\\([0-9]*\\)\\s-+\\(.\\)\\s-+\\(.+\\)")
      (let* ((info (planner-current-task-info))
             (priority (planner-task-priority info))
             (number (planner-task-number info))
             (status (planner-task-status info))
             (text (planner-task-description info))
             (link (planner-task-link-text info))
             (status-style (cond
                            ((string= status "_") "task_")
                            ((string= status "o") "tasko")
                            ((string= status ">") "taskd")
                            ((string= status "P") "taskp")
                            ((string= status "X") "taskX")
                            ((string= status "C") "task_cancelled")
                            (t "task"))))
        (delete-region (line-beginning-position)
                       (min (point-max) (1+ (line-end-position))))
        (insert (format ;"<li class=\"%s\"><span class=\"%s\">%s</span> %s</li>\n"
                        "<tr><td class=\"%s\">%s</td><td>%s</td><td class=\"%s\">%s %s</td></tr>\n"
                        (cond
                         ((string= priority "A") "taskA")
                         ((string= priority "B") "taskB")
                         ((string= priority "C") "taskC")
                         (t "task"))
                        (concat priority number)
                        status
                        status-style
                        text
                        (if link (format "<span style=\"font-size: x-small\">(%s)</span>" link) "")))))
    (insert "</table>\n")))

;;;_+ 2005.04.20: Recent posts

(defun sacha/planner-add-recent ()
  "Add the current note to the list of recent entries, trimmed."
  (interactive)
  (let* ((info (planner-current-note-info))
         (url (concat 
               "http://sacha.free.net.ph/notebook/wiki/"
               (emacs-wiki-published-name (planner-note-page info))
               "#anchor-" (planner-note-anchor info)))
         (title (planner-note-title info)))
    (when info
      (when (string-match (concat "\\s-*(\\(?:" emacs-wiki-name-regexp
                                  "\\)\\(?:" (regexp-quote planner-multi-separator)
                                  "\\(?:" emacs-wiki-name-regexp "\\)\\)*)\\s-*$")
                          title)
        (setq title (replace-match "" nil nil title)))
      (with-current-buffer
          (find-file-noselect "/home/sacha/notebook/wiki/include/footer.inc.php")
        (goto-char (point-min))
        (when (re-search-forward "<!--recent-->" nil t)
          (save-restriction
            (narrow-to-region (1+ (line-end-position))
                              (progn
                                (re-search-forward "<!--end of recent-->" nil t)
                                (line-beginning-position)))
            (goto-char (point-min))

            (insert "- <a href=\"" url "\">"
                    title
                    "</a><br/>\n")
            (forward-line 14)
            (delete-region (point) (point-max))
            (save-buffer))))))
  nil)
        
(add-hook 'remember-planner-append-hook 'sacha/planner-add-recent t)

;;;_+ Don't prompt for a date; always schedule tasks onto today

;(defun sacha/planner-read-task ()
;  "Return a list of information for a task.
;Universal prefix means don't schedule the task onto today."
;  (list
;   (read-string "Describe task: ")
;   (if current-prefix-arg
;       (planner-read-date)
;     (planner-today))
;   (when planner-use-plan-pages
;     (let ((planner-default-page
;            (if (and (planner-derived-mode-p 'planner-mode)
;                     (planner-page-name)
;                     (not (string-match planner-date-regexp
;                                        (planner-page-name))))
;                (planner-page-name)
;              planner-default-page)))
;       (planner-read-non-date-page
;        (planner-file-alist))))
;   planner-default-task-status))
(setq planner-default-task-priority "B")
(defun sacha/planner-read-task ()
  (list (read-string "Describe task: ")
        (and current-prefix-arg (planner-read-date))
        "TaskPool"
        planner-default-task-status))
(defalias 'planner-read-task 'sacha/planner-read-task)

;;;_+ Delete this page

(defun sacha/planner-delete-page ()
  "Delete this page and the published file."
  (interactive)
  (condition-case nil
      (delete-file (emacs-wiki-published-file))
    (error nil))
  (condition-case nil
      (delete-file (buffer-file-name))
    (error nil))
  (kill-buffer (current-buffer))
  (with-planner
    (emacs-wiki-refresh-file-alist)))

;;;_+ Automatically cross-reference new tasks onto TaskPool

(setq planner-multi-copy-tasks-to-page "TaskPool")

;;;_+ Always fix tasks

(defun peter/planner-fix-tasks-after-marking (old-status new-status)
  "Always fix tasks."
  (unless (string= old-status new-status)
    (save-window-excursion
      (let ((info (planner-current-task-info)))
        (planner-fix-tasks)
        (when (planner-task-link-text info)
          (if (string-match planner-multi-separator (planner-task-link-text info))
              (let ((links (planner-multi-task-link-as-list info)))
                (while links
                  (planner-find-file (car links))
                  (planner-fix-tasks)
                  (setq links (cdr links))))
            (planner-find-file (planner-task-link info))
            (planner-fix-tasks)))
        (planner-find-file (planner-task-page info))
        (planner-find-task info))))
  t)

; (add-hook 'planner-mark-task-hook 'peter/planner-fix-tasks-after-marking t)

;;;_+ publishing markup

(setq planner-publishing-markup
      '([emacs-wiki-tag-regexp 0 emacs-wiki-markup-custom-tags]
        sacha/planner-markup-tasks
        sacha/planner-markup-notes))

;;;_+ Sacha's funky task sorting

(setq planner-sort-tasks-key-function 'sacha/planner-sort-tasks-key)
(defun sacha/planner-sort-tasks-key ()
  "Provide old sorting behavior.
Day pages sort by status and priority. Plan pages sort by date,
status and priority."
  (if planner-on-date-page
      (sacha/planner-sort-tasks-basic)
    (sacha/planner-sort-tasks-by-date)))

(defun sacha/planner-sort-tasks-basic ()
  "Sort tasks by time (@1030, etc), priority, and status (oP_>XC)."
  (let* ((info (planner-current-task-info))
         (status (aref (planner-task-status info) 0)))      
    (concat
     ;; time
     (or (and (string-match "@[0-9][0-9][0-9][0-9]" (planner-task-description info))
              (match-string 0 (planner-task-description info)))
         "@9999")
     (planner-task-priority info)
     ;; status
     (cond
      ((eq status ?o) "1")
      ((eq status ?P) "2")
      ((eq status ?>) "4")
      ((eq status ?X) "5")
      ((eq status ?C) "6")
      (t "3")))))

(setq planner-sort-undated-tasks-equivalent "0000.00.00")
(defun sacha/planner-sort-tasks-by-date ()
  "Sort undated and unscheduled tasks first, then sort by status and priority."
  (skip-chars-forward "#ABC")
  (let ((ch (char-before))
        status)
    (skip-chars-forward "0123456789 ")
    (setq status (char-after))
    (goto-char (line-end-position))
    (skip-chars-backward "]) ")
    (format "%10s%1c%1c"
            (let ((date (or (planner-task-date (planner-current-task-info))
                            planner-sort-undated-tasks-equivalent)))
              (if (or (= status ?X)
                      (= status ?C))
                  (sacha/planner-invert-date date)
                date))
            (cond
             ((= status ?o) ?1)
             ((= status ?X) ?3)
             ((= status ?C) ?4)
             (t ?2))
            ch)))

(defun sacha/planner-invert-date (date)
  "Reverse the date in the sorting order."
  (mapconcat (lambda (ch)
               (if (string= ch ".")
                   ch
                 (number-to-string
                  (- 9 (string-to-number ch)))))
             (split-string date "" t) ""))
   
;;;_+ Specify task priority on creation

(defadvice planner-create-task-from-buffer (before cwebber activate)
  "Change the priority if specified.
You can set the priority of a task during creation by starting the
task description with #A, #B, or #C. This changes the default task
status."
  (when (string-match "^#\\([ABC]\\)[ \t]" title)
    (setq planner-default-task-priority (match-string 1 title))
    (setq title (substring title (match-end 0)))))

;;;_+ 2005.07.01 Technorati

(defun sacha/planner-technorati-resolve (url)
  "Add a link to the technorati tag named by URL."
  (when (string-match "technorati:\\(.+\\)" url)
    (concat "http://www.technorati.com/tag/" (match-string 1 url))))

(defun sacha/planner-technorati-browse (url)
  "Browse the Technorati tag named by URL (technorati:emacs, for example)."
  (when (string-match "technorati:\\(.+\\)" url)
    (browse-url (sacha/planner-technorati-resolve url))))

(planner-add-protocol "technorati" 'sacha/planner-technorati-browse 'sacha/planner-technorati-resolve)

;;;_+ Publishing

(defun sacha/emacs-wiki-publish-this-page ()
  "Save and publish this page."
  (interactive)
  (let ((new (not (emacs-wiki-page-file (planner-current-file)))))
    (save-buffer)
    (when new (emacs-wiki-refresh-file-alist))
    (emacs-wiki-publish-this-page)))

(define-key planner-mode-map (kbd "C-c C-g") 'sacha/emacs-wiki-publish-this-page)

;;;_+ Auto-schedule tasks onto today before marking them as done

(defun sacha/planner-track-finished-tasks (old-status new-status)
  "Automatically reschedule tasks onto today before marking them as done.
Add this to `planner-mark-task-hook'."
  (when (string= new-status "X")
    (let ((info (planner-current-task-info)))
      (unless (string= (planner-task-date info) (planner-today))
        (planner-copy-or-move-task (planner-today) t)
        (when (string-match planner-date-regexp (planner-page-name))
          (planner-find-file (planner-today))
          (planner-find-task info)))))
  t)
(add-hook 'planner-mark-task-hook 'sacha/planner-track-finished-tasks)

;;;_+ 2005.04.07 Podcasts

(defun sacha/planner-podcast-browse (url)
  "Play the given podcast."
  (when (string-match "pod:\\(.+\\)" url)
    (shell-command (concat "play ~/notebook/podcast/"
                           (planner-replace-regexp-in-string "#" "-"
                                                             (match-string 1 url)
                                                             ".mp3 &"))))) 

(defun sacha/planner-podcast-resolve (url)
  "Link to the given podcast."
  (when (string-match "pod:\\(.+\\)" url)
    (concat "http://sacha.free.net.ph/notebook/podcast/"
            (planner-replace-regexp-in-string "#" "-" (match-string 1 url)) ".mp3")))

(defun sacha/planner-podcast-tag (beg end &optional attrs)
  (interactive)
  (let ((filename (cdr (assoc "file" attrs))))
    (insert "<a href=\"http://sacha.free.net.ph/notebook/podcast/"
            (planner-replace-regexp-in-string "#" "-" filename) ".mp3\">"
            (cdr (assoc "title" attrs)) " <img src=\"http://sacha.free.net.ph/notebook/wiki/images/podcast.gif\" alt=\"MP3 podcast\" border=\"0\" valign=\"center\"></a>")))


(add-to-list 'emacs-wiki-markup-tags '("pod" nil t nil sacha/planner-podcast-tag))

(planner-add-protocol "pod" 'sacha/planner-podcast-browse 'sacha/planner-podcast-resolve)

;;;_+ Do not add cyclic tasks to pool

(defadvice planner-cyclic-create-task-maybe (around sacha activate)
  "Do not add cyclic tasks to TaskPool."
  (let ((planner-multi-copy-tasks-to-page nil))
    ad-do-it))

;;;###autoload
(defun sacha/planner-multi-remove-task-from-pool ()
  "Remove tasks from TaskPool."
  (interactive)
  (with-planner-update-setup
  (let ((info (planner-current-task-info)))
    (when (planner-task-link-text info)
      ;; If it is linked to TaskPool _and_ at least one other thing
      (cond
       ((string-match planner-multi-separator (planner-task-link-text info))
        (let ((remove-from
               (mapcar 'planner-link-base (planner-multi-split planner-multi-copy-tasks-to-page)))
              new-links)
          (setq new-links
                (delq nil
                      (mapcar (lambda (item)
                                (unless (member (planner-link-base item) remove-from)
                                  (planner-link-base item)))
                              (planner-multi-task-link-as-list info))))
          (save-excursion
            (planner-replan-task (mapconcat 'identity new-links planner-multi-separator)))
          ;; Make sure we are on the same task
          (when (string= (planner-page-name) planner-multi-copy-tasks-to-page)
            (planner-find-file (car new-links))
            (planner-find-task info))))
       ;; Else if it has a date and is linked to TaskPool
       ((and (planner-task-date info)
             (string= (planner-task-plan info) planner-multi-copy-tasks-to-page)
             (save-excursion (planner-replan-task nil))
             (when (string= (planner-page-name) planner-multi-copy-tasks-to-page)
               (planner-find-file (planner-task-date info))
               (planner-find-task info)))))))))

;;;_+ End

(provide 'planner-config)

;;;_* Local emacs vars.
;;;Local variables:
;;;allout-layout: (* 0 : )
;;;End:

;;; planner-config.el ends here
