
(defvar phil/calendars '("personal" "janrain"))

(defvar phil/calendar-dir (dropbox-dir "org" "calendars"))

;; TODO restrict import to time range?
;;;###autoload
(defun phil/import-calendars ()
  (interactive)
  (let* ((diary-name "calendar.diary")
         (diary-path (concat phil/calendar-dir "/" diary-name))
         window-conf)

    ;; (let ((buf (get-buffer diary-name)))
    ;;   (when (bufferp buf) (kill-buffer buf)))

    (let ((buf (find-buffer-visiting diary-path)))
      (when (bufferp buf) (kill-buffer buf)))

    (when (file-exists-p diary-path)
      (delete-file diary-path))

    (setq window-conf (current-window-configuration))

    (dolist (name phil/calendars)
      (let* ((ics-name (concat name ".ics"))
             (ics-path (concat phil/calendar-dir "/" ics-name)))

        ;; (let ((buf (get-buffer ics-name)))
        ;;   (when (bufferp buf) (kill-buffer buf)))

        ;; (let ((buf (find-buffer-visiting ics-path)))
        ;;   (when (bufferp buf) (kill-buffer buf)))

        (icalendar-import-file ics-path diary-path)

        ;; (let ((buf (get-buffer ics-name)))
        ;;   (when (bufferp buf) (kill-buffer buf)))

        (let ((buf (find-buffer-visiting ics-path)))
          (when (bufferp buf) (kill-buffer buf)))

        ))

    (set-window-configuration window-conf)

    (let ((buf (find-buffer-visiting diary-path)))
      (when (bufferp buf) (kill-buffer buf)))

    ))

(provide 'phil-calendar)
