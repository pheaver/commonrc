(require 'dired)

(defun dired-cleanup-marked-files ()
  (interactive)
   (let ((files (dired-get-marked-files nil nil)))
     (mapc 'cleanup files)))

(defun cleanup (file)
  (let* ((buffer0 (find-buffer-visiting file))
         (buffer1 (or buffer0 (find-file file))))
    (with-current-buffer buffer1
      (delete-trailing-whitespace)
      (untabify (point-min) (point-max)))
    (when (not buffer0)
      (save-buffer buffer1)
      (kill-buffer buffer1))))

