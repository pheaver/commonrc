(require 'dired)

(defun dired-cleanup-marked-files ()
  (interactive)
  (require 'phil-utils)
  (let ((files (dired-get-marked-files nil nil)))
    (mapc 'phil/cleanup files)))

(provide 'phil-dired)
