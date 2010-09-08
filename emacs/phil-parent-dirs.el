;; ----------------------------------------

;; utility functions
(defun parent-dir (path)
  (interactive)
  (file-name-directory (directory-file-name path)))

(defun parent-dirs (path)
  (interactive)
  (let ((parent (parent-dir path)))
    (cond
     ((null parent) ())
     ((string= parent path) ())
     (t (cons parent (parent-dirs parent))))))

;; ----------------------------------------

(provide 'phil-parent-dirs)
