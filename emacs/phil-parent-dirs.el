;; ----------------------------------------

(defun phil/parent-dir (path)
  (interactive)
  (file-name-directory (directory-file-name path)))

(defun phil/parent-dirs (path)
  (interactive)
  (let ((parent (phil/parent-dir path)))
    (cond
     ((null parent) ())
     ((string= parent path) ())
     (t (cons parent (phil/parent-dirs parent))))))

;; ----------------------------------------

(provide 'phil-parent-dirs)
