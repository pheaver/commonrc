;; ----------------------------------------

(require 'phil-paths)
(require 'phil-parent-dirs)

(defun find-tags-table (&optional filename)
  (interactive)
  (require 'etags)
  (make-local-variable 'tags-table-list)
  (let ((filename1 (or filename (buffer-file-name)))
        (mode-name (substring (symbol-name major-mode) 0 -5)))
    (make-tags-table-list (abbreviate-file-name filename1) mode-name)))

;; search for TAGS file in all parent directories of path,
;; and return a list that can be added to tags-table-list.
(defun make-tags-table-list (path &optional ext)
  (interactive)
  (setq result-dirs nil)
  (let ((dirs (reverse
               (append (parent-dirs (abbreviate-file-name path))
                       (parent-dirs (abbreviate-file-name (commonrc "."))))))
        (suffixes (if (null ext) '("") `("" ,(concat "." ext)))))
    (dolist (dir dirs)
      (let ((file1 (concat dir "TAGS"))
            (file2 (and ext (concat dir "TAGS" "." ext))))
        (when (file-exists-p file1)
          (add-to-list 'tags-table-list file1))
        (when (and file2 (file-exists-p file2))
          (add-to-list 'tags-table-list file2)))))
  result-dirs)

(defun my-find-tag ()
  (interactive)
  (find-tag (funcall
             (or find-tag-default-function
                 (get major-mode 'find-tag-default-function)
                 'find-tag-default))))

(eval-after-load "etags"
  '(make-tags-table-list (commonrc ".")))

(global-set-key "\M-]" 'my-find-tag)
(global-set-key "\M-[" 'pop-tag-mark)

(add-hook 'find-file-hook 'find-tags-table)

(setq tags-revert-without-query t)

;; ----------------------------------------

(provide 'phil-tags)
