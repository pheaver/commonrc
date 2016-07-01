(require 'phil-paths)

(defun find-tags-table (&optional filename)
  (interactive)
  (require 'etags)
  (make-local-variable 'tags-table-list)
  (let ((filename1 (or filename (buffer-file-name)))
        (mode-name (substring (symbol-name major-mode) 0 -5)))
    (make-tags-table-list commonrc-dir t mode-name)
    (make-tags-table-list (abbreviate-file-name filename1) t mode-name)))

;; search for TAGS file in all parent directories of path,
;; and return a list that can be added to tags-table-list.
(defun make-tags-table-list (path &optional parents ext)
  "Search PATH for TAGS files and add to `tags-table-list'.

If PARENTS is an integer, then search that number of parent directories of PATH.
If PARENTS is t, then search all parent directories of PATH.

If EXT is non-nil, then also search for TAGS.EXT in each directory, in addition
to TAGS."

  (interactive)
  (require 'phil-utils)
  (let ((all-dirs (phil/parent-dirs path))
        (suffixes (if (null ext) '("") `("" ,(concat "." ext))))
        (dirs nil))

    (when parents
      (if (equal parents t)
          (setq dirs (reverse all-dirs))
        (dotimes (n (min parents (length all-dirs)))
          (push (pop all-dirs) dirs))))

    (when (file-directory-p path)
      (push path dirs))

    (dolist (dir dirs)
      (let ((file1 (concat dir "TAGS"))
            (file2 (and ext (concat dir "TAGS" "." ext))))
        (when (file-exists-p file1)
          (add-to-list 'tags-table-list file1))
        (when (and file2 (file-exists-p file2))
          (add-to-list 'tags-table-list file2))))))

(defun my-find-tag ()
  (interactive)
  (find-tag (funcall
             (or find-tag-default-function
                 (get major-mode 'find-tag-default-function)
                 'find-tag-default))))

(eval-after-load "etags"
  '(make-tags-table-list commonrc-dir t))

(global-set-key "\M-]" 'my-find-tag)
(global-set-key "\M-[" 'pop-tag-mark)

(add-hook 'find-file-hook 'find-tags-table)

(setq tags-revert-without-query t)

(provide 'phil-tags)
