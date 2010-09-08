;; ----------------------------------------

(require 'phil-paths)
(require 'phil-parent-dirs)

(global-set-key "\M-]" 'my-find-tag)
(global-set-key "\M-[" 'pop-tag-mark)

(eval-after-load "etags"
  '(let ((dir (parent-dir (commonrc))))
     (when (file-exists-p (concat dir "/" "TAGS"))
       (add-to-list 'tags-table-list dir))))

(add-hook 'find-file-hook 'find-tags-table)

(setq tags-revert-without-query t)

(defun find-tags-table (&optional filename)
  (interactive)
  (require 'etags)
  (let* ((filename1 (or filename (buffer-file-name)))
         (mode-name (substring (symbol-name major-mode) 0 -5))
         (tagsfile (get-tags-table mode-name (abbreviate-file-name filename1))))
    (when tagsfile
      (add-to-list (make-local-variable 'tags-table-list) tagsfile))))

(defun get-tags-table (mode-name dir)
  (interactive)
  (let* ((ext (concat "." mode-name))
         (dirs (parent-dirs dir))
         (suffixes (list "" ext)))
    (locate-file "TAGS" dirs suffixes)))

(defun my-find-tag ()
  (interactive)
  (find-tag (funcall
             (or find-tag-default-function
                 (get major-mode 'find-tag-default-function)
                 'find-tag-default))))

;; ----------------------------------------

(provide 'phil-tags)
