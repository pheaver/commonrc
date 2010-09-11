;; ----------------------------------------
;; my cryptol/quattro settings

(require 'phil-paths)

(load "cryptol-site-file" 'noerror)

(when (string= system-type "darwin")
  (add-hook 'compilation-finish-functions
    (lambda (buffer str) (notify "Compile" str))))

(defvar quattro-source-dir nil)

(defun set-quattro-source-dir ()
  (interactive)
  (or quattro-source-dir
      (let ((dirs '("~/Quattro" "~/signali/Quattro")))
        (while (and dirs (not quattro-source-dir))
          (when (file-directory-p (concat (car dirs) "/base"))
            (setq quattro-source-dir (car dirs)))
          (pop dirs))
        quattro-source-dir)))

(set-quattro-source-dir)

(defun compile-quattro ()
  (interactive)
  (if (set-quattro-source-dir)
      (compile (concat "cd " quattro-source-dir " && make -k") t)
    (error "quattro-source-dir is not set")))

(global-set-key (kbd "C-x M") 'compile-quattro)

;; ----------------------------------------

(provide 'phil-quattro)
