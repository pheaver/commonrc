;; ----------------------------------------
;; my cryptol/quattro settings

(require 'phil-paths)
(require 'phil-notify-timer)

(load "cryptol-site-file" 'noerror)

(when (string= system-type "darwin")
  (add-hook 'compilation-finish-functions
    (lambda (buffer str) (notify "Compile" str))))

(defvar quattro-source-dir nil)

(defvar quattro-source-dirs
  '("~/Quattro" "~/signali/Quattro")
  "List of directories to search for Quattro source.")

(defun quattro-source-dir-p (dir)
  "Return non-nil if DIR is a Quattro source directory"
  (file-exists-p (concat dir "/quattro.cabal")))

(defun quattro-source-dir ()
  (interactive)
  (or quattro-source-dir
      (setq quattro-source-dir
            (find-if 'quattro-source-dir-p quattro-source-dirs))))

(defun compile-quattro ()
  (interactive)
  (if (quattro-source-dir)
      (compile (concat "cd " quattro-source-dir " && make -k") t)
    (error "quattro-source-dir is not set")))

(global-set-key (kbd "C-x M") 'compile-quattro)

;; ----------------------------------------

(provide 'phil-quattro)
