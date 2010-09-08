;; ----------------------------------------

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 500)
(setq recentf-max-menu-items 60)
;;(global-set-key (kbd "C-c r") 'recentf-open-files)
(global-set-key (kbd "C-c r") 'recentf-choose-file)
(global-set-key (kbd "C-c R") 'recentf-open-most-recent-file)

(defun recentf-choose-file ()
  "Select a recently opened file from the `recentf-list'"
  (interactive)
  (find-file
   (completing-read "Recentf open: "
                    (mapcar 'abbreviate-file-name recentf-list)
                    nil t)))

;; ----------------------------------------

(provide 'phil-recentf)
