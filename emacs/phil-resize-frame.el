;; ----------------------------------------

;; (defun fullscreen () (interactive)
;;   (set-frame-parameter nil 'fullscreen
;;                        (if (frame-parameter nil 'fullscreen)
;;                            nil
;;                          'fullboth)))

(defun resize-frame (frame) (interactive)
  (progn
    (set-frame-position frame 0 0)
    (mf-set-frame-pixel-size frame
                             1400
                             (- (display-pixel-height) 50))))

(defun resize-current-frame () (interactive)
  (resize-frame (selected-frame)))

(add-hook 'after-make-frame-functions 'phil-new-frame-hook)
;;(add-hook 'window-setup-hook 'maximize-frame t)

(defun phil-new-frame-hook (frame)
  (interactive)
  (require 'cl) ;; provides 'case'
  (case (framep frame)
    ('ns (when (require 'maxframe "maxframe" 'noerror)
           (select-frame frame)
           (ns-raise-emacs)
           (sleep-for 0 10)
           (maximize-frame)))
    ('x (raise-frame frame))
    ))

(when (fboundp 'ns-toggle-fullscreen)
  (global-set-key (kbd "<s-return>") 'ns-toggle-fullscreen))

;; ----------------------------------------

(provide 'phil-resize-frame)

