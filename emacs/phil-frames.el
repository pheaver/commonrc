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

(when (fboundp 'ns-toggle-fullscreen)
  (global-set-key (kbd "<s-return>") 'ns-toggle-fullscreen))

(defun phil/ns-raise-emacs ()
  (ns-do-applescript "tell application \"Emacs\" to activate"))

(defun phil/new-frame-hook (frame)
  (interactive)
  (select-frame frame)
  (raise-frame frame)
  (let ((x (framep frame)))
    (when (equal x 'ns)
      (phil/ns-raise-emacs)
      (when (require 'maxframe "maxframe" 'noerror)
        (sleep-for 0 10)
        (maximize-frame)))))

(add-hook 'after-make-frame-functions 'phil/new-frame-hook)
;;(add-hook 'window-setup-hook 'maximize-frame t)

;; ----------------------------------------

(provide 'phil-frames)

