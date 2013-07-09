;; ----------------------------------------

;; (defun fullscreen () (interactive)
;;   (set-frame-parameter nil 'fullscreen
;;                        (if (frame-parameter nil 'fullscreen)
;;                            nil
;;                          'fullboth)))

(setq color-theme-is-global nil)

(setq phil/solarized-path "~/local/src/solarized")

(if (>= emacs-major-version 24)
    (add-to-list 'custom-theme-load-path phil/solarized-path 'append)
  (add-load-path phil/solarized-path))

;; (autoload 'color-theme-solarized-light "color-theme-solarized")
;; (autoload 'color-theme-solarized-dark "color-theme-solarized")

(defun phil/set-face-attribute ( face frame &rest args)
  (if (facep face) (apply 'set-face-attribute face frame args)))

(defun phil/set-frame-theme (&optional frame)
  (interactive)

  ;; these break for any face attributes that aren't yet defined
  (phil/set-face-attribute 'jabber-activity-face frame
                      :foreground "orangered3" :weight 'bold)
  (phil/set-face-attribute 'jabber-activity-personal-face frame
                      :foreground nil :inherit 'outline-1 :weight 'bold)
  (phil/set-face-attribute 'jabber-chat-prompt-foreign frame
                      :foreground nil :inherit 'outline-2 :weight 'bold)
  (phil/set-face-attribute 'jabber-chat-prompt-local frame
                      :foreground nil :inherit 'outline-1 :weight 'bold)
  (phil/set-face-attribute 'jabber-chat-prompt-system frame
                      :foreground nil :inherit 'outline-3 :weight 'bold)
  (phil/set-face-attribute 'magit-item-highlight frame
                      :foreground nil :inherit 'highlight)
  (phil/set-face-attribute 'outline-2 frame
                      :inherit 'font-lock-variable-name-face :foreground "brown")
  (phil/set-face-attribute 'diff-changed frame :inverse-video nil)
  (phil/set-face-attribute 'diff-added frame :inverse-video nil)
  (phil/set-face-attribute 'diff-removed frame :inverse-video nil)
  (phil/set-face-attribute 'diff-header frame :background nil)
  (phil/set-face-attribute 'diff-file-header frame :background nil)
  )

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
  (set-variable 'color-theme-is-global nil)

  (when (and window-system
            (if (>= emacs-major-version 24)
                (load-theme 'solarized-dark t)
               (and (require 'color-theme-solarized nil 'noerror)
                    (color-theme-solarized-dark))))
    (phil/set-frame-theme frame))

  (let ((x (framep frame)))
    (when (equal x 'ns)
      (phil/ns-raise-emacs)
      ;; (when (require 'maxframe "maxframe" 'noerror)
      ;;   (sleep-for 0 10)
      ;;   (maximize-frame))
      )
    )
  )

(add-hook 'after-make-frame-functions 'phil/new-frame-hook)
;;(add-hook 'window-setup-hook 'maximize-frame t)

;; ----------------------------------------

(provide 'phil-frames)

