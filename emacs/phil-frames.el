
;;
;; old stuff for resizing and raising frames, mostly on 'ns (OS X)
;;
(defun resize-frame (frame) (interactive)
  (progn
    (set-frame-position frame 0 0)
    (mf-set-frame-pixel-size frame
                             1400
                             (- (display-pixel-height) 50))))

(defun resize-current-frame () (interactive)
  (resize-frame (selected-frame)))

(setq ns-use-native-fullscreen nil)

(defun phil/toggle-fullscreen (&optional frame)
  (interactive)
  (when (memq window-system '(x ns))
    (cond
     ((fboundp 'ns-toggle-fullscreen) (ns-toggle-fullscreen frame))
     ((fboundp 'toggle-frame-fullscreen) (toggle-frame-fullscreen frame))
     (t (set-frame-parameter frame 'fullscreen
                             (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))))


(global-set-key (kbd "<s-return>") 'phil/toggle-fullscreen)
(global-set-key [f11] 'phil/toggle-fullscreen)

(defun phil/ns-raise-emacs ()
  (ns-do-applescript "tell application \"Emacs\" to activate"))

;;
;; frame theming
;;
(defun phil/set-face-attribute ( face frame &rest args)
  (if (facep face) (apply 'set-face-attribute face frame args)))

(defun phil/set-face-attributes (&optional frame)
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

(setq phil/preferred-themes '(wombat ample ample-flat solarized-dark deeper-blue))

(defun phil/cycle-theme ()
  (interactive)
  (setq phil/preferred-themes (append (cdr phil/preferred-themes) (list (car phil/preferred-themes))))
  (phil/set-theme))

;; find first theme in phil/preferred-themes that is available
(defun phil/get-preferred-theme ()
  (defun find-theme (preferred-themes available-themes)
    (if preferred-themes
	(let ((theme (car preferred-themes)))
	  (if (member theme available-themes)
	      theme
	    (find-theme (cdr preferred-themes))))))
  (find-theme phil/preferred-themes (custom-available-themes)))

(defun phil/set-theme (&optional theme frame)
  (interactive)
  (let ( (theme (or theme (phil/get-preferred-theme))) )
    (when theme
      ;; disable other themes first, so we don't get a cumulative effect
      (dolist (current-theme custom-enabled-themes) (disable-theme current-theme))
      (load-theme theme t)
      (phil/set-face-attributes frame)
      (message (format "Current theme: %s" theme)))))

(defun phil/new-frame-hook (&optional frame)
  ;; select and raise frame
  (let ((frame-type (framep frame)))
    (when frame-type
      (select-frame frame)
      (raise-frame frame))
    (when (equal frame-type 'ns)
      (phil/ns-raise-emacs)
      ;; (when (require 'maxframe "maxframe" 'noerror)
      ;;   (sleep-for 0 10)
      ;;   (maximize-frame))
      ))

  (when window-system (phil/set-theme nil frame)))

(add-hook 'after-make-frame-functions 'phil/new-frame-hook)

(add-hook 'window-setup-hook 'phil/new-frame-hook t)

;; ----------------------------------------

(provide 'phil-frames)

