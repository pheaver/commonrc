
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
(add-to-list 'default-frame-alist
             '(cursor-color . "orange"))

(defun phil/set-face-attribute ( face frame &rest args)
  (if (facep face) (apply 'set-face-attribute face frame args)))

(defun phil/set-face-attributes (&optional frame)
  (interactive)

  ;; (set-face-background 'default "black" frame)

  (set-cursor-color "orange")

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

(defvar phil/preferred-themes
  '(wombat ample ample-flat cybperpunk grandshell solarized-dark))

(defvar phil/current-theme nil)

;; find first theme in phil/preferred-themes that is available
(defun phil/get-preferred-theme ()
  (defun find-theme (preferred-themes available-themes)
    (if preferred-themes
        (let ((theme (car preferred-themes)))
          (if (member theme available-themes)
              theme
            (find-theme (cdr preferred-themes) available-themes)))))
  (or phil/current-theme (find-theme phil/preferred-themes (custom-available-themes))))

(defun phil/unload-themes ()
  (interactive)
  (dolist (theme custom-enabled-themes) (disable-theme theme)))

(defun phil/set-theme (&optional theme frame)
  (interactive
   (list
    (intern (completing-read "Choose theme: " (mapcar 'symbol-name (custom-available-themes))))
    nil))
  (cond
   ((null theme) (phil/unload-themes))
   ((member theme (custom-available-themes))
    ;; disable other themes first, so we don't get a cumulative effect
    (load-theme theme t)
    (phil/set-face-attributes frame)
    (when (fboundp 'smart-mode-line-enable)
      (smart-mode-line-enable))
    (setq phil/current-theme theme)
    (message (format "Current theme: %s" theme)))
   (t (message "Invalid theme: %s" theme))))

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

  (when window-system (phil/set-theme (phil/get-preferred-theme) frame)))

(add-hook 'after-make-frame-functions 'phil/new-frame-hook)

(add-hook 'window-setup-hook 'phil/new-frame-hook t)

;; ----------------------------------------

(provide 'phil-frames)
