;; ----------------------------------------
;; code for selecting and switching buffers

;; TODO: swap buffers
;; TODO: scroll other window?
;; TODO: kill current buffer, kill other buffer?

(global-set-key (kbd "C-M-;")
 (lambda () (interactive) (switch-to-buffer (other-buffer)))) ;; nil means (other-buffer)
(global-set-key (kbd "C-M-'")
 (lambda () (interactive) (display-buffer (other-buffer) 'not-this-window)))
;; (lambda () (interactive) (set-window-buffer (next-window) (other-buffer))))

;(global-set-key (kbd "C-M-;")
; (lambda () (interactive) (display-buffer (other-buffer))))

; this one is insane; call it three times to swap
; (lambda () (interactive) (switch-to-buffer-other-window (other-buffer))))

;; (global-set-key [(control tab)]
;;  (lambda () (interactive) (other-window 1)))
;; (global-set-key [(control shift tab)]
;;  (lambda () (interactive) (other-window -1)))

;; ----------

(if (require 'ibuffer nil t)
    (global-set-key (kbd "C-x C-b") 'ibuffer))

(if (require 'ido nil t)
    (ido-mode t)
  (if (require 'iswitchb nil t)
      (iswitchb-mode t)))

(eval-after-load "ido"
  '(progn
     (ido-everywhere t)
     (defalias 'read-buffer 'ido-read-buffer)
     (defalias 'read-directory-name 'ido-read-directory-name)
     (defalias 'read-file-name 'ido-read-file-name)))

(defvar ido-enable-replace-completing-read t
  "If t, use ido-completing-read instead of completing-read if possible.

Set it to nil using let in around-advice for functions where the
original completing-read is required.  For example, if a function
foo absolutely must use the original completing-read, define some
advice like this:

(defadvice foo (around original-completing-read-only activate)
  (let (ido-enable-replace-completing-read) ad-do-it))")

;; Replace completing-read wherever possible, unless directed otherwise
(defadvice completing-read
  (around use-ido-when-possible activate)
  (if (or (not ido-enable-replace-completing-read) ; Manual override disable ido
          (boundp 'ido-cur-list)) ; Avoid infinite loop from ido calling this
      ad-do-it
    (let ((allcomp (all-completions "" collection predicate)))
      (if allcomp
          (setq ad-return-value
                (ido-completing-read prompt
                               allcomp
                               nil require-match initial-input hist def))
        ad-do-it))))

(setq ido-execute-command-cache nil)

(defun ido-execute-command ()
  (interactive)
  (call-interactively
   (intern
    (ido-completing-read
     "M-x "
     (progn
       (unless ido-execute-command-cache
         (mapatoms (lambda (s)
                     (when (commandp s)
                       (setq ido-execute-command-cache
                             (cons (format "%S" s) ido-execute-command-cache))))))
       ido-execute-command-cache)))))

(global-set-key (kbd "M-X") 'ido-execute-command)

;(setq ido-enable-flex-matching nil)

;; (when (and darwin-system window-system)
;;     (add-hook 'ido-setup-hook
;;            '(lambda ()
;;               (set-face-attribute 'ido-first-match '() :foreground "blue4" :weight 'bold)
;;               (set-face-attribute 'ido-subdir '() :foreground "khaki")
;;               ))
;; )

;(setq iswitchb-buffer-ignore
;      '("^ " "*Buffer" "*Help*" "*Messages" "*Shell Command Output" "*Completions"))
(setq ido-ignore-buffers
      '("^ " "*Buffer" "*Help*" "*Messages" "*Shell Command Output" "*Completions"))

(setq ido-max-prospects 30)
(setq ido-max-window-height 2) ;; nil means use max-mini-window-height

(setq ido-decorations '("" "" "," " ..." "[" "]" " [No match]" " [Matched]"))

;; kinda neat:
;; (setq ido-decorations '("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))

;; ----------------------------------------

(provide 'phil-buffers)

