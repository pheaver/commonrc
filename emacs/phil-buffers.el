(defun revert-buffer-noconfirm (&optional ignore-auto preserve-modes)
  (interactive "P")
  (revert-buffer ignore-auto t preserve-modes))

(defun phil/bury-buffer (&optional arg buffer-or-name)
  (interactive "P")
  (bury-buffer buffer-or-name)
  (when arg (other-window 1)))

(global-set-key (kbd "C-M-]") 'phil/bury-buffer)
(global-set-key (kbd "C-x g") 'revert-buffer-noconfirm)

(global-set-key (kbd "C-M-;")
 (lambda () (interactive) (switch-to-buffer (other-buffer)))) ;; nil means (other-buffer)
(global-set-key (kbd "C-M-'")
 (lambda () (interactive) (display-buffer (other-buffer) 'not-this-window)))

;; emacs should have ibuffer set to autoload, so this will bind C-x C-b when
;; ibuffer is available, even if it's not yet loaded.
(if (fboundp 'ibuffer)
    (global-set-key (kbd "C-x C-b") 'ibuffer))

(setq ibuffer-default-sorting-mode 'major-mode)
(setq ibuffer-always-compile-formats t)
(setq ibuffer-use-other-window nil)

(if (require 'ido nil t)
    (ido-mode t))

(eval-after-load "ido"
  '(progn
     (ido-everywhere t)
     (defalias 'read-buffer 'ido-read-buffer)
     (defalias 'read-directory-name 'ido-read-directory-name)
     (defalias 'read-file-name 'ido-read-file-name)))

(setq magit-completing-read-function 'magit-ido-completing-read)
(setq gnus-completing-read-function 'gnus-ido-completing-read)

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
          (and (boundp 'ido-cur-list) ido-cur-list)) ; Avoid infinite loop from ido calling this
      ad-do-it
    (let ((allcomp (all-completions "" collection predicate)))
      (if allcomp
          (setq ad-return-value
                (ido-completing-read prompt
                               allcomp
                               nil require-match initial-input hist def))
        ad-do-it))))

(defvar ido-execute-command-cache nil)

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

(setq ido-ignore-buffers
      '("^ " "*Buffer" "*Help*" "*Messages" "*Shell Command Output" "*Completions"))

(setq ido-max-prospects 30)
(setq ido-max-window-height 2) ;; nil means use max-mini-window-height

(setq ido-decorations '("" "" "," " ..." "[" "]" " [No match]" " [Matched]"))

;; kinda neat:
;; (setq ido-decorations '("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]"))

;; ----------------------------------------

(provide 'phil-buffers)

