;; ----------------------------------------
;; code for selecting and switching buffers

;; TODO: swap buffers
;; TODO: scroll other window?
;; TODO: kill current buffer, kill other buffer?

(defun revert-buffer-noconfirm (&optional ignore-auto preserve-modes)
  (interactive "P")
  (revert-buffer ignore-auto t preserve-modes))

(global-set-key (kbd "C-M-]") 'bury-buffer)
(global-set-key (kbd "C-x g") 'revert-buffer)
(global-set-key (kbd "C-x G") 'revert-buffer-noconfirm)

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

;; emacs should have ibuffer set to autoload, so this will bind C-x C-b when
;; ibuffer is available, even if it's not yet loaded.
(if (fboundp 'ibuffer)
    (global-set-key (kbd "C-x C-b") 'ibuffer))

(setq ibuffer-default-sorting-mode 'major-mode)
(setq ibuffer-always-compile-formats t)
(setq ibuffer-use-other-window nil)

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

;; zzz This triggers a bug on Windows, something with the c:/
;; (add-hook 'ido-make-file-list-hook 'ido-sort-mtime)
;; (add-hook 'ido-make-dir-list-hook 'ido-sort-mtime)
(defun ido-sort-mtime ()
  (setq ido-temp-list
        (sort ido-temp-list
              (lambda (a b)
                (let ((ta (nth 5 (file-attributes (concat ido-current-directory a))))
                      (tb (nth 5 (file-attributes (concat ido-current-directory b)))))
                  (if (= (nth 0 ta) (nth 0 tb))
                      (> (nth 1 ta) (nth 1 tb))
                    (> (nth 0 ta) (nth 0 tb)))))))
  (ido-to-end  ;; move . files to end (again)
   (delq nil (mapcar
              (lambda (x) (if (string-equal (substring x 0 1) ".") x))
              ido-temp-list))))

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

