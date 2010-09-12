
(autoload 'multi-term "multi-term" nil t)

(setq multi-term-program "/bin/bash")
;(setq multi-term-program "/opt/local/bin/bash")
;(setq multi-term-program "/opt/local/bin/zsh")
;(setq multi-term-program "/bin/zsh")

(setq term-term-name "eterm-color")
(setq multi-term-switch-after-close nil)

;; (define-derived-mode phil-term-mode
;;   term-mode "phil-term" "Phil's variant of term-mode")

;; (add-hook 'phil-term-mode-hook
;;  (lambda ()
;;    (term-key-setup)
;;    (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
;;    ))

;; -----------

(setq term-unbind-key-list
  '("C-z" "C-x" "C-c" "C-h" "<ESC>"))

(setq term-bind-key-alist
  '(
    ("C-c C-c" . term-interrupt-subjob)
    ("C-c C-k" . term-char-mode)
    ("C-c C-j" . term-line-mode)
;    ("C-c C-a" . term-bol)
    ("C-c C-a" . move-beginning-of-line)
    ("C-c C-g" . keyboard-quit)
    ("C-c C-q" . term-pager-toggle)
    ("C-u" . universal-argument)
;    ("C-c C-u" . universal-argument)
    ("C-p" . term-send-raw)
    ("C-n" . term-send-raw)
    ("s-v" . phil-term-yank)
;    ("C-c C-y" . yank)
;    ("C-p" . previous-line)
;    ("C-n" . next-line)
;    ("M-p" . term-send-up)
;    ("M-n" . term-send-down)
    ("C-v" . term-send-raw)
    ("M-v" . term-send-raw-meta)
    ("C-m" . term-send-raw)
    ("M-," . term-send-input)
    ("M-d" . term-send-raw-meta)
;    ("M-." . comint-dynamic-complete)
; I don't care for these
;    ("C-s" . isearch-forward)
;    ("C-r" . isearch-backward)
;    ("M-f" . term-send-forward-word)
;    ("M-b" . term-send-backward-word)
;    ("M-o" . term-send-backspace)
;    ("M-M" . term-send-forward-kill-word)
;    ("M-N" . term-send-backward-kill-word)
    ))

;; switch to line mode, yank, then switch back to character mode
(defun phil-term-yank ()
  (interactive)
  (progn (term-line-mode)
         (yank)
         (term-char-mode)))

;; (defun phil-ido-select (desc alist)
;;   (let* ((x (ido-completing-read desc (mapcar 'car alist)))
;;       (y (assoc-string x alist)))
;;     (if y
;;      (cdr y)
;;       x)))

;; (defun phil-create-term-buffer (name)
;;   (switch-to-buffer-other-window name)
;;   (set-buffer name)
;;   (phil-term-mode)
;;   (term-exec (get-buffer name) name "bash" nil nil)
;;   (term-char-mode)
;;   )

(defun term-modep () (eq major-mode 'term-mode))

(defun multi-term-other-window ()
  (interactive)
  (require 'multi-term)
  (let ((term-buffer (multi-term-get-buffer)))
    (set-buffer term-buffer)
    (multi-term-internal)
    (switch-to-buffer-other-window term-buffer)))

(defun phil-term (other-window create-new)
  (require 'multi-term)
  (let ((buffers (multi-term-list)))
    (if (or create-new (null buffers))
        (if other-window
            (multi-term-other-window)
          (multi-term))
      (if other-window
          (switch-to-buffer-other-window (car buffers))
        (switch-to-buffer (car buffers))))))

(defun phil-toggle-term (&optional arg)
  "Open or close a multi-term buffer.  If ARG is `t', create a
  new buffer.  If ARG is a number, switch to a terminal whose
  name is based on `multi-term-buffer-name` and that number.
  Otherwise, if current buffer is a terminal, close it and
  restore previous window configuration.  Otherwise, create a new
  buffer."
  (interactive "P")
  (setq current-prefix-arg ())
  (cond ((numberp arg)
         ;; TODO: use prefix number to choose which terminal to open
         (message "Whoops!"))
        (arg
         (if (term-modep)
             (phil-term nil t)
           (setq phil-toggle-term-win-conf (current-window-configuration))
           (phil-term t t)))
        ((term-modep)
         (let ((b (current-buffer)))
           (bury-buffer b)
           (other-window -1)))
        (t
         (setq phil-toggle-term-win-conf (current-window-configuration))
         (phil-term t nil))
        ))

;(defun phil-return-from-term () (bury-buffer))

  ;; (setq b (current-buffer))
  ;; (when (window-configuration-p phil-toggle-term-win-conf)
  ;;   (set-window-configuration phil-toggle-term-win-conf)
  ;;   (setq phil-toggle-term-win-conf nil))
  ;; (bury-buffer b))


(setq multi-term-dedicated-select-after-open-p t)
(setq multi-term-dedicated-skip-other-window-p t)

;(global-set-key (kbd "C-c C-;") 'multi-term-dedicated-toggle)
(global-set-key (kbd "C-c C-;") 'phil-toggle-term)
;(global-set-key (kbd "C-c C-b") 'phil-select-term-buffer)

(provide 'phil-term)
