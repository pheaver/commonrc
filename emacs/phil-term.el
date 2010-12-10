
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

(defun term-modep () (eq major-mode 'term-mode))

(defun multi-term-other-window ()
  (interactive)
  (require 'multi-term)
  (let ((term-buffer (multi-term-get-buffer)))
    (set-buffer term-buffer)
    (multi-term-internal)
    (switch-to-buffer-other-window term-buffer)))

(defun phil-term-get-buffer (&optional arg special-shell)
  (let ((shell-name (or multi-term-program ;shell name
                        (getenv "SHELL")
                        (getenv "ESHELL")
                        "/bin/sh"))
        (index 0)
        term-name)
    (if (eq arg 'dedicated)
        (setq term-name multi-term-dedicated-buffer-name)

      (cd (or default-directory (expand-file-name multi-term-default-dir)))

      ;; select which terminal to use based on `arg'
      (setq buf-or-name
            (cond ((numberp arg)
                   (format "%s<%s>" multi-term-buffer-name arg))
                  ((eq arg 'dedicated)
                   multi-term-dedicated-buffer-name)
                  ((eq arg nil)
                   (or (car (multi-term-list))
                       (format "%s<%s>" multi-term-buffer-name 0)))
                  (t
                   (while (buffer-live-p (get-buffer (format "*%s<%s>*" multi-term-buffer-name index)))
                     (setq index (1+ index)))
                   (format "%s<%s>" multi-term-buffer-name index)))))

    ;; Try get other shell name if `special-shell' is non-nil.
    (if special-shell
        (setq shell-name (read-from-minibuffer "Run program: " shell-name)))

    (if (bufferp buf-or-name)
        buf-or-name
      (make-term buf-or-name shell-name))))

(defun phil-term (&optional arg other-window)
  (interactive "P")
  (require 'multi-term)
  (let (term-buffer)
    ;; Set buffer.
    (setq term-buffer (phil-term-get-buffer arg))
    (set-buffer term-buffer)
    ;; Internal handle for `multi-term' buffer.
    (multi-term-internal)

    ;; Switch buffer
    (if other-window
        (switch-to-buffer-other-window term-buffer)
    (switch-to-buffer term-buffer))))

(defun phil-toggle-term (&optional arg)
  "Open or close a multi-term buffer.  If ARG is `t', create a
  new buffer.  If ARG is a number, switch to a terminal whose
  name is based on `multi-term-buffer-name` and that number.
  Otherwise, if current buffer is a terminal, close it and
  restore previous window configuration.  Otherwise, create a new
  buffer."
  (interactive "P")
  (if (not (term-modep))
      (setq phil-toggle-term-win-conf (current-window-configuration)))

  (if (and (term-modep) (not arg))
      (progn
        ;; (set-window-configuration phil-toggle-term-win-conf)
        ;; (bury-buffer)
        (other-window -1))
    (phil-term arg (not (term-modep)))))

(setq multi-term-dedicated-select-after-open-p t)
(setq multi-term-dedicated-skip-other-window-p t)

(global-set-key (kbd "C-;") 'multi-term-dedicated-toggle)
;; (global-set-key (kbd "C-c C-;") 'phil-toggle-term)
(global-set-key (kbd "C-c C-;") 'phil-toggle-term)
;(global-set-key (kbd "C-c C-b") 'phil-select-term-buffer)

(provide 'phil-term)
