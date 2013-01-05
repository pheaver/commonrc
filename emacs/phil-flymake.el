;; ----------------------------------------
;; my flymake settings

(setq flymake-no-changes-timeout 1)
(setq flymake-start-syntax-check-on-newline nil)

;; http://www.emacswiki.org/emacs/FlymakeRuby
;; Invoke ruby with '-c' to get syntax checking
(defun flymake-ruby-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))

(defun turn-on-ruby-flymake ()
  (unwind-protect
  ;; Don't want flymake mode for ruby regions in rhtml files and also on read only files
  (if (and (not (null buffer-file-name)) (file-writable-p buffer-file-name))
      (flymake-mode))
  ))

(add-hook 'ruby-mode-hook 'turn-on-ruby-flymake)

;; everything below is from http://www.emacswiki.org/emacs/FlyMake
(defun my-flymake-err-at (pos)
  (let ((overlays (overlays-at pos)))
    (remove nil
            (mapcar (lambda (overlay)
                      (and (overlay-get overlay 'flymake-overlay)
                           (overlay-get overlay 'help-echo)))
                    overlays))))

(defun my-flymake-err-echo ()
  (message "%s" (mapconcat 'identity (my-flymake-err-at (point)) "\n")))

(eval-after-load "flymake"
  '(progn
     (push '(".+\\.rb$" flymake-ruby-init) flymake-allowed-file-name-masks)
     (push '("Rakefile$" flymake-ruby-init) flymake-allowed-file-name-masks)
     (push '("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)

     (require 'advice)
     (defadvice flymake-goto-next-error (after display-message activate compile)
       (my-flymake-err-echo))

     (defadvice flymake-goto-prev-error (after display-message activate compile)
       (my-flymake-err-echo))
     ))

;; ----------------------------------------

(provide 'phil-flymake)
