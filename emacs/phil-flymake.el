;; ----------------------------------------
;; my flymake settings

;; don't auto-start flymake after idling
(setq flymake-no-changes-timeout nil)

;; don't auto-start flymake on newline
(setq flymake-start-syntax-check-on-newline nil)

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
     (require 'advice)
     (defadvice flymake-goto-next-error (after display-message activate compile)
       (my-flymake-err-echo))

     (defadvice flymake-goto-prev-error (after display-message activate compile)
       (my-flymake-err-echo))
     ))

;; ----------------------------------------

(provide 'phil-flymake)
