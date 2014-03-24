(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(unless (package-installed-p 'scala-mode2)
  (package-refresh-contents) (package-install 'scala-mode2))

(add-load-path "~/local/src/ensime/dist_2.10.3")

(setq ensime-sbt-compile-on-save nil)

(defun make-spray-doc-url (type &optional member)

  (let ((url-base "http://spray.io/documentation/api/"))
    ;; broken, but i don't know why:
    ;; (message (ensime-make-scala-doc-url-helper url-base type member))
    ;; (ensime-make-scala-doc-url-helper url-base type member)))
    (concat url-base "#" (ensime-type-full-name type))))

(defun make-akka-doc-url (type &optional member)
  (ensime-make-scala-doc-url-helper "http://doc.akka.io/api/akka/snapshot/" type member))

(defun ensime-sbt-do-test ()
  (interactive)
  (ensime-sbt-switch)
  (ensime-sbt-action "test"))

(when (require 'scala-mode2 nil 'noerror)
    (add-hook 'scala-mode-hook 'phil/scala-mode-hook))

(when (require 'sbt-mode nil 'noerror)
    (add-to-list 'grep-find-ignored-directories "target")
    (add-hook 'sbt-mode-hook 'phil/sbt-mode-hook))

(when (require 'ensime nil 'noerror)
  (add-to-list 'ensime-doc-lookup-map '("^spray\\." . make-spray-doc-url))
  (add-to-list 'ensime-doc-lookup-map '("^akka\\." . make-akka-doc-url)))

(defun phil/sbt-mode-hook ()
  (interactive)
  ;; compilation-skip-threshold tells the compilation minor-mode
  ;; which type of compiler output can be skipped. 1 = skip info
  ;; 2 = skip info and warnings.
  (setq compilation-skip-threshold 1)

  ;; Bind C-a to 'comint-bol when in sbt-mode. This will move the
  ;; cursor to just after prompt.
  (local-set-key (kbd "C-a") 'comint-bol)

  ;; Bind M-RET to 'comint-accumulate. This will allow you to add
  ;; more than one line to scala console prompt before sending it
  ;; for interpretation. It will keep your command history cleaner.
  (local-set-key (kbd "M-RET") 'comint-accumulate)
  )

(defun phil/scala-mode-hook ()
  (interactive)
  (when (require 'sbt-mode nil 'noerror)
    ;; sbt-find-definitions is a command that tries to find (with grep)
    ;; the definition of the thing at point.
    ;; (local-set-key (kbd "M-.") 'sbt-find-definitions)

    ;; use sbt-run-previous-command to re-compile your code after changes
    ;; (local-set-key (kbd "C-x '") 'sbt-run-previous-command)

    ;; (local-set-key (kbd "M-RET") 'sbt-send-region)
    )

  (when (require 'ensime nil 'noerror)
    (ensime-scala-mode-hook)
    (define-key ensime-mode-map (kbd "C-c C-b t") 'ensime-sbt-do-test)
    ;; (define-key ensime-mode-map (kbd "M-.") nil)
    (define-key ensime-mode-map (kbd "M-.") 'ensime-edit-definition)

    ;; ensime-typecheck-curent-file has lots of false positives, so i disable it
    (remove-hook 'ensime-source-buffer-saved-hook
                 'ensime-typecheck-current-file)
    ))

(provide 'phil-scala)
