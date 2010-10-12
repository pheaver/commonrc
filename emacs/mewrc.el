;; mew mail per-account settings

(setq mew-imap-delete ())       ;; retain all msgs
(setq mew-imap-size 0)          ;; no size limit

;(setq mew-user "pweaver")
;(setq mew-name "Philip Weaver")
(setq mew-smtp-user "pweaver")

;; according to http://www.mew.org/beta/info/mew_90.html.en#SEC90
;(setq mew-smtp-port "submission") ;; The default is "smtp" (25)
;(setq mew-smtp-ssl t)
;(setq mew-smtp-ssl-port mew-smtp-port) ;; The default is 465

(setq mew-smtp-port 25)
(setq mew-smtp-ssl t)
(setq mew-smtp-ssl-port 25)
(setq mew-use-smtp-auth t)

(setq mew-imap-ssl t)

(setq mew-config-alist
 '((default
     (mailbox-type       imap)
     (proto              "%")
     (imap-server        "mail.signalicorp.com")
     (ssl-verify-level   0)
     (smtp-server        "smtp.signalicorp.com")
     (mail-domain        "signalicorp.com")
     (fcc                "%Sent")
     (imap-trash-folder  "%Trash")
     (inbox-folder       "%inbox")
     )
   )
 )

;     (signature-file  "~/.signature.mew")
;     (fcc  "%mail_imap/Sent")
;     (imap-friend-folder  "%mail_imap/from")
;     (imap-trash-folder  "%mail_imap/Trash")

;        ("galois"
;         ("mailbox-type" . imap)
;         ("proto" . "%")
;         ("imap-server" . "mail.galois.com")
;         ("imap-ssl" . t)
;         ("imap-ssl-port" . "993")
;         ("ssl-verify-level" . 0)
;         ("imap-delete" . nil)                ;; retain all msgs
;         ("imap-size" . 0)                    ;; no size limit
;         ("smtp-server"       . "mail.galois.com")
;         ("smtp-port"         . "25")
;         ("smtp-ssl"          . t)
;         ("mail-domain"       . "galois.com")
;         ("fcc"               . "%Sent")
;         ("imap-trash-folder" . "%Trash")
;         ("inbox-folder"      . "%inbox")
;         )
;        )
;      )

;; mew settings common to all machines

(setq mew-auto-get nil) ;; don't query imap whenever 'mew is run

;(setq mew-imap-header-only t)

;; setup read mail menu
(if (boundp 'read-mail-command)
    (setq read-mail-command 'mew))

;; C-xm for sending a message
(autoload 'mew-user-agent-compose "mew" nil t)

(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'mew-user-agent))

(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'mew-user-agent
      'mew-user-agent-compose
      'mew-draft-send-message
      'mew-draft-kill
      'mew-send-hook))

(setq mew-flowed-fold-length 80)

(add-hook 'mew-init-hook (lambda () (require 'mew-fancy-summary)))
(add-hook 'mew-draft-mode-hook (function (lambda () (auto-fill-mode 1))))
(add-hook 'mew-draft-mode-hook (function (lambda () (ispell-minor-mode 1))))

;(defvar mew-path (commonrc-dir "mew-6.1/"))
;(defun mew-bin (name) (interactive) (concat mew-path "bin/" name))

;; global settings
(setq mew-prog-ssl         "stunnel")
;(setq mew-prog-ssl         "/opt/local/bin/stunnel")
;(setq mew-prog-mewl        (mew-bin "mewl"))
;(setq mew-prog-mime-encode (mew-bin "mewencode"))
;(setq mew-prog-mime-decode (mew-bin "mewencode"))
;(setq mew-prog-8bit        (mew-bin "mewencode"))

(setq mew-use-cached-passwd t)
(setq mew-passwd-lifetime 24)  ;; password lifetime = 4 hours (24 units of 10 mins)
(setq mew-use-unread-mark t)
(setq mew-delete-unread-mark-by-mark nil)
(setq mew-use-full-window t)
(setq mew-summary-form-from-me-prefix nil)
(setq mew-summary-form-list
      '((t
     (type " " (5 date) "    " (40 from) "    " t (100 subj)))))

;; reply citation format
;(require 'supercite)
;(setq mew-cite-hook 'sc-cite-original)
(setq mew-summary-reply-with-citation-position 'end) ;; 'body to reply before
                                                     ;; 'end to reply after
(setq sc-citation-leader "")
(setq mew-cite-format "%s writes:\n\n")
(setq sc-preferred-header-style 5)
(setq sc-auto-fill-region-p nil) ;; t
(setq sc-confirm-always-p nil)
;(setq mew-addrbook-for-cite-label 'nickname)
;(setq mew-addrbook-for-cite-prefix 'nickname)

;; grep
(setq mew-prog-grep "grep") ;; `C-u ?'
(setq mew-prog-grep-opts '("-i" "-l" "-e")) ;; '("-l" "-e")
(setq mew-prog-vgrep "grep") ;; Virtual mode
(setq mew-prog-vgrep-opts '("-i" "-l" "-e")) ;; '("-l" "-e")

;; biff
(setq mew-use-biff t)
(setq mew-use-biff-bell t)
(setq mew-biff-interval 1) ;; 1 minute

(defun mew-biff-bark (n)
  (if (= n 0)
      (setq mew-biff-string nil)
    (if (and mew-use-biff-bell (eq mew-biff-string nil))
        (beep))
    (setq mew-biff-string (format "Mail(%d)" n))
    (notify mew-biff-string "")))
