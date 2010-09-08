;; ----------------------------------------
;; my erc settings

(autoload 'erc "erc" "erc mode" t)

(setq erc-autojoin-channels-alist
      '(("docserver" "#galois")
        ("docserver.galois.com" "#galois")))

;(defun erc-osd-notice (str msg buffer sender)
;  (osd-echo (concat str "; " msg "; " buffer "; " sender))
;  t)

;(setq erc-auto-query 'window-noselect)

(setq erc-notify t)

(defun erc-docserver () (interactive)
  (erc :server "docserver.galois.com" :port 6667 :nick "phil"))

;(defun joel-erc-notify (proc parsed) ())
(defun joel-erc-notify (proc parsed)
  (let* ((nick (car (erc-parse-user (erc-response.sender parsed))))
         (target (car (erc-response.command-args parsed)))
         (msg (erc-response.contents parsed))
         (query (if (not erc-query-on-unjoined-chan-privmsg)
                     nick
                   (if (erc-current-nick-p target)
                       nick
                     target))))
    (and (not (erc-ignored-user-p (erc-response.sender parsed)))
         (not (eq (erc-get-buffer query proc) (current-buffer)))
         ;; this portion of the predicate filters out channel activity and
         ;; restricts to just private messages
         (or erc-query-on-unjoined-chan-privmsg
             (string= target (erc-current-nick)))
         (not (erc-is-message-ctcp-and-not-action-p msg))
         (if erc-notify (notify nick msg))
         nil)))

(add-hook 'erc-after-connect
 (lambda (server nick)
   (add-hook 'erc-server-PRIVMSG-functions 'joel-erc-notify)))

;(add-hook 'erc-server-NOTICE-hook 'erc-auto-query)))

;(add-hook 'erc-server-PRIVMSG-functions 'erc-auto-query)

;; ----------------------------------------

(provide 'phil-erc)
