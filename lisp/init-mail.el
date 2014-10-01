;;; Package --- Summary
;;; Commentary:
;;; Code:
(autoload 'wl "wl" "Wanderlust" t)

(setq elmo-imap4-default-server "imap.gmail.com"
      elmo-imap4-default-user "renanranelli@gmail.com"
      elmo-imap4-default-authenticate-type 'clear
      elmo-imap4-default-port '993
      elmo-imap4-default-stream-type 'ssl

      ;;for non ascii-characters in folder-names
      elmo-imap4-use-modified-utf7 t)

(setq wl-batch-prefetch-folder-list "%INBOX")

;; Accounts
(setq
 wl-template-default-name "gmail"

 wl-draft-config-matchone t
 wl-draft-reply-buffer-style 'full
 wl-dispose-folder-alist
 '((".*gmail" . "%[Gmail]/Trash:\"renanranelli@gmail.com\"/clear@imap.gmail.com:993!")
   (".*locaweb" . remove))

 wl-template-alist
 '(("gmail"
    (wl-from . "Renan Ranelli <renanranelli@gmail.com>")
    ("From" . wl-from)
    (wl-fcc . "%[Gmail]/Sent")
    (wl-trash-folder . "%[Gmail]/Trash:\"renanranelli@gmail.com\"/clear@imap.gmail.com:993!")
    (wl-smtp-connection-type . 'starttls)
    (wl-smtp-posting-port . 587)
    (wl-smtp-authenticate-type . "plain")
    (wl-local-domain . "gmail.com")
    (wl-smtp-posting-user . "renanranelli@gmail.com")
    (wl-smtp-posting-server . "smtp.gmail.com")
    (wl-message-id-domain . "smtp.gmail.com"))
   ("locaweb"
    (wl-from . "Renan Ranelli <renan.ranelli@locaweb.com.br>")
    ("From" . wl-from)
    ("Fcc" . "%sent items:\"renan.ranelli\"/clear@outlook.locaweb.com.br:993!")
    (wl-smtp-connection-type . 'starttls)
    (wl-smtp-posting-port . 587)
    (wl-smtp-authenticate-type . "login")
    (wl-smtp-posting-user . "renan.ranelli")
    (wl-local-domain . "locaweb.com.br")
    (wl-smtp-posting-server . "outlook.locaweb.com.br")
    (wl-message-id-domain . "outlook.locaweb.com.br")))

 wl-draft-config-alist '(((string-match "locaweb" wl-draft-parent-folder)
			  (template . "locaweb"))
			 ((string-match "." wl-draft-parent-folder)
			  (template . "gmail"))))

;; Set header fields to show
(setq
 wl-message-ignored-field-list '("^.*:")
 wl-message-visible-field-list
 '("^\\(To\\|Cc\\):"
   "^Subject:"
   "^\\(From\\|Reply-To\\):"
   "^Organization:"
   "^Message-Id:"
   "^\\(Posted\\|Date\\):"))

(setq
 ;; do not limit summary width
 wl-summary-width 180
 ;; dont know what that is
 wl-message-buffer-prefetch-depth 0)


(provide 'init-mail)
;;; init-mail.el ends here
