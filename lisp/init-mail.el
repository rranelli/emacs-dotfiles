;;; Package --- Summary
;;; Commentary:
;;; Code:
(autoload 'wl "wl" "Wanderlust" t)
(require 'wl)

;; I should check something like `http://www.emacswiki.org/emacs/hgw-init-wl.el'
;; In order to save your passwords, run `elmo-passwd-alist-save' interactively

(setq
 wl-summary-width 180 ;; do not limit summary width
 wl-message-buffer-prefetch-depth 0 ;; dont know what that is
 mime-edit-split-message nil ;; do not split large attachments in many messages
 wl-summary-move-direction-toggle nil ;; do not remember mark direction
 wl-summary-auto-refile-skip-marks '("$")
 wl-summary-always-sticky-folder-list '("inbox" "INBOX")
 wl-message-ignored-field-list '("^.*:") ;; Set header fields to show
 wl-message-visible-field-list
 '("^\\(To\\|Cc\\):"
   "^Subject:"
   "^\\(From\\|Reply-To\\):"
   "^\\(Posted\\|Date\\):"))

;; -- elmo configuration --
(setq elmo-imap4-default-server "imap.gmail.com"
      elmo-imap4-default-user "renanranelli@gmail.com"
      elmo-imap4-default-authenticate-type 'clear
      elmo-imap4-default-port '993
      elmo-imap4-default-stream-type 'ssl
      elmo-imap4-use-modified-utf7 t ;;for non ascii-characters in folder-names
      elmo-message-fetch-confirm nil) ;; do not ask confirmation for message fetching

;; Accounts
(setq
 wl-template-default-name "locaweb"

 wl-draft-config-matchone t
 wl-draft-reply-buffer-style 'full
 wl-dispose-folder-alist
 '((".*gmail" . remove)
   (".*locaweb" . remove))

 wl-template-alist
 '(("gmail"
    (wl-from . "Renan Ranelli <renanranelli@gmail.com>")
    ("From" . wl-from)
    (wl-fcc . "%[Gmail]/Sent")
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

 wl-default-spec "% "

 wl-draft-config-alist '(((string-match "locaweb" wl-draft-parent-folder)
			  (template . "locaweb"))
			 ((string-match "." wl-draft-parent-folder)
			  (template . "gmail"))))

(defun wl-fill-cleanup-fuckedup-message ()
  "Cleans up a fucked up message"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (forward-sentence)
    (read-only-mode -1)
    (fill-region (point) (point-max))
    (rr-strip-whitespace)
    (read-only-mode 1)))

;; -- auto refile configuration --
(let ((junk-folder "%Junk:\"renan.ranelli\"/clear@outlook.locaweb.com.br:993!")
      (prb-folder "%PRBs:\"renan.ranelli\"/clear@outlook.locaweb.com.br:993!")
      (archive-folder "%[Gmail]/All Mail:renanranelli/clear@imap.gmail.com:993!")
      (gmail-trash "%[Gmail]/Trash:renanranelli/clear@imap.gmail.com:993!"))
  (setq wl-refile-rule-alist
	`(
	  ;; rules for work mail
	  ("Subject" ("Confirmação de contratação de produtos" . ,junk-folder))
	  ("Subject" ("\\[PRB[0-9]+\\] - \\[HOSP\\]" . ,prb-folder))
	  ("Subject" ("\\[PRB[0-9]+\\] - " . ,junk-folder))
	  ("Subject" ("\\[Ger\\. Mudanças\\]" . ,junk-folder))
	  ("Subject" ("Build failed in Jenkins:". ,junk-folder))
	  ("From" ("info@locaweb.com.br" . ,junk-folder))
	  ("From" ("continuous.integration@locaweb.com.br" . ,junk-folder))
	  ("From" ("billing@softaculous.com" . ,junk-folder))
	  ("From" ("gitlab@code.locaweb.com.br" . ,junk-folder))
	  ("From" ("reporting_service@locaweb.com.br" . ,junk-folder))
	  ("From" ("no-reply@slack.com" . ,junk-folder))
	  ("From" ("reservadesala@locaweb.com.br" . ,junk-folder))
	  ("To" ("scrum-hospedagem@locaweb.com.br" . ,junk-folder))
	  ;; rules for personal mail
	  ("From" ("enews@automation.com" . ,gmail-trash))
	  ("From" (".*@newsletter.lojascolombo.com.br" . ,gmail-trash))
	  ("From" ("info@huinforma.com.br" . ,gmail-trash))
	  ("From" ("Control Global" . ,gmail-trash))
	  ("From" ("mail-comsol-br@comsol.com" . ,gmail-trash))
	  (("From" "Subject")
	   (".*@mail.goodreads.com" . ,gmail-trash)
	   ("newsletter" . ,gmail-trash))
	  (("Subject" "From")
	   ("Extrato Semanil" . ,gmail-trash)
	   ("Citibank.brazil@citi.com" . ,gmail-trash)
	   ))))

;; -- view images and stuff --
(require 'w3m)
(require 'mime-w3m)
(setq w3m-default-display-inline-images t)
(setq mime-w3m-safe-url-regexp ".*")
(add-hook 'wl-init-hook 'mime-w3m-insinuate)

;; -- bindings --
(define-key global-map (kbd "<f11>") 'wl)

(define-bindings w3m-minor-mode-map
  `(("C-m" . w3m-view-url-with-browse-url)
    ("RET" . w3m-view-url-with-browse-url)))

(define-bindings wl-summary-mode-map
  `(("RET" . wl-summary-jump-to-current-message)
    ("n" . next-line)
    ("p" . previous-line)
    ("O" . wl-summary-refile-prev-destination)
    ("q" . (lambda ()
	     (interactive)
	     (wl-summary-toggle-disp-msg 'off)))))

(add-hook 'mime-view-mode-hook
	  (lambda ()
	    (local-set-key (kbd "q") (lambda ()
				       (interactive)
				       (mime-preview-quit)
				       (wl-summary-toggle-disp-msg)))
	    (local-set-key (kbd "n") 'mime-preview-next-line-entity)
	    (local-set-key (kbd "p") 'mime-preview-previous-line-entity)
	    (local-set-key (kbd "f") 'wl-fill-cleanup-fuckedup-message)
	    (local-set-key (kbd "C-c m f") 'wl-fill-cleanup-fuckedup-message)))

(expose-bindings wl-summary-mode-map bindings-to-expose)
(expose-bindings wl-template-mode-map bindings-to-expose)

(defadvice wl-summary-write (after wl-summary-write-select-template activate)
  "Run wl-template-select when writing a new email"
  (wl-template-select "locaweb"))

(provide 'init-mail)
;;; init-mail.el ends here
