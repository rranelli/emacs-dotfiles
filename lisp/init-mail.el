;;; Package --- Summary
;;; Commentary:
;;; Code:
(autoload 'wl "wl" "Wanderlust" t)
(require 'wl)

;; I should check something like `http://www.emacswiki.org/emacs/hgw-init-wl.el'

;; -- preferences --

;; In order to save your passwords, run
;; `elmo-passwd-alist-save' interactively
(setq
 ;; do not limit summary width
 wl-summary-width 180
 ;; dont know what that is
 wl-message-buffer-prefetch-depth 0
 ;; do not split large attachments in many messages
 mime-edit-sbplit-message nil
 ;; do not remember mark direction
 wl-summary-move-direction-toggle nil

 wl-summary-auto-refile-skip-marks '("!" "$")

 ;; Set header fields to show
 wl-message-ignored-field-list '("^.*:")
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

      ;;for non ascii-characters in folder-names
      elmo-imap4-use-modified-utf7 t)

;; Accounts
(setq
 wl-template-default-name "locaweb"

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

;; sorting backwards
(defun wl-summary-overview-entity-compare-by-rdate (x y)
  (not (wl-summary-overview-entity-compare-by-date x y)))
(add-to-list 'wl-summary-sort-specs 'rdate)

(defun wl-summary-sort-by-rdate ()
  (interactive)
  (wl-summary-rescan "rdate")
  (goto-char (point-min)))

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
      (prb-folder "%PRBs:\"renan.ranelli\"/clear@outlook.locaweb.com.br:993!"))
  (setq wl-refile-rule-alist
	;; (field (regexp . target) ...)
	`(("Subject" ("Confirmação de contratação de produtos" . ,junk-folder))
	  ("Subject" ("\\[PRB[0-9]+\\] - \\[HOSP\\]" . ,prb-folder))
	  ("Subject" ("\\[PRB[0-9]+\\] - " . ,junk-folder))
	  ("Subject" ("\\[Ger\\. Mudanças\\]" . ,prb-folder))
	  ("From" ("info@locaweb.com.br" . ,junk-folder))
	  ("From" ("continuous.integration@locaweb.com.br" . ,junk-folder))
	  ("From" ("billing@softaculous.com" . ,junk-folder))
	  ("From" ("gitlab@code.locaweb.com.br" . ,junk-folder))
	  ("From" ("reporting_service@locaweb.com.br" . ,junk-folder))
	  ("From" ("no-reply@slack.com" . ,junk-folder))
	  ("From" ("reservadesala@locaweb.com.br" . ,junk-folder))
	  ("To" ("scrum-hospedagem@locaweb.com.br" . ,junk-folder)))))

;; -- bindings --
(define-key global-map (kbd "<f11>") 'wl)

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

(define-key wl-summary-mode-map (kbd "RET") 'wl-summary-jump-to-current-message)
(define-key wl-summary-mode-map (kbd "O") 'wl-summary-refile-prev-destination)

(expose-bindings wl-summary-mode-map bindings-to-expose)
(expose-bindings wl-template-mode-map bindings-to-expose)

(provide 'init-mail)
;;; init-mail.el ends here
