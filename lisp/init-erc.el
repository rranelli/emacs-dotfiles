;;; init-irc.el --- Sets up `IRC' with `erc'.
;;; Commentary:
;;; Code
(require 'erc)
(require 'erc-join)
(require 'erc-match)
(require 'erc-track)
(require 'erc-fill)
(require 'erc-ring)
(require 'erc-netsplit)

(require 'erc-hl-nicks)
(add-to-list 'erc-modules 'hl-nicks)

(require 'erc-image)
(add-to-list 'erc-modules 'image)

;; vendorized libs
(require 'erc-nick-notify)
(eval-after-load 'erc '(erc-nick-notify-mode t))

;;
;;; update them modules !
;;
(erc-update-modules)

(erc-hl-nicks-mode t)
(erc-notifications-mode t)
(erc-match-mode t)
(erc-autojoin-mode t)
(erc-track-mode t)
(erc-fill-mode t)
(erc-ring-mode t)
(erc-netsplit-mode t)
(erc-timestamp-mode t)
(erc-button-mode (- 1))
(erc-spelling-mode t)
(erc-log-mode)

(setq erc-spelling-dictionaries '(("locaweb.irc.slack.com:6667" "brasileiro")))

(setq
 erc-track-exclude-types '("MODE" "AWAY" "JOIN" "PART")
 erc-track-use-faces t
 erc-hide-list '("JOIN" "PART" "QUIT" "NICK" "AWAY" "MODE")
 erc-max-buffer-size 20000
 erc-auto-query 'bury
 erc-query-display 'buffer
 erc-query-display 'buffer
 erc-log-channels t
 erc-log-channels-directory "~/.irclogs/"
 erc-log-insert-log-on-open nil
 erc-log-write-after-insert t
 erc-log-write-after-send t
 erc-kill-buffer-on-part t
 erc-keywords '("deploy" "Failure" "@channel"))

(setq erc-autojoin-channels-alist
      '(("freenode" "#haskell" "haskell-emacs" "#elixir-lang" "#emacs-elixir")))

;;; view logs
(require 'erc-view-log)

(defun erc-get-color-for-nick (nick)
  "Gets a color for NICK. If NICK is specified in erc-nick-color-alist, use it, else hash the nick and get a color from that"
  (or (cdr (assoc nick erc-nick-color-alist))
      (nth
       (mod (string-to-number
	     (substring (md5 nick) 0 6) 16)
	    (length erc-colors-list))
       erc-colors-list)))

(defun erc-get-face-for-nick (nick)
  "Returns the face for the given nick."
  `((:foreground ,(erc-get-color-for-nick nick))
    (:weight bold)))

(setq erc-view-log-nickname-face-function 'erc-get-face-for-nick)
(setq erc-view-log-my-nickname-match '("milhouse" "milhouse`")) ;set this one in your .priv_emacs with your other nicks if needed
(add-to-list 'auto-mode-alist '("\\.irclogs/.*\\.txt" . erc-view-log-mode))

(defun rr/erc-browse-log ()
  (interactive)
  (find-file (erc-current-logfile))
  (end-of-buffer))

;;
;;; custom functions
;;
(defun rr/irc-freenode ()
  "Connect to freenode IRC."
  (interactive)
  (erc-tls :server "irc.freenode.net"
           :port 6697
           :nick "milhouse`"
           :full-name "renanranelli@gmail.com"
           :password rr/freenode-passwd))

(defun rr/irc-locaweb-slack ()
  "Connect to locaweb's Slack via IRC."
  (interactive)
  (add-to-list 'erc-networks-alist '(Locaweb . "locaweb.irc.slack.com:6667"))
  (erc-tls :server "locaweb.irc.slack.com"
           :port 6667
           :nick "milhouse"
           :full-name "milhouse"
           :password rr/slack-passwd))

(defun rr/join-irc ()
  "Connect to all irc servers"
  (interactive)
  (rr/irc-freenode)
  (rr/irc-locaweb-slack))

(defun rr/clear-erc-unseen ()
  "Clears irc modified channels notification."
  (interactive)
  (setq erc-modified-channels-alist nil)
  (erc-modified-channels-display))

;; this is a bugfix
;; source: http://www.emacswiki.org/emacs/ErcLogging
(defun erc-log-all-but-server-buffers (buffer)
  (with-current-buffer buffer
    (not (erc-server-buffer-p))))

(add-hook 'erc-text-matched-hook 'rr/erc-kw-notify)
(defun rr/erc-kw-notify (match-type nick message)
  "notify when text is matched"
  (when (and (eq match-type 'keyword)
             ;; I don't want to see anything from the erc server
             (null (string-match "^[sS]erver" nick))
             ;; or bots
             (null (string-match "\\(bot\\|serv\\)!" nick)))
    (notifications-notify
     :title nick
     :body message
     :urgency 'normal)))
;;
;;; keybindings
;;
(global-set-key (kbd "C-c e u") 'rr/clear-erc-unseen)

(define-key erc-mode-map (kbd "C-x C-s") 'erc-save-buffer-in-logs)
(define-key erc-mode-map (kbd "C-c C-l") 'rr/erc-browse-log)
(define-key erc-mode-map (kbd "C-l") '(lambda () (interactive) (erc-cmd-CLEAR)))

(provide 'init-erc)
;;; init-erc.el ends here
