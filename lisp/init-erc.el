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

(setq erc-track-exclude-types '("MODE" "AWAY" "JOIN" "PART")
      erc-track-use-faces t
      erc-hide-list '("JOIN" "PART" "QUIT" "AWAY")
      erc-max-buffer-size 20000
      erc-auto-query 'bury
      erc-query-display 'buffer
      erc-query-display 'buffer
      erc-log-insert-log-on-open t
      erc-log-channels t
      erc-log-channels-directory "~/.irclogs/"
      erc-save-buffer-on-part t
      erc-kill-buffer-on-part t)

(setq erc-autojoin-channels-alist
      '(("freenode" "#haskell" "haskell-emacs" "#elixir-lang" "#emacs-elixir" "#emacs" "#ruby-lang")))

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
  (erc-tls :server "locaweb.irc.slack.com"
           :port 6667
           :nick "milhouse"
           :full-name "milhouse"
           :password rr/slack-passwd))

(defun rr/join-irc ()
  "Connect to all irc servers"
  (rr/irc-freenode)
  (rr/irc-locaweb-slack))

(defun rr/clear-erc-unseen ()
  "Clears irc modified channels notification."
  (interactive)
  (setq erc-modified-channels-alist nil)
  (erc-modified-channels-display))

;;
;;; keybindings
;;
(global-set-key (kbd "C-c C-u") 'rr/clear-erc-unseen)

(define-key erc-mode-map (kbd "C-x C-s") 'ignore)
(define-key erc-mode-map (kbd "C-c C-u") 'rr/clear-erc-unseen)
(define-key erc-mode-map (kbd "C-l") '(lambda () (interactive) (erc-cmd-CLEAR)))

(provide 'init-erc)
;;; init-erc.el ends here
