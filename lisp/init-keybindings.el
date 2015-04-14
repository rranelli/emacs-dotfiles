;;; init-keybindings.el -- Configures global keybindings and other keybinding-related stuff.
;;; Commentary:
;;; Code:

;; Keybindings macros
(defvar bindings-to-expose
  '("M-x"
    "C-c C-f"
    "C-M-f"
    "C-M-b"
    "C-h"
    "C-c o"
    "M-h"
    "M-k"
    "M-o"
    "M-1"
    "M-2"
    "M-3"
    "M-0"
    "M-i"
    "M-l")
  "Custom keybindings to expose on every mode.")

(defun expose-rr/default-bindings (mode-map)
  (expose-bindings mode-map bindings-to-expose))

(defmacro expose-global-keybinding (binding map)
  `(define-key ,map ,binding `,(lookup-key `,(current-global-map) ,binding)))

(defmacro expose-bindings (map bindings)
  `(dolist (bnd ,bindings)
     `,(expose-global-keybinding `,(kbd bnd) ,map)))

(defun define-bindings (keymap binding-alist)
  "Define keys for KEYMAP given a BINDING-ALIST."
  (dolist (p binding-alist)
    (let ((key (car p))
	  (command (cdr p)))
      (define-key keymap (kbd key) command))))

;; unset irritant suspend-frame
(global-unset-key (kbd "C-x z"))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-M-z"))
(global-unset-key (kbd "C-x C-z"))

;; join lines!
(global-set-key (kbd "C-\\") 'join-line)

;; compilation
(global-set-key (kbd "C-c ,c") 'compile)

;; train myself not to use return or backspace
(defun rr/disable-backspace-and-return ()
  (interactive)
  (define-key prog-mode-map (kbd "<return>") 'ignore)
  (define-key prog-mode-map (kbd "<backspace>") 'ignore))
(rr/disable-backspace-and-return)

(defun rr/enable-backspace-and-return ()
  (interactive)
  (define-key prog-mode-map (kbd "<return>") 'newline)
  (define-key prog-mode-map (kbd "<backspace>") 'backward-delete-char))

;; evaluation
(define-key emacs-lisp-mode-map (kbd "C-c C-r") 'eval-region)
(define-key emacs-lisp-mode-map (kbd "C-c C-e") 'eval-and-replace)
(define-key emacs-lisp-mode-map (kbd "C-c C-v") 'eval-buffer)

;; indentation
(global-set-key (kbd "C-I") 'indent-region)
(global-set-key (kbd "C-c i") (lambda () (interactive)
				(indent-region (point-min) (point-max))))

;; commands
(global-set-key (kbd "C-x m") 'execute-extended-command)

;; movement and editing
(global-set-key (kbd "M-n") (lambda () (interactive) (next-line 5)))
(global-set-key (kbd "M-p") (lambda () (interactive) (previous-line 5)))
(global-set-key (kbd "C-q") 'comment-or-uncomment-region)
(global-set-key (kbd "C-a") 'move-smart-beginning-of-line)
(global-set-key (kbd "C-h") 'backward-delete-char)
(global-set-key (kbd "M-d") 'kill-word)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "M-u") 'zap-to-char)
(global-set-key (kbd "C-M-SPC") 'er/expand-region)
(global-set-key (kbd "M-T") 'transpose-sexps)
(global-set-key (kbd "M-Q") 'quoted-insert)

(global-set-key (kbd "C-;") 'ace-jump-word-mode)
(global-set-key (kbd "C-:") 'ace-jump-char-mode)

(global-set-key (kbd "M-j") 'better-registers-jump-to-register)
(define-key better-registers-map (kbd "C-r (") 'rr/replace-wrapper-around-point)


;; window and buffer manipulation
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-i") 'other-frame)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-k") 'kill-buffer)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "C-x 2") 'vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'hsplit-last-buffer)
(global-set-key (kbd "C-c -") 'swap-buffers-in-windows)

(define-key ctl-x-map "n" 'narrow-or-widen-dwim)

;; other mode compatibilities
(expose-bindings dired-mode-map '("M-o"))
(expose-bindings yaml-mode-map '("C-m"))
(expose-bindings text-mode-map '("M-r"))

(expose-bindings better-registers-map
		 '("<f1>" "C-j" "C-x r" "C-x r"))

(expose-rr/default-bindings markdown-mode-map)
(expose-rr/default-bindings sh-mode-map)

(add-hook 'python-mode-hook
	  (lambda () (expose-rr/default-bindings python-mode-map)))
(add-hook 'sgml-mode-hook
	  (lambda () (expose-rr/default-bindings sgml-mode-map)))
(add-hook 'html-mode-hook
	  (lambda () (expose-rr/default-bindings html-mode-map)))
(add-hook 'nxml-mode-hook
	  (lambda () (expose-rr/default-bindings nxml-mode-map)))
(add-hook 'diff-mode-hook
	  (lambda () (expose-rr/default-bindings diff-mode-map)))

;; makefile
(add-hook 'makefile-mode-hook
	  (lambda ()
	    (define-key makefile-mode-map (kbd "TAB") 'self-insert-command)
	    (expose-rr/default-bindings makefile-mode-map)))

(provide 'init-keybindings)
;;; init-keybindings ends here
