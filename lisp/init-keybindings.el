;;; init-keybindings.el -- Configures global keybindings and other keybinding-related stuff.
;;; Commentary:
;;; Code:

;; Keybindings macros
(defvar rr/default-bindings-to-expose
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
    "M-4"
    "M-5"
    "M-0"
    "M-i"
    "M-l")
  "Custom keybindings to expose on every mode.")

(defun rr/expose-default-bindings (mode-map)
  "Expose `rr/default-bindings-to-expose' in MODE-MAP."
  (rr/expose-bindings mode-map rr/default-bindings-to-expose))

(defmacro rr/expose-global-binding (binding map)
  "Overwrite the BINDING in MAP with the command defined in `current-global-map' for BINDING."
  `(define-key ,map ,binding `,(lookup-key `,(current-global-map) ,binding)))

(defmacro rr/expose-bindings (map bindings)
  "Expose in MAP all BINDINGS from `current-global-map'."
  `(dolist (bnd ,bindings)
     `,(rr/expose-global-binding `,(kbd bnd) ,map)))

(defun rr/define-bindings (keymap binding-alist)
  "Define keys for KEYMAP given a BINDING-ALIST."
  (dolist (p binding-alist)
    (let ((key (car p))
	  (command (cdr p)))
      (define-key keymap (kbd key) command))))

(defmacro rr/expose-default-bindings-with-hook (mode-namez)
  "Expose default keybidings using hook for MODE-NAME."
  `(add-hook (rr/format-symbol "%s-hook" ,mode-namez)
             (lambda ()
               (->> ,mode-namez
                    (rr/format-symbol "%s-map")
                    (symbol-value)
                    (rr/expose-default-bindings)))))

;; Kinesis friendly help command
(global-set-key (kbd "C-S-h") 'help)

;;
;;; unset annoying suspend-frame
;;
(global-unset-key (kbd "C-x z"))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-M-z"))
(global-unset-key (kbd "C-x C-z"))

(global-set-key [remap save-buffers-kill-terminal] 'rr/ask-before-killing-frame)

;; evaluation
(define-key emacs-lisp-mode-map (kbd "C-c C-r") 'eval-region)
(define-key emacs-lisp-mode-map (kbd "C-c C-e") 'rr/eval-and-replace)
(define-key emacs-lisp-mode-map (kbd "C-c C-v") 'eval-buffer)

(global-set-key (kbd "C-c ,c") 'compile)
(global-set-key (kbd "C-c !!") 'flycheck-mode)

;; indentation
(global-set-key (kbd "C-c i")
                (lambda () (interactive)
                  (indent-region (point-min) (point-max))))

;; movement and editing
(global-set-key (kbd "C-q") 'comment-or-uncomment-region)
(global-set-key (kbd "C-a") 'rr/move-smart-beginning-of-line)
(global-set-key (kbd "C-h") 'backward-delete-char)
(global-set-key (kbd "M-d") 'rr/kill-word)
(global-set-key (kbd "M-h") 'rr/backward-kill-word)
(global-set-key (kbd "C-S-y") 'helm-show-kill-ring)
(global-set-key (kbd "M-u") 'zap-to-char)
(global-set-key (kbd "C-M-SPC") 'er/expand-region)
(global-set-key (kbd "M-u") 'er/expand-region)
(global-set-key (kbd "M-T") 'transpose-sexps)
(global-set-key (kbd "M-Q") 'quoted-insert)
(global-set-key (kbd "C-\\") 'join-line)

(global-set-key (kbd "C-x r <RET>") 'iedit-rectangle-mode)
(global-set-key (kbd "C-;") 'iedit-mode)

;; window and buffer manipulation
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-i") 'other-frame)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-k") 'kill-buffer)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-vertically)
(global-set-key (kbd "M-3") 'split-window-horizontally)
(global-set-key (kbd "M-4") 'delete-other-windows)
(global-set-key (kbd "M-5") 'delete-window)

(define-key ctl-x-map "2" 'vsplit-last-buffer)
(define-key ctl-x-map "3" 'hsplit-last-buffer)
(define-key ctl-x-map "-" 'swap-buffers-in-windows)
(define-key ctl-x-map "n" 'narrow-or-widen-dwim)

;;
;;; Customization compatibility with other modes
;;
(rr/expose-bindings dired-mode-map '("M-o"))
(rr/expose-bindings yaml-mode-map '("C-m"))
(rr/expose-bindings text-mode-map '("M-r"))

(rr/expose-default-bindings markdown-mode-map)
(rr/expose-default-bindings sh-mode-map)
(rr/expose-default-bindings iedit-mode-keymap)
(add-hook 'shell-mode-hook
          (lambda () (rr/expose-default-bindings shell-mode-map)))
(add-hook 'sh-mode-hook
          (lambda () (rr/expose-default-bindings sh-mode-map)))

(rr/expose-default-bindings-with-hook python-mode)
(rr/expose-default-bindings-with-hook sgml-mode)
(rr/expose-default-bindings-with-hook html-mode)
(rr/expose-default-bindings-with-hook nxml-mode)
(rr/expose-default-bindings-with-hook diff-mode)
(rr/expose-default-bindings-with-hook c-mode)
(rr/expose-default-bindings-with-hook makefile-mode)

(add-hook 'makefile-mode-hook
          (lambda ()
            (define-key makefile-mode-map (kbd "TAB") 'self-insert-command)))

(provide 'init-keybindings)
;;; init-keybindings ends here
