;;; package -- Summary
;;; Commentary:
;;; Code:

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
(define-key prog-mode-map (kbd "<return>") 'ignore)
(define-key prog-mode-map (kbd "<backspace>") 'ignore)

;; evaluation
(define-key emacs-lisp-mode-map (kbd "C-c C-r") 'eval-region)
(define-key emacs-lisp-mode-map (kbd "C-c C-e") 'esk-eval-and-replace)
(define-key emacs-lisp-mode-map (kbd "C-c C-v") 'eval-buffer)

;; indentation
(global-set-key (kbd "C-I") 'indent-region)
(global-set-key (kbd "C-c i") 'esk-indent-buffer)

;; commands
(global-set-key (kbd "C-x C-m") 'shell)
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

(global-set-key (kbd "C-;") 'ace-jump-word-mode)
(global-set-key (kbd "M-m") 'new-line-below)

;; window and buffer manipulation
(global-set-key (kbd "C->") 'maximize-window-vertically)
(global-set-key (kbd "C-<") 'minimize-window-vertically)
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

;; other mode compatibilities
(define-key dired-mode-map (kbd "M-o") 'other-window)
(define-key yaml-mode-map (kbd "C-m") 'newline-and-indent)

(provide 'init-keybindings)
;;; init-keybindings ends here
