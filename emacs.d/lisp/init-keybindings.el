;;; package -- Summary
;;; Commentary:
;;; Code:

;; compilation
(global-set-key (kbd "C-c ,c") 'compile)

;; evaluation
(define-key emacs-lisp-mode-map (kbd "C-c C-r") 'eval-region)
(define-key emacs-lisp-mode-map (kbd "C-c C-e") 'esk-eval-and-replace)

;; movement and editing
(global-set-key (kbd "M-n") 'next5)
(global-set-key (kbd "M-p") 'prev5)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-q") 'comment-or-uncomment-region)
(global-set-key (kbd "C-a") 'move-smart-begining-of-line)
(global-set-key (kbd "C-h") 'backward-delete-char)
(global-set-key (kbd "M-d") 'kill-word)
(global-set-key (kbd "M-h") 'backward-kill-word)
(global-set-key (kbd "M-u") 'zap-to-char)

(global-set-key (kbd "C-;") 'ace-jump-word-mode)

(global-set-key (kbd "M-<left>") 'smartscan-symbol-go-backward)
(global-set-key (kbd "M-<right>") 'smartscan-symbol-go-forward)
(global-set-key (kbd "M-\"") 'smartscan-symbol-replace)

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
(global-set-key (kbd "C-c t") 'git-project-dirtree)
(global-set-key (kbd "C-c -") 'swap-buffers-in-windows)

;; indentation
(global-set-key (kbd "C-I") 'indent-region)
(global-set-key (kbd "C-c i") 'esk-indent-buffer)

(provide 'init-keybindings)
;;; init-keybindings ends here
