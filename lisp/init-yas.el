;;; init-yas.el -- Configures `yasnippet-mode' preferences.
;;; Commentary:
;;; Code:
(require 'yasnippet)
(yas-global-mode 1)

(defcustom rr/yas-snippet-dirs
  (expand-file-name "snippets" user-emacs-directory)
  "Directory to load yasnippet's snippet files."
  :group 'init-yas)
(setq yas-snippet-dirs rr/yas-snippet-dirs)

(setq yas-prompt-functions (delete 'yas-x-prompt yas-prompt-functions))

;; -- keybindings --
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<tab>") nil)

(define-key yas-minor-mode-map (kbd "M-y") 'yas-expand)

(define-key yas-minor-mode-map (kbd "C-c y n") 'yas-new-snippet)
(define-key yas-minor-mode-map (kbd "C-c y f") 'yas-visit-snippet-file)
(define-key yas-minor-mode-map (kbd "C-c y i") 'company-yasnippet)

;; -- hooks --
(add-hook 'prog-mode-hook
          '(lambda () (yas-minor-mode)))

(provide 'init-yas)
;;; init-yas.el ends here
