;;; package -- Summary
;;; Commentary:
;;; Code:
(require 'yasnippet)
(yas-global-mode 1)

(diminish 'yas-minor-mode "")

(defcustom rr-yas-snippet-dirs
  (expand-file-name "snippets" user-emacs-directory)
  "Directory to load yasnippet's snippet files."
  :group 'init-yas)
(setq yas-snippet-dirs rr-yas-snippet-dirs)

(setq yas-prompt-functions (delete 'yas-x-prompt yas-prompt-functions))

;; add to autocomplete sources
(when (and (boundp 'ac-sources)
	   (file-exists-p yas-snippet-dirs))
  (add-to-list 'ac-sources ac-source-yasnippet))

;; -- keybindings --
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<tab>") nil)

(global-set-key (kbd "M-y") 'yas-expand)

(define-key yas-minor-mode-map (kbd "C-c y n") 'yas-new-snippet)
(define-key yas-minor-mode-map (kbd "C-c y f") 'yas-visit-snippet-file)
(define-key yas-minor-mode-map (kbd "C-c y i") 'yas-insert-snippet)

;; -- hooks --
(add-hook 'prog-mode-hook
          '(lambda () (yas-minor-mode)))

(provide 'init-yas)
;;; init-yas.el ends here
