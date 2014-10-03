;;; package -- Summary
;;; Commentary:
;;; Code:
(require 'org-jekyll-mode)

(add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))

;; -- hooks --p
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

;; nested hooks are amazing!
;; ref: (http://stackoverflow.com/questions/6138029/how-to-add-a-hook-to-only-run-in-a-particular-mode)
(add-hook 'markdown-mode-hook 'org-jekyll-select-proper-dictionary-language)
(add-hook 'markdown-mode-hook
          (lambda () (add-hook 'after-save-hook 'org-jekyll-select-proper-dictionary-language nil 'make-it-local)))

(add-hook 'org-mode-hook 'org-jekyll-select-proper-dictionary-language)
(add-hook 'org-mode-hook
          (lambda () (add-hook 'after-save-hook 'org-jekyll-select-proper-dictionary-language nil 'make-it-local)))

(add-hook 'flyspell-mode-hook
	  (lambda ()
	    (expose-bindings flyspell-mode-map '("C-;"))))

(expose-bindings markdown-mode-map '("C-:" "C-c C-f" "C-;"))
(expose-bindings flyspell-mode-map '("C-:" "C-;" "C-c C-f"))

(provide 'init-writing)
;;; init-markdown.el ends here
