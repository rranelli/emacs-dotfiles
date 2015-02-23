;;; package -- Summary
;;; Commentary:
;;; Code:
(require 'org-jekyll-mode)

(add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))

;; langtool
(require 'langtool)
(setq langtool-language-tool-jar "~/.langtool/languagetool-commandline.jar")

;; -- hooks --
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

(let ((map org-mode-map))
  (define-key map (kbd "C-c t c") 'langtool-check)
  (define-key map (kbd "C-c t C") 'langtool-check-done)
  (define-key map (kbd "C-c t l") 'langtool-switch-default-language)
  (define-key map (kbd "C-c t p") 'langtool-show-message-at-point)
  (define-key map (kbd "C-c t b") 'langtool-correct-buffer))

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

(add-hook 'markdown-mode-hook #'(lambda ()
				  (expose-bindings markdown-mode-map bindings-to-expose)
				  (expose-bindings markdown-mode-map '("C-:" "C-c C-f" "C-;"))))
(expose-bindings flyspell-mode-map '("C-:" "C-;" "C-c C-f"))

(provide 'init-writing)
;;; init-writing.el ends here
