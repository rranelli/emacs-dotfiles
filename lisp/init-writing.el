;;; package -- Summary
;;; Commentary:
;;; Code:
(require 'org-jekyll-mode)

(add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))

;; langtool
(require 'langtool)
(setq langtool-language-tool-jar "~/.langtool/languagetool-commandline.jar")
(setq langtool-disabled-rules
      '("WHITESPACE_RULE"
	"EN_UNPAIRED_BRACKETS"))

;; -- hooks --
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

(define-bindings global-map
  '(("C-s-r" . synosaurus-choose-and-replace)
    ("C-s-l" . synosaurus-lookup)))

(define-bindings org-mode-map
  '(("C-s-c" . langtool-check)
    ("C-s-v" . langtool-check-done)
    ("C-s-k" . langtool-correct-buffer)
    ("C-s-x" . langtool-show-message-at-point)
    ("C-s-n" . langtool-goto-next-error)
    ("C-s-p" . langtool-goto-previous-error)))

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
