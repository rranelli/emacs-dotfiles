;;; init-writting.el -- Configures features that enhance one's prose writing in Emacs.
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

(add-hook 'org-jekyll-mode-hook 'org-jekyll-set-compile-on-save)

(rr/define-bindings global-map
  '(("C-s-o" . synosaurus-choose-and-replace)
    ("C-s-l" . synosaurus-lookup)))

(rr/define-bindings org-mode-map
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
	    (rr/expose-bindings flyspell-mode-map '("C-;"))))

(add-hook 'markdown-mode-hook #'(lambda ()
				  (rr/expose-bindings markdown-mode-map rr/default-bindings-to-expose)
				  (rr/expose-bindings markdown-mode-map '("C-:" "C-c C-f" "C-;"))))
(rr/expose-bindings flyspell-mode-map '("C-:" "C-;" "C-c C-f"))

(provide 'init-writing)
;;; init-writing.el ends here
