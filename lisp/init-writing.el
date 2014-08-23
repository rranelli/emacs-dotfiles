;;; package -- Summary
;;; Commentary:
;;; Code:
(require 'markdown-mode)

(add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))

(defun select-proper-dictionary-language ()
  "Change Ispell dictionary if a tag language: LANGUAGE is found in file."
  (let* ((lang-regexp "-+\nlanguage: ?\\(\\w+\\)\\(?:\n.*\\)*-+\n")
         (text (buffer-string))
         (lang (progn
                 (string-match lang-regexp text)
                 (match-string 1 text))))
    (when lang
      (progn
        (ispell-change-dictionary lang)
        (flyspell-buffer)))))

(defun org-publish-org-as-markdown ()
  (interactive)
  (let* ((post-file-org (buffer-file-name))
         (post-file-md (replace-regexp-in-string "\\.org" ".md" post-file-org))
         (target-file-md (replace-regexp-in-string "/_org" "" post-file-md)))
    (when (string= (file-name-extension post-file-org) "org")
      (org-md-export-to-markdown)
      (rename-file post-file-md target-file-md t))))

(defun org-prepend)

;; -- hooks --
;; nested hooks are amazing!
;; ref: (http://stackoverflow.com/questions/6138029/how-to-add-a-hook-to-only-run-in-a-particular-mode)
(add-hook 'markdown-mode-hook 'select-proper-dictionary-language)
(add-hook 'markdown-mode-hook
          (lambda () (add-hook 'after-save-hook 'select-proper-dictionary-language nil 'make-it-local)))
(add-hook 'org-mode-hook 'select-proper-dictionary-language)
(add-hook 'org-mode-hook
          (lambda () (add-hook 'after-save-hook 'select-proper-dictionary-language nil 'make-it-local)))

;; -- keybindings --
(define-key text-mode-map (kbd "C-c o p") 'org-publish-org-as-markdown)

(provide 'init-writing)
;;; init-markdown.el ends here
