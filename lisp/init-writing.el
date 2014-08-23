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
      (ispell-change-dictionary lang))))

(defun org-jekyll-publish-org-to-jekyll ()
  "Renames draft prepending date, export to markdown and promote it to Jekyll's post folder."
  (interactive)
  (org-jekyll-prepend-date-to-file-name)
  (org-jekyll-export-to-jekyll)
  (org-jekyll-promote-draft-to-post))

(defun org-jekyll-export-to-jekyll ()
  (let* ((post-file-org (buffer-file-name))
         (post-file-md (replace-regexp-in-string "\\.org" ".md" post-file-org))
         (target-file-md (replace-regexp-in-string "/_org" "" post-file-md)))
    (when (string= (file-name-extension post-file-org) "org")
      (org-md-export-to-markdown)
      (rename-file post-file-md target-file-md t))))

(defun org-jekyll-prepend-date-to-file-name ()
  (let* ((org-jekyll-file-name (buffer-file-name))
         (org-jekyll-is-org-file-p (string= (file-name-extension org-jekyll-file-name) "org"))
         (org-jekyll-is-date-prepended nil)

         (org-jekyll-file-name-nondirectory (file-name-nondirectory org-jekyll-file-name))
         (org-jekyll-file-directory (file-name-directory org-jekyll-file-name))

         (current-date (format-time-string "%d-%m-%Y"))
         (org-jekyll-file-name-new (concat
                                    org-jekyll-file-directory
                                    "/"
                                    current-date
                                    "-"
                                    org-jekyll-file-name-nondirectory)))
    (when org-jekyll-is-org-file-p
      (set-visited-file-name org-jekyll-file-name-new t t)
      (rename-file org-jekyll-file-name org-jekyll-file-name-new t))))

(defun org-jekyll-promote-draft-to-post ()
  (let* ((org-jekyll-file-name (buffer-file-name))
         (org-jekyll-is-org-file-p (string= (file-name-extension org-jekyll-file-name) "org"))
         (is-org-jekyll-draft-file-p (string-match "/_drafts/" org-jekyll-file-name))

         (md-file-name (replace-regexp-in-string "/_drafts" "" org-jekyll-file-name))
         (md-file-exist-p (file-exists-p md-file-name))

         (org-jekyll-file-name-new (replace-regexp-in-string "/_drafts" "/_posts" org-jekyll-file-name))
         (md-file-name-new (replace-regexp-in-string "/_drafts" "/_posts" md-file-name)))
    (when (and
           org-jekyll-is-org-file-p
           is-org-jekyll-draft-file-p)
      (set-visited-file-name org-jekyll-file-name-new t t)
      (rename-file org-jekyll-file-name org-jekyll-file-name-new t))))

;; -- hooks --
;; nested hooks are amazing!
;; ref: (http://stackoverflow.com/questions/6138029/how-to-add-a-hook-to-only-run-in-a-particular-mode)
(add-hook 'markdown-mode-hook 'select-proper-dictionary-language)
(add-hook 'markdown-mode-hook
          (lambda () (add-hook 'after-save-hook 'select-proper-dictionary-language nil 'make-it-local)))

(add-hook 'org-jekyll-mode-hook 'select-proper-dictionary-language)
(add-hook 'org-jekyll-mode-hook
          (lambda () (add-hook 'after-save-hook 'select-proper-dictionary-language nil 'make-it-local)))

;; -- keybindings --
(define-key text-mode-map (kbd "C-c w p") 'org-jekyll-publish-org-to-jekyll)
(define-key text-mode-map (kbd "C-c w s c") 'flyspell-buffer)
(define-key text-mode-map (kbd "C-c w s t") 'flyspell-mode)

(provide 'init-writing)
;;; init-markdown.el ends here
