;;; package -- Summary
;;; Commentary:
;;; Code:
(require 'markdown-mode)
(require 'ox-gfm)
(add-to-list 'auto-mode-alist '("\\.md$" . gfm-mode))

(defun select-proper-dictionary-language ()
  "Change Ispell dictionary if a tag language: LANGUAGE is found in file."
  (let* ((lang-regexp "-+\nlanguage: ?\\(\\w+\\)\\(?:\n.*\\)*-+\n")
         (text (buffer-string))
	 (lang-p (string-match lang-regexp text))
	 (lang (match-string-no-properties 1 text)))
    (when lang-p
      (ispell-change-dictionary lang))))

(defun org-jekyll-publish-org-to-jekyll ()
  "Renames draft prepending date, export to markdown and promote it to Jekyll's post folder."
  (interactive)
  (org-jekyll-prepend-date-to-file-name)
  (org-jekyll-export-to-jekyll)
  (org-jekyll-promote-draft-to-post))

(defun org-jekyll-export-to-jekyll ()
  (interactive)
  (let* ((post-file-org (buffer-file-name))
         (post-file-md (replace-regexp-in-string "\\.org" ".md" post-file-org))
         (target-file-md (replace-regexp-in-string "/org" "" post-file-md)))
    (when (string= (file-name-extension post-file-org) "org")
      (org-gfm-export-to-markdown)
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
         (is-org-jekyll-file-p (string= (file-name-extension org-jekyll-file-name) "org"))
         (is-org-jekyll-draft-file-p (string-match "/_drafts" org-jekyll-file-name))

         (md-file-name (replace-regexp-in-string "\\.org" ".md" org-jekyll-file-name))
         (md-target-file-name (replace-regexp-in-string "/org" "" md-file-name))


         (org-jekyll-file-name-new (replace-regexp-in-string "/_drafts" "/_posts" org-jekyll-file-name))
         (md-file-name-new (replace-regexp-in-string "/_drafts" "/_posts" md-target-file-name)))
    (when (and
           is-org-jekyll-file-p
           is-org-jekyll-draft-file-p)
      (set-visited-file-name org-jekyll-file-name-new t t)
      (rename-file org-jekyll-file-name org-jekyll-file-name-new t)

      (rename-file md-target-file-name md-file-name-new))))

(defun org-jekyll-new-draft ()
  "Open new draft file."
  (interactive)
  (let* ((post-title (read-string "post title: "))
         (post-title-dashed (replace-regexp-in-string " " "-" post-title))
         (drafts-template-path "org/_drafts/%s.org"))
    (find-file (concat (ffip-project-root) (format drafts-template-path post-title-dashed)))

    (insert "blog")
    (yas-expand)
    (insert post-title)))

(defun org-jekyll-fill ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (re-search-forward "END_HTML" nil t)
    (fill-region (point) (point-max))))

;; -- hooks --p
(add-hook 'text-mode-hook 'auto-fill-mode)
(add-hook 'text-mode-hook 'flyspell-mode)

;; nested hooks are amazing!
;; ref: (http://stackoverflow.com/questions/6138029/how-to-add-a-hook-to-only-run-in-a-particular-mode)
(add-hook 'markdown-mode-hook 'select-proper-dictionary-language)
(add-hook 'markdown-mode-hook
          (lambda () (add-hook 'after-save-hook 'select-proper-dictionary-language nil 'make-it-local)))

(add-hook 'org-mode-hook 'select-proper-dictionary-language)
(add-hook 'org-mode-hook
          (lambda () (add-hook 'after-save-hook 'select-proper-dictionary-language nil 'make-it-local)))

;; -- keybindings --
(defvar org-jekyll-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") 'org-jekyll-publish-org-to-jekyll)
    (define-key map (kbd "e") 'org-jekyll-export-to-jekyll)
    (define-key map (kbd "s c") 'flyspell-buffer)
    (define-key map (kbd "s t") 'flyspell-mode)
    (define-key map (kbd "n") 'org-jekyll-new-draft)
    (define-key map (kbd "f") 'org-jekyll-fill)
    (define-key map (kbd "r") 'fill-region)
    map))

(define-key global-map (kbd "C-c w") org-jekyll-mode-map)

(provide 'init-writing)
;;; init-markdown.el ends here
