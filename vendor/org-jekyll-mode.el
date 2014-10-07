;;; package -- Summary
;;; Commentary:
;;; Code:
(require 'ox-gfm)

(defun org-jekyll-select-proper-dictionary-language ()
  "Change Ispell dictionary if a tag language: LANGUAGE is found in file."
  (let* ((lang-regexp "-+\nlanguage: ?\\(\\w+\\)\\(?:\n.*\\)*-+\n")
         (text (buffer-string))
	 (lang-p (string-match lang-regexp text))
	 (lang (match-string-no-properties 1 text)))
    (when lang-p
      (ispell-change-dictionary lang))))

(defun org-jekyll-view-md ()
  "Goes to exported markdown file"
  (interactive)
  (find-file (org-jekyll-get-md-filename)))

(defun org-jekyll-get-md-filename ()
  "Finds target markdown file."
  (let* ((post-file-org (buffer-file-name))
         (post-file-md (replace-regexp-in-string "\\.org" ".md" post-file-org)))
    (replace-regexp-in-string "/org" "" post-file-md)))

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
         (target-file-md (org-jekyll-get-md-filename)))
    (when (string= (file-name-extension post-file-org) "org")
      (org-gfm-export-to-markdown)
      (rename-file post-file-md target-file-md t))))

(defun org-jekyll-prepend-date-to-file-name ()
  (let* ((org-jekyll-file-name
	  (buffer-file-name))
         (org-jekyll-is-org-file-p
	  (string= (file-name-extension org-jekyll-file-name) "org"))
         (org-jekyll-is-date-prepended
	  (string-match "[0-9]\\{2\\}-[0-9]\\{2\\}-[0-9]\\{4\\}-.*" org-jekyll-file-name))

         (org-jekyll-file-name-nondirectory (file-name-nondirectory org-jekyll-file-name))
         (org-jekyll-file-directory (file-name-directory org-jekyll-file-name))

         (current-date (format-time-string "%d-%m-%Y"))
         (org-jekyll-file-name-new (concat
                                    org-jekyll-file-directory
                                    "/"
                                    current-date
                                    "-"
                                    org-jekyll-file-name-nondirectory)))
    (when (and
	   org-jekyll-is-org-file-p
	   (not org-jekyll-is-date-prepended))
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
  (cl-labels ((no-code-fill ()
			    (set-mark (point))
			    (re-search-forward "\\(#\\+begin_src\\)" nil t)
			    (if (match-string 1)
				(progn
				  (fill-region (mark) (point))
				  (re-search-forward "#\\+end_src" nil t)
				  (no-code-fill))
			      (fill-region (point) (point-max)))))
    (save-excursion
      (beginning-of-buffer)
      (re-search-forward "END_HTML" nil t)
      (no-code-fill))))

;; --  Keybindings --
;;;###autoload
(defvar org-jekyll-mode-ccw-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") 'org-jekyll-publish-org-to-jekyll)
    (define-key map (kbd "e") 'org-jekyll-export-to-jekyll)
    (define-key map (kbd "s c") 'flyspell-buffer)
    (define-key map (kbd "s t") 'flyspell-mode)
    (define-key map (kbd "n") 'org-jekyll-new-draft)
    (define-key map (kbd "f") 'org-jekyll-fill)
    (define-key map (kbd "r") 'fill-region)
    (define-key map (kbd "t") 'org-jekyll-view-md)
    map))

;;;###autoload
(defvar org-jekyll-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c w") org-jekyll-mode-ccw-map)
    (define-key map (kbd "C-c i") 'org-jekyll-fill)
    map))

;;;###autoload
(define-minor-mode org-jekyll-mode
  "This minor mode define utilities to use org-mode to write jekyll blog posts."
  :init-value nil
  :keymap org-jekyll-mode-map
  :lighter "Org-Jekyll"
  :group 'org-jekyll)

(provide 'org-jekyll-mode)
;;; org-jekyll-mode.el ends here
