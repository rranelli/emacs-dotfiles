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

;;; Toggle functions
;;
(defun org-jekyll-toggle-between-org-and-md ()
  "Go to exported markdown file."
  (interactive)
  (if (org-jekyll-is-org-file-p)
      (find-file (org-jekyll-get-md-filename))
    (find-file (org-jekyll-get-org-file-from-md-filename))))

(defun org-jekyll-is-org-file-p ()
  "Check wether current file is an org file."
  (string= (file-name-extension (buffer-file-name)) "org"))

(defun org-jekyll-get-md-filename ()
  "Find target markdown file."
  (let* ((post-file-org (buffer-file-name))
         (post-file-md (replace-regexp-in-string "\\.org" ".md" post-file-org)))
    (replace-regexp-in-string "/org/" "/" post-file-md)))

(defun org-jekyll-get-org-file-from-md-filename ()
  "Get corresponding markdown filename for current visited file."
  (let* ((post-file-md (buffer-file-name))
         (post-file-org (replace-regexp-in-string "\\.md" ".org" post-file-md)))
    (replace-regexp-in-string "/_posts" "/org/_posts" post-file-org)))

;;; Publish and export
;;
(defun org-jekyll-publish-org-to-jekyll ()
  "Renames draft prepending date, export to markdown and promote it to Jekyll's post folder."
  (interactive)
  (delete-file (org-jekyll-get-md-filename))
  (org-jekyll-prepend-date-to-file-name)
  (org-jekyll-export-to-jekyll)
  (org-jekyll-promote-draft-to-post))

(defun org-jekyll-export-to-jekyll ()
  "Export currenttly visited file to markdown."
  (interactive)
  (let* ((post-file-org (buffer-file-name))
         (post-file-md (replace-regexp-in-string "\\.org" ".md" post-file-org))
         (target-file-md (org-jekyll-get-md-filename)))
    (when (org-jekyll-is-org-file-p)
      (org-gfm-export-to-markdown)
      (rename-file post-file-md target-file-md t))))

(defun org-jekyll-prepend-date-to-file-name ()
  "Prepend date to currently visited file."
  (let* ((org-jekyll-file-name
	  (buffer-file-name))
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
	   (org-jekyll-is-org-file-p)
	   (not org-jekyll-is-date-prepended))
      (set-visited-file-name org-jekyll-file-name-new t t)
      (rename-file org-jekyll-file-name org-jekyll-file-name-new t))))

(defun org-jekyll-promote-draft-to-post ()
  "Promote draft to post."
  (let* ((org-jekyll-file-name (buffer-file-name))
         (is-org-jekyll-draft-file-p
          (string-match "/_drafts" org-jekyll-file-name))

         (md-file-name (org-jekyll-get-md-filename))
         (md-file-name-new
          (replace-regexp-in-string "/_drafts" "/_posts" md-file-name))

         (org-jekyll-file-name-new
          (replace-regexp-in-string "/_drafts" "/_posts" org-jekyll-file-name)))
    (when (and
           (org-jekyll-is-org-file-p)
           is-org-jekyll-draft-file-p)
      (set-visited-file-name org-jekyll-file-name-new t t)
      (rename-file org-jekyll-file-name org-jekyll-file-name-new t)
      (rename-file md-file-name md-file-name-new))))

(defun org-jekyll-new-draft ()
  "Open new draft file."
  (interactive)
  (let* ((post-title (read-string "post title: "))
         (post-title-dashed (replace-regexp-in-string " " "-" post-title))
         (drafts-template-path "org/_drafts/%s.org"))
    (find-file (expand-file-name (format drafts-template-path post-title-dashed) (projectile-project-root)))

    (insert "blog")
    (yas-expand)
    (insert post-title)
    (org-mode)
    (org-jekyll-mode)))

(defun org-jekyll-fill-and-indent ()
  "Fill and indent current file with awareness of code blocks."
  (interactive)
  (cl-labels ((fill-and-ignore-block
	       ()
	       (let ((start (point)))
		 (if (re-search-forward "\\(#\\+begin_\\|#\\+INCLUDE:\\|#\\+name\\|#\\+results\\)" nil t)
		     (progn
		       (move-beginning-of-line 1)
		       (fill-region start (point))
		       (indent-region start (point))
		       (re-search-forward "\\(#\\+end_\\|#\\+INCLUDE:.*$\\)" nil t)
		       (forward-line)
		       (fill-and-ignore-block))
		   (fill-region (point) (point-max))
		   (indent-region start (point-max))))))
    (save-excursion
      (beginning-of-buffer)
      (re-search-forward "END_HTML" nil t)
      (fill-and-ignore-block))))

(defun org-jekyll-include-code-file ()
  "Include separate code file."
  (interactive)
  (let* ((file-name (file-name-base (buffer-file-name)))
         (relative-code-dir (format "../../_code/%s/" file-name))
         (extension-to-language-assoc '(("rb" . "ruby")
                                        ("sh" . "bash"))))
    (mkdir relative-code-dir t)
    (setq-local chosen-code-file
                (ido-read-file-name "code file to include: " relative-code-dir))
    (setq-local chosen-code-file-language
                (or (cdr (assoc (file-name-extension chosen-code-file)
                                extension-to-language-assoc))
                    (read-string "language: ")))
    (insert (format "#+INCLUDE: %s%s src %s"
                    relative-code-dir
                    (file-name-nondirectory chosen-code-file)
                    chosen-code-file-language))))

(defun org-jekyll-visit-code-dir ()
  "Visit external code files directory."
  (interactive)
  (->> (buffer-file-name)
       (file-name-base)
       (format "../../_code/%s/")
       (find-file)))

(defun org-jekyll-set-compile-on-save ()
  "Add hook to export org files to markdown on save."
  (interactive)
  (add-hook 'after-save-hook 'org-jekyll-export-to-jekyll nil t))

;; --  Keybindings --
;;;###autoload
(defvar org-jekyll-mode-ccw-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") 'org-jekyll-publish-org-to-jekyll)
    (define-key map (kbd "e") 'org-jekyll-export-to-jekyll)
    (define-key map (kbd "s c") 'flyspell-buffer)
    (define-key map (kbd "s t") 'flyspell-mode)
    (define-key map (kbd "n") 'org-jekyll-new-draft)
    (define-key map (kbd "f") 'org-jekyll-fill-and-indent)
    (define-key map (kbd "c i") 'org-jekyll-include-code-file)
    (define-key map (kbd "c d") 'org-jekyll-visit-code-dir)
    (define-key map (kbd "t") 'org-jekyll-toggle-between-org-and-md)
    map))

;;;###autoload
(defvar org-jekyll-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c w") org-jekyll-mode-ccw-map)
    (define-key map (kbd "C-c , v") 'org-jekyll-export-to-jekyll)
    (define-key map (kbd "C-c i") 'org-jekyll-fill-and-indent)
    map))

;;;###autoload
(define-minor-mode org-jekyll-mode
  "This minor mode define utilities to use org-mode to write jekyll blog posts."
  :init-value nil
  :keymap org-jekyll-mode-map
  :lighter " Org-Jekyll"
  :group 'org-jekyll)

(provide 'org-jekyll-mode)
;;; org-jekyll-mode.el ends here
