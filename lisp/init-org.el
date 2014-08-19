;;; package -- Summary
;;; Commentary:
;;; Code:
(if (file-exists-p "~/Dropbox/org/life.org")
    ;; Set up org-agenda files
    (let ((org-dir "~/Dropbox/org/")
          (org-files '("diary.org"
                       "life.org"
                       "study.org"
                       "refile.org"
                       "opensource.org"
                       "locaweb.org"
                       "emacs.org")))

      (setq org-user-agenda-files
            (mapcar (lambda (filename) (concat org-dir filename)) org-files))

      ;; loading org custom
      (require 'org-mode-custom)
      ;; Setting up babel support for languages
      (org-babel-do-load-languages 'org-babel-load-languages
                                   '((sql . t)))

      ;; Setting up fonfitication
      (setq org-src-fontify-natively t)
      (add-hook 'sql-mode-hook (lambda () (sql-highlight-mysql-keywords)))

      ;; make org beautifull
      (setq
       org-odd-levels-only t
       org-hide-leading-stars t
       org-startup-indented t)

      ;; removing useless conflict keys on org-mode.
      (define-key org-agenda-mode-map (kbd "C-c p") nil)
      (define-key org-mode-map (kbd "M-h") nil))

  (message "skipping org-mode load"))

(defun org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in `org-mode'."
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (newline-and-indent)
  (insert (format "#+BEGIN_SRC %s\n" src-code-type))
  (newline-and-indent)
  (insert "#+END_SRC\n")
  (previous-line 2)
  (org-edit-src-code))

(define-key org-mode-map (kbd "C-c o s") 'org-insert-src-block)

(provide 'init-org)
;;; init-org.el ends here
