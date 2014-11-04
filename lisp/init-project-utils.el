;;; package -- Summary
;;; Commentary:
;;; Code:
(require 'helm-projectile)

(projectile-global-mode 1)
(diminish 'projectile-mode "")

;; variables
(defvar project-sources
  '("~/code/"
    "~/locaweb/"))

(defun helm-rr-open-project ()
  "Bring up a Spotify search interface in helm."
  (interactive)
  (helm :sources '(helm-source-list-projects)
	:buffer "*helm-list-projects*"))

(defvar helm-source-list-projects
  '((name . "Open Project")
    (volatile)
    (delayed)
    (candidates . rr-list-projects)
    (action-transformer . rr-open-project)))

(defun rr-list-projects ()
  "Lists all projects given project sources."
  (cl-labels ((dir-to-files (dir)
			    (if (file-exists-p dir)
				(directory-files dir t directory-files-no-dot-files-regexp)))
	      (flatten (x)
		       (cond ((null x) nil)
			     ((listp x) (append (car x) (flatten (cdr x)))))))
    (progn (flatten (mapcar #'dir-to-files  project-sources)))))

(defun rr-open-project (actions path)
  "Do nothing with ACTIONS. Open project given PATH."
  ;; TODO: Add default file get.
  (cl-flet ((find-default-file () (if (file-exists-p (expand-file-name "Gemfile" path))
				      (expand-file-name "Gemfile" path)
				    path)))
    (find-file (find-default-file))))

;; ===============
;; -- ag config --
;; ===============
(setq
 ag-highlight-search t ;; highlight the matches
 ag-reuse-window nil   ;; do not use the same window for the search result
 ag-reuse-buffers t)   ;; use the same buffer for many searches

;; ====================
;; -- neotree config --
;; ====================
(setq
 neo-persist-show nil
 neo-keymap-style 'concise)

(require 'neotree)

(define-key neotree-mode-map (kbd "C-x C-s") 'noop)

(defun neotree-git-project ()
  "Open dirtree using the git root."
  (interactive)
  (let ((project-dir (ffip-project-root))
        (file-name (buffer-file-name)))
    (if project-dir
        (progn
          (neotree-dir project-dir)
          (neotree-find file-name))
      (message "Could not find git project root."))))

;; ==========================
;; -- project utils keymap --
;; ==========================
(let ((map projectile-command-map))
  ;; general utils
  (define-key map "c" 'helm-rr-open-project)
  (define-key map "n" 'rr-show-file-name)

  (define-key map "m" 'git-timemachine)

  ;; ag
  (define-key map "s" 'ag-project)
  (define-key map "a" 'ag-project-regexp)

  ;; neotree
  (define-key map "t" 'neotree-git-project)
  (define-key map "f" 'neotree-find))

(global-set-key (kbd "C-c o") 'helm-rr-open-project)
(global-set-key (kbd "C-c C-f") 'helm-projectile-find-file)

(provide 'init-project-utils)
;;; init-project-utils.el ends here
