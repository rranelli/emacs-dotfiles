;;; package -- Summary
;;; Commentary:
;;; Code:

;; ==============
;; -- projects --
;; ==============

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
    (find-file (find-default-file))
    (delete-other-windows)
    (neotree-git-project)
    (magit-branch-manager)
    (split-window-vertically)
    (other-window 1)
    (magit-status path)
    (other-window -2)))

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
 neo-keymap-style-consise t)

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
(defvar project-global-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" 'helm-rr-open-project)
    (define-key map "m" 'magit-status)
    (define-key map "b" 'magit-branch-manager)
    (define-key map "l" 'magit-log)
    (define-key map "i" 'git-timemachine)
    map))

(defvar ag-global-map
  (let ((map project-global-map))
    (define-key map "s" 'ag-project)
    (define-key map "a" 'ag-project-regexp)
    map))

(defvar neotree-global-map
  (let ((map project-global-map))
    (define-key map "g" 'neotree-git-project)
    (define-key map "t" 'neotree-toggle)
    (define-key map "d" 'neotree-dir)
    (define-key map "f" 'neotree-find)
    map))

(define-key global-map (kbd "C-c p") project-global-map)

(provide 'init-project-utils)
;;; init-project-utils.el ends here
