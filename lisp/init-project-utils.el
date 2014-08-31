;;; package -- Summary
;;; Commentary:
;;; Code:
(require 'find-file-in-project)
(require 'neotree)

;; ==============
;; -- projects --
;; ==============

;; variables
(defvar project-sources
  '("~/code/"
    "~/locaweb/"))

(defun helm-open-project ()
  "Bring up a Spotify search interface in helm."
  (interactive)
  (helm :sources '(helm-source-list-projects)
	:buffer "*helm-list-projects*"))

(defvar helm-source-list-projects
  '((name . "Open Project")
    (volatile)
    (delayed)
    (candidates . list--projects)
    (action-transformer . open--project)))

(defun list--projects ()
  "Lists all projects given project sources."
  (cl-labels ((dir-to-files (dir)
			  (if (file-exists-p dir)
			      (directory-files dir t directory-files-no-dot-files-regexp)))
	    (flatten (x)
		     (cond ((null x) nil)
			   ((listp x) (append (car x) (flatten (cdr x)))))))
    (progn (flatten (mapcar #'dir-to-files  project-sources)))))

(defun open--project (actions path)
  "Do nothing with ACTIONS. Open project given PATH."
  ;; TODO: Add default file get.
  (let ((default-file (if (file-exists-p (expand-file-name "Gemfile" path))
			  (expand-file-name "Gemfile" path)
			path)))
    (find-file default-file)
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
(setq neo-persist-show nil)

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

;; =================================================
;; maybe will be merged into neotree
(let ((map neotree-mode-map))
  (define-key map (kbd "$") 'neotree-change-root)
  (define-key map (kbd "c") 'neotree-create-node)
  (define-key map (kbd "+") 'neotree-create-node)
  (define-key map (kbd "d") 'neotree-delete-node)
  (define-key map (kbd "r") 'neotree-rename-node)
  (define-key map (kbd "e") 'neotree-enter)
  (define-key map (kbd "C-x C-s") 'noop))
;; =================================================

;; ==========================
;; -- project utils keymap --
;; ==========================
(defvar project-global-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" 'helm-open-project)
    (define-key map "m" 'magit-status)
    (define-key map "b" 'magit-branch-manager)
    (define-key map "l" 'magit-log)
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
