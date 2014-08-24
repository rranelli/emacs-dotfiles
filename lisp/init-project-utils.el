;;; package -- Summary
;;; Commentary:
;;; Code:
(require 'find-file-in-project)
(require 'neotree)

;; ==============
;; -- projects --
;; ==============

;; variables
(defvar loca-projects-dir "~/locaweb/")
(defvar code-projects-dir "~/code/")

;; custom open project. Should probably start using projectile
(defun open--project (base-path)
  "Open project at path, starting at BASE-PATH."
  (let ((path (read-directory-name "Which project?: " base-path)))
    (find-file path)
    (find-file (cdr (car (ffip-project-files))))
    (delete-other-windows)
    (neotree-git-project)
    (magit-branch-manager)
    (other-window -1)))

(defun open-loca-project ()
  "Open locaweb project."
  (interactive)
  (open--project loca-projects-dir))

(defun open-code-project ()
  "Open personal project."
  (interactive)
  (open--project code-projects-dir))

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
    (define-key map "l" 'open-loca-project)
    (define-key map "c" 'open-code-project)
    (define-key map "m" 'magit-status)
    (define-key map "b" 'magit-branch-manager)
    (define-key map "n" 'magit-log)
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
