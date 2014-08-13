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
    (other-window -1)))

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

(defadvice neotree-show (around neotree-show-jump-advice activate)
  "Go to neotree after showing it."
  ad-do-it
  (pop-to-buffer (neo-global--get-buffer)))

(defadvice neotree-dir (around neotree-show-jump-advice activate)
  "Go to neotree after showing it."
  ad-do-it
  (pop-to-buffer (neo-global--get-buffer)))

;; ==========================================================
;; maybe will be merged into a the core
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
;; ==========================================================

(define-key neotree-mode-map (kbd "$") 'neotree-change-root)
(define-key neotree-mode-map (kbd "c") 'neotree-create-node)
(define-key neotree-mode-map (kbd "d") 'neotree-delete-node)
(define-key neotree-mode-map (kbd "r") 'neotree-rename-node)
(define-key neotree-mode-map (kbd "C-x C-s") 'noop)

;; ==========================
;; -- project utils keymap --
;; ==========================
(defvar project-global-map
  (let ((map (make-sparse-keymap)))
    (define-key map "l" (lambda () (interactive) (open--project loca-projects-dir)))
    (define-key map "c" (lambda () (interactive) (open--project code-projects-dir)))
    (define-key map "m" 'magit-status)
    map))

(defvar ag-global-map
  (let ((map project-global-map))
    (define-key map "a" 'ag-project)
    (define-key map "r" 'ag-project-regexp)
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
