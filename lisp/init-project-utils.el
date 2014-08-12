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
(defun open-loca-project (arg)
  "Open locaweb project starting at ARG."
  (interactive (list (read-directory-name "Which loca project?: " loca-projects-dir)))
  (open--project arg))

(defun open-code-project (arg)
  "Open code project starting at ARG."
  (interactive (list (read-directory-name "Which code project?: " code-projects-dir)))
  (open--project arg))

(defun open--project (path)
  "Opens project at path."
  (find-file path)
  (find-file (cdr (car (ffip-project-files))))
  (delete-other-windows)
  (neotree-git-project)
  (other-window -1))

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

(defun neotree-git-project ()
  "Open dirtree using the git root."
  (interactive)
  (let ((project-dir (ffip-project-root))
        (file-name (buffer-file-name)))
    (when project-dir
      (neotree-dir project-dir)
      (neotree-find file-name)
      (message "Could not find git project root."))))

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
    (define-key map "l" 'open-loca-project)
    (define-key map "c" 'open-code-project)
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
