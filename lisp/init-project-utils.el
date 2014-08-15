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
(defun neotree-delete-node ()
  (interactive)
  (catch 'end
    (let* ((filename (neo-buffer--get-filename-current-line))
           (buffer (find-buffer-visiting filename)))
      (if (null filename) (throw 'end nil))
      (if (not (file-exists-p filename)) (throw 'end nil))
      (if (not (yes-or-no-p (format "Do you really want to delete %S?"
                                    filename)))
          (throw 'end nil))
      (if (file-directory-p filename)
          (progn
            (if (neo-path--has-subfile-p filename)
                (if (yes-or-no-p (format
                                  "%S is a directory, delete it recursively?"
                                  filename)))
              (delete-directory filename)))
        (progn
          (delete-file filename)
          (when buffer
            (kill-buffer-ask buffer))))
      (message "%S deleted." filename)
      (neo-buffer--refresh t)
      filename)))

(defun neo-buffer--rename-node ()
  "Rename current node as another path."
  (interactive)
  (let* ((current-path (neo-buffer--get-filename-current-line))
         (buffer (find-buffer-visiting current-path))
         to-path
         msg)
    (unless (null current-path)
      (setq msg (format "Rename [%s] to: " (neo-path--file-short-name current-path)))
      (setq to-path (read-file-name msg (file-name-directory current-path)))
      (if buffer
          (with-current-buffer buffer
            (set-visited-file-name to-path nil t)))
      (rename-file current-path to-path)
      (neo-buffer--refresh t)
      (message "Rename successed."))))
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
