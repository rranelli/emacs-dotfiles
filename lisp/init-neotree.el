;;; package -- summary
;;; Commentary:
;;; Code:
(require 'find-file-in-project)

(defadvice neotree-show (around neotree-show-jump-advice activate)
  "Go to neotree after showing it."
  ad-do-it
  (pop-to-buffer (neo-global--get-buffer)))

(defadvice neotree-dir (around neotree-show-jump-advice activate)
  "Go to neotree after showing it."
  ad-do-it
  (pop-to-buffer (neo-global--get-buffer)))

(defun git-project-neotree ()
  "Open dirtree using the git root."
  (interactive)
  (let ((project-dir (ffip-project-root))
        (file-name (buffer-file-name)))
    (when project-dir
      (neotree-dir project-dir)
      (neotree-find file-name)
      (message "Could not find git project root."))))

;; keymappings
(define-key global-map (kbd "C-c o") neotree-global-map)
(defvar neotree-global-map
  (let ((map (make-sparse-keymap)))
    (define-key map "t" 'neotree-toggle)
    (define-key map "d" 'neotree-dir)
    (define-key map "f" 'neotree-find)
    (define-key map "g" 'git-project-neotree)
    map)
  "Keymap for combinations with C-ct first.")

(define-key neotree-mode-map (kbd "$") 'neotree-change-root)
(define-key neotree-mode-map (kbd "c") 'neotree-create-node)
(define-key neotree-mode-map (kbd "d") 'neotree-delete-node)
(define-key neotree-mode-map (kbd "r") 'neotree-rename-node)

(define-key neotree-mode-map (kbd "C-x C-s") 'noop)

(provide 'init-neotree)
;;; init-neotree.el ends here
