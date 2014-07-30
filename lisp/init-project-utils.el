;;; package -- Summary
;;; Commentary:
;;; Code:

;; variables
(defvar loca-projects-dir "~/locaweb/")
(defvar code-projects-dir "~/code/")

;; custom open project. Should probably start using projectile
(defun open-loca-project (arg)
  "Open locaweb project starting at ARG."
  (interactive (list (read-directory-name "Which loca project?: " loca-projects-dir)))
  (find-file arg))

(defun open-code-project (arg)
  "Open code project starting at ARG."
  (interactive (list (read-directory-name "Which code project?: " code-projects-dir)))
  (find-file arg))

(global-set-key (kbd "C-c p l") 'open-loca-project)
(global-set-key (kbd "C-c p c") 'open-code-project)

;; -- ag config --
(setq
 ag-highlight-search t
 ag-reuse-window t
 ag-reuse-buffers t)

(global-set-key (kbd "C-c s a") 'ag)
(global-set-key (kbd "C-c s p") 'ag-project)

(global-set-key (kbd "C-c s g") 'ag-regexp)
(global-set-key (kbd "C-c s r") 'ag-project-regexp)


(provide 'init-project-utils)
;;; init-project-utils.el ends here
