;;; init-project-utils.el -- Personal configuration and enhancements to project management.
;;; Commentary:
;;; Code:
;; you need to do this before requiring the lib
(setq helm-projectile-fuzzy-match nil)
(require 'helm-projectile)

(projectile-global-mode 1)

;;
;;; Jumping between projects
;;
(defvar rr/project-sources
  '("~/code/"
    "~/locaweb/"))

(defvar rr/default-file-regexps
  '("Gemfile$"
    "mix.exs$"
    "Readme"
    "README"))

(defun rr/helm-open-project ()
  "Bring up a Project search interface in helm."
  (interactive)
  (helm :sources '(rr/helm-open-project--source)
	:buffer "*helm-list-projects*"))

(defvar rr/helm-open-project--source
  '((name . "Open Project")
    (delayed)
    (candidates . rr/list-projects)
    (action . rr/open-project)))

(defun rr/list-projects ()
  "Lists all projects given project sources."
  (->> rr/project-sources
       (-filter 'file-exists-p)
       (-mapcat (lambda (dir) (directory-files dir t directory-files-no-dot-files-regexp)))))

(defun rr/open-project (path)
  "Open project available at PATH."
  (let* ((candidates (-mapcat (lambda (d) (directory-files path t d)) rr/default-file-regexps))
         (elected (car candidates)))
    (find-file (or elected path))))

;;
;;; Creating new git project
;;
(defun rr/new-git-project ()
  "Create a new git project."
  (interactive)
  (let* ((source (ido-completing-read "create new project in which source?: " rr/project-sources))
	 (project-name (read-input "new project name: "))
	 (project-dir (file-name-as-directory (expand-file-name project-name source))))
    (condition-case nil
	(mkdir project-dir)
      (error nil))

    (shell-command (format "cd %s; git init" project-dir))
    (rr/add-gitignore-file project-dir)))

(defun rr/add-gitignore-file (repo-path)
  "Add .gitignore to the repository at REPO-PATH."
  (interactive (list
		(read-directory-name
		 "Which repository?: "
		 (if (projectile-project-root)
		     (projectile-project-root)
		   (file-name-directory (buffer-file-name))))))
  (let* ((default-project-source (car rr/project-sources))
         (gitignore-dir (expand-file-name "gitignore/" default-project-source))
	 (gitignore-files (directory-files
			   gitignore-dir
			   nil
			   directory-files-no-dot-files-regexp))
	 (gitignore-file (ido-completing-read "choose gitignore file: " gitignore-files)))
    (if gitignore-file
	(copy-file
	 (expand-file-name gitignore-file gitignore-dir)
	 (expand-file-name ".gitignore" repo-path)
	 t))))


;;
;;; Colors in compilation buffer
;;
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (when (eq major-mode 'compilation-mode)
    (toggle-read-only)
    (ansi-color-apply-on-region (point-min) (point-max))
    (toggle-read-only)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;
;;; ag config
;;
(setq
 ag-highlight-search t ;; highlight the matches
 ag-reuse-window nil   ;; do not use the same window for the search result
 ag-reuse-buffers t)   ;; use the same buffer for many searches

;;
;;; neotree config
;;
(setq
 neo-persist-show nil
 neo-keymap-style 'concise)

(require 'neotree)

(define-key neotree-mode-map (kbd "C-x C-s") 'noop)

(defun neotree-git-project ()
  "Open dirtree using the git root."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if project-dir
        (progn
          (neotree-dir (projectile-project-root))
          (neotree-find file-name))
      (message "Could not find git project root."))))

;;
;;; extensions to projectile keymap
;;
(rr/define-bindings projectile-command-map
                    '(;; misc
                      ("n" . rr/show-file-name)
                      ("m" . git-timemachine)

                      ;; ag
                      ("s" . ag-project)
                      ("\C-s" . ag-project-regexp)

                      ;; neotree
                      ("d" . neotree-git-project)
                      ("x" . neotree-find)

                      ;; simple-highlight
                      ("h" . simple-highlight-at-point)
                      ("u" . simple-highlight-unhighlight-at-point)
                      ("U" . simple-highlight-unhighlight-all)

                      ;; projectile extras
                      ("f" . rr/helm-open-project)
                      ("y" . projectile-find-implementation-or-test-other-window)
                      ("a" . projectile-test-project)
                      ("F" . helm-projectile-find-file-in-known-projects)))

(global-set-key (kbd "C-c o") 'rr/helm-open-project)
(global-set-key (kbd "C-c C-f") 'helm-projectile-find-file)
(global-set-key (kbd "C-M-l") 'helm-projectile-switch-to-buffer)

(provide 'init-project-utils)
;;; init-project-utils.el ends here
