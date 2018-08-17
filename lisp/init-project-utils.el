;;; init-project-utils.el -- Personal configuration and enhancements to project management.
;;; Commentary:
;;; Code:
;; you need to do this before requiring the lib
(projectile-global-mode 1)

;;
;;; Jumping between projects
;;
(defvar rr/project-sources
  (cdr (s-split ":" (getenv "CDPATH"))))

(defvar rr/default-file-regexps
  '("^mix.exs$"
    "^package.json$"))

(add-to-list 'projectile-project-root-files-bottom-up ".fetch")

(defun rr/ivy-open-project ()
  "Bring up a Project search interface in ivy."
  (interactive)
  (ivy-read "Select project: "
            (->> rr/project-sources
                 (-filter 'file-exists-p)
                 (-mapcat 'rr/ls)
                 (-filter 'file-directory-p))

            :action (lambda (selection)
                      (let* ((default-file (->> rr/default-file-regexps
                                                (-mapcat (-partial 'rr/ls selection))
                                                (car))))
                        (find-file (or default-file
                                       selection))))))

;;
;;; ag config
;;
(setq ag-highlight-search t ;; highlight the matches
      ag-reuse-window nil   ;; do not use the same window for the search result
      ag-reuse-buffers t)   ;; use the same buffer for many searches

;;
;;; neotree config
;;
(setq neo-persist-show nil
      neo-keymap-style 'concise
      neo-window-fixed-size nil)

(require 'neotree)

(define-key neotree-mode-map (kbd "C-x C-s") 'ignore)

(defun rr/neotree-git-project ()
  "Open dirtree using the git root."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (projectile-project-p)
        (progn
          (neotree-dir (projectile-project-root))
          (neotree-find file-name))
      (message "Could not find git project root."))))

;;
;;; extensions to projectile keymap
;;
(setq projectile-completion-system 'ivy)

(define-key global-map (kbd "C-c p") 'projectile-command-map)
(rr/define-bindings projectile-command-map
                    '(;; misc
                      ("n" . rr/show-file-name)
                      ("m" . git-timemachine)
                      ("v" . magit-status)
                      ;; ag
                      ("S"   . ag)
                      ("s"   . ag-project)
                      ("C-s" . ag-project-regexp)
                      ;; neotree
                      ("d" . rr/neotree-git-project)
                      ("x" . neotree-find)
                      ;; highlight-symbol
                      ("h" . highlight-symbol)
                      ("u" . highlight-symbol-remove-all)
                      ;; projectile extras
                      ("f" . projectile-find-file)
                      ("y" . projectile-find-implementation-or-test-other-window)
                      ("a" . projectile-test-project)))

(global-set-key (kbd "C-c C-f") 'projectile-find-file)
(global-set-key (kbd "C-c o") 'rr/ivy-open-project)

(rr/expose-default-bindings ag-mode-map)

(provide 'init-project-utils)
;;; init-project-utils.el ends here
