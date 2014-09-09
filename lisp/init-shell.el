;;; package -- Summary
;;; Commentary:
;;; Code:

(autoload 'bash-completion-dynamic-complete
  "bash-completion"
  "BASH completion hook")
(add-hook 'shell-dynamic-complete-functions
	  'bash-completion-dynamic-complete)
(add-hook 'shell-command-complete-functions
	  'bash-completion-dynamic-complete)

(defun kill-completion-window-buffer (&optional output)
  (interactive)
  (dolist (win (window-list))
    (when (string= (buffer-name (window-buffer win)) "*Completions*")
      (kill-buffer "*Completions*")))
  output)
(add-hook 'comint-preoutput-filter-functions 'kill-completion-window-buffer)

(add-hook 'sql-mode-hook 'sql-highlight-mysql-keywords)

(defadvice shell (after do-not-query-shell-exit
			first (&optional buffer)
			activate)
  "Do not query exit confirmation for shell process buffer."
  (interactive)
  (let* ((shell-processes (remove-if-not
			   (lambda (process) (string-match-p "shell" (process-name process)))
			   (process-list))))
    (dolist (p shell-processes)
      (set-process-query-on-exit-flag p nil))))

(defun new-shell (arg)
  "Create shell with given name. If ARG is present, open a new shell regardless ."
  (interactive "P")
  (cl-flet ((get-dir-name-last (path)
                               (string-match "/\\([^/]*\\)/$" path)
                               (match-string 1 path)))
    (let* ((project-root (ffip-project-root))
	   (in-project-p (stringp project-root))
           (dir-name-last (when project-root (get-dir-name-last project-root)))

	   (project-name (format "<%s>" (if in-project-p
					    dir-name-last
					  "out-of-project")))
           (custom-name (if arg
			    (format " [%s]" (read-string "Shell name: "))
			  ""))
	   (shell-name (format "shell: %s%s" project-name custom-name))

	   (shell-exists-p (bufferp (get-buffer shell-name))))

      (shell shell-name)
      (when (and
	     in-project-p
	     (not shell-exists-p))
	(insert (format "cd %s" project-root))
	(comint-send-input nil t)))))

;; -- keybindings --
(global-set-key (kbd "C-x C-m") 'new-shell)
(define-key shell-mode-map (kbd "C-c C-f") nil)

(provide 'init-shell)
;;; init-shell.el ends here
