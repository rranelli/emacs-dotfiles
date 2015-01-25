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

(defmacro do-not-query-process-kill (function-name process-name)
  "Do not query process kill for FUNCTION-NAME that spawns process
PROCESS-NAME."
  `(defadvice ,function-name (after do-not-query-shell-exit
				    first (&optional buffer)
				    activate)
     (interactive)
     "Do not query exit confirmation for shell process buffer."
     (let* ((processes (remove-if-not
			(lambda (process) (string-match-p ,process-name (process-name process)))
			(process-list))))
       (dolist (p processes)
	 (set-process-query-on-exit-flag p nil)))))

(do-not-query-process-kill shell "shell")
(do-not-query-process-kill term "terminal")

;; new shells

(defun new-shell (arg)
  "Create shell with given name. If ARG is present, open a new shell
regardless."
  (interactive "P")
  (let* ((custom-name (if arg
			  (format "[%s]" (read-string "Shell name: "))
			""))
	 (shell-name (format "shell: %s %s" (rr-shell-project-name) custom-name))
	 (shell-exists-p (bufferp (get-buffer shell-name))))

    (shell shell-name)

    (when (and
	   (rr-in-project)
	   (not shell-exists-p))
      (goto-char (point-max))
      (insert (format "cd %s # [Enter] cds to root" (rr-shell-wd))))))

(defun new-term (arg)
  "Create a new terminal giving it a nice name.
If ARG is present, open a new term regardless."
  (interactive "P")
  (let* ((custom-name (if arg
			  (format "[%s]" (read-string "Terminal name: "))
			""))
	 (term-name (format "term: %s %s" (rr-shell-project-name) custom-name))
	 (shell-exists-p (bufferp (get-buffer term-name))))

    (if (not shell-exists-p)
	(progn (term "/bin/bash")
	       (rename-buffer term-name)
	       (term-line-mode)
	       (goto-char (point-max))
	       (insert (format "cd %s # [Enter] cds to root" (rr-shell-wd)))
;;	       (term-char-mode)
	       )
      (switch-to-buffer term-name))))

(defun rr-shell-project-name ()
  (file-name-base (directory-file-name (rr-shell-wd))))

(defun rr-shell-wd ()
  (if (rr-in-project)
      (ffip-project-root)
    default-directory))

(defun rr-in-project ()
  (stringp (ffip-project-root)))

;; hooks
(add-hook 'comint-preoutput-filter-functions 'kill-completion-window-buffer)

;; -- keybindings --
(global-set-key (kbd "C-x RET") 'new-term)
(global-set-key (kbd "C-x C-M-M") 'new-shell)

(expose-bindings shell-mode-map bindings-to-expose)
(add-hook 'term-mode-hook
	  (lambda ()
	    (expose-bindings term-raw-map
			     (remove "C-h" (remove "M-h" bindings-to-expose)))))

(add-hook 'sh-mode-hook
	  (lambda ()
	    (expose-bindings sh-mode-map bindings-to-expose)))

(provide 'init-shell)
;;; init-shell.el ends here
