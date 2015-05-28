;;; init-shell.el -- Configures features that enhances one's work with terminals inside Emacs.
;;; Commentary:
;;; Code:
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
	 (shell-name (format "shell: %s %s" (rr/shell-project-name) custom-name))
	 (shell-exists-p (bufferp (get-buffer shell-name))))

    (shell shell-name)

    (when (and
	   (rr/in-project)
	   (not shell-exists-p))
      (goto-char (point-max))
      (insert (format "cd %s # [Enter] cds to root" (rr/shell-wd))))))

(defun new-term (arg)
  "Create a new terminal giving it a nice name.
If ARG is present, open a new term regardless."
  (interactive "P")
  (let* ((custom-name (if arg
			  (format "[%s]" (read-string "Terminal name: "))
			""))
	 (term-name (format "term: %s %s" (rr/shell-project-name) custom-name))
	 (shell-exists-p (bufferp (get-buffer term-name))))

    (if (not shell-exists-p)
	(progn (term "/bin/bash")
	       (rename-buffer term-name)
	       (term-line-mode)
	       (goto-char (point-max))
	       (insert (format "cd %s # [Enter] cds to root" (rr/shell-wd)))
	       (term-char-mode)
	       )
      (switch-to-buffer term-name))))

(defun rr/shell-project-name ()
  (file-name-base (directory-file-name (rr/shell-wd))))

(defun rr/shell-wd ()
  (if (projectile-project-p)
      (projectile-project-root)
    default-directory))

;; -- keybindings --
(global-set-key (kbd "C-x RET") 'new-term)
(global-set-key (kbd "C-x C-M-M") 'new-shell)

(rr/expose-bindings shell-mode-map rr/default-bindings-to-expose)
(add-hook 'term-mode-hook
	  (lambda ()
	    (rr/expose-bindings term-raw-map
                             (->> rr/default-bindings-to-expose
                                  (remove "C-h")
                                  (remove "M-h")))))

(add-hook 'sh-mode-hook
	  (lambda ()
	    (rr/expose-bindings sh-mode-map rr/default-bindings-to-expose)))

(provide 'init-shell)
;;; init-shell.el ends here
