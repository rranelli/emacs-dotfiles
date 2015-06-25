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

(do-not-query-process-kill term "eshell")

;; new shells
(defun new-eshell (arg)
  "Create an eshell with given name.
If ARG is present, open a new eshell regardless."
  (interactive "P")
  (let* ((custom-name (if arg
			  (format "[%s]" (read-string "Shell name: "))
			""))
	 (shell-name (format "eshell: %s %s" (rr/shell-project-name) custom-name))
	 (shell-exists-p (bufferp (get-buffer shell-name))))

    (if (not shell-exists-p)
        (progn (eshell)
               (rename-buffer shell-name))
      (switch-to-buffer shell-name))

    (when (not shell-exists-p)
      (goto-char (point-max))
      (insert (format "cd %s # [Enter] cds to root" (rr/shell-wd))))))

(defun rr/shell-project-name ()
  (file-name-base (directory-file-name (rr/shell-wd))))

(defun rr/shell-wd ()
  (if (projectile-project-p)
      (projectile-project-root)
    default-directory))

(defun rr/run-command-in-bash (&rest cmd)
  "Run CMD as if you were in a bash shell instead of Eshell."
  (insert (format "bash -c 'source ~/.bashrc; cd %s; %s'"
                  (eshell/pwd)
                  (s-join " " cmd)))
  (eshell-send-input))

;; -- keybindings --
(rr/expose-default-bindings-with-hook eshell-mode)
(global-set-key (kbd "C-x RET") 'new-eshell)

(add-hook 'eshell-mode-hook
          #'(lambda ()
              (define-key eshell-mode-map [remap eshell-pcomplete] 'helm-esh-pcomplete)
              (define-key eshell-mode-map (kbd "C-x C-s") 'ignore)
              (define-key eshell-mode-map (kbd "M-r") 'helm-eshell-history)))

(provide 'init-shell)
;;; init-shell.el ends here
