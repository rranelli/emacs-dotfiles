;;; package -- Summary
;;; Commentary:
;;; Code:

(defun rr-path-rehash ()
  "Reloads PATH environment variable."
  (interactive)
  (let ((path (shell-command-to-string ". ~/.bashrc; echo -n $PATH")))
    (setenv "PATH" path)
    (setq exec-path
	  (append
	   (split-string-and-unquote path ":")
	   exec-path))))

(rr-path-rehash)

(provide 'init-path)
;;; init-path.el ends here
