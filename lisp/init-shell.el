;;; init-shell.el -- Configures features that enhances one's work with terminals inside Emacs.
;;; Commentary:
;;; Code:

;; new shells
(defun rr/new-shell (arg shell-fn shell)
  "Create an eshell with given name.
If ARG is present, open a new eshell regardless."
  (interactive "P")
  (let* ((custom-name (if arg (format "𝈙 %s" (read-string "Shell tag: ")) ""))
	 (shell-name (format "⍟ mimi-term ⍟ %s %s" (rr/shell-project-name) custom-name))
	 (shell-exists-p (bufferp (get-buffer shell-name))))

    (if (not shell-exists-p)
        (progn (funcall shell-fn)
               (rename-buffer shell-name))
      (switch-to-buffer shell-name))

    (when (not shell-exists-p)
      (goto-char (point-max))
      (insert (format "cd %s # [Enter] cds to root" (rr/shell-wd))))))

(defun rr/new-ansi-term (arg)
  (interactive "P")
  (rr/new-shell arg '(lambda () (ansi-term "/bin/bash")) 'ansi-term))

(defun rr/shell-project-name ()
  "Return project name from directory."
  (->> (if (projectile-project-p) (projectile-project-root) default-directory)
       (directory-file-name)
       (file-name-base)))

(defun rr/shell-wd ()
  "Return what should be the shell working directory."
  (if (projectile-project-p)
      (projectile-project-root)
    default-directory))

(defun rr/rename-term ()
  (interactive)
  (rename-buffer (read-string "set new buffer name: "
                              (format "%s 𝈙" (buffer-name)))))

;; -- keybindings --

;; term
(defun last-line? ()
  (<= (line-number-at-pos (- (point-max) 1))
      (line-number-at-pos (point))))

(defmacro rr/term-key (binding alternative-f)
  `(defun ,(intern (format "rr/term-%s" binding)) ()
     (interactive)
     (funcall (if (last-line?)
                  ',alternative-f
                ',(lookup-key (current-global-map) (kbd binding))))))

(rr/term-key "C-a" term-send-raw)
(rr/term-key "C-e" term-send-raw)
(rr/term-key "C-f" term-send-right)
(rr/term-key "C-b" term-send-left)
(rr/term-key "C-k" (lambda () (kill-line) (term-send-raw)))

(add-hook 'term-load-hook
          (lambda ()
            (setq term-buffer-maximum-size 10000)
            (rr/expose-bindings term-raw-map
                                (-difference (-concat rr/default-bindings-to-expose
                                                      '("M-:" "M-w" "C-u" "C-x" "C-x C-f"))
                                             '("C-h" "M-h" "C-r")))
            (rr/define-bindings term-raw-map
                                '(("C-c C-c" . term-interrupt-subjob)
                                  ("C-p" . previous-line)
                                  ("C-n" . next-line)
                                  ("C-a" . rr/term-C-a)
                                  ("C-e" . rr/term-C-e)
                                  ("C-f" . rr/term-C-f)
                                  ("C-b" . rr/term-C-b)
                                  ("C-k" . rr/term-C-k)
                                  ("C-s" . isearch-forward)
                                  ("M-n" . term-send-down)
                                  ("M-p" . term-send-up)
                                  ("M-." . completion-at-point)
                                  ("C-y" . term-paste)))))

(global-set-key (kbd "C-x C-<return>") 'rr/new-ansi-term)
(global-set-key (kbd "C-x M-RET") 'rr/new-ansi-term)

(provide 'init-shell)
;;; init-shell.el ends here
