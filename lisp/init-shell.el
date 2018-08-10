;;; init-shell.el -- Configures features that enhances one's work with terminals inside Emacs.
;;; Commentary:
;;; Code:

(use-package sh-script
  :ensure nil
  :config
  (rr/expose-default-bindings sh-mode-map))

;;;;;;;;;;;;;;;
;; Mimiterm! ;;
;;;;;;;;;;;;;;;
(defvar rr/mimiterm-buffer-name-prefix
  "⍟ mimiterm ⍟ "
  "Prefix for mimiterm buffers.")

(defvar rr/ivy-mimiterm--source
  '((name . "Open terminal")
    (volatile)
    (candidates . rr/mimiterm-list)
    (action     . rr/mimiterm-open)
    (persistent-action . ivy-yank-selection)))

(defun rr/ivy-mimiterm ()
  "Bring up a Project search interface in ivy."
  (interactive)
  (ivy-read "terminal: " (rr/mimiterm-list)
            :initial-input rr/mimiterm-buffer-name-prefix
            :action 'rr/mimiterm-open))

(defun rr/mimiterm-list ()
  "Lists all projects given project sources."
  (->> (buffer-list)
       (-map 'buffer-name)
       (cons (rr/mimiterm-default-name))
       (-filter (-partial 's-contains? rr/mimiterm-buffer-name-prefix))
       (-sort 's-less?)))

(global-set-key (kbd "C-x m") 'rr/ivy-mimiterm)

(defun rr/mimiterm-default-name ()
  (s-concat rr/mimiterm-buffer-name-prefix (rr/project-name)))

(defun rr/mimiterm-open (&optional input)
  "Create an eshell with given name.
If ARG is present, open a new eshell regardless."
  (let* ((shell-name (or input (rr/mimiterm-default-name))) ;
	 (shell-exists-p (bufferp (get-buffer shell-name))))
    (if shell-exists-p
        (switch-to-buffer shell-name)
      (let ((default-directory (rr/project-root))) ;; dynamic scope ftw!
        (ansi-term "/bin/bash")
        (rename-buffer shell-name)))))

(defun rr/set-no-process-query-on-exit ()
  (let ((proc (get-buffer-process (current-buffer))))
    (when (processp proc)
      (set-process-query-on-exit-flag proc nil))))

;; term
(defun rr/mimiterm-clear ()
  (interactive)
  (save-excursion
    (term-line-mode)
    (delete-region (point-min) (point-max))
    (term-char-mode)
    (term-send-raw)))

(defun last-line? ()
  (>= (line-number-at-pos (point))
      (line-number-at-pos (or (save-excursion
                                (goto-char (point-max))
                                (re-search-backward "\\($ \\|iex\([0-9]+\)>\\|>>>\\)" nil t))
                              0))))

(defmacro rr/mimiterm-key (binding alternative-f)
  `(defun ,(intern (format "rr/mimiterm-%s" binding)) ()
     (interactive)
     (funcall (if (last-line?)
                  ',alternative-f
                ',(lookup-key (current-global-map) (kbd binding))))))
(rr/mimiterm-key "C-r" term-send-raw)
(rr/mimiterm-key "C-a" term-send-raw)
(rr/mimiterm-key "C-e" term-send-raw)
(rr/mimiterm-key "C-f" term-send-right)
(rr/mimiterm-key "C-b" term-send-left)
(rr/mimiterm-key "C-k" term-send-raw)

(defun rr/mimiterm-fix-keybindings ()
  (interactive)
  (rr/expose-bindings term-raw-map
                      (-concat (-difference rr/default-bindings-to-expose '("C-h" "M-h"))
                               '("M-:" "M-w" "C-u" "C-x" "C-x C-f" "C-c c")))
  (rr/define-bindings term-raw-map
                      '(("C-c C-c" . term-interrupt-subjob)
                        ("C-p" . previous-line)
                        ("C-n" . next-line)
                        ("C-l" . rr/mimiterm-clear)
                        ("C-a" . rr/mimiterm-C-a)
                        ("C-e" . rr/mimiterm-C-e)
                        ("C-f" . rr/mimiterm-C-f)
                        ("C-b" . rr/mimiterm-C-b)
                        ("C-k" . rr/mimiterm-C-k)
                        ("C-s" . swiper)
                        ("M-n" . term-send-down)
                        ("M-p" . term-send-up)
                        ("M-." . completion-at-point)
                        ("C-y" . term-paste))))

;; -- keybindings --
(add-hook 'term-exec-hook 'goto-address-mode)
(add-hook 'term-exec-hook 'rr/set-no-process-query-on-exit)
(add-hook 'term-load-hook
          (lambda ()
            (setq term-buffer-maximum-size 10000)
            (rr/mimiterm-fix-keybindings)))

;;; quoting inside double quotes
(defun sh-script-extra-font-lock-match-var-in-double-quoted-string (limit)
  "Search for variables in double-quoted strings."
  (let (res)
    (while
        (and (setq res (progn (if (eq (get-byte) ?$) (backward-char))
                              (re-search-forward
                               "[^\\]\\$\\({#?\\)?\\([[:alpha:]_][[:alnum:]_]*\\|[-#?@!]\\|[[:digit:]]+\\)"
                               limit t)))
             (not (eq (nth 3 (syntax-ppss)) ?\")))) res))

(defvar sh-script-extra-font-lock-keywords
  '((sh-script-extra-font-lock-match-var-in-double-quoted-string
     (2 font-lock-variable-name-face prepend))))

(defun sh-script-extra-font-lock-activate ()
  (interactive)
  (font-lock-add-keywords nil sh-script-extra-font-lock-keywords)
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode (with-no-warnings (font-lock-fontify-buffer)))))

(add-hook 'sh-mode-hook 'sh-script-extra-font-lock-activate)

(provide 'init-shell)
;;; init-shell.el ends here
