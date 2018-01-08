(defun rr/pytest-compile (command)
  (compile (format "cd %s && %s"
                   (locate-dominating-file (buffer-file-name) ".git")
                   command)))

(defun rr/pytest-file ()
  (interactive)
  (rr/pytest-compile (format "pytest %s" (rr/--pytest-find-test))))

(defun rr/pytest-this ()
  (interactive)
  (if (rr/--pytest-is-test-file)
      (rr/pytest-compile (format "pytest %s::%s"
                                 (rr/--pytest-find-test)
                                 (save-excursion
                                   (end-of-line)
                                   (re-search-backward "def \\(test_.+?\\)(.*):")
                                   (match-string 1))))
    (error "You're not in a pytest file")))

(defun rr/pytest-all ()
  (interactive)
  (rr/pytest-compile "pytest"))

(defun rr/pytest-find-file-other-window ()
  (find-file-other-window (rr/--pytest-find-file)))

(defun rr/pytest-find-test-other-window ()
  (find-file-other-window (rr/--pytest-find-test)))

(defun rr/pytest-find-file ()
  (find-file (rr/--pytest-find-file)))

(defun rr/pytest-find-test ()
  (find-file (rr/--pytest-find-test)))

(defun rr/--pytest-is-test-file ()
  (s-matches? "/tests/.+?_test.py" (buffer-file-name)))

(defun rr/--pytest-find-file ()
  (if (rr/--pytest-is-test-file)
      (->> (buffer-file-name)
           (s-replace "/tests/" "/")
           (s-replace "_test.py" ".py"))
    (buffer-file-name)))

(defun rr/--pytest-find-test ()
  (if (rr/--pytest-is-test-file)
      (buffer-file-name)
    (let ((root-dir (-> (buffer-file-name)
                        (locate-dominating-file  ".git")
                        (expand-file-name))))
      (->> (buffer-file-name)
           (s-replace root-dir (file-name-as-directory (f-join root-dir "tests")))
           (s-replace ".py" "_test.py")))))

(defun rr/pytest-find-other ()
  (interactive)
  (if (s-ends-with? "_test.py" (buffer-file-name))
      (rr/pytest-find-file)
    (rr/pytest-find-test)))

(defun rr/pytest-find-other-other-window ()
  (interactive)
  (if (s-ends-with? "_test.py" (buffer-file-name))
      (rr/pytest-find-file-other-window)
    (rr/pytest-find-test-other-window)))

;;;###autoload
(defvar rr/pytest-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c , s") 'rr/pytest-this)
    (define-key map (kbd "C-c , v") 'rr/pytest-file)
    (define-key map (kbd "C-c , a") 'rr/pytest-all)
    (define-key map (kbd "C-c , t") 'rr/pytest-find-other)
    (define-key map (kbd "C-c , y") 'rr/pytest-find-other-other-window)
    (define-key map (kbd "C-c , r") 'recompile)
    map))

;;;###autoload
(define-minor-mode rr/pytest-mode
  "pytest utilities"
  :init-value nil
  :global nil
  :lighter " pytest"
  :keymap rr/pytest-mode-map
  :group 'rr/pytest)

(provide 'rr/pytest-mode)
