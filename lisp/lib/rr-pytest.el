(defun rr/pytest-compile (command)
  (compile (format "cd %s && %s"
                   (locate-dominating-file (buffer-file-name) ".git")
                   command)))

(defun rr/pytest-file ()
  (interactive)
  (rr/pytest-compile (format "pytest %s" (buffer-file-name))))

(defun rr/pytest-this ()
  (interactive)
  (rr/pytest-compile (format "pytest %s::%s"
                             (buffer-file-name)
                             (save-excursion
                               (end-of-line)
                               (re-search-backward "def \\(test_.+?\\)(.*):")
                               (match-string 1)))))

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

(defun rr/--pytest-find-file ()
  (->> (buffer-file-name)
       (s-replace "/tests/" "/")
       (s-replace "_test.py" ".py")))

(defun rr/--pytest-find-test ()
  (let ((root-dir (-> (buffer-file-name)
                      (locate-dominating-file  ".git")
                      (expand-file-name))))
    (->> (buffer-file-name)
         (s-replace root-dir (file-name-as-directory (f-join root-dir "tests")))
         (s-replace ".py" "_test.py"))))

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
  :init-value t
  :keymap rr/pytest-mode-map
  :lighter "rr/pytest"
  :group 'rr/pytest)

(provide 'rr-pytest)
