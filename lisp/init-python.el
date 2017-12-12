;;; init-python.el -- Configures python development features
;;; Commentary:
;;; Code:

;;
;;; Configure elpy
;;
;; install-it: `pip install jedi flake8 importmagic autopep8 ipython rope yapf`'
(require 'elpy)

(setq elpy-rpc-backend "rope")
(delete 'elpy-module-flymake elpy-modules)

(elpy-enable)

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt -i")

(setq py-shell-switch-buffers-on-execute-p t) ; switch to the interpreter after executing code
(setq py-switch-buffers-on-execute-p t)
(setq py-split-windows-on-execute-p nil) ; don't split windows
(setq py-smart-indentation t) ; try to automagically figure out indentation

(define-key python-mode-map (kbd "C-c i") 'elpy-autopep8-fix-code)
(define-key python-mode-map (kbd "C-c C-d") 'elpy-doc)

(define-key elpy-mode-map (kbd "C-c C-e") 'python-shell-send-region)

(add-hook 'elpy-mode-hook (lambda ()
                            (flymake-mode -1)
                            (flycheck-mode 1)))

;; pretty symbols
(add-hook
 'python-mode-hook
 (lambda ()
   (mapc (lambda (pair) (push pair prettify-symbols-alist))
         '(;; Syntax
           ("def" .      #x2131)
           ("not" .      #x2757)
           ("in" .       #x2208)
           ("not in" .   #x2209)
           ("return" .   #x27fc)
           ("yield" .    #x27fb)
           ("for" .      #x2200)
           ;; ;; Base Types
           ;; ("int" .      #x2124)
           ;; ("float" .    #x211d)
           ;; ("str" .      #x1d54a)
           ;; ("True" .     #x1d54b)
           ;; ("False" .    #x1d53d)
           ;; ;; Mypy
           ;; ("Dict" .     #x1d507)
           ;; ("List" .     #x2112)
           ;; ("Tuple" .    #x2a02)
           ;; ("Set" .      #x2126)
           ;; ("Iterable" . #x1d50a)
           ;; ("Any" .      #x2754)
           ;; ("Union" .    #x22c3)
           ))
   (prettify-symbols-mode)))

(rr/expose-bindings elpy-mode-map '("M-<up>" "M-<down>" "C-c C-f"))

(define-key python-mode-map (kbd "C-c i") 'elpy-autopep8-fix-code)
(define-key python-mode-map (kbd "C-c C-d") 'elpy-doc)

;;
;;; pytest testing tools
;;
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

(rr/define-bindings python-mode-map '(("C-c , s" . rr/pytest-this)
                                      ("C-c , v" . rr/pytest-file)
                                      ("C-c , a" . rr/pytest-all)
                                      ("C-c , t" . rr/pytest-find-other)
                                      ("C-c , y" . rr/pytest-find-other-other-window)
                                      ("C-c , r" . recompile)))

(provide 'init-python)
;;; init-python.el ends here
