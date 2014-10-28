;;; package -- Summary
;;; Commentary:
;;; Code:
(require 's)
(require 'find-file-in-project)

;;; Keybindings
;;
;;;###autoload
(defvar maven-test-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd  "C-c , a") 'maven-test-all)
    (define-key map (kbd  "C-c , v") 'maven-test-file)
    (define-key map (kbd  "C-c , s") 'maven-test-method)
    (define-key map (kbd  "C-c , r") 'recompile)
    (define-key map (kbd  "C-c , t") 'maven-test-toggle-between-test-and-class)
    (define-key map (kbd  "C-c , y") 'maven-test-toggle-between-test-and-class-other-window)
    map))

;;; Test commands
;;
(defun maven-test-all ()
  "Run maven test task."
  (interactive)
  (compile (s-concat
	    (maven-test-format-clear-surefire-reports)
	    (maven-test-format-task)
	    (maven-test-format-show-surefire-reports))))

(defun maven-test-file ()
  "Run maven test task for current file."
  (interactive)
  (compile (s-concat
	    (maven-test-format-clear-surefire-reports)
	    (maven-test-format-task)
	    (maven-test-class-name-from-buffer)
	    (maven-test-format-show-surefire-reports))))

(defun maven-test-method ()
  "Run maven test task for current method"
  (interactive)
  (compile (s-concat
	    (maven-test-format-clear-surefire-reports)
	    (maven-test-format-task)
	    (maven-test-class-name-from-buffer)
	    (maven-test-get-prev-test-method-name)
	    (maven-test-format-show-surefire-reports))))

;;; Command formatting
;;
(defun maven-test-format-task ()
  (format "cd %s && mvn test" (ffip-project-root)))

(defun maven-test-format-show-surefire-reports ()
  (format ";cat %s/target/surefire-reports/*.txt" (ffip-project-root)))

(defun maven-test-format-clear-surefire-reports ()
  (format "rm -rf %s/target/surefire-reports/*.txt;" (ffip-project-root)))

(defun maven-test-class-name-from-buffer ()
  (let* ((class-file (file-name-base (buffer-file-name)))
	 (class-name (s-replace ".java" "" class-file)))
    (format " -Dtest=%s" class-name)))

(defun maven-test-get-prev-test-method-name ()
  (save-excursion
    (re-search-backward "void \\(test[a-zA-Z]+\\) *() *{")
    (s-concat "#" (match-string 1))))

;;; Toggle between test and class
;;
(defun maven-test-is-test-file-p ()
  (string-match "/src/test/" (buffer-file-name)))

(defvar maven-test-test-to-class-subs
  '(("/src/test/" . "/src/main/")
    ("Test.java" . ".java")))

(defvar maven-test-class-to-test-subs
  '(("/src/main/" . "/src/test/")
    (".java" . "Test.java")))

(defun maven-test-toggle-between-test-and-class ()
  (interactive)
  (maven-test--toggle-between-test-and-class #'find-file))

(defun maven-test-toggle-between-test-and-class-other-window ()
  (interactive)
  (maven-test--toggle-between-test-and-class #'find-file-other-window))

(defun maven-test--toggle-between-test-and-class (func)
  (let* ((subs (if (maven-test-is-test-file-p)
		   maven-test-test-to-class-subs
		 maven-test-class-to-test-subs)))
    (funcall func (s-replace-all subs (buffer-file-name)))))

;;;###autoload
(define-minor-mode maven-test-mode
  "This minor mode define utilities to use org-mode to write jekyll blog posts."
  :init-value nil
  :keymap maven-test-mode-map
  :lighter "MvnTest"
  :group 'maven-test)

(provide 'maven-test-mode)
;;; maven-test-mode.el ends here.
