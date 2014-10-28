;;; package -- Summary
;;; Commentary:
;;; Code:
(require 's)
(require 'find-file-in-project)

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

;;;###autoload
(defvar maven-test-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd  "C-c , a") 'maven-test-all)
    (define-key map (kbd  "C-c , v") 'maven-test-file)
    (define-key map (kbd  "C-c , s") 'maven-test-method)
    (define-key map (kbd  "C-c , r") 'recompile)
    (define-key map (kbd  "C-c j d") 'javadoc-lookup)
    (define-key map (kbd  "C-c j s") 'sort-java-imports)
    (define-key map (kbd  "C-c j i") 'add-java-import)
    map))

;;;###autoload
(define-minor-mode maven-test-mode
  "This minor mode define utilities to use org-mode to write jekyll blog posts."
  :init-value nil
  :keymap maven-test-mode-map
  :lighter "MvnTest"
  :group 'maven-test)

(provide 'maven-test-mode)
;;; maven-test-mode.el ends here.
