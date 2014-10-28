;;; package -- Summary
;;; Commentary:
;;; Code:
(require 'cc-mode)
(require 'javadoc-lookup)

(javadoc-add-artifacts [com.nullprogram native-guide "0.2"]
                       [org.apache.commons commons-math3 "3.0"])

(require 's)
(require 'ffip)

(defun rr-maven-test-all ()
  "Run maven test task."
  (interactive)
  (compile (s-concat
	    (rr-maven-format-test-task)
	    (rr-maven-format-result-cat))))

(defun rr-maven-test-file ()
  "Run maven test task for current file."
  (interactive)
  (compile (s-concat
	    (rr-maven-format-test-task)
	    (rr-maven-java-class-from-buffer)
	    (rr-maven-format-result-cat))))

(defun rr-maven-test-method ()
  "Run maven test task for current method"
  (interactive)
  (compile (s-concat
	    (rr-maven-format-test-task)
	    (rr-maven-java-class-from-buffer)
	    (rr-maven-get-prev-test-method)
	    (rr-maven-format-result-cat))))

(defun rr-maven-format-test-task ()
  (format "cd %s && mvn test" (ffip-project-root)))

(defun rr-maven-format-result-cat ()
  (format ";cat %s/target/surefire-reports/*.txt" (ffip-project-root)))

(defun rr-maven-java-class-from-buffer ()
  (let* ((class-file (file-name-base (buffer-file-name)))
	 (class-name (s-replace ".java" "" class-file)))
    (format " -Dtest=%s" class-name)))

(defun rr-maven-get-prev-test-method ()
  (save-excursion
    (re-search-backward "void \\(test[a-zA-Z]+\\) *() *{")
    (s-concat "#" (match-string 1))))

(define-bindings java-mode-map
  '(("C-c , a" . rr-maven-test-all)
    ("C-c , v" . rr-maven-test-file)
    ("C-c , s" . rr-maven-test-method)
    ("C-c , r" . recompile)
    ("C-c j d" . javadoc-lookup)
    ("C-c j s" . sort-java-imports)
    ("C-c j i" . add-java-import)))

(provide 'init-java)
;;; init-java.el ends here
