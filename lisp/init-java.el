;;; package -- Summary
;;; Commentary:
;;; Code:
(require 'cc-mode)
(require 'javadoc-lookup)

;; (javadoc-add-artifacts [com.nullprogram native-guide "0.2"]
;;                        [org.apache.commons commons-math3 "3.0"])

(defun rr-maven-test ()
  "Run maven test task."
  (interactive)
  (compile (format
	    "cd %s && mvn test; cat %s/target/surefire-reports/*.txt"
	    (ffip-project-root)
	    (ffip-project-root))))

(define-key java-mode-map (kbd "C-c j a") 'rr-maven-test)
(define-key java-mode-map (kbd "C-c j d") 'javadoc-lookup)
(provide 'init-java)
;;; init-java.el ends here
