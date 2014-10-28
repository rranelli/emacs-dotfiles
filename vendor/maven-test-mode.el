;;; maven-test-mode.el -- Run maven tests from emacs

;; Copyright (C) 2014 Renan Ranelli <http://rranelli.com>
;; Author: Renan Ranelli
;; URL: http://github.com/rranelli/maven-test-mode
;; Created: 2014
;; Version: 0.1
;; Keywords: java maven test
;; Package-Requires: ((s "1.9.0") (find-file-in-project "3.3"))

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; See <http://www.gnu.org/licenses/> for a copy of the GNU General
;; Public License.

;;; Commentary:
;;
;; This minor mode provides some enhancements to java-mode in order to use maven
;; test tasks with little effort. It's largely based on the philosophy of
;; `rspec-mode' by Peter Williams. Namely, it provides the following
;; capabilities:
;;
;;  * toggle back and forth between a test and it's class (bound to `\C-c ,t`)
;;
;;  * verify the test class associated with the current buffer (bound to `\C-c ,v`)
;;
;;  * verify the test defined in the current buffer if it is a test file (bound
;;    to `\C-c ,v`)
;;
;;  * verify the test method defined at the point of the current buffer (bound
;;    to `\C-c ,s`)
;;
;;  * re-run the last verification process (bound to `\C-c ,r`)
;;
;;  * run tests for entire project (bound to `\C-c ,a`)
;;
;; If you want maven-test-mode to be enabled automatically with java-mdoe, add this to your .emacs:
;; (add-hook 'java-mode-hook 'maven-test-mode)
;;
;; Check the full list of available keybindings at `maven-test-mode-map'
;;
;;; Change Log:
;;
;; 0.1 - First release

;;; Code:
(require 's)
(require 'find-file-in-project)

;;; Customization
;;
(defcustom maven-test-test-to-class-subs
  '(("/src/test/" . "/src/main/")
    ("Test.java" . ".java"))
  "Patterns to substitute into test's filename to jump to the associated class."
  :group 'maven-test)

(defcustom maven-test-class-to-test-subs
  '(("/src/main/" . "/src/test/")
    (".java" . "Test.java"))
  "Patterns to substitute into class' filename to jump to the associated test."
  :group 'maven-test)

(defcustom maven-test-test-task-options
  "-q"
  "Options to add to the test task."
  :group 'maven-test)

;;; Keybindings
;;
;;;###autoload
(defvar maven-test-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd  "C-c , a") 'maven-test-all)
    (define-key map (kbd  "C-c , v") 'maven-test-file)
    (define-key map (kbd  "C-c , s") 'maven-test-method)
    (define-key map (kbd  "C-c , i") 'maven-test-install)
    (define-key map (kbd  "C-c , C") 'maven-test-clean-install)
    (define-key map (kbd  "C-c , r") 'recompile)
    (define-key map (kbd  "C-c , t") 'maven-test-toggle-between-test-and-class)
    (define-key map (kbd  "C-c , y") 'maven-test-toggle-between-test-and-class-other-window)
    map))

;;; Test commands
;;
(defun maven-test-all ()
  "Run maven test task."
  (interactive)
  (compile (maven-test-all-command)))

(defun maven-test-install ()
  "Run maven build task."
  (interactive)
  (compile (maven-test-format-task "install")))

(defun maven-test-clean-test-all ()
  "Run maven clean and test task."
  (interactive)
  (compile (maven-test-format-task "clean test")))

(defun maven-test-file ()
  "Run maven test task for current file."
  (interactive)
  (save-excursion
    (unless (maven-test-is-test-file-p)
      (maven-test-toggle-between-test-and-class))
    (compile (maven-test-file-command))))

(defun maven-test-method ()
  "Run maven test task for current method"
  (interactive)
  (unless (maven-test-is-test-file-p)
    (error "Not visiting test file."))
  (compile (maven-test-method-command)))

(defun maven-test-all-command ()
  (maven-test-wrap-command-with-surefire-results
   (maven-test-format-task (maven-test--test-task))))

(defun maven-test-file-command ()
  (maven-test-wrap-command-with-surefire-results
   (s-concat
    (maven-test-format-task (maven-test--test-task))
    (maven-test-class-name-from-buffer))))

(defun maven-test-method-command ()
  (maven-test-wrap-command-with-surefire-results
   (s-concat
    (maven-test-format-task (maven-test--test-task))
    (maven-test-class-name-from-buffer)
    (maven-test-get-prev-test-method-name))))

(defun maven-test-wrap-command-with-surefire-results (command)
  (s-concat
   (maven-test-format-clear-surefire-reports)
   command
   (maven-test-format-show-surefire-reports)))

;;; Command formatting
;;
(defun maven-test-format-task (task)
  (format "cd %s && mvn %s" (ffip-project-root) task))

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
    (or
     (re-search-backward "void \\(test[a-zA-Z]+\\) *() *{" nil t)
     (error "No test method definition before point."))
    (s-concat "#" (match-string 1))))

(defun maven-test--test-task ()
  (format "test %s" maven-test-test-task-options))

;;; Toggle between test and class
;;
(defun maven-test-is-test-file-p ()
  (string-match "/src/test/" (buffer-file-name)))

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
