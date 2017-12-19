(require 'org-jira)

(setq jiralib-url "https://quintoandar.atlassian.net")

(defconst jiralib-token
  `("Cookie" . ,(shell-command-to-string "mimipass get 5a/jira-cookie")))

(defcustom org-jira-default-jql
  "resolution = unresolved and project = RNT and Sprint in openSprints() ORDER BY Rank ASC"
  "Default jql for querying all Jira tickets."
  :group 'org-jira
  :type 'string)

(defvar rr/current-jira-task)

(defun rr/copy-jira-issue ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (delete-char 1)
    (setq rr/current-jira-task (org-jira-copy-current-issue-key))
    (insert "*")))

(define-key org-jira-entry-mode-map (kbd "C-c i k") 'rr/copy-jira-issue)

(provide 'init-org-jira)
