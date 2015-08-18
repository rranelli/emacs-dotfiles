(require 'request)

;;
;;; Helper functions
;;
(defun rr/file-join (&rest rest)
  (-reduce (lambda (acc x) (concat (file-name-as-directory acc) x))
           rest))

(defun -rr/get (field)
  (-compose 'cdr (-partial 'assoc field)))

(defun rr/get (field alist)
  (cdr (assoc field alist)))


;;
;;; Configuration
;;
(setq rr/kanbanery-org-file (rr/file-join rr/org-files-directory "kanbanery.org"))

(setq rr/kanbanery-api-token
      (apu--read-file-as-string (rr/file-join rr/org-files-directory "kanbanery.token")))

(setq rr/kanbanery-column-to-todo-alist
      '(("Backlog" . none)
        ("To Do" . "TODO")
        ("Doing" . "NEXT")
        ("Test" . "WAITING")
        ("Waiting to Deploy" . "HOLD")
        ("Done" . "DONE")))

(setq rr/kanbanery-project-id "45757")
(setq rr/kanbanery-endpoint "https://paaslocaweb.kanbanery.com/api/v1")
(setq rr/all-columns-url (rr/file-join rr/kanbanery-endpoint "projects" rr/kanbanery-project-id "columns.json"))
(setq rr/all-tasks-url (rr/file-join rr/kanbanery-endpoint "projects" rr/kanbanery-project-id "tasks.json"))

;;
;;; Reading data from your board from kanbanery api
;;
(defun rr/kanbanery-response (url)
  (request-response-data (request url
                                  :sync t
                                  :parser 'json-read
                                  :headers `(("X-Kanbanery-ApiToken" . ,rr/kanbanery-api-token)))))

(defun rr/kanbanery-load-data ()
  (setq rr/columns
        (rr/kanbanery-response rr/all-columns-url))
  (setq rr/all-tasks
        (rr/kanbanery-response rr/all-tasks-url)))

(defun rr/kanbanery-create-tasks ()
  (with-current-buffer (find-buffer-visiting rr/kanbanery-org-file)
    (->> (append rr/all-tasks nil)
         (-reject (lambda (task)
                    (org-id-find-id-in-file (format "kanbanery:task:%s"
                                                    (rr/get 'id task))
                                            rr/kanbanery-org-file)))
         (-map (lambda (task)
                 (goto-char (point-max))
                 (insert (format "* %s\n" (rr/get 'title task)))
                 (org-set-property "ID"
                                   (format "kanbanery:task:%s"
                                           (rr/get 'id task))))))))

(defun rr/kanbanery-update-tasks ()
  (with-current-buffer (find-buffer-visiting rr/kanbanery-org-file)
    (-map (lambda (task)
            (when-let (location (org-id-find-id-in-file (format "kanbanery:task:%s"
                                                                (rr/get 'id task))
                                                        rr/kanbanery-org-file))
                      (goto-char (cdr location))
                      (let* ((column (car (-filter (lambda (c) (eq (rr/get 'column_id task)
                                                              (rr/get 'id c)))
                                                   (append rr/columns nil))))
                             (todo-state (cdr (assoc (rr/get 'name column)
                                                     rr/kanbanery-column-to-todo-alist))))
                        (let ((prefix-arg 0))
                          (org-set-tags-to "KANBAN")
                          (org-todo todo-state)))))
          (append rr/all-tasks nil))))

;; part3 -- update the things you need.
(defun rr/kanbanery-reload ()
  (interactive)
  (rr/kanbanery-load-data)
  (rr/kanbanery-create-tasks)
  (rr/kanbanery-update-tasks))

(provide 'init-org-kanbanery)
;;; init-org-kanbanery.el ends here
