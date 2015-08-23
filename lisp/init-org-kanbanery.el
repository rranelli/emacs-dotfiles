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
(setq rr/kanbanery-project-id "45757")
;; you can find this via: GET https://WORKSPACE.kanbanery.com/api/v1/user.json
(setq rr/kanbanery-owner-id 49363)

(setq rr/kanbanery-org-file
      (rr/file-join rr/org-files-directory "kanbanery.org"))
(setq rr/kanbanery-api-token
      (apu--read-file-as-string (rr/file-join rr/org-files-directory "kanbanery.token")))

(setq rr/kanbanery-column-to-todo-alist
      '(("Backlog" . none)
        ("To Do" . "TODO")
        ("Doing" . "NEXT")
        ("Test" . "WAITING")
        ("Waiting to Deploy" . "HOLD")
        ("Done" . "DONE")))

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
        (append (rr/kanbanery-response rr/all-columns-url) nil))
  (setq rr/all-tasks
        (append (rr/kanbanery-response rr/all-tasks-url) nil)))

(defun rr/kanbanery-create-tasks ()
  (with-current-buffer (find-buffer-visiting rr/kanbanery-org-file)
    (->> rr/all-tasks
         (-filter (lambda (task)
                    (eq rr/kanbanery-owner-id (rr/get 'owner_id task))))
         (-reject (lambda (task)
                    (or (org-id-find-id-in-file (format "kanbanery:task:%s"
                                                        (rr/get 'id task))
                                                rr/kanbanery-org-file)
                        (org-id-find-id-in-file (format "kanbanery:task:%s"
                                                        (rr/get 'id task))
                                                (format "%s_archive" rr/kanbanery-org-file)))))
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
                                                   rr/columns)))
                             (todo-state (cdr (assoc (rr/get 'name column)
                                                     rr/kanbanery-column-to-todo-alist))))
                        (let ((prefix-arg 0)) (org-todo todo-state)))))
          rr/all-tasks)))

;; part3 -- update the things you need.
(defun rr/kanbanery-reload ()
  (interactive)
  (rr/kanbanery-load-data)
  (rr/kanbanery-create-tasks)
  (rr/kanbanery-update-tasks))

(provide 'init-org-kanbanery)
;;; init-org-kanbanery.el ends here

;; ((position . 24) (global_in_context_url . "https://kanbanery.com/tasks/1406487/in-context?project_id=45757") (type . "Task") (child_changed_at) (blocked . :json-false) (deadline) (moved_at . "2015-01-27T19:51:43+00:00") (ready_to_pull . :json-false) (priority . 0) (owner_id . 49363) (creator_id . 43570) (column_id . 311062) (estimate_id) (task_type_id . 270708) (description . "") (title . "[product] Configurar todos os Planos de Hospedagem utilizando o Modelo de Dados") (sync_created_at . 1402487766.916167) (sync_updated_at . 1422478833.646277) (weight . -18750000.0) (updated_at . "2015-01-28T21:00:33+00:00") (created_at . "2014-06-11T11:56:06+00:00") (id . 1406487))
