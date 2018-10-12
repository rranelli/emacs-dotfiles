(use-package restclient-mode
  :ensure restclient
  :mode "\\.restclient$"

  :hook
  (restclient-mode . custom-add-watchwords)
  :config
  (defun rr/pairs-to-query (pairs)
    (s-replace "=%3A" "=:" (url-build-query-string (-map (lambda (pair) (list (car pair) (cdr pair))) pairs) nil t)))
  (defun rr/restclient-scratch ()
    "Create a new restclient scratch buffer."
    (interactive)
    (switch-to-buffer
     (get-buffer-create "*restclient-scratch*"))
    (restclient-mode)))


(provide 'init-restclient)
