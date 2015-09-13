;;; init-isearch.el --- Customizes isearch behavior.
;;; Commentary:
;;; Code:
(global-anzu-mode t)

(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
(global-set-key [remap query-replace] 'anzu-query-replace)

(global-set-key (kbd "C-'") 'anzu-query-replace-at-cursor)
(global-set-key (kbd "M-<down>") 'highlight-symbol-next)
(global-set-key (kbd "M-<up>") 'highlight-symbol-prev)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

(provide 'init-isearch)
;;; init-isearch ends here
