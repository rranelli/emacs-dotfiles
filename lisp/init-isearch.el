;;; package -- Summary
;;; Commentary:
;;; Code:
(global-anzu-mode t)
(diminish 'anzu-mode)

(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
(global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
(global-set-key [remap query-replace] 'anzu-query-replace)

(global-set-key (kbd "C-'") 'anzu-query-replace-at-cursor)
(global-set-key (kbd "M-<left>") 'smartscan-symbol-go-backward)
(global-set-key (kbd "M-<right>") 'smartscan-symbol-go-forward)
(global-set-key (kbd "C-M-'") 'smartscan-symbol-replace)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(define-key isearch-mode-map (kbd "C-o")
  (lambda () (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string))))))

(provide 'init-isearch)
;;; init-isearch ends here
