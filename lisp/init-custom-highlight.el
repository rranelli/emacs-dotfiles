;;; init-custom-defuns.el -- Simple functions for highlighting sexp at point.
;;; Commentary:
;;; Code:

(defun rr/highlight-next-face ()
  "Gets the next highlight face from list"
  (interactive)
  (car (setq rr/highlight-faces
             (-rotate 1 rr/highlight-faces))))

(defvar rr/highlight-faces
  '(helm-match-item
    hi-yellow
    hi-blue
    hi-green
    ido-indicator
    helm-selection
    highlight
    popup-menu-mouse-face
    hi-pink
    magit-log-message
    magit-log-reflog-label-cherry-pick
    neo-vc-unlocked-changes-face
    org-agenda-filter-category))

(defun rr/highlight-at-point ()
  "Highlight the sexp at point."
  (interactive)
  (let ((rgxp (regexp-quote (symbol-name (sexp-at-point)))))
    (highlight-regexp rgxp (rr/highlight-next-face))))

(defun rr/unhighlight-at-point ()
  "Unhighlight the sexp at point."
  (interactive)
  (let ((rgxp (regexp-quote (symbol-name (sexp-at-point)))))
    (unhighlight-regexp rgxp)))

(provide 'init-custom-highlight)
;;; init-custom-highlight.el ends here
