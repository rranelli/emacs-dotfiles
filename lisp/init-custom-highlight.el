;;; init-custom-defuns.el -- Simple functions for highlighting sexp at point.
;;; Commentary:
;;; Code:

(defun rr/highlight-next-face ()
  "Gets the next highlight face from list"
  (interactive)
  (car (setq rr/highlight-faces
             (-rotate 1 rr/highlight-faces))))

(defvar rr/highlight-faces
  '(hi-black-hb
    hi-yellow
    hi-blue
    hi-green
    hi-red
    hi-pink
    hi-yellow-b
    hi-blue-b
    hi-green-b
    hi-red-b
    hi-black-b))

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
