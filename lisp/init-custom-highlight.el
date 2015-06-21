;;; init-custom-defuns.el -- Simple functions for highlighting sexp at point.
;;; Commentary:
;;; Code:
(require 'dash)

(defun rr/highlight-next-face ()
  "Gets the next highlight face from list."
  (interactive)
  (car (setq rr/highlight-faces--carousel
             (-rotate 1 rr/highlight-faces--carousel))))

(defvar rr/highlight-faces
  '(hi-black-hb
    hi-green
    hi-pink
    hi-yellow
    hi-blue
    hi-black-b
    hi-green-b
    hi-red-b
    hi-yellow-b
    hi-blue-b))

(defvar rr/highlight-faces--carousel
  rr/highlight-faces)

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

(defun rr/unhighlight-all ()
  "Unhighlight the sexp at point."
  (interactive)
  (-each hi-lock-interactive-patterns
    (lambda(entry) (unhighlight-regexp  (car entry))))
  (setq rr/highlight-faces--carousel rr/highlight-faces))

(provide 'init-custom-highlight)
;;; init-custom-highlight.el ends here
