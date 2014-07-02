;;; package -- Summary
;;; Commentary:
;;; Code:

;; context: if using terminal:
;; disables global hl line mode
(add-hook 'after-make-frame-functions
          (lambda (given-frame)
            (if (display-graphic-p given-frame)
                (global-hl-line-mode 1)
              (global-hl-line-mode -1))))

;; changes some faces
(add-hook 'after-make-frame-functions
          (lambda (given-frame)
            (unless (display-graphic-p given-frame)
              (custom-set-faces
               '(helm-buffer-directory ((t (:foreground "red"))))
               '(helm-ff-directory ((t (:foreground "red"))))
               '(helm-selection ((t (:background "cyan" :foreground "black"))))))))

;; context: if using X
(add-hook 'after-make-frame-functions 'toggle-transparency)

(defun toggle-transparency (&optional given-frame)
  "Toggle frame transparency."
  (interactive)
  (let* ((max-transp '(100 100))
        (min-transp '(90 90))
        (frame (if given-frame given-frame (selected-frame)))
        (transp (frame-parameter frame 'alpha)))
    (if (equal transp min-transp)
        (set-frame-parameter frame 'alpha max-transp)
      (set-frame-parameter frame 'alpha min-transp))))

(provide 'init-frame-hooks)
;;; init-frame-hooks.el ends here
