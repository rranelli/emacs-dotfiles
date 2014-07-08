;;; package -- Summary
;;; Commentary:
;;; Code:

;; -- single hook for whole stuff --
(add-hook 'after-make-frame-functions 'config-new-frame)

;; -- frame config dispatch --
(defun config-new-frame (frame)
  "Configure FRAME with specific settings for terminal or x."
  (if (display-graphic-p frame)
      (config-x-frame frame)
    (config-terminal-frame frame)))

;; -- Terminal frame --
(defun config-terminal-frame (frame)
  "Configure terminal FRAME."
  (set-terminal-faces frame)
  (global-hl-line-mode -1))

(defun set-terminal-faces (frame)
  "Set specific faces for a terminal FRAME."
  (set-face-attribute 'helm-buffer-directory frame :foreground "red")
  (set-face-attribute 'helm-ff-directory frame :foreground "red")
  (set-face-attribute 'helm-selection frame :background "cyan" :foreground "black"))

;; -- X frame --
(defun config-x-frame (frame)
  "Configure x FRAME."
  (toggle-transparency frame)
  (global-hl-line-mode 1))

(defun toggle-transparency (&optional frame)
  "Toggle frame transparency for FRAME.  Use selected frame if frame not given."
  (interactive)
  (let* ((max-transp '(100 100))
         (min-transp '(90 90))
         (frame (if frame frame (selected-frame)))
         (transp (frame-parameter frame 'alpha)))
    (if (equal transp min-transp)
        (set-frame-parameter frame 'alpha max-transp)
      (set-frame-parameter frame 'alpha min-transp))))

(provide 'init-frame-hooks)
;;; init-frame-hooks.el ends here
