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
    (config-xterm-256color-terminal-frame frame)))

;; -- X frame --
(setq theme-loaded nil)
(defun config-x-frame (frame)
  "Configure x FRAME."

  (global-hl-line-mode 1)
  (toggle-transparency frame)
  (unless theme-loaded
    (apply-window-system-color-theme))
  (config-powerline frame))

(defun toggle-transparency (&optional frame)
  "Toggle frame transparency for FRAME.  Use selected frame if frame not given."
  (interactive)
  (let* ((max-transp '(100 100))
         (min-transp '(95 95))
         (frame (if frame frame (selected-frame)))
         (transp (frame-parameter frame 'alpha)))
    (if (equal transp min-transp)
        (set-frame-parameter frame 'alpha max-transp)
      (set-frame-parameter frame 'alpha min-transp))))

(defun apply-window-system-color-theme ()
  "Apply window system specific color theme."
  (setq solarized-high-contrast-mode-line nil
        solarized-distinct-fringe-background nil
        x-underline-at-descent-line t)

  (set-face-attribute 'cursor nil :background "cyan")

  (load-theme 'solarized-dark t)
  (setq theme-loaded t))

(defun config-powerline (frame)
  "Set up powerline faces for FRAME."
  (load-file (expand-file-name "vendor/powerline.el" user-emacs-directory))

  (setq powerline-arrow-shape 'arrow
        powerline-color1 "grey22"
        powerline-color2 "#002b36"
        powerline-column 50)

  (set-face-attribute 'mode-line frame
                      :background "DeepSkyBlue4"
                      :foreground "Snow"
                      :box nil)
  (set-face-attribute 'mode-line-inactive frame
                      :box nil))

;; -- Terminal frame --
;; Use the functions bellow if not working from a 256color terminal.
(defun config-terminal-frame (frame)
  "Configure terminal FRAME."
  (set-terminal-faces frame)
  (global-hl-line-mode -1))

(defun set-terminal-faces (frame)
  "Set specific faces for a terminal FRAME.  This thing is hard as hell."
  (set-face-attribute 'helm-selection frame :background "black")
  (set-face-attribute 'helm-match frame :foreground "blue")
  (set-face-attribute 'helm-source-header frame :foreground "white" :background "blue")

  (set-face-attribute 'mode-line frame :foreground "black" :background "white" :box nil)

  (set-face-attribute 'magit-branch frame :background "black")
  (set-face-attribute 'magit-log-head-label-remote frame :foreground "black")
  (set-face-attribute 'magit-log-head-label-local frame :foreground "red" :background "black"))

;; -- 256 color terminal frame --
(defun config-xterm-256color-terminal-frame (frame)
  "Set specific faces for a 256 color terminal FRAME."
  (load-theme 'sanityinc-tomorrow-night t)

  (set-face-attribute 'helm-selection frame :background "black")
  (set-face-attribute 'helm-ff-directory frame :background "black")

  (set-face-attribute 'magit-diff-add nil :foreground "white" :background "#005f00"))

(provide 'init-frame-hooks)
;;; init-frame-hooks.el ends here
