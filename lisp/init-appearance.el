;;; package -- Summary
;;; Commentary:
;;; Code:

;; Customizations
(defcustom chosen-theme 'solarized-dark
  "Theme chosen to be initialized." :group 'init-appearance)

(defcustom chosen-terminal-theme 'gruvbox
  "Theme chosen to be initialized in terminal sessions." :group 'init-appearance)

(defcustom cursor-color "sky blue"
  "Cursor color to be applied when loading themes." :group 'init-appearance)

;; modeline stuff
(defvar set-mode-line-faces-p t)
(defvar powerline-p t)

(defcustom mode-line-background "DeepSkyBlue4"
  "Background for the mode-line." :group 'init-appearance)

(defcustom mode-line-foreground "Snow"
  "Background for the mode-line." :group 'init-appearance)

(defvar paren-highlight-style 'expression)

;; -- frame config dispatch --
(defun config-frame-appearance (&optional frame)
  "Configure FRAME with specific settings for terminal or x."
  (interactive)
  (defun load-appearance (frame force-reload)
    (if (display-graphic-p frame)
        (config-x-frame frame)
      (config-xterm-256color-terminal-frame frame)))

  (if (called-interactively-p 'any)
      (load-appearance (selected-frame) t)
    (load-appearance frame nil)))

;; -- config frames
(defun config-x-frame (frame)
  "Configure x FRAME."

  (global-hl-line-mode 1)
  (toggle-transparency frame)

  (load-theme chosen-theme t)
  (when powerline-p (config-powerline frame))

  (set-face-attribute 'cursor frame :background cursor-color)
  (when set-mode-line-faces-p
    (set-face-attribute 'mode-line frame
                        :background mode-line-background
                        :foreground mode-line-foreground))

  (setq theme-loaded t))

(defun toggle-transparency (&optional frame)
  "Toggle frame transparency for FRAME.  Use selected frame if frame not given."
  (interactive)
  (defun other--thing (thing first second)
    (if (equal thing first) second first))

  (defun toggle--transparency (frame min-transp max-transp)
    (let ((transp (frame-parameter frame 'alpha)))
      (set-frame-parameter frame 'alpha (other--thing transp min-transp max-transp))))

  (let ((max-transp '(100 100))
        (min-transp '(95 95)))
    (if (called-interactively-p 'any)
        (toggle--transparency (selected-frame) min-transp max-transp)
      (toggle--transparency frame min-transp max-transp))))

(defun config-powerline (&optional frame)
  "Set up powerline faces for FRAME."
  (load-file (expand-file-name "vendor/powerline.el" user-emacs-directory))

  (setq powerline-arrow-shape 'arrow-14
        powerline-color1 "CadetBlue4"
        powerline-color2 "#002b36"
        powerline-column 50)

  (set-face-attribute 'mode-line frame :box nil)
  (set-face-attribute 'mode-line-inactive frame :box nil))

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

(defun config-xterm-256color-terminal-frame (frame)
  "Set specific faces for a 256 color terminal FRAME."
  (load-theme chosen-terminal-theme t)

  (set-face-attribute 'helm-selection frame :background "black")
  (set-face-attribute 'helm-ff-directory frame :background "black")

  (set-face-attribute 'magit-diff-add nil :foreground "white" :background "#005f00"))

;; nice paren-style highlight, but with buffer local configuration ;)
(defun expression-style-show-paren ()
  "Make show-paren expression only for lisp modes"
  (make-variable-buffer-local 'show-paren-style)
  (setq show-paren-style paren-highlight-style))
(add-hook 'emacs-lisp-mode-hook 'expression-style-show-paren)

;; make cursor type a bar
(modify-all-frames-parameters (list (cons 'cursor-type 'bar)))

;; -- hooks --
(add-hook 'after-make-frame-functions 'config-frame-appearance)

(provide 'init-appearance)
;;; init-appearance.el ends here
