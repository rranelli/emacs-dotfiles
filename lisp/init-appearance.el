;;; package -- Summary
;;; Commentary:
;;; Code:

;; themes
(defcustom chosen-x-theme 'soothe
  "Theme chosen to be initialized." :group 'init-appearance)

(defcustom chosen-terminal-theme 'gruvbox
  "Theme chosen to be initialized in terminal sessions." :group 'init-appearance)

(defvar chosen-theme chosen-x-theme
  "Chosen theme to be used at the config loaders.")

;; support
(defun get-color-config (config-name)
  "Gets the configuration from the config list by CONFIG-NAME."
  (let* ((color-settings
          '((zenburn
             . '((mode-line-background . "Gray33" )
                 (mode-line-foreground . "#8FB28F")
                 (powerline-arrow . "Gray20")
                 (powerline-other . "#3F3F3F")
                 (cursor . "SkyBlue")
                 (use-powerline-p . t)
                 (set-mode-line-faces-p . nil)
                 (paren-highlight-style . parenthesis)
                 (custom-faces-fn . (lambda ()))))
            (gruvbox
             . '((mode-line-background . "#5F7F5F" )
                 (mode-line-foreground . "Snow")
                 (powerline-arrow . "Gray50")
                 (powerline-other . "#3F3F3F")
                 (cursor . "SkyBlue")
                 (use-powerline-p . t)
                 (set-mode-line-faces-p . t)
                 (paren-highlight-style . parenthesis)
                 (custom-faces-fn . (lambda ()
                                      (set-face-attribute 'helm-selection nil :background "Gray20")))))
            (soothe
             . '((mode-line-background . "#5F7F5F" )
                 (mode-line-foreground . "Snow")
                 (powerline-arrow . "Gray50")
                 (powerline-other . "#3F3F3F")
                 (cursor . "SkyBlue")
                 (use-powerline-p . t)
                 (set-mode-line-faces-p . t)
                 (paren-highlight-style . parenthesis)
                 (custom-faces-fn . (lambda ()
                                      (set-face-attribute 'highlight nil :foreground nil)
                                      (set-face-attribute 'helm-selection nil :background "Gray20")))))
            (solarized-dark
             . '((mode-line-background . "DeepSkyBlue4" )
                 (mode-line-foreground . "Snow")
                 (powerline-arrow . "CadetBlue4")
                 (powerline-other . "#002b36")
                 (cursor . "Skyblue")
                 (use-powerline-p . t)
                 (set-mode-line-faces-p . t)
                 (paren-highlight-style . expression)
                 (custom-faces-fn . (lambda ()))))))

         (themed-assoc (eval (cdr (assoc chosen-theme color-settings))))
         (color (cdr (assoc config-name themed-assoc))))
    color))

;; nice paren-style highlight, but with buffer local configuration ;)
(defun expression-style-show-paren ()
  "Make show-paren expression only for LISP modes."
  (make-variable-buffer-local 'show-paren-style)
  (setq show-paren-style (get-color-config 'paren-highlight-style)))
(add-hook 'emacs-lisp-mode-hook 'expression-style-show-paren)

;; make cursor type a bar
(modify-all-frames-parameters (list (cons 'cursor-type 'bar)))

;; ===========================
;; == frame config dispatch ==
;; ===========================
(defun config-frame-appearance (&optional frame)
  "Configure FRAME with specific settings for terminal or x."
  (interactive)
  (cl-flet
      ((load-appearance (frame force-reload)
                        (if (display-graphic-p frame)
                            (config-x-frame frame)
                          (config-xterm-256color-terminal-frame frame))))
    (if (called-interactively-p 'any)
        (load-appearance (selected-frame) t)
      (load-appearance frame nil))))

;; -- config frames
(defun config-x-frame (frame)
  "Configure x FRAME."

  (global-hl-line-mode 1)

  ;; frame is set as nil in face in order to work for every frame
  (set-face-attribute 'cursor nil :background (get-color-config 'cursor))

  (toggle-transparency frame)

  (setq chosen-theme chosen-x-theme)
  (load-theme chosen-theme t)

  (when (get-color-config 'use-powerline-p)
    (config-powerline))
  (when (get-color-config 'get-mode-line-faces-p)
    ;; frame is set to nil in face in order for it to run for all frames when a new frame is created
    (set-face-attribute 'mode-line nil
                        :background (get-color-config 'mode-line-background)
                        :foreground (get-color-config 'mode-line-foreground)))
  (funcall (get-color-config 'custom-faces-fn))

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

(defun config-powerline ()
  "Set up powerline faces for FRAME."
  (load-file (expand-file-name "vendor/powerline.el" user-emacs-directory))

  (setq powerline-arrow-shape 'arrow-14
        powerline-color1 (get-color-config 'powerline-arrow)
        powerline-color2 (get-color-config 'powerline-other)
        powerline-column 50)

  (set-face-attribute 'mode-line nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :box nil))

;; -- xterm256colors terminal frame --
(defun config-xterm-256color-terminal-frame (frame)
  "Set specific faces for a 256 color terminal FRAME."
  (load-theme chosen-terminal-theme t)

  (set-face-attribute 'helm-selection frame :background "black")
  (set-face-attribute 'helm-ff-directory frame :background "black")

  (set-face-attribute 'magit-diff-add nil :foreground "white" :background "#005f00"))

;; -- Terminal frame --
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


;; load the configuration
(add-hook 'after-make-frame-functions 'config-frame-appearance)

(provide 'init-appearance)
;;; init-appearance.el ends here
