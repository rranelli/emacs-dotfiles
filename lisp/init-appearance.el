;;; init-appearance.el -- Configures some helpers for fast color scheme changing.
;;; Commentary:
;;; Code:

;; themes
(defcustom chosen-x-theme 'sanityinc-tomorrow-night
  "Theme chosen to be initialized."
  :group 'init-appearance)
(defcustom chosen-terminal-theme 'gruvbox
  "Theme chosen to be initialized in terminal sessions."
  :group 'init-appearance)
(defvar chosen-theme chosen-x-theme
  "Chosen theme to be used at the config loaders.")

(defcustom min-transp '(87 87)
  "Minimum transparency for Emacs in X."
  :group 'init-appearance)

(defcustom rr/theme-custom-color-alist
  '((sanityinc-tomorrow-night . '((custom-faces-fn . (lambda ()
                                                       (set-face-attribute 'fringe nil
                                                                           :background nil)
                                                       (set-face-attribute 'linum nil
                                                                           :background nil)
                                                       (rr/setup-telephone-line)

                                                       (set-face-attribute 'telephone-line-accent-active nil
                                                                           :background "#b294bb"
                                                                           :foreground "black"
                                                                           :box nil)

                                                       (set-face-attribute 'mode-line-inactive nil
                                                                           :box nil
                                                                           :foreground "#b294bb"
                                                                           :background "#373b41")

                                                       (set-face-attribute 'mode-line nil
                                                                           :box '(:line-width -1 :style raised)
                                                                           :foreground "#b294bb"
                                                                           :background "#373b41")

                                                       (set-face-attribute 'default nil
                                                                           :height 140)

                                                       (set-default-font "Fira Code Bold")

                                                       (rr/set-transparency 70)))))
    (dichromacy . '((cursor . "Red")
                    (paren-highlight-style . parenthesis)
                    (custom-faces-fn . (lambda ()
                                         (set-face-attribute 'fringe nil
                                                             :background nil)
                                         (set-face-attribute 'default nil
                                                             :height 130)
                                         (rr/setup-telephone-line)))))
    (zenburn . '((cursor . "Red")
		 (set-mode-line-faces-p . nil)
		 (paren-highlight-style . parenthesis)
		 (custom-faces-fn . (lambda ()
				      (set-face-attribute 'fringe nil
                                                          :background nil)
                                      (set-face-attribute 'default nil
                                                          :height 130)
                                      (rr/setup-telephone-line)

                                      ;; telephone line faces !!
                                      (set-face-attribute 'telephone-line-accent-active nil
                                                          :background "#8FB28F"
                                                          :foreground "black"
                                                          :box nil)

                                      (set-face-attribute 'mode-line-inactive nil
                                                          :box nil
                                                          :foreground "#8FB28F")

                                      (set-face-attribute 'mode-line nil
                                                          :box '(:line-width -1 :style raised)
                                                          :foreground "#8FB28F")))))
    (gruvbox . '((mode-line-background . "peru")
		 (mode-line-foreground . "snow")
		 (cursor . nil)
		 (set-mode-line-faces-p . t)
		 (paren-highlight-style . parenthesis)
		 (custom-faces-fn . (lambda ()
				      (set-face-attribute 'helm-selection nil
							  :background "gray20")
				      (set-face-attribute 'helm-ff-directory nil
							  :background "#282828"
							  :foreground "#fb4934")
				      (set-face-attribute 'wl-highlight-summary-refiled-face nil
							  :foreground "dark turquoise")))))

    (solarized-dark . '((mode-line-background . "DeepSkyBlue4")
			(mode-line-foreground . "snow")
			(cursor . "SkyBlue")
			(set-mode-line-faces-p . t)
			(paren-highlight-style . expression)
			(custom-faces-fn . ignore)))

    (tsdh-dark . '((mode-line-background . "IndianRed4")
		   (mode-line-foreground . "SlateGray1")
		   (cursor . "SkyBlue")
		   (set-mode-line-faces-p . t)
		   (paren-highlight-style . expression)
		   (custom-faces-fn . (lambda ()
                                        (set-face-attribute 'mode-line-inactive nil
                                                            :background "gray33")
                                        (set-face-attribute 'fringe nil
                                                            :background nil)
                                        (set-face-attribute 'header-line nil
                                                            :box nil
                                                            :inverse-video nil)
                                        (set-face-attribute 'sp-pair-overlay-face nil
                                                            :foreground "khaki1")
                                        (set-face-attribute 'yas-field-highlight-face nil
                                                            :foreground "khaki1")
                                        (set-face-attribute 'font-lock-type-face nil
                                                            :foreground "khaki1")
                                        (set-face-attribute 'font-lock-comment-face nil
                                                            :foreground "gray50")
                                        (set-face-attribute 'helm-selection nil
                                                            :background "black")
                                        (set-face-attribute 'helm-ff-dotted-directory nil
                                                            :foreground "white"
                                                            :background "nil")
                                        (set-face-attribute 'minibuffer-prompt nil
                                                            :foreground "white"
                                                            :background nil
                                                            :box nil)
                                        (set-face-attribute 'helm-candidate-number nil
                                                            :foreground nil
                                                            :background nil)
                                        (set-face-attribute 'helm-ff-directory nil
                                                            :background "#282828"
                                                            :foreground "#fb4934"))))))
  "Custom color configuration."
  :group 'init-appearance)

;; configuration
(defun get-color-config (config-name)
  "Gets the configuration from the config list by CONFIG-NAME."
  (let* ((themed-assoc (eval (cdr (assoc chosen-theme rr/theme-custom-color-alist))))
         (color (cdr (assoc config-name themed-assoc))))
    color))

;; nice paren-style highlight, but with buffer local configuration ;)
(defun expression-style-show-paren ()
  "Make show-paren expression only for LISP modes."
  (make-variable-buffer-local 'show-paren-style)
  (setq show-paren-style (get-color-config 'paren-highlight-style)))
(add-hook 'emacs-lisp-mode-hook 'expression-style-show-paren)

;;
;;; frame config dispatch
;;
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

  (setq chosen-theme chosen-x-theme)
  (load-theme chosen-theme t)

  ;; make cursor type a bar
  (modify-all-frames-parameters `((cursor-type . bar)))

  (toggle-transparency frame t)

  (when (get-color-config 'cursor)
    (set-face-attribute 'cursor nil
                        :background (get-color-config 'cursor)))
  (when (get-color-config 'set-mode-line-faces-p)
    ;; frame is set to nil in face in order for it to run for all frames when a new frame is created
    (set-face-attribute 'mode-line nil
                        :background (get-color-config 'mode-line-background)
                        :foreground (get-color-config 'mode-line-foreground)))
  (funcall (get-color-config 'custom-faces-fn))

  (setq theme-loaded t))

(defun toggle-transparency (&optional frame force-transp)
  "Toggle frame transparency for FRAME.
Use selected frame if frame not given.
If FORCE-TRANSP is non-nil, sets transparency to the custom variable min-transp."
  (interactive)
  (cl-labels ((other--thing (thing first second)
                            (if (equal thing first) second first))
              (toggle--transparency (frame min-transp max-transp)
                                    (let ((transp (frame-parameter frame 'alpha)))
                                      (if force-transp
                                          (set-frame-parameter frame 'alpha min-transp)
                                        (set-frame-parameter frame 'alpha (other--thing transp min-transp max-transp))))))
    (let ((max-transp '(100 100)))
      (if (called-interactively-p 'any)
          (toggle--transparency (selected-frame) min-transp max-transp)
        (toggle--transparency frame min-transp max-transp)))))

(defun rr/set-transparency (&optional desired-transp frame)
  (interactive)
  (unless frame
    (setq frame (selected-frame)))
  (unless desired-transp
    (setq desired-transp (read-number "Desired transparency: " (car min-transp))))
  (set-frame-parameter frame 'alpha `(,desired-transp ,desired-transp)))

;; -- xterm256colors terminal frame --
(defun config-xterm-256color-terminal-frame (frame)
  "Set specific faces for a 256 color terminal FRAME."
  (load-theme chosen-terminal-theme t)

  (remove-hook 'prog-mode-hook 'linum-mode)

  (set-face-attribute 'helm-selection frame
                      :background "black")
  (set-face-attribute 'helm-ff-directory frame
                      :background "black")

  (set-face-attribute 'magit-diff-add nil
                      :foreground "white"
                      :background "#005f00"))

;; -- Terminal frame --
(defun config-terminal-frame (frame)
  "Configure terminal FRAME."
  (set-terminal-faces frame))

(defun set-terminal-faces (frame)
  "Set specific faces for a terminal FRAME.  This thing is hard as hell."
  (set-face-attribute 'helm-selection frame
                      :background "black")
  (set-face-attribute 'helm-match frame
                      :foreground "blue")
  (set-face-attribute 'helm-source-header frame
                      :foreground "white"
                      :background "blue")

  (set-face-attribute 'mode-line frame
                      :foreground "black"
                      :background "white"
                      :box nil)

  (set-face-attribute 'magit-branch frame
                      :background "black")
  (set-face-attribute 'magit-log-head-label-remote frame
                      :foreground "black")
  (set-face-attribute 'magit-log-head-label-local frame
                      :foreground "red"
                      :background "black"))

(defun rr/setup-telephone-line ()
  "Set up telephone line !"
  (interactive)

  (require 'telephone-line)
  (telephone-line-defsegment* rr/telephone-line-buffer-segment
    `("" mode-line-remote " "
      ,(telephone-line-raw mode-line-buffer-identification t)))

  (telephone-line-defsegment* rr/telephone-line-projectile-project-name
    `(""
      ,(telephone-line-raw (ignore-errors (format "prj:%s" (projectile-project-name))))))

  (setq telephone-line-lhs
        '((accent . (rr/telephone-line-buffer-segment
                     telephone-line-process-segment
                     telephone-line-erc-modified-channels-segment))
          (nil . (telephone-line-vc-segment
                  rr/telephone-line-projectile-project-name))))

  (setq telephone-line-rhs
        '((nil    . (telephone-line-misc-info-segment))
          (accent . (telephone-line-major-mode-segment
                     telephone-line-position-segment))))

  (setq telephone-line-primary-left-separator 'telephone-line-cubed-left
        telephone-line-left-separator 'telephone-line-cubed-left
        telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left)

  (setq telephone-line-primary-right-separator 'telephone-line-cubed-right
        telephone-line-right-separator 'telephone-line-cubed-right
        telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-right)

  (telephone-line-mode 1))

;; load the configuration
(add-hook 'after-make-frame-functions 'config-frame-appearance)
(add-hook 'after-init-hook 'config-frame-appearance)

(provide 'init-appearance)
;;; init-appearance.el ends here
