;;; init-appearance.el -- Configures some helpers for fast color scheme changing.
;;; Commentary:
;;; Code:
(unicode-fonts-setup)
(defvar rr/theme-loaded nil)

;; themes
(defcustom rr/chosen-theme 'solarized-dark ;; 'dichromacy
  "Theme chosen to be initialized."
  :group 'init-appearance)

(defcustom min-transp '(90 90)
  "Minimum transparency for Emacs in X."
  :group 'init-appearance)

(defcustom rr/theme-custom-color-alist
  '((dichromacy . '((cursor . "Red")
                    (paren-highlight-style . parenthesis)
                    (custom-faces-fn . (lambda ()
                                         (set-face-attribute 'fringe nil :background nil)))))

    (zenburn . '((cursor . "Red")
		 (set-mode-line-faces-p . nil)
		 (paren-highlight-style . parenthesis)
		 (custom-faces-fn . (lambda ()
				      (set-face-attribute 'fringe nil
                                                          :background nil)
                                      (set-face-attribute 'default nil
                                                          :height 140)
                                      (set-face-attribute 'mode-line-inactive nil
                                                          :box nil
                                                          :foreground "#8FB28F")

                                      (set-face-attribute 'markdown-code-face nil
                                                          :background nil )

                                      (set-face-attribute 'mode-line nil
                                                          :box '(:line-width -1 :style raised)
                                                          :foreground "#8FB28F")
                                      ))))

    (gruvbox . '((mode-line-background . "peru")
		 (mode-line-foreground . "snow")
		 (cursor . nil)
		 (set-mode-line-faces-p . t)
		 (paren-highlight-style . parenthesis)
		 (custom-faces-fn . (lambda ()
                                      (set-face-attribute 'hl-line nil
                                                          :background nil)
                                      (set-face-attribute 'hl-line nil
                                                          :box '(:line-width -2 :color "gray30" :style raised))
				      (set-face-attribute 'helm-selection nil
							  :background "gray20")
				      (set-face-attribute 'helm-ff-directory nil
							  :background "#282828"
							  :foreground "#fb4934")
                                      (set-default-font "Fira Code Bold 14")))))


    (solarized-dark . '((mode-line-background . "DeepSkyBlue4")
			(mode-line-foreground . "snow")
			(cursor . "SkyBlue")
                        (set-face-attribute 'default nil :height 120)
			(set-mode-line-faces-p . t)
			(paren-highlight-style . parenthesis)
			(custom-faces-fn . (lambda ()
                                             (set-default-font "Fira Code Bold 14"))))))
  "Custom color configuration."
  :group 'init-appearance)

;; configuration
(defun get-color-config (config-name)
  "Gets the configuration from the config list by CONFIG-NAME."
  (let* ((themed-assoc (eval (cdr (assoc rr/chosen-theme rr/theme-custom-color-alist))))
         (color (cdr (assoc config-name themed-assoc))))
    color))

;; nice paren-style highlight, but with buffer local configuration ;)
(defun expression-style-show-paren ()
  "Make show-paren expression only for LISP modes."
  (make-variable-buffer-local 'show-paren-style)
  (setq show-paren-style (get-color-config 'paren-highlight-style)))
(add-hook 'emacs-lisp-mode-hook 'expression-style-show-paren)

;; -- config frames
(defun config-frame-appearance (&optional frame)
  "Configure x FRAME."
  (unless rr/theme-loaded (load-theme rr/chosen-theme t))

  (modify-all-frames-parameters `((cursor-type . bar)))

  (rr/toggle-transparency (or frame (selected-frame)) t)

  (when (get-color-config 'cursor)
    (set-face-attribute 'cursor nil
                        :background (get-color-config 'cursor)))

  (when (get-color-config 'set-mode-line-faces-p)
    ;; frame is set to nil in face in order for it to run for all frames when a new frame is created
    (set-face-attribute 'mode-line nil
                        :background (get-color-config 'mode-line-background)
                        :foreground (get-color-config 'mode-line-foreground)))

  (funcall (get-color-config 'custom-faces-fn))

  (setq rr/theme-loaded t))

(defun rr/toggle-transparency (&optional frame force-transp)
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

;; load the configuration
(add-hook 'after-make-frame-functions 'config-frame-appearance)
(add-hook 'after-init-hook 'config-frame-appearance)

(provide 'init-appearance)
;;; init-appearance.el ends here
