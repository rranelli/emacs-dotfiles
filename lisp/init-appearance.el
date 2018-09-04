;;; init-appearance.el -- Configures some helpers for fast color scheme changing.
;;; Commentary:
;;; Code:
(unicode-fonts-setup)
(defvar rr/theme-loaded nil)

(use-package northcode-theme)

;; themes
(defvar rr/presenting? (getenv "KEYNOTEMACS"))
(defvar rr/presentation-theme 'dichromacy)

(defcustom rr/chosen-theme 'northcode
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
    (northcode . '((custom-faces-fn . (lambda ()
                                        (require 'hl-line)
                                        (set-face-attribute 'hl-line nil :background "gray21")
                                        (set-face-attribute 'vhl/default-face nil :background "gray")
                                        (set-face-attribute 'region nil :foreground "gray21")
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
(defun rr/expression-style-show-paren ()
  "Make show-paren expression only for LISP modes."
  (make-variable-buffer-local 'show-paren-style)
  (setq show-paren-style (get-color-config 'paren-highlight-style)))
(add-hook 'emacs-lisp-mode-hook 'rr/expression-style-show-paren)

;; -- config frames
(defun rr/config-frame-appearance-with-presentation-awareness (&optional frame)
  (if rr/presenting?
      (setq rr/chosen-theme rr/presentation-theme))
  (rr/config-frame-appearance frame))

(defun rr/config-frame-appearance (&optional frame)
  "Configure x FRAME."
  (interactive)
  (unless rr/theme-loaded (load-theme rr/chosen-theme t))

  ;; (modify-all-frames-parameters `((cursor-type . block)))

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
(add-hook 'after-make-frame-functions 'rr/config-frame-appearance-with-presentation-awareness)
(add-hook 'after-init-hook 'rr/config-frame-appearance-with-presentation-awareness)

(provide 'init-appearance)
;;; init-appearance.el ends here
