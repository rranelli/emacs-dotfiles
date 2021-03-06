;;; init-custom-defuns.el -- Adds simple and useful functions that have nowhere else to go.
;;; Commentary:
;;; Code:

(defmacro rr/format-symbol (template &rest symbols)
  "Format a symbol out of a TEMPLATE and other SYMBOLS.
Works just like `format' formats a string from strings.

The symbol is returned using `intern'"
  `(intern (format ,template ,@(mapcar 'symbol-name symbols))))

;; -- window management --
(defun rr/vsplit-last-buffer ()
  "Vertically split window showing last buffer."
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun rr/hsplit-last-buffer ()
  "Horizontally split window showing last buffer."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun rr/swap-buffers-in-windows ()
  "Put the buffer from the selected window in next window, and vice versa."
  (interactive)
  (let* ((this (selected-window))
	 (other (next-window))
	 (this-buffer (window-buffer this))
	 (other-buffer (window-buffer other)))
    (set-window-buffer other this-buffer)
    (set-window-buffer this other-buffer)))

(defun rr/ask-before-killing-frame ()
  "Ask for confirmation when killing the current frame."
  (interactive)
  (if (y-or-n-p (format "Are you sure you want to kill this frame? "))
      (save-buffers-kill-terminal)
    (message "Thank you ;)")))

;; -- misc --
(defun rr/nxml-pretty-format (begin end)
  "Pretty print xml in region BEGIN to END."
  (interactive "r")
  (save-excursion
    (shell-command-on-region (point-min) (point-max) "xmllint --format -" (buffer-name) t)
    (indent-region begin end)))

(defun rr/narrow-or-widen-dwim (p)
  "If the buffer is narrowed, it widens. Otherwise, it narrows intelligently.
Intelligently means: region, org-src-block, org-subtree, or defun,
whichever applies first.
Narrowing to org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer is already
narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p))
	 (widen))
	((region-active-p)
	 (narrow-to-region (region-beginning) (region-end)))
	((and (boundp 'org-src-mode) org-src-mode (not p)) ; <-- Added
	 (org-edit-src-exit))
	((derived-mode-p 'org-mode)
	 (cond ((org-in-src-block-p)
		(org-edit-src-code))
	       ((org-at-block-p)
		(org-narrow-to-block))
	       (t (org-narrow-to-subtree))))
	(t (narrow-to-defun))))

(defun rr/insert-lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
	  "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
	  "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
	  "aliquip ex ea commodo consequat. Duis aute irure dolor in "
	  "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
	  "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
	  "culpa qui officia deserunt mollit anim id est laborum."))

;;
;;; Fix line endings encodings
;;
(defun rr/convert-to-unix-file ()
  "Change the current buffer to Latin 1 with Unix line-ends."
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-unix t))

(defun rr/convert-to-dos-file ()
  "Change the current buffer to Latin 1 with DOS line-ends."
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-dos t))

(defun rr/convert-to-mac-file ()
  "Change the current buffer to Latin 1 with Mac line-ends."
  (interactive)
  (set-buffer-file-coding-system 'iso-latin-1-mac t))

(defun rr/convert-to-utf8-file ()
  "Change the current buffer to Latin 1 with Unix line-ends."
  (interactive)
  (set-buffer-file-coding-system 'utf-8 t))

;; don't know why, but starter kit added this monkey patch
(defun vc-git-annotate-command (file buf &optional rev)
  (let ((name (file-relative-name file)))
    (vc-git-command buf 0 name "blame" "-w" rev)))

(defmacro rr/toggle-env (env-var)
  `(defun ,(intern (concat "rr/toggle-" env-var)) ()
     (interactive)
     (-> (getenv ,env-var)
         (equal "true")
         (unless "true")
         (->> (setenv ,env-var)
              (format "%s set to %s" ,env-var))
         (message))))

(defun rr/insert-today ()
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

(defun rr/insert-now ()
  (interactive)
  (insert (shell-command-to-string "echo -n $(date)")))

(defun rr/send-window-to-other-frame ()
  (interactive)
  (let ((bname (buffer-name)))
    (delete-window)
    (other-frame 1)
    (switch-to-buffer bname)))

(defun rr/display-ansi-colors ()
  (interactive)
  (require 'ansi-color)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun rr/setup-presentation ()
  (interactive)

  (require 'inf-ruby)
  (require 'org)
  (setq-default mode-line-format nil)
  (with-current-buffer (get-buffer " *Echo Area 0*")
    (setq-local face-remapping-alist '((default (:height 0) variable-pitch))))
  (rr/set-transparency 100)

  (setq global-prettify-symbols-mode nil)
  (set-frame-parameter nil 'internal-border-width 35)

  (set-face-attribute 'default nil
                      :height 290)
  (set-face-attribute 'org-level-1 nil
                      :height 450)
  (set-face-attribute 'org-level-2 nil
                      :height 400)
  (set-face-attribute 'org-level-3 nil
                      :height 350)
  (defun rr/next-slide (p)
    (interactive "P")
    (rr/narrow-or-widen-dwim p)
    (outline-next-visible-heading 1)
    (org-show-subtree)
    (rr/narrow-or-widen-dwim p))

  (defun rr/previous-slide (p)
    (interactive "P")
    (rr/narrow-or-widen-dwim p)
    (outline-previous-visible-heading 1)
    (org-show-subtree)
    (rr/narrow-or-widen-dwim p))

  (defun rr/org-babel-tangle-block ()
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'org-babel-tangle)
      (shell-command-to-string "cd '/home/renan/Dropbox/empex' && mix compile >/dev/null")))

  (defun rr/send-ruby-buffer-and-go ()
    (interactive)
    (unless (get-buffer "*ruby*")
      (save-window-excursion
        (inf-ruby)))
    (ruby-send-buffer)
    (switch-to-buffer "*ruby*"))

  (define-key inf-ruby-minor-mode-map (kbd "C-c C-c") 'rr/send-ruby-buffer-and-go)

  (define-key org-mode-map (kbd "C-c c") 'rr/org-babel-tangle-block)
  (define-key org-mode-map (kbd "<f5>") 'rr/next-slide)
  (define-key org-mode-map (kbd "<f4>") 'rr/previous-slide)
  (define-key org-mode-map (kbd "C-x C-n") 'rr/next-slide)
  (define-key org-mode-map (kbd "C-x C-p") 'rr/previous-slide)
  (define-key org-mode-map (kbd "<next>") 'rr/next-slide)
  (define-key org-mode-map (kbd "<prior>") 'rr/previous-slide)
  (define-key org-src-mode-map (kbd "C-x n") 'org-edit-src-exit)

  (global-set-key (kbd "C-x C-n") 'rr/next-slide)
  (global-set-key (kbd "C-x C-p") 'rr/previous-slide)

  (global-flycheck-mode -1)
  (show-smartparens-global-mode -1)

  (rr/disable-elixir-pretty-symbols)
  (rr/disable-python-pretty-symbols))

(provide 'init-custom-defuns)
;;; init-custom-defuns.el ends here
