;;; init-custom-defuns.el -- Adds simple and useful functions that have nowhere else to go.
;;; Commentary:
;;; Code:

(defmacro rr/format-symbol (template &rest symbols)
  "Format a symbol out of a TEMPLATE and other SYMBOLS.
Works just like `format' formats a string from strings.

The symbol is returned using `intern'"
  `(intern (format ,template ,@(mapcar 'symbol-name symbols))))

;; -- window management --
(defun vsplit-last-buffer ()
  "Vertically split window showing last buffer."
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun hsplit-last-buffer ()
  "Horizontally split window showing last buffer."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun swap-buffers-in-windows ()
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
(defun nxml-pretty-format (begin end)
  "Pretty print xml in region BEGIN to END."
  (interactive "r")
  (save-excursion
    (shell-command-on-region (point-min) (point-max) "xmllint --format -" (buffer-name) t)
    (indent-region begin end)))

(defun narrow-or-widen-dwim (p)
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

(defun insert-lorem ()
  "Insert a lorem ipsum."
  (interactive)
  (insert "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do "
	  "eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim"
	  "ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut "
	  "aliquip ex ea commodo consequat. Duis aute irure dolor in "
	  "reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla "
	  "pariatur. Excepteur sint occaecat cupidatat non proident, sunt in "
	  "culpa qui officia deserunt mollit anim id est laborum."))

(defun rr/restclient-scratch ()
  "Create a new restclient scratch buffer."
  (interactive)
  (switch-to-buffer
   (get-buffer-create "*restclient-scratch*"))
  (restclient-mode))

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

(defun rr/send-window-to-other-frame ()
  (interactive)
  (let ((bname (buffer-name)))
    (delete-window)
    (other-frame 1)
    (switch-to-buffer bname)))

(defun rr/write-string (string file)
  (with-temp-buffer
    (insert string)
    (write-region (point-min) (point-max) file)))

(defun rr/project-root ()
  "Return project name from directory."
  (if (projectile-project-p)
      (projectile-project-root)
    default-directory))

(defun rr/project-name ()
  "Return project name from directory."
  (-> (rr/project-root)
      (directory-file-name)
      (file-name-base)))

(global-set-key (kbd "C-c m") 'rr/helm-mimipass)
(defun rr/helm-mimipass ()
  "Helm interface to `mimipass copy'."
  (interactive)
  (helm :sources '((name . "Mimipass")
                   (match . helm-mm-match)
                   (candidates . (lambda ()
                                   (->> "mimipass list 2>/dev/null | cut -c 3-"
                                        (shell-command-to-string)
                                        (split-string))))
                   (action     . (lambda (selection)
                                   (->> selection
                                        (format "mimipass get %s 2>/dev/null")
                                        (shell-command-to-string)
                                        (gui-set-selection 'CLIPBOARD))
                                   selection))
                   (persistent-action . helm-yank-selection))
        :prompt "Select password: "
        :buffer "*helm-mimipass*"))

(defun rr/ls (dir &optional match)
  "List all files in directory DIR.
If MATCH regexp is given, return only the files that match it"
  (directory-files dir t (or match directory-files-no-dot-files-regexp)))

(defun rr/setup-presentation ()
  (interactive)

  (set-face-attribute 'default nil
                      :height 280)
  (set-face-attribute 'org-level-1 nil
                      :height 500)
  (set-face-attribute 'org-level-2 nil
                      :height 450)
  (set-face-attribute 'org-level-3 nil
                      :height 400)
  (setq-default mode-line-format nil)

  (defun rr/next-slide (p)
    (interactive "P")
    (narrow-or-widen-dwim p)
    (outline-next-visible-heading 1)
    (org-show-subtree)
    (narrow-or-widen-dwim p))

  (defun rr/previous-slide (p)
    (interactive "P")
    (narrow-or-widen-dwim p)
    (outline-previous-visible-heading 1)
    (org-show-subtree)
    (narrow-or-widen-dwim p))

  (defun rr/clear-with-separation ()
    (interactive)
    (save-excursion
      (mark-whole-buffer)
      (delete-region (point) (mark))
      (insert "--- compiled stuff will show up above this line 🖢🖢🖢\n"))
    (goto-char (point-max)))

  (defun rr/org-babel-tangle-block ()
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'org-babel-tangle)
      (shell-command-to-string "cd '/home/renan/Dropbox/empex' && mix compile >/dev/null")))

  (define-key org-mode-map (kbd "C-c c") 'rr/org-babel-tangle-block)
  (define-key org-mode-map (kbd "<f5>") 'rr/next-slide)
  (define-key org-mode-map (kbd "<f4>") 'rr/previous-slide)
  (define-key org-mode-map (kbd "C-x C-n") 'rr/next-slide)
  (define-key org-mode-map (kbd "C-x C-p") 'rr/previous-slide)

  (global-set-key (kbd "C-l") 'rr/clear-with-separation))
  (global-set-key (kbd "C-x C-n") 'rr/next-slide)
  (global-set-key (kbd "C-x C-p") 'rr/previous-slide))

(provide 'init-custom-defuns)
;;; init-custom-defuns.el ends here
