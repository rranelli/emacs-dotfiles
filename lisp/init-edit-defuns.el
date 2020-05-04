;;; init-edit-defuns.el -- General functions to customize text editing experience.
;;; Commentary:
;;; Code:

;; -- movement --
(defun rr/backward-kill-word (arg)
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(defun rr/move-smart-beginning-of-line ()
  "Move to beginning of line or to beginning of indentation depending on POINT."
  (interactive)
  (if (= (point) (line-beginning-position))
      (back-to-indentation)
    (move-beginning-of-line nil)))

(defun rr/sudo-edit (&optional arg)
  "Edit file as sudo. ARG as point."
  (interactive "p")
  (find-file (concat "/sudo:root@localhost:" (buffer-file-name))))

(defun rr/show-file-name ()
  "Show the full path filename in the minibuffer."
  (interactive)
  (let ((text (format "%s:%i" (buffer-file-name) (line-number-at-pos))))
    (message text)
    (kill-new text)))

(defun rr/uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
	  (progn
	    (goto-char start)
	    (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
	(replace-match "\\1\n\\2")))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single
line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-beginning-position 2)))))

(defun uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (uniquify-all-lines-region (point-min) (point-max)))

(defun rr/strip-whitespace ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp "[\s\t]+" " " nil (point-min) (point-max)))
  (indent-region (point-min) (point-max)))

(provide 'init-edit-defuns)
;;; init-edit-defuns.el ends here
