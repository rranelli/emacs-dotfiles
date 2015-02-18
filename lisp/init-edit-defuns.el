;;; package -- summary
;;; Commentary:
;;; Code:

(defun sudo-edit (&optional arg)
  "Edit file as sudo. ARG as point."
  (interactive "p")
  (find-file (concat "/sudo:root@localhost:" (helm-read-file-name "File: "))))

(defun rr-show-file-name ()
  "Show the full path filename in the minibuffer."
  (interactive)
  (let ((text (format "%s:%i" (buffer-file-name) (line-number-at-pos))))
    (message text)
    (kill-new text)))

(defun uniquify-all-lines-region (start end)
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

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single
line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position) (line-beginning-position 2)))))

(defun uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (uniquify-all-lines-region (point-min) (point-max)))

(defun rr-strip-whitespace ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (replace-regexp "[\s\t]+" " " nil (point-min) (point-max)))
  (indent-region (point-min) (point-max)))

(provide 'init-edit-defuns)
;;; init-edit-defuns.el ends here
