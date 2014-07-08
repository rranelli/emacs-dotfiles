;;; package -- summary
;;; Commentary:
;;; Code:

;; -- movement --
(defun next5()
  "Move five lines forward."
  (interactive)
  (next-line 5))

(defun prev5()
  "Move five lines backward."
  (interactive)
  (previous-line 5))

(defun move-smart-begining-of-line ()
  (interactive)
  (if (= (point) (line-beginning-position))
      (back-to-indentation)
    (move-beginning-of-line nil)))

;; -- window management --
(defun maximize-window-vertically ()
  "Maximizes the current window vertically the same way vi does."
  (interactive)
  (enlarge-window 180 nil))

(defun minimize-window-vertically ()
  "Minimizes the current window vertically the same way vi does."
  (interactive)
  (shrink-window 180 nil))

(defun vsplit-last-buffer ()
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer)
  )

(defun hsplit-last-buffer ()
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer)
  )

(defun swap-buffers-in-windows ()
  "Put the buffer from the selected window in next window, and vice versa"
  (interactive)
  (let* ((this (selected-window))
         (other (next-window))
         (this-buffer (window-buffer this))
         (other-buffer (window-buffer other)))
    (set-window-buffer other this-buffer)
    (set-window-buffer this other-buffer)))

(defvar bury-compile-buffer nil)
(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer (as BUFFER) if succeeded without warnings (given by STRING argument)."
  (if (and
       bury-compile-buffer
       (string-match "compilation" (buffer-name buffer))
       (string-match "finished" string)
       (not (with-current-buffer buffer
              (goto-char 1)
              (search-forward "warning" nil t))))
      (run-with-timer
       1
       nil
       (lambda (buf)
         (if (get-buffer-window buf)
             (progn (delete-window (get-buffer-window buf))
                    (bury-buffer buf))))
       buffer)))

(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)

;; -- utilities --

;; shows current file name in the minibuffer
(defun show-file-name ()
  "Show the full path filename in the minibuffer."
  (interactive)
  (message (buffer-file-name))
  (kill-new (file-truename buffer-file-name)))

;; Uniquifty lines
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

(defun uniquify-all-lines-buffer ()
  "Delete duplicate lines in buffer and keep first occurrence."
  (interactive "*")
  (uniquify-all-lines-region (point-min) (point-max)))

(provide 'init-custom-defuns)
;;; init-custom-defuns.el ends here
