;;; package -- summary
;;; Commentary:
;;; Code:

(defun next5()
  "Move five lines forward."
  (interactive)
  (next-line 5))

(defun prev5()
  "Move five lines backward."
  (interactive)
  (previous-line 5))

(defun delete-word (arg)
  "Delete ARG characters forward until encountering the end of a word."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  "Delete ARG characters backward until encountering the end of a word."
  (interactive "p")
  (delete-word (- arg)))

(defun whitespace-cleanup-all ()
  (interactive)
  (setq indent-tab-mode nil)
  (whitespace-cleanup))

(defun whitespace-clean-and-compile ()
  "Cleans up whitespace and compiles.  The 'compile-command' varies with the active mode."
  (interactive)
  (whitespace-cleanup-all)
  (compile compile-command))

(defun maximize-window-vertically ()
  "Maximizes the current window vertically the same way vi does."
  (interactive)
  (enlarge-window 180 nil))

(defun minimize-window-vertically ()
  "Minimizes the current window vertically the same way vi does."
  (interactive)
  (shrink-window 180 nil))

(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer (as BUFFER) if succeeded without warnings (given by STRING argument)."
  (if (and
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
       buffer)
    ))

(add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)

;; shows current file name in the minibuffer
(defun show-file-name ()
  "Show the full path filename in the minibuffer."
  (interactive)
  (message (buffer-file-name))
  (kill-new (file-truename buffer-file-name))
  )

;; makes new split buffer the buffer before current
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
    (set-window-buffer this other-buffer)
    )
  )

(provide 'init-custom-defuns)
;;; init-custom-defuns.el ends here
