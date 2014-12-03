;;; package -- Summary
;;; Commentary:
;;; Code:
(require 'package)


;;
;;; Customization
;;
(defcustom rr-last-update-day-filename
  ".last-package-update-day"
  "Name of the file in which the last update day is going to be stored."
  :group 'init-packages
  :type 'string)

(defcustom rr-package-update-interval
  7
  "Interval in DAYS for automatic package update."
  :group 'init-packages
  :type 'int)

(defvar rr-last-update-day-path
  (expand-file-name rr-last-update-day-filename user-emacs-directory))

;;
;;; File read/write
;;
(defun rr-read-file-contents (file)
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

(defun rr-write-string-to-file (file string)
  (with-temp-buffer
    (insert string)
    (when (file-writable-p file)
      (write-region (point-min)
		    (point-max)
		    file))))

;;
;;; Update day read/write functions
;;
(defun rr-today-day ()
  (time-to-days (current-time)))

(defun rr-write-current-day ()
  (rr-write-string-to-file
   rr-last-update-day-path
   (int-to-string (rr-today-day))))

(defun rr-read-last-update-day ()
  (string-to-int
   (rr-read-file-contents rr-last-update-day-path)))

;;
;;; Package update
;;
(defun rr-should-update-packages-p ()
  (or
   (not (file-exists-p rr-last-update-day-path))
   (let* ((last-update-day (rr-read-last-update-day))
	  (days-since (- (rr-today-day) last-update-day)))
     (and (not (= last-update-day (rr-today-day)))
	  (=
	   0
	   (mod days-since rr-package-update-interval))))))

;;;###autoload
(defun rr-update-packages ()
  "Update installed Emacs packages."
  (interactive)
  (save-excursion
    (package-refresh-contents)
    (package-list-packages)
    (package-menu-mark-upgrades)
    (package-menu-execute t)
    (kill-buffer)))

(defun rr-update-packages-if-needed ()
  (when (rr-should-update-packages-p)
    (rr-update-packages)
    (rr-write-current-day)
    (message "[PACKAGES UPDATED]")))

(provide 'auto-package-update)
;;; auto-package-update.el ends here
