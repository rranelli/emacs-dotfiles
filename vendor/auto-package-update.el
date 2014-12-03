;;; package -- Summary
;;; Commentary:
;;; Code:
(require 'package)


;;
;;; Customization
;;
(defcustom apu-last-update-day-filename
  ".last-package-update-day"
  "Name of the file in which the last update day is going to be stored."
  :group 'init-packages
  :type 'string)

(defcustom apu-package-update-interval
  7
  "Interval in DAYS for automatic package update."
  :group 'init-packages
  :type 'int)

(defvar apu-last-update-day-path
  (expand-file-name apu-last-update-day-filename user-emacs-directory))

;;
;;; File read/write
;;
(defun apu-read-file-contents (file)
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-string))))

(defun apu-write-string-to-file (file string)
  (with-temp-buffer
    (insert string)
    (when (file-writable-p file)
      (write-region (point-min)
		    (point-max)
		    file))))

;;
;;; Update day read/write functions
;;
(defun apu-today-day ()
  (time-to-days (current-time)))

(defun apu-write-current-day ()
  (apu-write-string-to-file
   apu-last-update-day-path
   (int-to-string (apu-today-day))))

(defun apu-read-last-update-day ()
  (string-to-int
   (apu-read-file-contents apu-last-update-day-path)))

;;
;;; Package update
;;
(defun apu-should-update-packages-p ()
  (or
   (not (file-exists-p apu-last-update-day-path))
   (let* ((last-update-day (apu-read-last-update-day))
	  (days-since (- (apu-today-day) last-update-day)))
     (and (not (= last-update-day (apu-today-day)))
	  (=
	   0
	   (mod days-since apu-package-update-interval))))))

;;;###autoload
(defun apu-update-packages ()
  "Update installed Emacs packages."
  (interactive)
  (save-excursion
    (package-refresh-contents)
    (package-list-packages)
    (package-menu-mark-upgrades)
    (package-menu-execute t)
    (kill-buffer)))

(defun apu-update-packages-if-needed ()
  (when (apu-should-update-packages-p)
    (apu-update-packages)
    (apu-write-current-day)
    (message "[PACKAGES UPDATED]")))

(provide 'auto-package-update)
;;; auto-package-update.el ends here
