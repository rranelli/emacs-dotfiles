;;; Package --- Summary
;;; Commentary:
;;; Code:
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq rr/initialization-errors nil)

(require 'init-bootstrap)
(rr/safe-load-init-files)

;; Finish!
(message "======================================")
(message (if rr/initialization-errors
             (mapconcat #'identity rr/initialization-errors "\n")
           "All is sane, and init.el got to its end"))
(message "======================================")
;;; init.el ends here
