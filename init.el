;;; Package --- Summary
;;; Commentary:
;;; Code:
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'init-bootstrap)
(rr-safe-load-init-files)

;; Finish!
(message
 "======================================
All is sane, and init.el got to its end
========================================")
;;; init.el ends here
