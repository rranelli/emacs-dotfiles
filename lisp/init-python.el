;;; init-python.el -- Configures python development features
;;; Commentary:
;;; Code:

;; run this command to install required dependencies:
;; `pip install jedi flake8 importmagic autopep8`'
(when (require 'elpy nil t)
  (elpy-enable))

(setq elpy-rpc-backend "jedi")

(provide 'init-python)
;;; init-python.el ends here
