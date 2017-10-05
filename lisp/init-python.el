;;; init-python.el -- Configures python development features
;;; Commentary:
;;; Code:

;; run this command to install required dependencies:
;; `pip install jedi flake8 importmagic autopep8 ipython rope yapf`'
(require 'elpy)

(setq elpy-rpc-backend "jedi")
(delete 'elpy-module-flymake elpy-modules)

(elpy-enable)

(setq python-shell-interpreter "ipython"
      python-shell-interpreter-args "--simple-prompt -i")

(setq py-shell-switch-buffers-on-execute-p t) ; switch to the interpreter after executing code
(setq py-switch-buffers-on-execute-p t)
(setq py-split-windows-on-execute-p nil) ; don't split windows
(setq py-smart-indentation t) ; try to automagically figure out indentation

(define-key python-mode-map (kbd "C-c i") 'elpy-autopep8-fix-code)
(define-key python-mode-map (kbd "C-c C-d") 'elpy-doc)

(define-key elpy-mode-map (kbd "C-c C-e") 'python-shell-send-region)

(add-hook 'elpy-mode-hook (lambda ()
                            (flymake-mode -1)
                            (flycheck-mode 1)))

(rr/expose-bindings elpy-mode-map '("M-<up>" "M-<down>" "C-c C-f"))

(provide 'init-python)
;;; init-python.el ends here
