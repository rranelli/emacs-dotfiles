;;; init-python.el -- Configures python development features
;;; Commentary:
;;; Code:

;; run this command to install required dependencies:
;; `sudo pip3 install jedi flake8 importmagic autopep8 ipython rope`'
(require 'elpy)

(setq elpy-rpc-backend "jedi")
(elpy-enable)

(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")
(setq py-python-command-args ; use the wx backend, for both mayavi and matplotlib
  '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
(setq py-force-py-shell-name-p t)
(setq py-shell-switch-buffers-on-execute-p t) ; switch to the interpreter after executing code
(setq py-switch-buffers-on-execute-p t)
(setq py-split-windows-on-execute-p nil) ; don't split windows
(setq py-smart-indentation t) ; try to automagically figure out indentation

(define-key python-mode-map (kbd "C-c i") 'elpy-autopep8-fix-code)
(define-key python-mode-map (kbd "C-c C-d") 'elpy-doc)

(add-hook 'elpy-mode-hook (lambda ()
                            (flymake-mode -1)
                            (flycheck-mode 1)))

(provide 'init-python)
;;; init-python.el ends here
