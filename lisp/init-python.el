;;; init-python.el --- Summary
;;; Commentary:
;;; Code:
(use-package rr-pytest-mode
  :ensure nil
  :diminish

  :hook
  (python-mode . rr/pytest-mode))

(use-package python-mode
  :mode "\\.py"

  :custom
  (python-shell-interpreter "ipython")
  (python-shell-interpreter-args "--simple-prompt -i")
  (py-shell-switch-buffers-on-execute-p t)
  (py-switch-buffers-on-execute-p t)
  (py-split-windows-on-execute-p nil)
  (py-smart-indentation t)
  (rr/python-symbols '(;; Syntax
                       ("==" .       #x2a75)
                       ("!=" .       #x2260)
                       ("->" .       #x27f6)
                       ("<=" .       #x2a7d)
                       (">=" .       #x2a7e)
                       ("not" .      #x2757)
                       ("in" .       #x2208)
                       ("not in" .   #x2209)
                       ("lambda" .   #x1d6cc)
                       ("for" .      #x2200)
                       ("return" .   #x27fc)
                       ("yield" .    #x27fb)
                       ("raise" .    #x1f4a3)
                       ("pass" .     #x26d2)
                       ;; definitions
                       ("@" .        #xff20)
                       ("async" .    #x1f5d8)
                       ("await" .    #x2b33)
                       ("def" .      #x2131)
                       ("class" .    #x1d49e)
                       ("self" . (?𝔰 (Br . Bl) ?𝔢 (Br . Bl) ?𝔩 (Br . Bl) ?𝔣))
                       ("from" .     #x2abc)
                       ("import" .   #x2abb)
                       ;; Base Types
                       ("int" .      #x2124)
                       ("float" .    #x211d)
                       ("str" .      #x1d54a)
                       ("True" .     #x1d54b)
                       ("False" .    #x1d53d)
                       ("None" .     #x2205)
                       ;; Mypy
                       ("Dict" .     #x1d507)
                       ("List" .     #x2112)
                       ("Tuple" .    #x2a02)
                       ("Set" .      #x2126)
                       ("Iterable" . #x1d50a)
                       ("Any" .      #x2754)
                       ("Union" .    #x22c3)
                       ("Type" .     ?𝓣)))

  :bind
  (:map python-mode-map
        ("C-c C-e" . python-shell-send-region))

  :hook
  (python-mode . prettify-symbols-mode)
  (python-mode . rr/set-prettify-symbols)

  :config
  (defun rr/disable-python-pretty-symbols ()
    (interactive)
    (remove-hook 'python-mode-hook 'prettify-symbols-mode))
  (defun rr/set-prettify-symbols ()
    (setq prettify-symbols-alist rr/python-symbols))
  (rr/expose-default-bindings python-mode-map))

(use-package elpy
  ;; install-it: `pip install jedi flake8 importmagic autopep8 ipython rope yapf epc`'
  :custom
  (elpy-rpc-backend "rope")

  :bind
  (:map elpy-mode-map
        ("C-c i" . elpy-autopep8-fix-code)
        ("C-c C-d" . elpy-doc))

  :hook
  (python-mode . elpy-mode)
  (python-mode . elpy-enable)

  :config
  (delete 'elpy-module-flymake elpy-modules)
  (delete 'elpy-module-highlight-indentation elpy-modules)
  (rr/expose-default-bindings elpy-mode-map))

(provide 'init-python)
;;; init-python.el ends here
