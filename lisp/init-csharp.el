;;; init-company.el -- Trolls the world by setting up a C# development environment.
;;; Commentary: Omfg, this is so frikin nice
;;; Code:
(require 'csharp-mode)

(setq omnisharp-server-executable-path "/home/renan/code/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe")

(rr/define-bindings csharp-mode-map
                    '(("M-/" . omnisharp-auto-complete)
                      ("M-." . omnisharp-go-to-definition)
                      ("C-4 M-." . omnisharp-go-to-definition-other-window)))

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-omnisharp))

(add-hook 'csharp-mode-hook 'omnisharp-mode)

(provide 'init-csharp)
;;; init-csharp.el ends here
