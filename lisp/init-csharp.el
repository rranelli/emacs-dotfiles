;;; init-csharp.el -- Troll the world doing C# development with Emacs.
;;; Commentary:
;;;   Omfg, this is so frikin nice
;;; Code:
(require 'csharp-mode)

(setq omnisharp-server-executable-path "/home/renan/code/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe"
      omnisharp-auto-complete-popup-help-delay 2)

(rr/define-bindings csharp-mode-map
                    '(("M-/"     . omnisharp-auto-complete)
                      ("M-."     . omnisharp-go-to-definition)
                      ("C-4 M-." . omnisharp-go-to-definition-other-window)
                      ("C-c i"   . omnisharp-code-format)
                      ("C-c C-c" . omnisharp-fix-code-issue-at-point)
                      ("C-c x"   . omnisharp-run-code-action-refactoring)))

(defun rr/omnisharp-rename-file ()
  "Rename current file and fix the file path in .csproj file."
  (interactive)
  (let ((new-file-name (read-file-name "new name: ")))
    (omnisharp-remove-from-project-current-file)
    (dired-rename-file (buffer-file-name)
                       new-file-name
                       nil)
    (omnisharp-add-to-solution-current-file)))

(eval-after-load 'company
  '(add-to-list 'company-backends 'company-omnisharp))

(add-hook 'csharp-mode-hook 'omnisharp-mode)
(add-hook 'csharp-mode-hook (lambda () (setq c-basic-offset 2)))

(provide 'init-csharp)
;;; init-csharp.el ends here
