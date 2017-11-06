;;; init-csharp.el -- Troll the world doing C# development with Emacs.
;;; Commentary:
;;;   Omfg, this is so frikin nice
;;; Code:
(require 'csharp-mode)

(setq omnisharp-server-executable-path "/home/renan/code/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe"
      omnisharp-auto-complete-popup-help-delay 2)

;; Open csproj files into nxml-mode
(add-to-list 'auto-mode-alist '("\\.csproj" . nxml-mode))

(rr/define-bindings csharp-mode-map
                    '(("M-/"     . omnisharp-auto-complete)
                      ("M-."     . omnisharp-go-to-definition)
                      ("C-4 M-." . omnisharp-go-to-definition-other-window)
                      ("C-c i"   . omnisharp-code-format)
                      ("C-c C-c" . omnisharp-fix-code-issue-at-point)
                      ("C-c x"   . omnisharp-run-code-action-refactoring)
                      ("C-c r r" . omnisharp-rename-interactively)
                      ("C-c r R" . omnisharp-rename)
                      ("C-c r F" . rr/omnisharp-rename-file)
                      ("C-c r u" . omnisharp-fix-usings)
                      ("C-c r l" . omnisharp-reload-solution)
                      ("C-c r i" . omnisharp-current-type-information)
                      ("C-c C-d"   . omnisharp-current-type-documentation)))

(defun rr/omnisharp-rename-file ()
  "Rename current file and fix the file path in .csproj file."
  (interactive)
  (let ((new-file-name (read-file-name "new name: ")))
    (omnisharp-remove-from-project-current-file)
    (dired-rename-file (buffer-file-name)
                       new-file-name
                       nil)
    (omnisharp-add-to-solution-current-file)))

(add-hook 'csharp-mode-hook 'omnisharp-mode)
(add-hook 'csharp-mode-hook (lambda () (setq c-basic-offset 2)))

;; this will automagically add files to omnisharp solution
(add-hook 'omnisharp-mode-hook 'omnisharp-add-to-solution-current-file)

;; this will fix a terribly ugly face for popup completion
(add-hook 'omnisharp-mode-hook
          (lambda () (set-face-attribute 'popup-isearch-match nil
                                    :box '(:line-width -1 :color "black")
                                    :background "grey"
                                    :foreground "black")))

(provide 'init-csharp)
;;; init-csharp.el ends here
