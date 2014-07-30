;;; package -- Summary
;;; Commentary:
;;; Code:

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories (expand-file-name "ac-dict" user-emacs-directory))
(ac-config-default)

(setq ac-auto-start 4)
(setq ac-auto-show-menu 0.5)
(setq ac-ignore-case nil)

(add-hook 'ruby-mode-hook
          (lambda ()
            (make-local-variable 'ac-ignores)
            ;; ruby keywords
            (add-to-list 'ac-ignores "do")
            (add-to-list 'ac-ignores "end")
            (add-to-list 'ac-ignores "begin")
            (add-to-list 'ac-ignores "true")
            (add-to-list 'ac-ignores "false")
            (add-to-list 'ac-ignores "for")
            (add-to-list 'ac-ignores "rescue")
            (add-to-list 'ac-ignores "fail")
            (add-to-list 'ac-ignores "while")
            ))

;; -- keybindings --
;; navigate by C-n C-p in completion options
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)

;; complete with tab, return and \C-m
(define-key ac-complete-mode-map "\r" 'ac-complete)
(define-key ac-complete-mode-map "\C-m" 'ac-complete)

(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

(global-set-key (kbd "M-/") 'auto-complete)

(provide 'init-ac)
;;; init-ac.el ends here
