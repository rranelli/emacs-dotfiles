;;; init-ivy.el --- Ivy configuration
;;; Commentary:
;;; Code:
(use-package counsel
  :after ivy
  :config (counsel-mode)
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("<f1> f" . counsel-describe-function)
         ("<f1> v" . counsel-describe-variable)
         ("<f1> l" . counsel-load-library)
         ("<f2> i" . counsel-info-lookup-symbol)
         ("<f2> u" . counsel-unicode-char)
         ("C-x C-l" . counsel-locate)
         ("M-n b b" . counsel-bookmarks)
         ("C-c h M-y" . counsel-yank-pop)))

(use-package ivy
  :defer 0.1
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-c v" . ivy-push-view)
         ("C-c V" . ivy-pop-view)
         ("M-l" . ivy-switch-buffer)
         ("M-L" . ivy-recentf))
  :custom
  (ivy-initial-inputs-alist nil) ;; Drops the `^' in all m-x
  (ivy--regex-function 'ivy--regex-ignore-order)
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  (ivy-height 15)
  (ivy-initial-inputs-alist nil)
  (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  :config (ivy-mode))

(use-package ivy-rich
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :config (ivy-rich-mode 1))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-S-S" . isearch-forward)
         ("C-r" . swiper))

  :bind (:map swiper-map ("C-;" . swiper-avy)))

(provide 'init-ivy)
;;; init-ivy.el ends here
