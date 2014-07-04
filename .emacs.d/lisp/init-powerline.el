;;; package -- Summary
;;; Commentary:
;;; Code:
(require 'powerline)

(setq powerline-arrow-shape 'arrow
      powerline-color1 "grey22"
      powerline-column 50)

(set-face-attribute 'mode-line nil
                    :background "DeepSkyBlue4"
                    :foreground "snow1"
                    :box nil)

(set-face-attribute 'mode-line-inactive nil
                    :box nil)

(provide 'init-powerline)
;;; init-powerline.el ends here
