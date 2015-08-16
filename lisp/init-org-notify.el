;;; init-org-notify.el -- org notification magic
;;; Commentary:
;;; Code:

(require 'org-notify)
(org-notify-start 60)

(org-notify-add 'default
                '(:time "10m" :period "2m" :duration 25 :actions -notify/window)
                '(:time "1h" :period "15m" :duration 25 :actions -notify/window)
                '(:time "2h" :period "30m" :duration 25 :actions -notify/window))

(provide 'init-org-notify)
;;; init-org-notify.el ends here
