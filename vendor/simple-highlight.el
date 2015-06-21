;;; simple-highlight.el --- Extremelly simple highlight functionallity.

;; Copyright (C) 2015 Renan Ranelli <renanranelli at google mail>

;; Author: Renan Ranelli
;; URL: http://github.com/rranelli/simple-highlight.el
;; Version: 1.0
;; Keywords: highlight
;; Package-Requires: ((emacs "24.4") (dash "2.1.0"))

;; This file is NOT part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; See <http://www.gnu.org/licenses/> for a copy of the GNU General
;; Public License.

;;; Commentary:
;;
;; This package provides functionality for highlighting the thing at point.
;; Multiple calls to the highlight function will result in changing the
;; highlight color. There is also

;;; Requirements:
;;
;; This package was tested for GNU Emacs 24.4 and above. Older Emacsen should
;; work but I have not tested with them.

;;; Installation:
;;
;; You can install via `MELPA`, or manually by downloading `simple-highlight.el`
;; and adding the following to your init file:
;;
;; ```elisp
;; (add-to-list 'load-path "/path/to/simple-highlight")
;; ```

;;; Usage:
;;
;; Just require `simple-highlight' somewhere in your init file:
;;
;; ```elisp
;; (require 'simple-highlight)
;; ```
;;
;; See the [Function Documentation](#Function Documentation) for more details.
;;
;;; Customization:
;;
;; You can change the faces you want to use for highlight by setting the
;; variable `simple-highlight-faces'. For example:
;;
;; ```elisp
;; (setq simple-highlight-faces '(hi-green hi-yellow hi-blue))
;; ```
;;
;;; Changelog:
;;
;; 1.0 - First release. <br/>

;;; Code:
(require 'dash)

(defun simple-highlight-next-highlight-face ()
  "Gets the next highlight face from list."
  (interactive)
  (car (setq simple-highlight--faces-carousel
             (-rotate 1 simple-highlight--faces-carousel))))

(defvar simple-highlight-faces
  '(hi-black-hb
    hi-green
    hi-pink
    hi-yellow
    hi-blue
    hi-black-b
    hi-green-b
    hi-red-b
    hi-yellow-b
    hi-blue-b)
  "The list of faces that will be used for highlighting.")

(defvar simple-highlight--faces-carousel
  simple-highlight-faces)

(defun simple-highlight-at-point ()
  "Highlight the sexp at point.
The color of the highlight is changed with each highlighting."
  (interactive)
  (let ((rgxp (regexp-quote (symbol-name (sexp-at-point)))))
    (highlight-regexp rgxp (simple-highlight-next-highlight-face))))

(defun simple-highlight-unhighlight-at-point ()
  "Unhighlight the sexp at point."
  (interactive)
  (let ((rgxp (regexp-quote (symbol-name (sexp-at-point)))))
    (unhighlight-regexp rgxp)))

(defun simple-highlight-unhighlight-all ()
  "Unhighlight all the highlights in current buffer."
  (interactive)
  (-each hi-lock-interactive-patterns
    (lambda(entry) (unhighlight-regexp  (car entry))))
  (setq simple-highlight--faces-carousel simple-highlight-faces))

(provide 'simple-highlight)
;;; simple-highlight.el ends here
