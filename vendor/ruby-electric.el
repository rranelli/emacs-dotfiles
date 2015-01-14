;;; ruby-electric.el --- Minor mode with electric editing commands for Ruby files
;;
;; Copyright (C) 2005 by Dee Zsombor
;;
;; Authors: Dee Zsombor <dee dot zsombor at gmail dot com>
;; Maintainer: Jakub Kuźma <qoobaa@gmail.com>
;; URL: http://github.com/qoobaa/ruby-electric/raw/master/ruby-electric.el
;; Keywords: languages ruby
;; Version: 1.1

;;; Code:

(require 'ruby-mode)

(defgroup ruby-electric nil
  "Minor mode providing electric editing commands for ruby files"
  :group 'ruby)

(defconst ruby-electric-expandable-do-re
  "do\\s-$")

(defconst ruby-electric-expandable-bar
  "\\(\\s-do\\s-+\\)\\|\\({\\s-*\\)")

(defvar ruby-electric-matching-delimeter-alist
  '((?\[ . ?\])
    (?\( . ?\))
    (?\' . ?\')
    (?\` . ?\`)
    (?\" . ?\")))

(defcustom ruby-electric-simple-keywords-re
  (regexp-opt '("def" "if" "class" "module" "unless" "case" "while" "do" "until" "for" "begin") t)
  "*Regular expresion matching keywords for which closing 'end'
is to be inserted."
  :type 'regexp :group 'ruby-electric)

(defcustom ruby-electric-expand-delimiters-list '(all)
  "*List of contexts where matching delimiter should be
inserted. The word 'all' will do all insertions."
  :type '(set :extra-offset 8
              (const :tag "Everything" all )
              (const :tag "Curly brace" ?\{ )
              (const :tag "Square brace" ?\[ )
              (const :tag "Round brace" ?\( )
              (const :tag "Quote" ?\' )
              (const :tag "Double quote" ?\" )
              (const :tag "Back quote" ?\` )
              (const :tag "Vertical bar" ?\| ))
  :group 'ruby-electric)

(defcustom ruby-electric-newline-before-closing-bracket nil
  "*Controls whether a newline should be inserted before the
closing bracket or not."
  :type 'boolean :group 'ruby-electric)

;;;###autoload
(define-minor-mode ruby-electric-mode
  "Toggle Ruby Electric minor mode.
With no argument, this command toggles the mode.  Non-null prefix
argument turns on the mode.  Null prefix argument turns off the
mode.

When Ruby Electric mode is enabled, an indented 'end' is
heuristicaly inserted whenever typing a word like 'module',
'class', 'def', 'if', 'unless', 'case', 'until', 'for', 'begin',
'do'. Simple, double and back quotes as well as braces are paired
auto-magically. Expansion does not occur inside comments and
strings. Note that you must have Font Lock enabled."
  ;; initial value.
  nil
  ;;indicator for the mode line.
  " REl"
  ;;keymap
  ruby-mode-map
  (ruby-electric-setup-keymap))

(defun ruby-electric-setup-keymap ()
  (define-key ruby-mode-map " " 'ruby-electric-space)
  (define-key ruby-mode-map "{" 'ruby-electric-curlies)
  (define-key ruby-mode-map "(" 'ruby-electric-matching-char)
  (define-key ruby-mode-map "[" 'ruby-electric-matching-char)
  (define-key ruby-mode-map "\"" 'ruby-electric-quote)
  (define-key ruby-mode-map "\'" 'ruby-electric-quote)
  (define-key ruby-mode-map "|" 'ruby-electric-bar)
  (define-key ruby-mode-map (kbd "RET") 'ruby-electric-return)
  (define-key ruby-mode-map (kbd "C-j") 'ruby-electric-return)
  (define-key ruby-mode-map (kbd "C-m") 'ruby-electric-return)
  (define-key ruby-mode-map "}" 'ruby-electric-close-matching-char)
  (define-key ruby-mode-map ")" 'ruby-electric-close-matching-char)
  (define-key ruby-mode-map "]" 'ruby-electric-close-matching-char))

(defun ruby-electric-space (arg)
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (if (ruby-electric-space-can-be-expanded-p)
      (save-excursion
        (ruby-indent-line t)
        (newline)
        (ruby-electric-insert-end))))

(defun ruby-electric-code-at-point-p ()
  (and ruby-electric-mode
       (let* ((properties (text-properties-at (point))))
         (and (null (memq 'font-lock-string-face properties))
              (null (memq 'font-lock-comment-face properties))))))

(defun ruby-electric-string-at-point-p ()
  (and ruby-electric-mode
       (consp (memq 'font-lock-string-face (text-properties-at (point))))))

(defun ruby-electric-is-last-command-char-expandable-punct-p ()
  (or (memq 'all ruby-electric-expand-delimiters-list)
      (memq last-command-event ruby-electric-expand-delimiters-list)))

(defun ruby-electric-space-can-be-expanded-p ()
  (if (ruby-electric-code-at-point-p)
      (let* ((ruby-electric-keywords-re
              (concat ruby-electric-simple-keywords-re "\\s-$"))
             (ruby-electric-single-keyword-in-line-re
              (concat "\\s-*" ruby-electric-keywords-re)))
        (save-excursion
          (backward-word 1)
          (or (looking-at ruby-electric-expandable-do-re)
              (and (looking-at ruby-electric-keywords-re)
                   (not (string= "do" (match-string 1)))
                   (progn
                     (beginning-of-line)
                     (looking-at ruby-electric-single-keyword-in-line-re))))))))

(defun ruby-electric-curlies(arg)
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (if (ruby-electric-is-last-command-char-expandable-punct-p)
      (cond ((ruby-electric-code-at-point-p)
             (save-excursion
               (if ruby-electric-newline-before-closing-bracket
                   (progn
                     (newline)
                     (insert "}")
                     (ruby-indent-line t))
                 (insert "}"))))
            ((ruby-electric-string-at-point-p)
             (if (eq last-command-event ?{)
                 (save-excursion
                   (backward-char 1)
                   (or (char-equal ?\# (preceding-char))
                       (insert "#"))
                   (forward-char 1)
                   (insert "}")))))))

(defun ruby-electric-quote (arg)
  (interactive "P")
  (if (ruby-electric-is-last-command-char-expandable-punct-p)
      ;; if outside quotes, do the self-insert as before
      (if (ruby-electric-code-at-point-p)
	  (progn
	    (self-insert-command (prefix-numeric-value arg))
	    (save-excursion
	      (insert (cdr (assoc last-command-event
				  ruby-electric-matching-delimeter-alist)))))
	;; else, inside quote so see if we need to just hop over the
	;; closing quote
	(if (and
             (looking-at (string last-command-event))
             ;; allow escaping - don't hop over the quote if the
             ;; previous char is a backslash
             (not (char-equal ?\\ (preceding-char))))
	    (forward-char 1)
	  ;; else inside quote but not at the end.
	  (self-insert-command (prefix-numeric-value arg))))
    ;; else electric mode is off, just do self-insert
    (self-insert-command (prefix-numeric-value arg))))

(defun ruby-electric-matching-char (arg)
  (interactive "P")
  (if (looking-at (regexp-quote (string last-command-event)))
      (forward-char 1)
    (progn
      (self-insert-command (prefix-numeric-value arg))
      (and (ruby-electric-is-last-command-char-expandable-punct-p)
           (ruby-electric-code-at-point-p)
           (save-excursion
             (insert (cdr (assoc last-command-event
                                 ruby-electric-matching-delimeter-alist))))))))

(defun ruby-electric-close-matching-char (arg)
  (interactive "P")
  (if (looking-at (regexp-quote (string last-command-event)))
      (forward-char 1)
    (self-insert-command (prefix-numeric-value arg))))

(defun ruby-electric-bar (arg)
  (interactive "P")
  (if (and (ruby-electric-is-last-command-char-expandable-punct-p)
           (ruby-electric-code-at-point-p))
      (if (and (save-excursion (re-search-backward ruby-electric-expandable-bar nil t))
               (= (point) (match-end 0)))
          ;; expand bar after ' do ' or ' { '
          (progn
            (self-insert-command (prefix-numeric-value arg))
            (save-excursion
              (insert "|")))
        ;; behave like on closing ')'
        (ruby-electric-close-matching-char arg))
    (self-insert-command (prefix-numeric-value arg))))

(defun ruby-electric-return-can-be-expanded-p ()
  (if (ruby-electric-code-at-point-p)
      (let* ((ruby-electric-keywords-re
              (concat ruby-electric-simple-keywords-re "$")))
        (save-excursion
          (skip-chars-backward "A-Za-z0-9_:")
          (looking-at ruby-electric-keywords-re)))))

(defun ruby-electric-return ()
  (interactive "*")
  (if (ruby-electric-return-can-be-expanded-p)
      (save-excursion
        (newline)
        (ruby-electric-insert-end)))
  (reindent-then-newline-and-indent))

(defun ruby-electric-insert-end ()
  (interactive)
  (insert "end")
  (ruby-indent-line t)
  (end-of-line))

(provide 'ruby-electric)

;;; ruby-electric.el ends here
