;; A highly opinionated indentation mode of the COOL language.
;; COOL language is used in Coursera Compilers course, a public
;; version of Stanford CS143.
;;
;; It is based on a stackoverflow.com answer
;;  http://stackoverflow.com/questions/9425466/emacs-mode-for-a-c-like-language
;; It heavily borrows code from the following mode:
;;  1. http://gosu-lang.org/downloads/gosu-mode.el
;;  2. http://go.googlecode.com/hg/misc/emacs/go-mode.el
;;
;; <s>Authored</s> Remixed by srirampc.(http://bitbucket.org/srirampc)
;; - using (quite significant) portions of code from
;; (1) and (2) above;
;; TODO::
;;  1. Handle escaped strings - remove the go-style-string
;;  2. Hanlde the strange behaviour, when a keyword is in a
;;     string or comment. (Is not able reproduce consistently)
;;  3. Handle nested comments
;;  4. Compilation stuff(?)

;; Keywords in the COOL language
(defvar cool-mode-keywords
  '("class"  "else"       "false"    "fi"    "if"
    "in"     "inherits"   "isvoid"   "let"   "loop"
    "pool"   "then"       "while"    "case"  "esac"
    "new"    "of"         "not"      "true")
  "All keywords in the Cool language.  Used for font locking and
some syntax analysis.")

;; Keywords in the COOL language that are fontlocked
;; builtins, constants, functions and types
(defvar cool-mode-font-lock-keywords
  (let ((builtins '("length" "concat" "substr" "abort" "type_name" "copy"
                    "new" "out_string" "out_int" "in_string" "in_int"
                    "IO" "Object" "String" "SELF_TYPE" "Int" "Bool"
                    "self"))
        (constants '("true" "false"))
        (type-name "\\s *\\([A-Z]\\w*\\)"))
    `((,(regexp-opt cool-mode-keywords 'words) . font-lock-keyword-face)
      (,(regexp-opt builtins 'words) . font-lock-builtin-face)
      (,(regexp-opt constants 'words) . font-lock-constant-face)
      ;; Function names in calls and declarations
      ("\\(\\w+\\)\\s *(" 1 font-lock-function-name-face)
      ;; Type names
      ("\\<class\\>\\s *\\(\\w+\\)" 1 font-lock-type-face)
      (,(concat "" type-name) 1 font-lock-type-face)
      ;; new type
      (,(concat "\\<\\(?:new\\)\\>\\(?:\\s \\|)\\)*(" type-name) 1 font-lock-type-face)
      ;; Type conversion
      (,(concat "\\.\\s *(" type-name) 1 font-lock-type-face)
      ;; numbers
      ("\\b[0-9]+\\b" . font-lock-constant-face)))
  "Basic font lock keywords for Cool mode.  Highlights keywords,
   built-ins, functions, and some types.")

;; Syntax table
(defvar cool-mode-syntax-table
  (let ((st (make-syntax-table)))
    ;; Adding _ to the words
    (modify-syntax-entry ?_  "w" st)

    (modify-syntax-entry ?+  "." st)
    (modify-syntax-entry ?-  ". 12b" st) ;; comment begin
    (modify-syntax-entry ?\(  "\(\)1" st) ;; comment begin and match )
    (modify-syntax-entry ?*  ". 23" st)  ;; comment begin second
    (modify-syntax-entry ?\)  "\)\(4" st)  ;; comment end and match (
    (modify-syntax-entry ?~  "." st)
    (modify-syntax-entry ?=  "." st)
    (modify-syntax-entry ?<  "." st)
    (modify-syntax-entry ?>  "." st)
    (modify-syntax-entry ?\/  "." st)
    ;; Newline is a comment-ender.
    (modify-syntax-entry ?\n "> b" st)
    st))

;; Begin - functions taken from gosu-mode.el
(defun line-matchesp (regexp offset)
  "Return t if line matches regular expression REGEXP.  The
selected line is chosen by applying OFFSET as a numeric
increment or decrement away from the current line number.
This function modifies the match data that `match-beginning',
`match-end' and `match-data' access; save and restore the match
data if you want to preserve them."
  (interactive)
  (save-excursion
    (forward-line offset)
    (beginning-of-line)
    (looking-at regexp)))

(defun previous-line-matchesp (regexp)
  "Return t if previous line matches regular expression REGEXP.
This function modifies the match data that `match-beginning',
`match-end' and `match-data' access; save and restore the match
data if you want to preserve them."
  (interactive)
  (line-matchesp regexp -1))

(defun current-line-matchesp (regexp)
  "Return t if current line matches regular expression REGEXP.
This function modifies the match data that `match-beginning',
`match-end' and `match-data' access; save and restore the match
data if you want to preserve them."
  (interactive)
  (line-matchesp regexp 0))

;; End - functions from gosu-mode.el


;;; Begin - functions taken from go-mode.el
(defvar cool-mode-mark-comment-end 1
  "The point at which the comment cache ends.  The buffer
will be marked from the beginning up to this point (that is, up
to and including character (1- cool-mode-mark-comment-end)).")
(make-variable-buffer-local 'cool-mode-mark-comment-end)

(defvar cool-mode-mark-string-end 1
  "The point at which the string cache ends.  The buffer
will be marked from the beginning up to this point (that is, up
to and including character (1- cool-mode-mark-string-end)).")
(make-variable-buffer-local 'cool-mode-mark-string-end)

(defun cool-mode-mark-clear-cache (b e)
  "A before-change-function that clears the comment/string and
nesting caches from the modified point on."

  (save-restriction
    (widen)

    (when (<= b cool-mode-mark-string-end)
      ;; Remove the property adjacent to the change position.
      ;; It may contain positions pointing beyond the new end mark.
      (let ((b (let ((cs (get-text-property (max 1 (1- b)) 'cool-mode-string)))
		 (if cs (car cs) b))))
	(remove-text-properties
	 b (min cool-mode-mark-string-end (point-max)) '(cool-mode-string nil))
	(setq cool-mode-mark-string-end b)))

    (when (<= b cool-mode-mark-comment-end)
      ;; Remove the property adjacent to the change position.
      ;; It may contain positions pointing beyond the new end mark.
      (let ((b (let ((cs (get-text-property (max 1 (1- b)) 'cool-mode-comment)))
		 (if cs (car cs) b))))
	(remove-text-properties
	 b (min cool-mode-mark-string-end (point-max)) '(cool-mode-comment nil))
	(setq cool-mode-mark-comment-end b)))))


(defun cool-mode-in-comment (&optional pos)
  "Return the comment/string state at point POS.  If point is
inside a comment (including the delimiters), this
returns a pair (START . END) indicating the extents of the
comment or string."

  (unless pos
    (setq pos (point)))
  (when (> pos cool-mode-mark-comment-end)
    (cool-mode-mark-comment pos))
  (get-text-property pos 'cool-mode-comment))

(defun cool-mode-mark-comment (end)
  "Mark comments up to point END.  Don't call this directly;
use `cool-mode-in-comment'."
  (setq end (min end (point-max)))
  (cool-mode-parser
   (save-match-data
     (let ((pos
            ;; Back up to the last known state.
            (let ((last-comment
                   (and (> cool-mode-mark-comment-end 1)
                        (get-text-property (1- cool-mode-mark-comment-end)
                                           'cool-mode-comment))))
              (if last-comment
                  (car last-comment)
                (max 1 (1- cool-mode-mark-comment-end))))))
       (while (< pos end)
         (goto-char pos)
         (let ((comment-end			; end of the text property
                (cond
                 ((looking-at "--")
                  (end-of-line)
                  (1+ (point)))
                 ((looking-at "\(\\*")
                  (goto-char (+ pos 2))
                  (if (search-forward "*\)" (1+ end) t)
                      (point)
                    end)))))
           (cond
            (comment-end
             (put-text-property pos comment-end 'cool-mode-comment
                                (cons pos comment-end))
             (setq pos comment-end))
            ((re-search-forward "\([*]\\|--" end t)
             (setq pos (match-beginning 0)))
            (t
             (setq pos end)))))
       (setq cool-mode-mark-comment-end pos)))))

(defun cool-mode-backward-skip-comments ()
  "Skip backward over comments and whitespace."
  ;; only proceed if point is in a comment or white space
  ;; (interactive)
  (if (or (cool-mode-in-comment)
          (cool-mode-whitespace-p (char-after (point))))
      (let ((loop-guard t))
        (while (and loop-guard (not (bobp)))
          (cond ((cool-mode-whitespace-p (char-after (point)))
                 ;; moves point back over any whitespace
                 (re-search-backward "[^[:space:]]"))
                ((cool-mode-in-comment)
                 ;; move point to char preceeding current comment
                 (goto-char (1- (car (cool-mode-in-comment)))))
                ;; not in a comment or whitespace? we must be done.
                (t (setq loop-guard nil)
                   (forward-char 1)))))))

;;; TODO:: need to update this for escaped strings
(defun cool-mode-in-string (&optional pos)
  "Return the string state at point POS.  If point is
inside a string (including the delimiters), this
returns a pair (START . END) indicating the extents of the
comment or string."

  (unless pos
    (setq pos (point)))
  (when (> pos cool-mode-mark-string-end)
    (cool-mode-mark-string pos))
  (get-text-property pos 'cool-mode-string))

(defun cool-mode-mark-string (end)
  "Mark strings up to point END.  Don't call this
directly; use `cool-mode-in-string'."
  (setq end (min end (point-max)))
  (cool-mode-parser
   (save-match-data
     (let ((pos
            ;; Back up to the last known state.
            (let ((last-cs
                   (and (> cool-mode-mark-string-end 1)
                        (get-text-property (1- cool-mode-mark-string-end)
                                           'cool-mode-string))))
              (if last-cs
                  (car last-cs)
                (max 1 (1- cool-mode-mark-string-end))))))
       (while (< pos end)
         (goto-char pos)
         (let ((cs-end			; end of the text property
                (cond
                 ((looking-at "\"")
                  (goto-char (1+ pos))
                  (if (looking-at "[^\"\n\\\\]*\\(\\\\.[^\"\n\\\\]*\\)*\"")
                      (match-end 0)
                    (end-of-line)
                    (point)))
                 ((looking-at "'")
                  (goto-char (1+ pos))
                  (if (looking-at "[^'\n\\\\]*\\(\\\\.[^'\n\\\\]*\\)*'")
                      (match-end 0)
                    (end-of-line)
                    (point)))
                 ((looking-at "`")
                  (goto-char (1+ pos))
                  (while (if (search-forward "`" end t)
                             (if (eq (char-after) ?`)
                                 (goto-char (1+ (point))))
                           (goto-char end)
                           nil))
                  (point)))))
           (cond
            (cs-end
             (put-text-property pos cs-end 'cool-mode-string (cons pos cs-end))
             (setq pos cs-end))
            ((re-search-forward "[\"'`]" end t)
             (setq pos (match-beginning 0)))
            (t
             (setq pos end)))))
       (setq cool-mode-mark-string-end pos)))))


(defun cool-mode-whitespace-p (char)
  "Is newline, or char whitespace in the syntax table for go."
  (or (eq char ?\n)
      (eq char ?\t)
      (eq char ?\r)
      (eq 32 (char-syntax char))))

(defmacro cool-mode-parser (&rest body)
  "Evaluate BODY in an environment set up for parsers that use
text properties to mark text.  This inhibits changes to the undo
list or the buffer's modification status and inhibits calls to
the modification hooks.  It also saves the excursion and
restriction and widens the buffer, since most parsers are
context-sensitive."

  (let ((modified-var (make-symbol "modified")))
    `(let ((buffer-undo-list t)
           (,modified-var (buffer-modified-p))
           (inhibit-modification-hooks t)
           (inhibit-read-only t))
       (save-excursion
         (save-restriction
           (widen)
           (unwind-protect
               (progn ,@body)
             (set-buffer-modified-p ,modified-var)))))))

;; End - functions taken from from go-mode.el

(defun cool-mode-matching-kw (matchkw contrakw)
  " Looks for the matching keyword, by counting the expected
match and the contra match. For example, suppose we are looking for the
\"if\" that matches a \"fi\". Then starting at the current point,
go backward looking for \"fi\" or \"if\". If \"fi\" is found before \"if\",
keep counting the number of \"fi\"s found. After an equivalent number of \"if\"s
are found, we can now get the approprite \"if\".

Don't call this function without save-excursion.
"

  (let ((loop-guard t) (mcount 0) (retpoint nil)
        (remstr (concat "\\(\\<" contrakw "\\|" matchkw "\\>\\)")))
    ;; while not bobp and not pos found
    (while (and loop-guard (not (bobp)))
      (cond ((re-search-backward remstr nil t)
             ;; match for the contrakw or inkw backward
             (let ((mkw (match-string-no-properties 1)))
               (cond
                ;;if in comment continue
                ((cool-mode-in-comment)
                 (cool-mode-backward-skip-comments))
                ;; if in string, get out of the string
                ((cool-mode-in-string)
                 (goto-char (1- (car (cool-mode-in-string)))))
                ;; if not in comments and contrakw
                ;;   count++
                ((string-equal mkw contrakw) (incf mcount))
                ;; if not in comments and matchkw and count = 0
                ;;   return this position
                ((and (string-equal mkw matchkw) (eq mcount 0))
                 (setq loop-guard nil)
                 (setq retpoint (point)))
                ;; else
                ;;   count-- and continue
                ((string-equal mkw matchkw) (decf mcount))
                ;; this shouldn't happen
                (t (setq loop-guard nil)))))
            (t (setq loop-guard nil))))
    retpoint))

;; (defun test-cm-matching-kw ()
;;   (interactive)
;;   (cool-mode-matching-kw "if" "fi"))

;;
;; Functions that match the end of current/previous lines
(defun line-endsp (regexp offset)
  "Return t if line ends in regular expression REGEXP.
Skips all the comments backwards - so the line is not
necessarily the previous line. The
selected line is chosen by applying OFFSET as a numeric
increment or decrement away from the current line number.
This function modifies the match data that `match-beginning',
`match-end' and `match-data' access; save and restore the match
data if you want to preserve them."
  (interactive)
  (save-excursion
    (forward-line offset)
    (end-of-line)
    (cool-mode-backward-skip-comments)
    (when (looking-back regexp)
      (point))))

(defun previous-line-endsp (regexp)
  (interactive)
  (line-endsp regexp -1))

(defun current-line-endsp (regexp)
  (interactive)
  (line-endsp regexp 0))

(defun indent-matching-kw (matchkw contrakw)
  "Indent after finding the maching keyword"
  (let ((rpt (cool-mode-parser
              (cool-mode-matching-kw matchkw contrakw))))
       (cond (rpt
              (save-excursion
                (goto-char rpt)
                (current-indentation)))
             (t (cool-mode-failsafe)))))

(defun indent-prev-line-ends-pattern (pattern offset)
  "Indent based on the pattern in the previous line
Different from other indent functions, as this causes
a side effect!"
  (let ((rpt (previous-line-endsp pattern)))
    (when rpt
      (indent-line-to
       (save-excursion
         (goto-char rpt)
         (+ (current-indentation) offset)))
      rpt)))

(defun cool-mode-failsafe ()
  "If i couldn't find any context, this is a fail safe indentation"
  (save-excursion
    (condition-case nil
        (progn
          (beginning-of-line)
          (backward-up-list)
          (+ (current-indentation) default-tab-width))
      (error 0))))

(defun cool-mode-indent-line ()
  (interactive)
  "Establish a set of conditional cases for the types of lines that
point currently is on, and the associated indentation rules."
  (indent-line-to
   (cond
    ;; alignment of comments - first line
    ((and
      (previous-line-matchesp "^[ \t]*\\*")
      (current-line-matchesp "^[ \t]*\\*"))
     (save-excursion
       (forward-line -1)
       (current-indentation)))
    ;; alignment of comments  - second n consec. lines
    ((and
      (previous-line-matchesp "^[ \t]*\(\\*")
      (current-line-matchesp "^[ \t]*\\*"))
     (save-excursion
       (forward-line -1)
       (+ (current-indentation) 1)))
    ;; object chaining getting '.'s aligned
    ((and
      (previous-line-matchesp "^[ \t]*\\.")
      (current-line-matchesp "^[ \t]**\\."))
     (save-excursion
       (forward-line -1)
       (current-indentation)))
    ;; object chaining first line
    ((and
      (not (previous-line-matchesp "^[ \t]*\\."))
      (current-line-matchesp "^[ \t]*\\."))
     (save-excursion
       (forward-line -1)
       (+ (current-indentation) default-tab-width)))
    ;; when it has spaces and ends with } or )
    ((current-line-matchesp "^[ \t]*[})]")
     (save-excursion
       (beginning-of-line)
       (backward-up-list)
       (current-indentation)))
    ;; previous line ends with then else etc.
    ((indent-prev-line-ends-pattern
      "\\<\\(?:let\\|in\\|then\\|else\\|of\\)\\>[ \t]*" default-tab-width)
     (current-indentation))
    ;;
    ((indent-prev-line-ends-pattern
        "=\>[ \t]*" default-tab-width)
     (current-indentation))
    ;; matching if
    ((current-line-matchesp "^[ \t]*\\<else\\>")
     (indent-matching-kw "if" "fi"))
    ;; matching case
    ((current-line-matchesp "^[ \t]*\\<esac\\>")
     (indent-matching-kw "case" "esac"))
    ;; matching if
    ((current-line-endsp "\\<fi\\>;?[ \t]*")
     (indent-matching-kw "if" "fi"))
    ;; matching loop
    ((current-line-endsp "\\pool\\>;?[ \t]*")
     (indent-matching-kw "loop" "pool"))
    ;; look at the semi-colon in the previous statement
    ((indent-prev-line-ends-pattern ";[ \t]*" 0)
     (current-indentation))
    ;; previous line ends with then else etc.
    (t (cool-mode-failsafe)))))

;; (require 'generic-x)
;; (define-generic-mode 'cool-mode
;;   ;; comment-list
;;   nil
;;   ;; cool mode keywords
;;   cool-mode-keywords
;;   ;; font-lock keywords
;;   cool-mode-font-lock-keywords
;;   ;; auto-mode-list
;;   '(".cool\\'")
;;   ;; Set it up
;;   '((lambda ()
;;       (set-syntax-table cool-mode-syntax-table)
;;       (setq comment-start "--")
;;       (setq comment-end "")
;;       (set (make-local-variable 'indent-line-function) 'cool-mode-indent-line)))
;;   )

(define-derived-mode cool-mode nil "Cool"
    "Major mode for editing Cool source text.

This provides basic syntax highlighting for keywords, built-ins,
functions, and some types.  It also provides basic indentation"
    ;; Font lock
  (set (make-local-variable 'font-lock-defaults)
       '(cool-mode-font-lock-keywords nil nil nil nil))

  ;; Comments
  (set (make-local-variable 'comment-start) "--")
  (set (make-local-variable 'comment-end)   "")
  ;; clear the cache
  (add-hook 'before-change-functions #'cool-mode-mark-clear-cache nil t)

  ;; indentation
  (set (make-local-variable 'indent-line-function)
       #'cool-mode-indent-line)

    ;; Comments
  (set (make-local-variable 'comment-start) "--")
  (set (make-local-variable 'comment-end)   "")

  (setq indent-tabs-mode nil))

(add-to-list 'auto-mode-alist (cons "\\.cool$" #'cool-mode))

(provide 'cool-mode)
