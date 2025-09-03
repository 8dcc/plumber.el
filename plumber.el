;;; plumber.el ---  Run different commands depending on the text format -*- lexical-binding: t; -*-

;; Author: 8dcc <8dcc.git@gmail.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "25.1") (compat "28.1.2.2"))
;; Keywords: convenience, matching, tools
;; URL: https://github.com/8dcc/plumber.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package is an Emacs port of my plumber[1] command, inspired by the
;; "right click to plumb"[2] patch for the Simple Terminal[3], which was
;; inspired by Plan9's plumber[4].
;;
;; [1] https://github.com/8dcc/plumber
;; [2] https://st.suckless.org/patches/right_click_to_plumb/
;; [3] https://st.suckless.org/
;; [4] https://9p.io/wiki/plan9/using_plumbing/index.html

;;; Code:

(defgroup plumber nil
  "Run different commands depending on the text format."
  :link '(url-link :tag "Homepage" "https://github.com/8dcc/plumber.el")
  :link '(emacs-library-link :tag "Library Source" "plumber.el")
  :group 'convenience
  :prefix "plumber-")

(defcustom plumber-rules
  `(("URL"
     "https?://.+"
     browse-url)
    ("Mail address"
     ,(rx (one-or-more (any alnum ?. ?_ ?- ?+))
          "@"
          (one-or-more (any alnum ?-))
          "."
          (one-or-more (any alnum ?- ?.)))
     browse-url-mail)
    ("Man page"
     ,(rx (one-or-more (any alnum punct))
          "(" digit ")")
     man)
    ("Elisp expression"
     ,(rx "(" (one-or-more print) ")")
     (lambda (input)
       (eval-expression (read input))))
    ("Elisp symbol"
     ,(rx "`" (one-or-more (any alnum ?_ ?. ?: ?% ?? ?= ?/ ?* ?+ ?-)) "'")
     plumber-describe-symbol)
    ("Math"
     ,(rx (one-or-more digit)           ; First number
          (one-or-more
           (zero-or-more blank)
           (any ?+ ?- ?* ?/ ?^ ?%)      ; Operation
           (zero-or-more blank)
           (one-or-more digit)))        ; Other numbers
     (lambda (input)
       (message "Result: %s" (calc-eval input))))
    ("File"
     ,(rx (one-or-more
           (or (any alnum ?/ ?~ ?. ?_ ?-)     ; Path
               "\\ "))                        ; Optional escaped espaces
          (opt ":" (one-or-more digit)        ; Column
               (opt ":" (one-or-more digit))  ; Line
               (zero-or-more print)))         ; Remaining text
     plumber-find-file))
  "List of elements (NAME REGEXP FUNCTION) used for plumbing.

Each rule specifies the FUNCTION that should be called whenever the user is
plumbing a text that matches REGEXP.  The function should receive one string
argument: the text being plumbed.  The NAME should be a short description, used
by the `plumber-plumb-as' function.

The regular expressions will be checked in order, therefore expressions at the
start of the list should be more strict than the ones at the end.  Also note
that there is no need to wrap the regular expressions in \"^...$\", since this
is done internally by `plumber-plumb'.

Also note that, even though `plumber-plumb' ensures that the input matches the
REGEXP, `plumber-plumb-as' completely ignores it. This means that the specified
FUNCTION is guaranteed to receive a string as its argument, but it should not
expect any specific format."
  :type '(repeat
          (list (string :tag "Name")
                (regexp :tag "Regexp")
                (function :tag "Function"))))

(defcustom plumber-fill-text-prompt t
  "Set the initial input of the text prompt to the thing at point.
Rather than the default value.

Used by `plumber-get-user-text'.  The \"thing at point\" is obtained with
`plumber--thing-at-point'.

For more information on the differences between \"initial input\" and \"default
value\", see `read-string'."
  :type 'boolean)

(defvar plumber-history nil
  "History of plumbed strings.")

;;------------------------------------------------------------------------------
;; Functions used in `plumber-rules', can be overwritten

(defun plumber-describe-symbol (input)
  "Convert string INPUT to a symbol, and describe it as long as it exists.
If it doesn't exist, show an user error."
  (save-match-data
    (if (string-match "^`\\(.*\\)'$" input)
        (setq input (match-string 1 input))))
  (let ((symbol (intern input)))
    (if (or (boundp symbol)
            (fboundp symbol))
        (describe-symbol symbol)
      (user-error "Unbound symbol `%s'" symbol))))

(defun plumber-find-file (input)
  "Open a file, optionally at a specific line and column.

The INPUT should have the following format:

  FILENAME[:ROW][:COL]

Where ROW and COL are valid inputs for `string-to-number', and FILENAME is a
valid input for `find-file'.

By default, this function is used in `plumber-rules', since it allows the user
to plumb output from *grep* or *compilation* buffers.

Internally, the function uses `goto-char', `forward-line' and `move-to-column'."
  (save-match-data
    (string-match (rx (group-n 1 (one-or-more (not (any "\n" ":"))))
                      (opt ":" (group-n 2 (one-or-more digit)))
                      (opt ":" (group-n 3 (one-or-more digit))))
                  input)
    (let* ((target-file (match-string 1 input))
           (match2 (match-string 2 input))
           (target-row (and match2 (string-to-number match2)))
           (match3 (match-string 3 input))
           (target-col (and match3 (string-to-number match3))))
      (find-file target-file)
      (when target-row
        ;; NOTE: The `goto-line' function can only be used interactively.
        (goto-char (point-min))
        (forward-line (1- target-row)))
      (when target-col
        ;; We subtract one from the target column because most tools use
        ;; one-indexed coordinates, but Emacs doesn't.
        (move-to-column (1- target-col))))))

;;------------------------------------------------------------------------------
;; Auxiliary functions

(defun plumber--thing-at-point ()
  "Get a string representing the next blank-delimited word.

A character is blank if it matches either the beginning of a line, or the regexp
\"[[:blank:]]\". For more information, see Info node `(elisp)Char Classes'."
  (ignore-errors
    (save-excursion
      (re-search-backward "^\\|[[:blank:]]")
      (re-search-forward "[^[:blank:]\n]+")
      (let ((thing-start (match-beginning 0))
            (thing-end (match-end 0)))
        (buffer-substring-no-properties thing-start thing-end)))))

(defun plumber--string-match-p (regexp string)
  "Return t if STRING matches the REGEXP, from start to end.
Uses `string-match-p'."
  (string-match-p (concat "^" regexp "$") string))

;;------------------------------------------------------------------------------
;; Functions for obtaining user data

(defun plumber-get-user-text ()
  "Get a string for plumbing.

If the region is active, use the region text.  Otherwise, prompt for a string."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (let ((thing-at-point (plumber--thing-at-point)))
      (if plumber-fill-text-prompt
          (read-string "Plumb: " thing-at-point 'plumber-history thing-at-point)
        (read-string (format-prompt "Plumb" thing-at-point)
                     nil 'plumber-history thing-at-point)))))

(defun plumber-get-rule-name ()
  "Prompt for a rule name in `plumber-rules'."
  (completing-read "Rule: " (mapcar #'car plumber-rules) nil t))

;;------------------------------------------------------------------------------
;; Functions for searching in `plumber-rules'

(defun plumber-func-from-text (text)
  "Get the first function in `plumber-rules' whose regexp matches TEXT.
Returns nil if no match was found."
  (let ((match (seq-find
                ;; Find first matching regexp in `plumber-rules'.
                (lambda (element)
                  (plumber--string-match-p (cadr element) text))
                plumber-rules)))
    ;; If we found a match, return the function. Otherwise, nil.
    (if match
        (caddr match)
      nil)))

(defun plumber-func-from-rule-name (name)
  "Get the function associated to NAME in `plumber-rules'.
Returns nil if no match was found."
  (let ((match (assoc name plumber-rules)))
    (if match
        (caddr match)
      nil)))

;;------------------------------------------------------------------------------
;; Interactive functions

;;;###autoload
(defun plumber-plumb (text)
  "Plumb the specified TEXT.

The plumbing functionality is based on Plan9's plumber.  In Emacs, it simply
allows you to call a different function depending on the format of the TEXT.  It
checks the format against the regular expressions specified in the
`plumber-rules' alist.  Alternatively, the `plumber-plumb-as' function can also
be used for manually specifying the plumber rule.

When called interactively, the `plumber-get-user-text' function is used for
obtaining the TEXT argument."
  (interactive (list (plumber-get-user-text)))
  (let ((func (plumber-func-from-text text)))
    (unless func
      (user-error "No plumber rule matches the specified text"))
    (funcall func text)))

;;;###autoload
(defun plumber-plumb-as (text rule-name)
  "Plumb the specified TEXT with the rule matching RULE-NAME.

This function is similar to `plumber-plumb', but `plumber-rules' is filtered
using the \"name\" field, rather than \"regexp\".

When called interactively, the `plumber-get-user-text' function is used for
obtaining the TEXT argument, and the `plumber-get-rule-name' function is used
for obtaining the RULE-NAME argument."
  (interactive (list (plumber-get-user-text)
                     (plumber-get-rule-name)))
  (let ((func (plumber-func-from-rule-name rule-name)))
    (unless func
      (error "No plumber rule named '%s'" rule-name))
    (funcall func text)))

(provide 'plumber)
;;; plumber.el ends here
