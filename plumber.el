;;; plumber.el ---  Run different commands depending on the text format -*- lexical-binding: t; -*-

;; Author: 8dcc <8dcc.git@gmail.com>
;; Version: 1.0.0
;; URL: https://github.com/8dcc/plumber.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

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

(defgroup plumber ()
  "Run different commands depending on the text format"
  :group 'convenience)

(defcustom plumber-alist
  '(("URL"  "https?://.+" browse-url)
    ("File" "\\w+\\.\\w+" find-file))
  "List of elements (TYPE REGEXP FUNC) used for plumbing.

FUNC is the function that should be called whenever the user is plumbing a text
matching REGEXP. The function should receive one argument: the plumbed text. The
TYPE value will be used by `plumber-plumb-as'.

The regular expressions will be checked in order, therefore expressions at the
start of the list should be more strict than the ones at the end."
  :group 'plumber
  :type '(alist :key-type string :value-type (list string function))
  :safe t)

(defvar plumber-history nil
  "History of plumbed strings.")

(defun plumber-thing-at-point ()
  "Get a string representing the next space-delimited word. This function only
considers spaces, tabs and newlines as \"spaces\"."
  (ignore-errors
    (save-excursion
      (re-search-backward "^\\|[\t\n ]")
      (re-search-forward "[^\t\n ]+")
      (let ((thing-start (match-beginning 0))
            (thing-end (match-end 0)))
        (buffer-substring-no-properties thing-start thing-end)))))

(defun plumber-get-user-text ()
  "Get a string for plumbing.

If the region is active, use the region text. Otherwise, prompt for a string."
  (if (region-active-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (let ((thing-at-point (plumber-thing-at-point)))
      (read-string (format-prompt "Plumb" thing-at-point)
                   nil 'plumber-history thing-at-point))))

(defun plumber-prompt-data-type ()
  "Prompt for a data type in `plumber-alist'."
  (completing-read "Data type: " (mapcar #'car plumber-alist) nil t))

(defun plumber-func-from-type (type)
  "Get the function associated to TYPE in `plumber-alist'. Returns nil if no
match was found."
  (let ((match(assoc type plumber-alist)))
    (if match
        (caddr match)
      nil)))

(defun plumber-func-from-text (text)
  "Get the first function in `plumber-alist' whose associated regexp matches
TEXT. Returns nil if no match was found."
  (let ((match (seq-find
                ;; Find first matching regexp in `plumber-alist'.
                (lambda (element)
                  (string-match-p (cadr element) text))
                plumber-alist)))
    ;; If we found a match, return the function. Otherwise, nil.
    (if match
        (caddr match)
      nil)))

;;;###autoload
(defun plumber-plumb-as (text type)
  "Plumb the specified text as a specific data type.

This function is similar to `plumber-plumb', but `plumber-alist' is filtered
using the \"type\" field, rather than \"regexp\".

When called interactively, the `plumber-get-user-text' function is used for
obtaining the TEXT argument, and the `plumber-prompt-data-type' function is used
for obtaining the TYPE argument."
  (interactive (list (plumber-get-user-text)
                     (plumber-prompt-data-type)))
  (let ((func (plumber-func-from-type type)))
    (unless func
      (error "Invalid data type '%s'" type))
    (funcall func text)))

;;;###autoload
(defun plumber-plumb (text)
  "Plumb the specified TEXT.

The plumbing functionality is based on Plan9's plumber. In Emacs, it simply
allows you to call a different function depending on the format of the TEXT. It
checks the format against the regular expressions specified in
`plumber-alist'. The data type can also be manually specified using the
`plumber-plumb-as' function.

When called interactively, the `plumber-get-user-text' function is used for
obtaining the TEXT argument."
  (interactive (list (plumber-get-user-text)))
  (let ((func (plumber-func-from-text text)))
    (unless func
      (error "No plumber regexp matches the specified text"))
    (funcall func text)))

(provide 'plumber)
;;; plumber.el ends here
