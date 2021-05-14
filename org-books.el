;;; org-books.el --- Reading list management with Org mode and helm   -*- lexical-binding: t -*-

;; Copyright (C) 2017 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>
;; Version: 0.2.21
;; Package-Requires: ((enlive "0.0.1") (s "1.11.0") (helm "2.9.2") (helm-org "1.0") (dash "2.14.1") (org "9.3") (emacs "25"))
;; URL: https://github.com/lepisma/org-books
;; Keywords: outlines

;;; Commentary:

;; org-books.el is a tool for managing reading list in an Org mode file.
;; This file is not a part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'enlive)
(require 'json)
(require 'helm)
(require 'helm-org)
(require 'org)
(require 's)
(require 'subr-x)
(require 'url)
(require 'url-parse)

(defgroup org-books nil
  "Org reading list management."
  :group 'org)

(defcustom org-books-file nil
  "File for keeping reading list."
  :type 'file
  :group 'org-books)

(defcustom org-books-add-to-top t
  "Should add new books as the first item under a heading?"
  :type 'boolean
  :group 'org-books)

(defcustom org-books-file-depth 2
  "The max depth for adding book under headings."
  :type 'integer
  :group 'org-books)

(defcustom org-books-url-pattern-dispatches
  '(("^\\(www\\.\\)?amazon\\." . org-books-get-details-amazon)
    ("^\\(www\\.\\)?goodreads\\.com" . org-books-get-details-goodreads)
    ("openlibrary\\.org" . org-books-get-details-isbn))
  "Pairs of url patterns and functions taking url and returning
book details. Check documentation of `org-books-get-details' for
return structure from these functions."
  :type '(alist :key-type string :value-type symbol)
  :group 'org-books)

(defun org-books--get-json (url)
  "Parse JSON data from given URL."
  (with-current-buffer (url-retrieve-synchronously url)
    (goto-char (point-min))
    (re-search-forward "^$")
    (json-read)))

(defun org-books--clean-str (text)
  "Clean TEXT to remove extra whitespaces."
  (s-trim (s-collapse-whitespace text)))

(defun org-books-get-details-amazon-authors (page-node)
  "Return author names for amazon PAGE-NODE.

PAGE-NODE is the return value of `enlive-fetch' on the page url."
  (or (mapcar #'enlive-text (enlive-query-all page-node [.a-section .author .contributorNameID]))
      (mapcar #'enlive-text (enlive-query-all page-node [.a-section .author > a]))))

(defun org-books-get-details-amazon (url)
  "Get book details from amazon URL."
  (let* ((page-node (enlive-fetch url))
         (title (org-books--clean-str (enlive-text (enlive-get-element-by-id page-node "productTitle"))))
         (author (s-join ", " (org-books-get-details-amazon-authors page-node))))
    (if (not (string-equal title ""))
        (list title author `(("AMAZON" . ,url))))))

(defun org-books-get-details-goodreads (url)
  "Get book details from goodreads URL."
  (let* ((page-node (enlive-fetch url))
         (title (org-books-get-title page-node))
         (author (org-books-get-author page-node))
         (numpages (org-books-get-pages page-node))
         (date (org-books-get-date-dispatch page-node))
         (gr-rating (org-books-get-rating page-node)))
    (if (not (string-equal title ""))
        (list title author `(("YEAR" . ,date)
                             ("PAGES" . ,numpages)
                             ("GOODREADS-RATING" . ,gr-rating)
                             ("GOODREADS-URL" . ,url))))))

(defun org-books-get-author (page-node)
  "Retrieve author name(s) from PAGE-NODE of Goodreads page."
  (->> (enlive-query-all page-node [.authorName > span])
    (mapcar #'enlive-text)
    (s-join ", ")
    (org-books--clean-str)))

(defun filter-by-itemprop (itemprop elements)
  (--filter (string= itemprop (enlive-attr it 'itemprop)) elements))

(defun org-books-get-title (page-node)
  (let ((title (org-books--clean-str (enlive-text (enlive-get-element-by-id page-node "bookTitle"))))
        (series (org-books--clean-str (enlive-text (enlive-query page-node [:bookSeries > a])))))
    (if (equal "" series)
        title
      (s-join " " (list title series)))))

(defun org-books-get-pages (page-node)
  "Retrieve pagenum from PAGE-NODE of Goodreads page."
  (->>
   (enlive-query-all page-node [:details > .row > span])
   (filter-by-itemprop "numberOfPages")
   (first)
   (enlive-text)
   (s-split-words)
   (first)))

(defun org-books-get-date-dispatch (page-node)
  "Extract correct publication date from PAGE-NODE."
  (if (enlive-query page-node [:details > .row > .greyText])
      (org-books-get-original-date page-node)
      (org-books-get-date page-node)))

(defun org-books-get-date (page-node)
  "Retrieve date from PAGE-NODE of Goodreads page."
  (->>
   (enlive-query-all page-node [:details > .row])
   (second)
   (enlive-text)
   (s-match "[0-9]\\{4\\}")
   (first)))

(defun org-books-get-original-date (page-node)
  "Retrieve original publication date from PAGE-NODE.
Assumes it has one."
  (->>
   (enlive-query page-node [:details > .row > .greyText])
   (enlive-text)
   (s-trim)
   (s-match "[0-9]\\{4\\}")
   (first)))

(defun org-books-get-rating (page-node)
  "Retrieve average rating from PAGE-NODE of Goodreads page."
  (->>
   (enlive-query-all page-node [:bookMeta > span])
   (filter-by-itemprop "ratingValue")
   (first)
   (enlive-text)
   (s-trim)))

(defun org-books-get-url-from-isbn (isbn)
  "Make and return openlibrary url from ISBN."
  (concat "https://openlibrary.org/api/books?bibkeys=ISBN:" isbn "&jscmd=data&format=json"))

(defun org-books-get-details-isbn (url)
  "Get book details from openlibrary ISBN response from URL."
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (json (org-books--get-json url))
         (isbn (car (hash-table-keys json)))
         (data (gethash isbn json))
         (title (gethash "title" data))
         (author (gethash "name" (car (gethash "authors" data)))))
    (list title author `(("ISBN" . ,url)))))

(defun org-books-get-details (url)
  "Fetch book details from given URL.

Return a list of three items: title (string), author (string) and
an alist of properties to be applied to the org entry. If the url
is not supported, throw an error."
  (let ((output 'no-match)
        (url-host-string (url-host (url-generic-parse-url url))))
    (cl-dolist (pattern-fn-pair org-books-url-pattern-dispatches)
      (when (s-matches? (car pattern-fn-pair) url-host-string)
        (setq output (funcall (cdr pattern-fn-pair) url))
        (cl-return)))
    (if (eq output 'no-match)
        (error (format "Url %s not understood" url))
      output)))

(defun org-books-create-file (file-path)
  "Write initialization stuff in a new file at FILE-PATH."
  (interactive "FFile: ")
  (if (file-exists-p file-path)
      (message "There is already a file present, skipping.")
    (with-temp-file file-path
      (insert "#+TITLE: Reading List\n"
              "#+AUTHOR: " (replace-regexp-in-string "" " " user-full-name) "\n\n"
              "#+TODO: READING NEXT | READ\n\n"))))

(defun org-books-all-authors ()
  "Return a list of authors in the `org-books-file'."
  (with-current-buffer (find-file-noselect org-books-file)
    (->> (org-property-values "AUTHOR")
       (-reduce-from (lambda (acc line) (append acc (s-split "," line))) nil)
       (mapcar #'s-trim)
       (-distinct)
       (-sort #'s-less-p))))

(defun org-books-entry-p ()
  "Tell if current entry is an org-books entry."
  (if (org-entry-get nil "AUTHOR") t))

(defun org-books-get-closed-time ()
  "Return closed time of the current entry."
  (let ((ent-body (buffer-substring-no-properties (org-entry-beginning-position) (org-entry-end-position))))
    (if (string-match org-closed-time-regexp ent-body)
        (parse-time-string (match-string-no-properties 1 ent-body)))))

(defun org-books-map-entries (func &optional match scope &rest skip)
  "Similar to `org-map-entries' but only walks on org-books entries.

Arguments FUNC, MATCH, SCOPE and SKIP follow their definitions
from `org-map-entries'."
  (with-current-buffer (find-file-noselect org-books-file)
    (let ((ignore-sym (gensym)))
      (-remove-item ignore-sym
                    (apply #'org-map-entries
                           (lambda ()
                             (if (org-books-entry-p)
                                 (if (functionp func) (funcall func) (funcall (list 'lambda () func)))
                               ignore-sym))
                           match scope skip)))))

;;;###autoload
(defun org-books-cliplink ()
  "Clip link from clipboard."
  (interactive)
  (let ((url (substring-no-properties (current-kill 0))))
    (org-books-add-url url)))

;;;###autoload
(defun org-books-add-url (url)
  "Add book from web URL."
  (interactive "sUrl: ")
  (let ((details (org-books-get-details url)))
    (if (null details)
        (message "Error in fetching url. Please retry.")
      (apply #'org-books-add-book details))))

;;;###autoload
(defun org-books-add-isbn (isbn)
  "Add book from ISBN."
  (interactive "sISBN: ")
  (org-books-add-url (org-books-get-url-from-isbn isbn)))

(defun org-books-format (level title author &optional props)
  "Return details as an org headline entry.

LEVEL specifies the headline level. TITLE goes as the main text.
AUTHOR and properties from PROPS go as org-property."
  (with-temp-buffer
    (org-mode)
    (insert (make-string level ?*) " " title "\n")
    (org-set-property "AUTHOR" author)
    (org-set-property "ADDED" (format-time-string "[%Y-%02m-%02d]"))
    (dolist (prop props)
      (org-set-property (car prop) (cdr prop)))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun org-books--insert (level title author &optional props)
  "Insert book template at current position in buffer.

Formatting is specified by LEVEL, TITLE, AUTHOR and PROPS as
described in docstring of `org-books-format' function."
  (insert (org-books-format level title author props)))

(defun org-books--insert-at-pos (pos title author &optional props)
  "Goto POS in current buffer, insert a new entry and save buffer.

TITLE, AUTHOR and PROPS are formatted using `org-books-format'."
  (org-content)
  (goto-char pos)
  (let ((level (or (org-current-level) 0)))
    (org-books-goto-place)
    (insert "\n")
    (org-books--insert (+ level 1) title author props)
    (save-buffer)))

(defun org-books-goto-place ()
  "Move to the position where insertion should happen."
  (if org-books-add-to-top
      (let ((level (or (org-current-level) 0))
            (bound (save-excursion (org-get-next-sibling))))
        (if (re-search-forward (format "^\\*\\{%s\\}" (+ level 1)) bound t)
            (forward-line -1)))
    (org-get-next-sibling)
    (forward-line -1)))

(defun org-books-get-headers ()
  "Return list of categories under which books can be filed.

Each item in list is a pair of title (propertized) and marker
specifying the position in the file."
  (let ((helm-org-headings-max-depth org-books-file-depth))
    (mapcar (lambda (it)
              (cons it (get-text-property 0 'helm-realvalue it)))
            (helm-org--get-candidates-in-file org-books-file helm-org-headings-fontify t nil t))))

;;;###autoload
(defun org-books-add-book (title author &optional props)
  "Add a book (specified by TITLE and AUTHOR) to the `org-books-file'.

Optionally apply PROPS."
  (interactive
   (let ((completion-ignore-case t))
     (list
      (read-string "Book Title: ")
      (s-join ", " (completing-read-multiple "Author(s): " (org-books-all-authors))))))
  (if org-books-file
      (save-excursion
        (with-current-buffer (find-file-noselect org-books-file)
          (let ((headers (org-books-get-headers)))
            (if headers
                (helm :sources (helm-build-sync-source "org-book categories"
                                 :candidates (mapcar (lambda (h) (cons (car h) (marker-position (cdr h)))) headers)
                                 :action (lambda (pos) (org-books--insert-at-pos pos title author props)))
                      :buffer "*helm org-books add*")
              (goto-char (point-max))
              (org-books--insert 1 title author props)
              (save-buffer)))))
    (message "org-books-file not set")))

(defun org-books--safe-max (xs)
  "Extract the maximum value of XS with special provisions for nil and '(0).
Used to calculate the number of times a book has been started or finished."
  (pcase xs
    ('() 0)
    ('(0) 1)
    (_ (apply #'max xs))))

(defun org-books--max-property (name)
  "Return the highest numerical value of a property name.
Assumes multiple properties with names like PROPERTY,
PROPERTY-2, etc., and returns the highest of that number.
For bare PROPERTY, returns 1. For no properties of NAME,
returns 0.

Used to count STARTED and FINISHED properties in re-reads."
  (->> (org-entry-properties nil 'standard)
    (-map #'car)
    (--filter (s-contains? name it))
    (--map (s-chop-prefixes `(,name "-") it))
    (-map #'string-to-number)
    (org-books--safe-max)))

(defun org-books--format-property (name read-count)
  "Return a property name string given its parameters.
Based on the number of times a book has been read,
the string will either be a bare NAME, or NAME-N,
where N is the current read count.

Used with STARTED, FINISHED, and MY-RATING properties."
  (let ((n (1+ read-count)))
    (if (= n 1)
        name
      (format (concat name "-%d") n))))

;;;###autoload
(defun org-books-start-reading ()
  "Mark book at point as READING.
Also sets the started property to today's date.

This function keeps track of re-reads.
If the book has already been read at least once,
opens a new property with the read count and date."
  (interactive)
  (org-todo "READING")
  (let* ((finished (org-books--max-property "FINISHED"))
         (started (org-books--format-property "STARTED" finished)))
    (org-set-property started (format-time-string "[%Y-%02m-%02d]"))))

;;;###autoload
(defun org-books-rate-book (rating)
  "Apply RATING to book at current point, mark it as read, and datestamp it.
This function keeps track of re-reads. If the book is being re-read,
the rating and finish date are marked separately for each re-read."
  (interactive "nRating (1-5): ")
  (when (> rating 0)
    (org-todo "READ")
    (let* ((finished-count (org-books--max-property "FINISHED"))
           (finished-str (org-books--format-property "FINISHED" finished-count))
           (rating-prop (org-books--format-property "MY-RATING" finished-count)))
      (org-set-property rating-prop (number-to-string rating))
      (org-set-property finished-str (format-time-string "[%Y-%02m-%02d]")))))

(provide 'org-books)
;;; org-books.el ends here
