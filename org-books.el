;;; org-books.el --- Reading list management with Org mode -*- lexical-binding: t -*-

;; Copyright (C) 2017 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>
;; Version: 0.3.0
;; Package-Requires: ((enlive "0.0.1") (s "1.11.0") (dash "2.14.1") (org "9.3") (emacs "29.1"))
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
(require 'org)
(require 'consult)
(require 'consult-org)
(require 's)
(require 'subr-x)
(require 'url)
(require 'url-parse)
(require 'ht)

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

(defcustom org-books-librarything-get-amazon-details t
  "Parse linked Amazon page when getting details from LibraryThing?
Gets page number and year, if the latter is not present in the
LibraryThing node. Slows down scraping."
  :type 'boolean
  :group 'org-books)

(defcustom org-books-url-pattern-dispatches
  '(("amazon\\." . org-books-get-details-amazon)
    ("goodreads\\.com/series" . org-books-goodreads-series-get-urls)
    ("goodreads\\.com" . org-books-get-details-goodreads)
    ("openlibrary\\.org" . org-books-get-details-openlibrary)
    ("librarything\\.com/nseries" . org-books-librarything-series-get-urls)
    ("librarything\\.com" . org-books-get-details-librarything)
    ("app\\.thestorygraph\\.com/books/" . org-books-get-details-thestorygraph)
    ("app\\.thestorygraph\\.com/series/" . org-books-thestorygraph-series-get-urls))
  "Pairs of url patterns and functions taking url and returning book details.
Check documentation of `org-books-get-details' for
return structure from these functions."
  :type '(alist :key-type string :value-type symbol)
  :group 'org-books)

(defcustom org-books-genre-tag-associations (make-hash-table :test #'equal)
  "A hash table of genres scraped from Goodreads to be automatically assigned tags.
The keys are the genres as written on Goodreads, and the values are the text
of the tag to be assigned, e.g.:

\(\"Humor\" \"funny\")

Different genres can be assigned the same tag. Duplicate tags are removed
by the `org-books-add-genre-tags' function during assignment."
  :type '(hash-table :key-type string :value-type string)
  :group 'org-books)

(defvar org-books-after-insert-hook nil
  "Hook to run after inserting a book.")

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

(defun org-books-fetch-node-safe (url)
  "Fetch node from URL. Ask to retry on failure."
  (let ((node (enlive-fetch url)))
    (if (null node)
        (when (yes-or-no-p "Error in fetching url. Try again? ")
          (org-books-fetch-node-safe url))
      node)))

(defun org-books-get-amazon-numpages (page-node)
  "Get the number of pages in a book from amazon PAGE-NODE."
  (ignore-errors
    (->>
     (enlive-query page-node [.detail-bullet-list])
     (enlive-text)
     (s-match "[0-9]+ pages")
     (-first-item)
     (s-split-words)
     (-first-item))))

(defun org-books-get-amazon-year (page-node)
  "Get the publication year of a book from amazon PAGE-NODE."
  (->>
   (enlive-query page-node [.detail-bullet-list > li > span])
   (enlive-text)
   (s-match (rx (group (= 4 num)) ")"))
   (-second-item)))

(defun org-books-get-details-goodreads (url)
  "Get book details from goodreads URL."
  (let* ((page-node (enlive-fetch url))
         (title (org-books-get-goodreads-title page-node))
         (author (org-books-get-goodreads-author page-node))
         (featured-details (enlive-query-all page-node [.FeaturedDetails > p]))
         (numpages (org-books-get-goodreads-pages featured-details))
         (date (org-books-get-goodreads-date featured-details))
         (gr-rating (org-books-get-goodreads-rating page-node))
         (genres (org-books-get-goodreads-genres page-node)))
    (if (not (string-equal title ""))
        (list title author `(("YEAR" . ,date)
                             ("PAGES" . ,numpages)
                             ("GOODREADS-RATING" . ,gr-rating)
                             ("GOODREADS-URL" . ,url)
                             (:genres . ,genres))))))

(defun org-books-get-goodreads-author (page-node)
  "Retrieve author name(s) from PAGE-NODE of Goodreads page."
  (->> (enlive-query-all page-node [.ContributorLink__name])
    (-map #'enlive-text)
    (-uniq)
    (s-join ", ")
    (org-books--clean-str)))

(defun org-books--filter-by-itemprop (itemprop elements)
  "Filter the ELEMENTS in html soup by ITEMPROP."
  (--filter (string= itemprop (enlive-attr it 'itemprop)) elements))

(defun org-books-get-goodreads-title (page-node)
  "Retrieve book title from PAGE-NODE of Goodreads page."
  (let ((title (org-books--clean-str (enlive-text (enlive-query page-node [h1]))))
        (series (org-books--clean-str (enlive-text (enlive-query page-node [.Text__title3 > a])))))
    (if (equal "" series)
        title
      (s-concat title " (" (s-replace "#" "" series) ")"))))

(defun org-books-get-goodreads-pages (featured-details)
  "Retrieve pagenum from FEATURED-DETAILS of Goodreads page."
  (->> featured-details
       (car)
       (enlive-text)
       (s-match (rx (group (+ digit)) " pages"))
       (-second-item)))

(defun org-books-get-goodreads-date (featured-details)
  "Retrieve publication date from FEATURED-DETAILS of Goodreads page."
  (->> featured-details
       (-second-item)
       (enlive-text)
       (s-match (rx (= 4 digit)))
       (car)))

(defun org-books-get-goodreads-rating (page-node)
  "Retrieve average rating from PAGE-NODE of Goodreads page."
  (enlive-text (enlive-query page-node [.RatingStatistics__rating])))

(defun org-books-get-goodreads-genres (page-node)
  "Retrieve the genre tags from PAGE-NODE of Goodreads page."
  (-map #'enlive-text
      (enlive-query-all page-node [.BookPageMetadataSection__genres > ul > span > span > a])))

(defun org-books-goodreads-series-get-urls (series-url)
  "Collect the URLs of a book series from Goodreads at once using SERIES-URL."
  (--> (enlive-fetch series-url)
       (enlive-query it [.gr-col-md-8])
       (enlive-query-all it [.responsiveBook > .objectLockupContent > .u-paddingBottomXSmall > a])
       (--map (s-prepend "https://www.goodreads.com" (enlive-attr it 'href)) it)
       (ht (:fn #'org-books-get-details-goodreads) (:urls it))))

(defun org-books-get-details-openlibrary (url)
  "Get book details from OpenLibrary URL."
  (let* ((page-node (enlive-fetch url))
         (title (org-books-get-openlibrary-title page-node))
         (author (org-books-get-openlibrary-author page-node))
         (numpages (org-books-get-openlibrary-pages page-node))
         (date (org-books-get-openlibrary-date page-node)))
    (if (not (string-equal title ""))
        (list title author `(("YEAR" . ,date)
                             ("PAGES" . ,numpages)
                             ("OPENLIBRARY-URL" . ,url))))))

(defun org-books-get-openlibrary-title (page-node)
  "Retrieve title from PAGE-NODE of OpenLibrary page."
  (->> (enlive-query page-node [.work-title])
       (enlive-text)
       (s-replace "Book " "")))

(defun org-books-get-openlibrary-author (page-node)
  "Retrieve author name(s) from PAGE-NODE of OpenLibrary page."
  (->> (enlive-query-all page-node [.edition-byline > a])
       (-map #'enlive-text)
       (s-join ", ")
       (org-books--clean-str)))

(defun org-books-get-openlibrary-date (page-node)
  "Retrieve publication date from PAGE-NODE of OpenLibrary page."
  (->> (enlive-query page-node [.work-line])
       (-map #'enlive-text)
       (-last-item)
       (s-match (rx (= 4 digit)))
       (first)))

(defun org-books-get-openlibrary-pages (page-node)
  "Retrieve pagenum from PAGE-NODE of OpenLibrary page."
  (->> (enlive-query page-node [.edition-pages])
       (enlive-text)))

(defun org-books-get-details-librarything (url)
  "Get book details from librarything URL."
  (let* ((page-node (enlive-fetch url))
         (title (org-books-get-librarything-title-series page-node))
         (author (org-books-get-librarything-author page-node))
         (lt-rating (org-books-get-librarything-rating page-node))
         (amazon-url (when org-books-librarything-get-amazon-details
                       (org-books-get-librarything-amazon-url page-node)))
         (amazon-node (when amazon-url
                        (org-books-fetch-node-safe amazon-url)))
         (date (or (org-books-get-librarything-date page-node)
                   (when amazon-node
                     (org-books-get-amazon-year amazon-node))
                   (read-string (format "YEAR value for %s: " title))))
         ;; numpages doesn't always work, so I should have a fallback.
         (numpages (when amazon-node
                     (org-books-get-amazon-numpages amazon-node))))
    (list title author `(("YEAR" . ,date)
                         ("PAGES" . ,numpages)
                         ("LIBRARYTHING-RATING" . ,lt-rating)
                         ("LIBRARYTHING-URL" . ,url)))))

(defun org-books-get-librarything-author (page-node)
  "Retrieve author name from PAGE-NODE of a LibraryThing page."
  (->> (enlive-query-all page-node [.headsummary > h2 > a])
       (-map #'enlive-text)
       (s-join ", ")))

(defun org-books-get-librarything-title-series (page-node)
  "Retrieve title and series name from PAGE-NODE of a LibraryThing page."
  (let ((title (org-books-get-librarything-title-dispatch page-node))
        (series (org-books-get-librarything-series page-node)))
    (if series
        (s-concat title " " series)
      title)))

(defun org-books-get-librarything-title-dispatch (page-node)
  "Retrieve book title from a LibraryThing PAGE-NODE.
Tries to get the canonical title from the wiki, then the original title,
and finally falls back to the title from the page header if both of the
above are empty.

The header should always have the book title in it, however it sometimes
comes with additional information, like the publication date, so it's
tried last."
  (or (org-books-get-librarything-title-from page-node [.divcanonicaltitle])
      (org-books-get-librarything-title-from page-node [.divoriginaltitle])
      (org-books-get-librarything-title-from page-node [.headsummary > h1])))

(defun org-books-get-librarything-title-from (page-node query)
  "Retrieve the book title from PAGE-NODE using QUERY.
If the title cannot be found, return nil."
  (--> (enlive-query page-node query)
       (enlive-text it)
       (s-trim it)
       (unless (string-empty-p it)
         it)))

(defun org-books-get-librarything-series (page-node)
  "Retrieve series name from PAGE-NODE of a LibraryThing page.
If a series cannot be found, return nil."
  (let ((series-node (enlive-query-all page-node [:seriesforwork_container > div])))
    (when series-node
      (--> series-node
       (-map #'enlive-text it)
       (--map (replace-regexp-in-string "[()]" "" it) it)
       (s-join "; " it)
       (s-concat "(" it ")")))))

(defun org-books-get-librarything-date (page-node)
  "Retrieve the publication date from PAGE-NODE of a LibraryThing page."
  (->> (enlive-query page-node [.divoriginalpublicationdate])
       (enlive-text)
       (s-match (rx line-start (= 4 digit)))
       (first)))

(defun org-books-get-librarything-rating (page-node)
  "Retrieve the average rating from PAGE-NODE of a LibraryThing page."
  (->> (enlive-query page-node [.dark_hint])
       (enlive-text)
       (s-match (rx (seq num (? (seq "." (** 1 2 num))))))
       (first)))

(defun org-books-get-librarything-amazon-url (page-node)
  "Retrieve Amazon url from PAGE-NODE of a LibraryThing page."
  (-> page-node
      (enlive-query-all [.quicklinks_in_greenbox > div > a])
      (-third-item)
      (cadr)
      (map-elt 'blurb)))

(defun org-books-librarything-series-get-urls (series-url)
  "Collect the URLs of a book series from LibraryThing at once using SERIES-URL."
  (--> (enlive-fetch series-url)
       (enlive-query it [.lt_table > table])
       (enlive-query-all it [.gss_title > a])
       (--map (s-prepend "https://www.librarything.com" (enlive-attr it 'href)) it)
       (ht (:fn #'org-books-get-details-librarything) (:urls it))))

(defun org-books-get-details-thestorygraph (url)
  "Get book details from a TheStoryGraph URL."
  (let* ((page-node (enlive-fetch url))
         (title (org-books-get-thestorygraph-title page-node))
         (author (org-books-get-thestorygraph-author page-node))
         (pages-year (org-books-get-thestorygraph-pages-year page-node))
         (numpages (map-elt pages-year :pages))
         (year (map-elt pages-year :year))
         (rating (org-books-get-thestorygraph-rating page-node))
         (tags (org-books-get-thestorygraph-tags page-node)))
    (list title author `(("YEAR" . ,year)
                         ("PAGES" . ,numpages)
                         ("STORYGRAPH" . ,url)
                         (:tags . ,tags)))))

(defun org-books-get-thestorygraph-author (page-node)
  "Retrieve author name from PAGE-NODE of a TheStoryGraph page."
  (-> page-node
      (enlive-query-all [.book-title-author-and-series > p > a])
      (-last-item)
      (enlive-text)))

(defun org-books-get-thestorygraph-title (page-node)
  "Retrieve book title from PAGE-NODE of a TheStoryGraph page."
  (let ((title (-> page-node
                   (enlive-query [.book-title-author-and-series > h3])
                   (enlive-text)
                   (s-trim)))
        (series (org-books-get-thestorygraph-series page-node)))
    (concat title (when series (concat " (" series ")")))))

(defun org-books-get-thestorygraph-series (page-node)
  "Retrieve series info from PAGE-NODE of a TheStoryGraph page."
  (let ((series-and-author (enlive-query-all page-node [.book-title-author-and-series > p])))
    (when (> (length series-and-author) 3)
      (->> (enlive-query-all (-first-item series-and-author) [a])
           (-map #'enlive-text)
           (s-join " ")
           (s-replace "#" "")))))

(defun org-books-get-thestorygraph-pages-year (page-node)
  "Retrieve page and publication date from PAGE-NODE of a TheStoryGraph page."
  (let ((text (enlive-text (enlive-query page-node [.col-span-2 > p])))
        (regex (rx (* space) (group (+ digit)) " pages"
                   (+? anything) "first pub " (group (+ digit))
                   (* anything))))
    (when (string-match regex text)
      (list :pages (match-string 1 text)
            :year (match-string 2 text)))))

(defun org-books-get-thestorygraph-rating (page-node)
  "Retrieve rating from PAGE-NODE of a TheStoryGraph page."
  (->> (enlive-query page-node [.average-star-rating])
       (enlive-text)
       (s-trim)))

(defun org-books-get-thestorygraph-tags (page-node)
  "Retrieve tags from PAGE-NODE of a TheStoryGraph page."
  (->> (enlive-query-all page-node [.book-page-tag-section > span])
       (-map #'enlive-text)
       (--filter (not (-contains? '("fiction" "fantasy") it)))
       (-uniq)
       (-map #'s-snake-case)))

(defun org-books-thestorygraph-series-get-urls (series-url)
  "Collect the URLs of a book series from TheStoryGraph at once using SERIES-URL."
  (--> (enlive-fetch series-url)
       (enlive-query-all it [.book-title-author-and-series > h3 > a])
       (--map (s-prepend "https://app.thestorygraph.com" (enlive-attr it 'href)) it)
       (ht (:fn #'org-books-get-details-thestorygraph) (:urls it))))

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
  (let ((output (--find (s-matches? (car it) url) org-books-url-pattern-dispatches)))
    (if (not output)
        (error (format "Url %s not understood" url))
      (funcall (cdr output) url))))

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
       (--mapcat (s-split "," it))
       (-map #'s-trim)
       (-distinct)
       (-sort #'s-less-p))))

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
    (cond
     ((null details) (when (yes-or-no-p "Error in fetching url. Try again? ")
                       (org-books-add-url url)))
     ((hash-table-p details) (org-books-add-many details))
     (t (apply #'org-books-add-book details)))))

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
    (goto-char (point-min))
    (org-set-property "AUTHOR" author)
    (org-set-property "ADDED" (org-books--today-string))
    (dolist (prop props)
      (when (cdr prop) ; Make sure each property has a value.
        (cond
         ((eq (car prop) :genres) (org-books-add-genre-tags (cdr prop)))
         ((eq (car prop) :tags) (org-set-tags (cdr prop)))
         (t (org-set-property (car prop) (cdr prop))))))
    (buffer-string)))

(defun org-books-add-genre-tags (genre-list)
  "Add all tags from associations in GENRE-LIST to current heading."
  (cl-loop for genre in genre-list
           for tag = (map-elt org-books-genre-tag-associations genre)
           if tag collect tag into tags
           finally do (org-set-tags (-uniq tags))))

(defun org-books--insert (title author &optional props)
  "Under the current heading, insert a new entry and save buffer.
TITLE, AUTHOR and PROPS are formatted using `org-books-format'."
  (let ((level (1+ (or (org-current-level) 0))))
    (org-books-goto-place)
    (save-excursion
      (insert (org-books-format level title author props)))
    (run-hooks 'org-books-after-insert-hook)))

(defun org-books-goto-place ()
  "Move to the position where insertion should happen."
  (if org-books-add-to-top
      (org-next-visible-heading 1)
    (org-get-next-sibling)))

(defmacro with-org-books-file (&rest body)
  "A convenience macro to execute BODY within `org-books-file'.
Switches to the file and warns if it doesn't exist."
  `(if org-books-file
       (progn
         (find-file org-books-file)
         ,@body)
     (message "org-books-file not set")))

(defun org-books--select-parent-heading ()
  "Move to the heading under which the book(s) should be added.
Currently using consult."
  (let ((match (format "level<=%d" org-books-file-depth)))
    (consult-org-heading match)))

;;;###autoload
(defun org-books-add-book (title author &optional props)
  "Add a book (specified by TITLE and AUTHOR) to the `org-books-file'.
Optionally apply PROPS."
  (interactive)
  (with-org-books-file
   (org-books--select-parent-heading)
   (org-fold-show-all)
   (org-books--insert title author props)
   (save-buffer)))

(defun org-books-add-many (url-ht)
  "Add many books at once (using links in the URL-HT table).
Supports LibraryThing and TheStoryGraph."
  (interactive)
  (let ((fn (map-elt url-ht :fn))
        (urls (map-elt url-ht :urls)))
    (with-org-books-file
     (org-books--select-parent-heading)
     (org-fold-show-all)
     (dolist (url urls)
       (let ((details (funcall fn url)))
         (save-excursion
           (apply #'org-books--insert details)))))
    (save-buffer)))

(defun org-books--safe-max (xs)
  "Extract the maximum value of XS with special provisions for nil and '(0).
Used to calculate the number of times a book has been started or finished."
  (pcase xs
    ('() 0)
    ('(0) 1)
    (_ (apply #'max xs))))

(defun org-books--times-read ()
  "Return the number of times a book has been read.
Does this my looking up the FINISHED properties and
finding the one with the highest index."
  (->> (org-entry-properties nil 'standard)
    (-map #'car)
    (--filter (or (s-contains? "FINISHED" it)
                  (s-contains? "DNF" it)))
    (--map (s-chop-prefixes '("FINISHED" "DNF" "-") it))
    (-map #'string-to-number)
    (org-books--safe-max)))

(defun org-books--format-property (name times-read)
  "Return a property name string given its NAME and TIMES-READ number.
Based on the number of times a book has been read,
the string will either be a bare NAME, or NAME-N,
where N is the current read count.

Used with STARTED, FINISHED, and MY-RATING properties."
  (let ((n (1+ times-read)))
    (if (= n 1)
        name
      (format (concat name "-%d") n))))

(defun org-books--today-string ()
  "Return today's date as a formatted string."
  (format-time-string "[%Y-%02m-%02d]"))

;;;###autoload
(defun org-books-start-reading ()
  "Mark book at point as READING.
Also sets the started property to today's date.

This function keeps track of re-reads.
If the book has already been read at least once,
opens a new property with the read count and date."
  (interactive)
  (if (string= "READING" (org-get-todo-state))
      (message "Already reading!")
    (org-todo "READING")
    (let* ((times-read (org-books--times-read))
           (started-prop (org-books--format-property "STARTED" times-read)))
      (org-set-property started-prop (org-books--today-string)))))

(defun org-books-dnf ()
  "Mark book at point as DNF (did not finish).
This also timestamps it with the current date."
  (interactive)
  (org-todo "DNF")
  (let* ((times-read (org-books--times-read))
         (dnf-prop (org-books--format-property "DNF" times-read)))
    (org-set-property dnf-prop (org-books--today-string))))

;;;###autoload
(defun org-books-rate-book (rating)
  "Apply RATING to book at current point, mark it as read, and datestamp it.
This function keeps track of re-reads. If the book is being re-read,
the rating and finish date are marked separately for each re-read."
  (interactive "nRating (1-5): ")
  (when (> rating 0)
    (org-todo "READ")
    (let* ((times-read (org-books--times-read))
           (finished-prop (org-books--format-property "FINISHED" times-read))
           (rating-prop (org-books--format-property "MY-RATING" times-read)))
      (org-set-property rating-prop (number-to-string rating))
      (org-set-property finished-prop (org-books--today-string)))))

;;;###autoload
(defun org-books-jump-to-reading ()
  "Jump to a book currently being read.
Can be used from anywhere, not just org-books mode."
  (interactive)
  (with-org-books-file
   (consult-org-heading "/+READING")))

(provide 'org-books)
;;; org-books.el ends here
