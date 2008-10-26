;; btxdb.el -- BibTeX Database running on top of EDB
;; Copyright (C) 2004,2005,2008 Thien-Thi Nguyen
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software Foundation,
;; Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LCD Archive Entry:
;; btxdb|Thorsten Ohl|ohl@crunch.ikp.physik.th-darmstadt.de
;; |BibTeX database program for Emacs; runs on top of EDB
;; |Nov 20, 1993|0.6|~/packages/btxdb.tar.gz|
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst btxdb:RCS-Id
  (concat (substring
           "btxdb.el,v 1.23 1993/11/20 22:22:54 ohl Exp"
           0 -8)
          " (plus maintenance by ttn)")
  "RCS identification string.")

(require 'database)


;;; File names

(defvar btxdb:journals-file-name "journals.el"
  "Name of a file containing the definition of `btxdb:journals'.")

(defvar btxdb:journals-file-path '(".")
  "List of directories where to search for `btxdb:journals-file-name'.")


;;; Formatting

(defvar btxdb:always-use-full-format nil
  "Iff this variable is not nil, btxdb will always use the full
\(aka `unknown'\) format for display.")

(defconst btxdb:ws "[ \t\n]*" "White Space.")

(defvar btxdb:record-type-regexp "[A-Za-z]+"
  "Regular expression matching the record type.
It must *not* contain grouping parentheses!")

(defvar btxdb:record-tag-regexp "[^ \t\n,]+"
  "Regular expression matching the record tag.
It must *not* contain grouping parentheses!")

(defconst btxdb:right-record-delim-regexp "[})]"
  "Right delimiter for reading records.  A useful (i.e. BibTeX compatible)
value is `[})]'.")

(defconst btxdb:left-record-delim-regexp "[{(]"
  "Right delimiter for reading records.  A useful (i.e. BibTeX compatible)
value is `[{(]'.")

(defvar btxdb:left-record-delim-string "{"
  "Left delimiter for writing records.  Useful (i.e. BibTeX compatible)
values are `{' or `('.")

(defvar btxdb:right-record-delim-string "}"
  "Right delimiter for writing records.  Useful (i.e. BibTeX compatible)
values are `}' or `)'.")

(defconst btxdb:field-name-regexp "[^ \t\n=]+"
  "Regular expression matching the field name.
It must *not* contain grouping parentheses!")

(defvar btxdb:left-field-delim-string "{"
  "Left delimiter for writing values of fields.  Useful (i.e. BibTeX
compatible) values are `{' or `\"'.")

(defvar btxdb:right-field-delim-string "}"
  "Right delimiter for writing values of fields.  Useful (i.e. BibTeX
compatible) values are `}' or `\"'.")


;;; Some list functions

(defun tho:symbols->alist (symbols)
  (mapcar (lambda (field)
            (cons (symbol-name field) field))
	  symbols))

(defun tho:member (symbol list)
  "Returns T iff SYMBOL is a member of LIST, using EQUAL for comparison."
  (if (not list)
      nil
    (if (equal symbol (car list))
	t
      (tho:member symbol (cdr list)))))

(defun tho:merge-2lists (list1 list2)
  "Returns the unions of LIST1 and LIST2.
Caveat: this is a naive and slow recursive implementation."
  (if (not list2)
      list1
    (if (tho:member (car list2) list1)
	(tho:merge-2lists list1 (cdr list2))
      (tho:merge-2lists (cons (car list2) list1) (cdr list2)))))

(defun tho:merge-lists (lists)
  "Returns the unions of LISTS.
Caveat: this is a naive and slow recursive implementation."
  (if (not (cdr lists))
      (car lists)
    (tho:merge-lists
     (cons (tho:merge-2lists (car lists) (car (cdr lists)))
	   (cdr (cdr lists))))))

(defun tho:subtract-lists (lista listb)
  "Remove all elements of LISTB from LISTA"
  (if (not (car lista))
      nil
    (if (memq (car lista) listb)
	(tho:difference (cdr lista) listb)
      (cons (car lista) (tho:difference (cdr lista) listb)))))


;;; Other trivia

(defun tho:not-nil-or-empty-stringp (obj)
  "Return NIL iff OBJ is NIL or an empty string.  (Coded clumsily to
be fast even if the compiler doesn't know about boolean shortcuts.)"
  (if obj
      (if (stringp obj)
	  (not (string-equal obj ""))
	t)
    nil))


;;;

(defvar btxdb:fields
  ;;
  ;; Format: (... (class . (required optional ignored)) ...)
  ;;

  '((article . ((author title journal year)
                (volume number pages month note)
                (errata)))

    (book . ((author editor title publisher year)
             (volume number series address edition month
                     note)
             (isbn lcnum)))

    (booklet . ((title)
                (author howpublished address month year note)
                ()))

    (conference . ((author title booktitle year)
                   (editor volume number series pages address
                           month organization publisher note)
                   ()))

    (inbook . ((author editor title chapter pages publisher
                       year)
               (volume number series type address edition month
                       note)
               ()))

    (incollection . ((author title booktitle publisher year)
                     (editor volume number series type chapter
                             pages address edition month note)
                     ()))

    (inproceedings . ((author title booktitle year)
                      (editor volume number series pages
                              address month organization publisher
                              note)
                      ()))

    (manual . ((title)
               (author organization address edition month year
                       note)
               (isbn lcnum)))

    (mastersthesis . ((author title school year)
                      (type address month note)
                      ()))

    (misc . ((none)
             (author title howpublished month year note)
             ()))

    (phdthesis . ((author title school year)
                  (type address month note)
                  ()))

    (proceedings . ((title year)
                    (editor volume number series address month
                            organization publisher note)
                    (isbn lcnum)))

    (techreport . ((author title institution year)
                   (type number address month note)
                   (errata)))

    (unpublished . ((author title note)
                    (month year)
                    ()))

    ;; This serves a dual purpose: catch errors and provide
    ;; fields beyond the BibTeX specs.
    (unknown . (()
                (key crossref)
                (annote keywords updated)))))

(defun btxdb:required-fields (class)
  "Return the required fields in an entry of type CLASS."
  (nth 0 (cdr (assoc class btxdb:fields))))

(defun btxdb:optional-fields (class)
  "Return the optional fields in an entry of type CLASS."
  (nth 1 (cdr (assoc class btxdb:fields))))

(defun btxdb:ignored-fields (class)
  "Return the ignored (by BibTeX) fields in an entry of type CLASS."
  (nth 2 (cdr (assoc class btxdb:fields))))

(defun btxdb:useful-fields (class)
  "Return the useful fields in an entry of type CLASS."
  (tho:merge-lists (cdr (assoc class btxdb:fields))))

(defun btxdb:bogus-fields ()
  "Return the useless nonempty fields in the current entry."
  (interactive)
  (tho:subtract-lists btxdb:field-names
		      (btxdb:useful-fields
		       (intern (dbf-displayed-record-field 'class)))))

(setq btxdb:help-info
      '(
	(annote . "An annotation.
It is not used by the standard bibliography styles,
but may be used by others that produce an annotated bibliography.")

	(author . "The name(s) of the author(s),
in the format described in the LaTeX book.
However, instead of separating by \"and\" we keep them one per line.")

	(booktitle . "Title of a book, part of which is being cited.
See the LaTeX book for how to type titles.
For book entries, use the TITLE field instead.")

	(chapter . "A chapter (or section or whatever) number.")

	(crossref . "The database key of the entry being cross referenced.")

	(edition . "The edition of a book - for example, \"Second\".
This should be an ordinal, and should have the first letter capitalized,
as shown here; the standard styles convert to lower case when necessary.")

	(editor . "Name(s) of editor(s), typed as indicated in the LaTeX
book.  If there is also an `author' field, then the `editor'
field gives the editor of the book or collection in which
the reference appears.")

	(howpublished . "How something strange has been published.
The first word should be capitalized.")

	(institution . "The sponsoring institution of a technical report.")

	(journal . "A journal name.
Abbreviations are provided for many journals.

The display format is either

      Journal: SYMBOL -> FULL-NAME (aka ABBREVIATED-NAME)
or
      Journal: NAME

Everything after the string \" -> \" will be ignored on
input; this allows us use both symbolic and explicit names.")

	(key . "Used for alphabetizing, cross referencing,
and creating a label when the `author' information, is
missing. This field should not be confused with the key
that appears in the `\cite' command and at the beginning of
the database entry.")

	(month . "The month in which the work was published or, for
an unpublished work, in which it was written.
\(This is an enumerated type.\)")

	(note . "Any additional information that can help the reader.
The first word should be capitalized.")

	(number . "The number of a journal, magazine, technical report,
or of a work in a series.  An issue of a journal or
magazine is usually identified by its volume and number;
the organization that issues a technical report usually
gives it a number; and sometimes books are given numbers in
a named series.")

	(organization . "The organization that sponsors a conference or
that publishes a `manual'.")

	(pages . "One or more page numbers or range of numbers,
such as `42--111' or `7,41,73--97' or `43+' (the `+' in
this last example indicates pages following that don't form
a simple range).  To make it easier to maintain
`Scribe'-compatible databases, the standard styles convert
a single dash (as in `7-33') to the double dash used in
TeX to denote number ranges (as in `7--33').")

        (publisher . "The publisher's name.")

        (school . "The name of the school where a thesis was written.")

	(series . "The name of a series or set of books.
When citing an entire book, the `title' field gives its
title and an optional `series' field gives the name of a
series or multi-volume set in which the book is published.")

	(title . "The work's title, typed as explained in the LaTeX book.")

	(type . "The type of a technical report---for example,
`Research Note'.")

	(volume . "The volume of a journal or multivolume book.")

	(year . "The year of publication or, for an unpublished work,
the year it was written.  Generally it should consist of
four numerals, such as `1984', although the standard styles
can handle any `year' whose last four nonpunctuation
characters are numerals, such as `(about 1984)'.")

        (errata . "Known (or suspected) errors in this publication.
Either a reference to published errata, or a short note
explaining the error.")

        (isbn . "The ISBN Number(s) of this publication.")

	(lcnum . "The Library of Congress classification number of
this publication.  This is used by some other libraries also.")))


(setq btxdb:all-fields
      (append
       (mapcar (lambda (pair)
                 (append (car (cdr pair))
                         (car (cdr (cdr pair)))
                         (car (cdr (cdr (cdr pair))))))
	       btxdb:fields)))

(setq btxdb:field-names
      (sort (copy-sequence (tho:merge-lists btxdb:all-fields))
	    (lambda (s1 s2)
              (string-lessp (symbol-name s1) (symbol-name s2)))))

(setq btxdb:field-name-alist
      (tho:symbols->alist btxdb:field-names))

(setq btxdb:alternate-format-names
      (mapcar (lambda (record)
                (let ((name (symbol-name (car record))))
		   ;;; use bibtex.fmt as generic format file
                  (if (string= name "unknown")
                      (cons "unknown" "bibtex.fmt")
                    (cons name (concat name ".fmt")))))
	      btxdb:fields))

(defun btxdb:set-format-from-data (record)
  "Iff BTXDB:ALWAYS-USE-FULL-FORMAT is not nil, set the custom format.
This will suceed, because we filter out bogus record types during the
initial reading and the user can't enter invalid enumeration types."
  (db-change-format (if btxdb:always-use-full-format
                        "unknown"
                      (db-record-field record 'class))))

(defun btxdb:toggle-full-format (arg)
  "Toggle the use of custom formats.  A prefix argument will force
custom format."
  (interactive "P")
  (setq btxdb:always-use-full-format
	(or arg (not btxdb:always-use-full-format)))
  (if btxdb:always-use-full-format
      (message "Using `unknown' format exclusively.")
    (message "Using custom formats.")))

(defun btxdb:note-update (&optional field old new)
  "Update the 'EDITED field in this record.  Called automatically when any
other field is updated."
  (dbf-this-record-set-field
   'updated (let ((now (current-time-string)))
	     (concat (substring now 4 10) ", " (substring now 20)))))


(setq btxdb:record-type-alist
      (mapcar (lambda (record)
                (if (car record)
                    (let ((name (symbol-name (car record))))
                      (cons name name))))
	       btxdb:fields))

(defun btxdb:set-record-type ()
  "Change the type of the current record"
  (interactive)
  (let ((class (completing-read "New record type: "
				btxdb:record-type-alist nil t)))
    (dbf-displayed-record-set-field 'class class)
    (db-emergency-restore-format t)))

(defun btxdb:validate-record ()
  ""
  (interactive)
  (switch-to-buffer (get-buffer-create "*BibTeX-Warnings*"))
  (setq buffer-read-only nil)
  (insert-string "BTXDB:VALIDATE-RECORD is not implemented yet!\n")
  (setq buffer-read-only t))


;;; This *almost* allows us to use M-q (fill-paragraph) on multi-line
;;; fields.  But EDB insists on killing the last character ...

(defun btxdb:set-fill-prefix (index)
  ""
  (let ((amt (dbf-this-field-indent)))
    (if amt (setq fill-prefix (make-string amt ? )))))



;;; Datatype year

(edb-define-displaytype 'year-short nil
  :indent           nil
  :max-height       1
  :actual->display 'btxdb:extract-year)

(edb-define-recordfieldtype 'year nil
  :type           'nil-or-string
  :default-value  ""
  :sort-fn        'btxdb:compare-years
  :match-function 'btxdb:match-years)

(defun btxdb:extract-year (s)
  (if s
      (if (string-match "\\([0-9]+\\)[\\s.\\s)]*$" s)
	  (substring s (match-beginning 1) (match-end 1))
	"")
    ""))


(defun btxdb:compare-years (y1 y2)
  (< (string-to-number (btxdb:extract-year y1))
     (string-to-number (btxdb:extract-year y2))))

(defun btxdb:match-years (y1 y2)
  (= (string-to-number (btxdb:extract-year y1))
     (string-to-number (btxdb:extract-year y2))))


;;; Enumerated types:

;;; Datatype class

(edb-define-enumtype 'class
  (mapcar (lambda (record)
            (symbol-name (car record)))
	  btxdb:fields) nil)


;;; Datatype month

(edb-define-enumtype 'month
	'(;internal/input /display    /file
	  ( 0 "unknown"   "unknown"   "")
	  ( 1 "January"   "January"   "jan")
	  ( 2 "February"  "February"  "feb")
	  ( 3 "March"     "March"     "mar")
	  ( 4 "April"     "April"     "apr")
	  ( 5 "May"       "May"       "may")
	  ( 6 "June"      "June"      "jun")
	  ( 7 "July"      "July"      "jul")
	  ( 8 "August"    "August"    "aug")
	  ( 9 "September" "September" "sep")
	  (10 "October"   "October"   "oct")
	  (11 "November"  "November"  "nov")
	  (11 "November"  "November"  "nov")
	  (12 "December"  "December"  "dec")) nil)


;;; Datatype Author

(edb-define-displaytype 'author nil
  :indent           t
  :max-height       nil
  :actual->display 'btxdb:authors-list->display
  :display->actual 'btxdb:authors-display->list)

(edb-define-displaytype 'author-short nil
  :indent           nil
  :max-height       1
  :actual->display 'btxdb:authors-list->display-short)

(edb-define-recordfieldtype 'author nil
  :type           'author
  :default-value   nil
  :sort-fn        'btxdb:compare-authors
  :match-function 'btxdb:match-authors
  :actual->stored (put 'btxdb 'author 'btxdb:authors-list->string)
  :stored->actual 'btxdb:authors->list)

(defun btxdb:authors->list (authors)
  (if (tho:not-nil-or-empty-stringp authors)
      (with-temp-buffer
	(save-excursion (insert authors))
	(save-excursion
	  (while (re-search-forward "[ \t\n]+" nil t)
	    (replace-match " ")))
	(btxdb:authors->list-2 " and "))
    nil))


(defun btxdb:authors-display->list (authors)
  (if (tho:not-nil-or-empty-stringp authors)
      (with-temp-buffer
	(save-excursion (insert authors))
	(save-excursion
	  (while (re-search-forward "[ \t]+" nil t)
	    (replace-match " ")))
	(save-excursion
	  (while (re-search-forward "[ \t]+$" nil t)
            (delete-region (match-beginning 0) (match-end 0))))
	(btxdb:authors->list-2 "\n"))
    nil))

(defun btxdb:authors->list-2 (separator)
  (if (eq (point) (point-max))
      nil
    (let* ((beginning (point))
	   (end (if (search-forward separator (point-max) 'move)
		    (match-beginning 0)
		  (point)))
	   name)
      (save-excursion
	(goto-char end)
	(if (search-backward " " beginning t)
	    (setq name (cons (buffer-substring (1+ (point)) end)
			     (buffer-substring beginning (point))))
	  (setq name (cons (buffer-substring beginning end) ""))))
      (cons name (btxdb:authors->list-2 separator)))))

(defun btxdb:authors-list->string (authors)
  (if authors
      (concat btxdb:left-field-delim-string
	      (mapconcat (lambda (name)
                           (if (string-equal (cdr name) "")
                               (car name)
                             (concat (cdr name) " " (car name))))
			 authors
			 " and\n    ")
	      btxdb:right-field-delim-string)
    nil))


(defun btxdb:authors-list->display (authors)
  (mapconcat (lambda (name)
               (if (string-equal (cdr name) "")
                   (car name)
                 (concat (cdr name) " " (car name))))
	     authors
	     "\n"))

(defun btxdb:authors-list->display-short (authors)
  (mapconcat (lambda (name) (car name)) authors ", "))

(defun btxdb:compare-authors (a1 a2)
  "Compare the concatenated *last* names of the author lists A1 and A2."
  (string-lessp
   (mapconcat (lambda (name) (car name)) a1 "")
   (mapconcat (lambda (name) (car name)) a2 "")))

(defun btxdb:match-authors (pattern authors)
  (if (string-match (concat (cdr (car pattern)) " "
			    (car (car pattern)))
		    (concat (cdr (car authors)) " "
			    (car (car authors))))
      t
    (if (cdr authors)
	(btxdb:match-authors pattern (cdr authors))
      nil)))


;;; Datatype journal

(edb-define-displaytype 'journal nil
  :indent           t
  :max-height       nil
  :actual->display 'btxdb:journal->display
  :display->actual 'btxdb:display->journal)

(edb-define-recordfieldtype 'journal nil
  :type           'journal
  :default-value   nil
  :sort-fn        'btxdb:compare-journals
  :match-function 'btxdb:match-journals
  :actual->stored (put 'btxdb 'journal 'btxdb:journal->string)
  :stored->actual 'btxdb:string->journal)

(defun btxdb:build-journal-alists ()
  (let ((journals-file (db-locate-readable-file-prefer-cwd
                        btxdb:journals-file-name
                        btxdb:journals-file-path)))
    (if journals-file
	(load-file journals-file)
      (setq btxdb:journals nil)))
  (setq btxdb:journal-symbol-alist
	(mapcar (lambda (j)
                  (list (aref j 0) (aref j 1)  (aref j 2)))
		btxdb:journals))
  (setq btxdb:journal-full-alist
	(mapcar (lambda (j)
                  (list (aref j 1) (aref j 0)  (aref j 2)))
		btxdb:journals))
  (setq btxdb:journal-abbrev-alist
	(mapcar (lambda (j)
                  (list (aref j 2) (aref j 0)  (aref j 1)))
		btxdb:journals)))

;;; This is a minimalistic implementation, we could/should perform
;;; much better consistency checking here.

(defun btxdb:journal->display (j)
  (if j
      (let ((expansion (assoc j btxdb:journal-symbol-alist)))
	(if expansion
	    (concat j " -> " (car (cdr expansion)) " (aka "
		    (car (cdr (cdr expansion))) ")")
	  j))
    ""))

(defun btxdb:display->journal (j)
  (if (string-match " -> " j)
      (substring j 0 (match-beginning 0))
    j))

(defun btxdb:journal->string (j)
  (if (tho:not-nil-or-empty-stringp j)
      (if (assoc j btxdb:journal-symbol-alist)
	  j
	(concat btxdb:left-field-delim-string
		j
		btxdb:right-field-delim-string))
    nil))

(defun btxdb:string->journal (j) j)


;;; I/O functions.

;;; Input

(setq btxdb:syntax-table nil)

(defun btxdb:setup-read-buffer ()
  "Setup the sytax table etc. in the read buffer."
  (save-excursion
    (set-buffer db-buffer)
    (if (not btxdb:syntax-table)
	(setq btxdb:syntax-table (copy-syntax-table)))
    (set-syntax-table btxdb:syntax-table)
    (modify-syntax-entry ?{ "(}")
    (modify-syntax-entry ?} ")")
    (modify-syntax-entry ?( ".")
    (modify-syntax-entry ?) ".")
    (modify-syntax-entry ?[ ".")
    (modify-syntax-entry ?] ".")))

(setq btxdb:comments-alist nil)

(defun btxdb:read-comments ()
  (save-excursion
    (set-buffer db-buffer)
    (goto-char (point-min))
    (if (search-forward "@" nil t)
	(setf (sepinfo-pre-first-string (database-record-sepinfo database))
	      (buffer-substring (point-min) (point)))))
  ;; Clean up.
  (remove-hook 'db-before-read-hooks 'btxdb:read-comments))

(defun btxdb:rrfr ()
  "Parse a BibTeX record."

  ;; canonicalize whitespace
  (save-excursion
    (while (re-search-forward "^[ \t\n]+\\|[ \t\n]+$" nil t)
      (delete-region (match-beginning 0) (match-end 0))))

  (if (re-search-forward
       (concat "\\(" btxdb:record-type-regexp "\\)"
	       btxdb:ws
	       btxdb:left-record-delim-regexp
	       btxdb:ws
	       "\\(" btxdb:record-tag-regexp "\\)"
	       btxdb:ws
	       ",")
       nil t)
      (let ((class (downcase (match-string 1)))
	    (tag (match-string 2))
            plist)
        (flet ((plpush (f v) (setq plist (plist-put plist f v))))
          ;; don't allow bogus entry types which will screw up our
          ;; format file search later.
          (plpush 'class
                  (if (assoc class btxdb:alternate-format-names)
                      class
                    (db-warning "Entry type \"%s\" not recognized by BibTeX!"
                                (match-string 1))
                    "unknown"))
          (plpush 'tag tag)
          (plpush 'month 0)
          (while (re-search-forward
                  (concat "\\(" btxdb:field-name-regexp "\\)"
                          btxdb:ws
                          "=" btxdb:ws)
                  nil t)
            (let ((key (cdr (assoc (downcase (match-string 1))
                                   btxdb:field-name-alist)))
                  (beg (point)))
              (forward-sexp)
              (if key
                  (plpush key (if (or (equal ?{ (char-after beg))
                                      (equal ?\" (char-after beg)))
                                  (buffer-substring (1+ beg) (1- (point)))
                                (buffer-substring beg (point)))))))
          plist))
    (error "Illformed record!")))

(defun btxdb:stored->actual ()
  (database-stored->actual)
  ;; Clean up.
  (remove-hook 'db-after-read-hooks 'btxdb:stored->actual))


;;; Output

(defun btxdb:write-field (field)
  "Write a field in a BibTeX record, including a leading field
separator.  FIELD is a cons cell FIELDNAME . FIELDKEY."
  (let* ((name (car field))
	 (key (cdr field))
	 (val (db-record-field record key))
	 (a->s (get 'btxdb key)))
    (if a->s (setq val (funcall a->s val)))
    (if (tho:not-nil-or-empty-stringp val)
	(if a->s
	    (insert ",\n  " name "=" val)
	  (insert ",\n  " name "="
		  btxdb:left-field-delim-string
		  (replace-regexp-in-string "\n" "\n    " val)
		  btxdb:right-field-delim-string)))))


(defun btxdb:wrfr (record)
  "Write a BibTeX record."
  (insert (db-record-field record 'class)
	  btxdb:left-record-delim-string
	  (db-record-field record 'tag))
  (mapcar 'btxdb:write-field btxdb:field-name-alist)
  (insert btxdb:right-record-delim-string "\n"))


;;; Interface to EDB

(setq btxdb:initializedp nil)

(defun btxdb:initialize ()
  "Initialize the BibTeX subsystem for EDB."

  (database-set-fieldnames-to-list
   database (append '((class . class) tag)
		    (mapcar (lambda (key)
                              (cond ((memq key '(year month journal author))
                                     (cons key key))
                                    ((eq key 'editor)
                                     (cons 'editor 'author))
                                    (t key)))
			    btxdb:field-names))
   'string-or-nil)

  ;; Install field-specific help strings.
  (let (help)
    (mapc (lambda (field)
            (when (setq help (cdr (assq field btxdb:help-info)))
              (db-set-field-help database field help)))
          btxdb:field-names))

  (dbf-set-summary-format
   (concat "[\\tag,string-or-nil,width=8] \\author,author-short,width=20 /"
	   "/ \\title,string-or-nil,width=34  (\\year,year-short)"))

  (setq dbf-format-name-spec-alist btxdb:alternate-format-names)
  (setq dbf-before-display-record-function 'btxdb:set-format-from-data)
  (setq dbf-first-change-function 'btxdb:note-update)
  (setq dbf-enter-field-function 'btxdb:set-fill-prefix)
  (setf (database-read-record-from-region  database) 'btxdb:rrfr
        (database-write-region-from-record database) 'btxdb:wrfr)
  (add-hook 'db-before-read-hooks 'btxdb:read-comments)
  (add-hook 'db-after-read-hooks 'btxdb:stored->actual)

  (let ((si (database-record-sepinfo database)))
    (setf
     (sepinfo-pre-first-string          si) ""
     (sepinfo-pre-first-regexp          si) "[^@]*@"
     (sepinfo-pre-first-regexp-submatch si) 0
     (sepinfo-sep-string                si) "\n@"
     (sepinfo-post-last-string          si) (concat "\n# End of File,"
                                                    " written by\n# "
                                                    btxdb:RCS-Id "\n")
     (sepinfo-post-last-regexp          si) "\n+"
     (sepinfo-post-last-regexp-submatch si) 0))

  (btxdb:build-journal-alists)
  (if (boundp 'db-buffer)
      (btxdb:setup-read-buffer))

  (define-key database-view-mode-map "\C-xt" 'btxdb:set-record-type)
  (define-key database-edit-mode-map "\C-xt" 'btxdb:set-record-type)
  (define-key database-view-mode-map "\C-xv" 'btxdb:toggle-full-format)
  (define-key database-edit-mode-map "\C-xv" 'btxdb:toggle-full-format)

  (setq btxdb:initializedp t))

(defun btxdb-find-file (filename)
  "Read a .bib database from FILENAME; run EDB on it."
  (interactive "fBibTeX Database file: \n")
  (db-find-file filename "bibtex.fmt"))

(provide 'btxdb)

;;; btxdb.el ends here
