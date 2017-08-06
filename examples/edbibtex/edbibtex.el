;; $Id$
;; LCD Archive Entry:
;; edbibtex|Michael Burschik|burschik@uni-bonn.de|
;; A BibTeX interface for Michael Ernst's Emacs Database.|
;; $Date$|0.4|
;; This file can be folded Origami style with Jamie Lokier's "folding.el".

;;{{{ File Header


;; Copyright (C) 1995, Michael Burschik.

;; This file is intended to be used with GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;}}}

(require 'database)

;;{{{ Constants and Variables

;;{{{ Edbibtex version

;; self-explanatory
(defconst edbibtex-version "0.4")

;;}}}
;;{{{ bibtex-mode-syntax-table

;; Define this here, so we don't have to require bibtex.
(defvar bibtex-mode-syntax-table
  (eval-when-compile
    (let ((st (make-syntax-table)))
      (modify-syntax-entry ?\" "\"" st)
      (modify-syntax-entry ?$ "$$  " st)
      (modify-syntax-entry ?% "<   " st)
      (modify-syntax-entry ?'  "w   " st)
      (modify-syntax-entry ?@  "w   " st)
      (modify-syntax-entry ?\\ "\\" st)
      (modify-syntax-entry ?\f ">   " st)
      (modify-syntax-entry ?\n ">   " st)
      (modify-syntax-entry ?~ " " st)
      st)))

;;}}}
;;{{{ BibTeX-entry-type-alist


;; This alist is used to find the correct format file and to check and
;; validate entry types.

(defconst BibTeX-entry-type-alist
  '(("article" "article.fmt"
     (author title journal year))
    ("book" "book.fmt"
     ((xor author editor) title publisher year))
    ("booklet" "booklet.fmt"
     (title))
    ("conference" "inproceedings.fmt"
     (author title booktitle year))
    ("generic" "bibtex.fmt" nil)
    ("inbook" "inbook.fmt"
     ((xor author editor) title (or chapter pages)))
    ("incollection" "incollection.fmt"
     (author title booktitle publisher year))
    ("inproceedings" "inproceedings.fmt"
     (author title booktitle year))
    ("manual" "manual.fmt"
     (title))
    ("mastersthesis" "mastersthesis.fmt"
     (author title school year))
    ("misc" "misc.fmt" nil)
    ("phdthesis" "phdthesis.fmt"
     (author title school year))
    ("preamble" "preamble.fmt"
     (BibTeX-val))
    ("proceedings" "proceedings.fmt"
     (title year))
    ("string" "string.fmt"
     (BibTeX-var BibTeX-val))
    ("techreport" "techreport.fmt"
     (author title institution year))
    ("unpublished" "unpublished.fmt"
     (author title note)))

  "This constant contains a list of valid BibTeX entry types,
their required fields and of format files associated with them.")


;;}}}
;;{{{ BibTeX-field-alist


(defconst BibTeX-field-alist
  '(("address" . address)
    ("annote" . annote)
    ("author" . author)
    ("booktitle" . booktitle)
    ("chapter" . chapter)
    ("crossref" . crossref)
    ("edition" . edition)
    ("editor" . editor)
    ("howpublished" . howpublished)
    ("institution" . institution)
    ("journal" . journal)
    ("key" . key)
    ("month" . month)
    ("note" . note)
    ("number" . number)
    ("organization" . organization)
    ("pages" . pages)
    ("publisher" . publisher)
    ("school" . school)
    ("series" . series)
    ("title" . title)
    ("type" . type)
    ("volume" . volume)
    ("year" . year)
    ;; The following fields are non-standard.
    ;; Add your own here.
    ("abstract" . abstract)
    ("errata" . errata)
    ("isbn" . isbn)
    ("issn" . issn)
    ("keywords" . keywords)
    ("language" . language)
    ("lcnum" . lcnum)
    ("updated" . updated)
    ("warnings" . warnings))

  "This constant contains a list of valid BibTeX field names,
some of which will be ignored by the standard BibTeX styles.")


;;}}}
;;{{{ BibTeX-fields

(defconst BibTeX-fields
  (eval-when-compile
    (mapcar 'cdr BibTeX-field-alist))
  "A list of all acceptable fieldnames.")

(defconst BibTeX-special-fields
  '(BibTeX-entry-type
    BibTeX-tag
    BibTeX-val
    BibTeX-var)
  "A list of fields that have no print name.")

;;}}}
;;{{{ BibTeX-string-alist


(defvar BibTeX-string-alist
  '(("jan" . "January")
    ("feb" . "February")
    ("mar" . "March")
    ("apr" . "April")
    ("may" . "May")
    ("jun" . "June")
    ("jul" . "July")
    ("aug" . "August")
    ("sep" . "September")
    ("oct" . "October")
    ("nov" . "November")
    ("dec" . "December"))

  "This variable contains the string abbreviations and their expansions.")


;;}}}
;;{{{ BibTeX-alternate-formats

(defconst BibTeX-alternate-formats
  (eval-when-compile
    (cons '("generic" . "bibtex.fmt")
	  (mapcar (lambda (x)
                    (cons (nth 0 x) (nth 1 x)))
                  BibTeX-entry-type-alist)))
  "A list of formats for different BibTeX entry types.")

;;}}}
;;{{{ Keymaps


(define-key database-view-mode-map "\C-c\C-v"
  'BibTeX-validate-displayed-record)

(define-key database-view-mode-map "\C-c\C-f"
  'BibTeX-set-format)

;;}}}
;;{{{ User Options

(defvar BibTeX-multiple-format t
  "*Turns entry type dependent format selection on.")

(defvar BibTeX-use-default-format t
  "*Turns the use of the default format for new database entries on.")


;;}}}
;;{{{ Miscellaneous

(defvar BibTeX-default-format '("generic" . "bibtex.fmt")
  "contains the format info of the last record on display")

;;}}}

;;}}}
;;{{{ Predicates and Other Inline Functions

;; We need to check for nil although the default type used by
;; `database-set-fieldnames-to-list' is `string', since `db-make-record'
;; does not use this information. I contemplated changing the
;; definition of `db-make-record', but this might cause problems for
;; other databases.

;; Return t if FIELD is nil or "".
(defsubst BibTeX-field-empty-p (field)
  (or (null field) (string-equal field "")))

;; Return t if FIELD is neither nil nor "".
(defsubst BibTeX-field-not-empty-p (field)
  (and field (not (string-equal field ""))))

;; Return a record for DATABASE. Default field value is "".
(defsubst BibTeX-make-record (database)
  (make-vector (length (database-fieldnames database)) ""))


;;}}}
;;{{{ Accessor Functions

;; Looks up an ABBREVIATION in BibTeX-string-alist. Returns the
;; expansion, if there is one.
(defsubst BibTeX-get-expansion (string)
  (cdr (assoc (downcase string) BibTeX-string-alist)))

;; Returns the print name of FIELD.
(defsubst BibTeX-get-print-name (symbol)
  (car (rassq symbol BibTeX-field-alist)))

;; Returns the field name of STRING.
(defsubst BibTeX-get-field-name (string)
  (cdr (assoc (downcase string) BibTeX-field-alist)))

;; Looks up TYPE in BibTeX-entry-type-alist. Returns a list of
;; required BibTeX fields.
(defsubst BibTeX-get-required-field-list (entry-type)
  (nth 2 (assoc (downcase entry-type) BibTeX-entry-type-alist)))


;;}}}
;;{{{ Delimiting Records


;; This could have been done with regular expressions, of course, but
;; this way it seemed safer, since the BibTeX files I looked at all
;; tended to have all kinds of stuff between the entries.
;; Unfortunately, folding information and comments will be lost
;; converting the database into EDB internal representation.
;; Therefore, comments should be placed within the record itself, say
;; in a field with the name "COMMENT", which will simply be ignored by
;; BibTeX.

(defun BibTeX-delimit-record (prev-end-pos)

  "This is the sepinfo-sep-function, which takes the end position
of the previous record as an argument and returns a dotted list
of the end position of this record and the starting position of
the next record."

  (if (re-search-forward
       "^[ \t]*@\\([A-z]*\\)[ \t\n]*\\([({]\\)" nil t 2)
      (cons (match-beginning 0)
	    (match-beginning 0))
    (list (point-max))))


;;}}}
;;{{{ Delimiting Fields


;;{{{ BibTeX-find-value

(defun BibTeX-find-value ()

  "Returns the value of a BibTeX field, can handle concatenations."

  (let ((result ""))
    (catch 'done
      (while (skip-chars-forward " \t\n")
	;; Skip over whitespace.
	(let ((start (point))
	      (char (char-after (point))))

	  (cond ((or (char-equal char ?\,)  ; end of field
		     (char-equal char ?\})  ; end of record
		     (char-equal char ?\))) ; end of record
		 ;; Hit the end of field (and record maybe).
		 (throw 'done result))

		((char-equal char ?#) ; concatenation operator
		 ;; Ignore: abbreviations are handled later
		 (forward-char 1))

		((or (char-equal char ?\") ; beginning of delimited token
		     (char-equal char ?{)) ; beginning of delimited token
		 ;; Move to the end of the delimited token and add the
		 ;; token to result.
		 (forward-sexp) ; this requires a suitable syntax table
		 (setq result
		       (concat result
			       (buffer-substring (+ start 1) (- (point) 1)))))

		((looking-at "\\([0-9]+\\)\\b") ; number
		 ;; Add token matched to result.
		 (setq result (concat result (match-string 1)))
		 (goto-char (match-end 1)))

		(t ; abbreviation
		 ;; Move to the end of token, mark it and add it to
		 ;; result.
		 (forward-word 1)
		 (setq result
		       (concat result "#"
			       (buffer-substring start (point)) "#")))))))))



;;}}}


;;}}}
;;{{{ Reading the Database

;;{{{ BibTeX-set-up-db-buffer

;; This function should be added to db-before-read-hooks for the
;; database reading functions to work correctly!

(defun BibTeX-set-up-db-buffer ()
  "Sets the syntax table of db-buffer to the BibTeX syntax table."
  (save-excursion
    (set-buffer db-buffer)
    (set-syntax-table bibtex-mode-syntax-table))
  ;; Clean up.
  (remove-hook 'db-after-read-hook 'BibTeX-set-up-db-buffer))

;;}}}
;;{{{ BibTeX-rrfr


;; This function assumes the buffer is narrowed to the region.

(defun BibTeX-rrfr ()

  "Reads a BibTeX record from the region and returns a database record
with the appropriate slots set."

  (if (re-search-forward "@\\([A-z]+\\)[ \t\n]*[({]" nil t)
      (let ((record (BibTeX-make-record database))
	    (warnings "")
	    (entry-type (downcase (match-string 1))))

	(cond ((string-equal entry-type "string")
	       (db-record-set-field record 'BibTeX-entry-type "string")
	       ;; This special entry types contains only two items
	       ;; of interest, namely an abbreviation and its
	       ;; expansion. First, we look for the abbreviation:
	       (re-search-forward
		"\\([^ \"#%'(),={}\n\t]+\\)[ \t]*=[ \t]*" nil t)
	       (db-record-set-field record 'BibTeX-var (downcase (match-string 1)))
	       ;; Anything that follows should be the expansion.
	       (db-record-set-field record 'BibTeX-val (BibTeX-find-value)))

	      ((string-equal entry-type "preamble")
	       (db-record-set-field record 'BibTeX-entry-type "preamble")
	       ;; This special entry-type contains only one field.
	       (db-record-set-field record 'BibTeX-val (BibTeX-find-value)))

	      ((assoc entry-type BibTeX-entry-type-alist)
	       (db-record-set-field record 'BibTeX-entry-type entry-type)
	       ;; Note: the following regexp deliberately accepts
	       ;; empty tags, as long as the comma is present.
	       (re-search-forward "[ \t\n]*\\(\\w*\\)[ \t\n]*," nil t)
	       (db-record-set-field record 'BibTeX-tag (match-string 1))
	       (while (re-search-forward "\\(\\w+\\)[ \t\n]*=" nil t)
		 (let ((current-field
			(BibTeX-get-field-name
			 (downcase (match-string 1)))))
		   (if current-field
		       (db-record-set-field
			record current-field (BibTeX-find-value))
		     (setq warnings
			   (concat warnings
				   (format "\nUnknown field %s ignored."
					   (match-string 1))))
		     (message "Warning: ignoring unknown field: %s."
			      (match-string 1)))))

	       (if (not (string-equal warnings ""))
		   (db-record-set-field
		    record 'warnings (substring warnings 1)))))
	record)

    ;; In case something went wrong, return an empty record. Returning
    ;; anything else might cause severe problems.
    (BibTeX-make-record database)))


;;}}}
;;{{{ BibTeX-define-abbreviations

;; As with BibTeX itself, strings must be defined before they are
;; used. Edbibtex will simply issue a warning if an undefined string
;; is used.

(defun BibTeX-define-abbreviations ()
  "db-after-read-hook for defining abbreviations."
  (db-maprecords
   (lambda (record)
     (let ((abbreviation
            (db-record-field record 'BibTeX-var)))
       ;; This is a string entry, update the list of
       ;; abbreviations.
       (if (not (or (BibTeX-field-empty-p abbreviation)
                    (BibTeX-get-expansion abbreviation)))
           (setq BibTeX-string-alist
                 (cons (cons (downcase abbreviation)
                             (db-record-field record 'BibTeX-val))
                       BibTeX-string-alist)))))))



;;}}}


;;}}}
;;{{{ Writing the Database


(defun BibTeX-wrfr (record)

  "Write RECORD to buffer."

  (let ((entry-type
	 (downcase (db-record-field record 'BibTeX-entry-type))))
    (cond ((string-equal entry-type "string")
	   (insert (format "@string{ %s = {%s} }\n"
			   (db-record-field record 'BibTeX-var)
			   (db-record-field record 'BibTeX-val))))
	  ((string-equal entry-type "preamble")
	   (insert (format "@preamble{ {%s} }\n"
		  (db-record-field record 'BibTeX-val))))
	  (t
	   (insert
	    (format "@%-16s%s"
		    (concat
		     (downcase
		      (db-record-field record 'BibTeX-entry-type))
		     "{ "
		     (db-record-field record 'BibTeX-tag))))
	   (save-excursion
	     (insert
	      (mapconcat
               (lambda (field)
                 (let ((value (db-record-field record field)))
                   (if (BibTeX-field-empty-p value)
                       ""
                     (format ",\n  %-15s{%s}"
                             (concat
                              (BibTeX-get-print-name field)
                              " = ")
                             value))))
	       BibTeX-fields "")
	      "\n}\n"))
	   (while (re-search-forward
		   "\\({?\\)#\\([^ \"#%'(),={}\n\t]+\\)#\\(}?\\)" nil 1)
	     (let ((replacement (match-string 2)))
	       (if (not (string-equal (match-string 1) "{"))
		   (setq replacement
			 (concat "} # " replacement)))
	       (if (not (string-equal (match-string 3) "}"))
		   (setq replacement
			 (concat replacement " # {")))
	       (replace-match replacement nil t)))))))


;;}}}
;;{{{ Displaying the Database


;;{{{ BibTeX-set-format

(defun BibTeX-set-format ()
  "This will prompt the user for an entry type and set the display
format accordingly, if the entry type is undefined (this is the
case with entries newly added)."
  (interactive)
  (let* ((completion-ignore-case t)
	 (format
	  (completing-read "Entry type: " BibTeX-alternate-formats nil t)))
    (dbf-set-this-record-modified-p t)
    (dbf-displayed-db-record-set-field-and-redisplay
     'BibTeX-entry-type format)
    (db-change-format format)))

;;}}}
;;{{{ BibTeX-set-format-from-data


(defun BibTeX-set-format-from-data (record)
  "This checks the entry type of the record on display and
chooses an appropriate format from BibTeX-entry-type-alist."
  (if BibTeX-multiple-format
      (let ((entry-type
	      (downcase (db-record-field record 'BibTeX-entry-type))))
	(if (BibTeX-field-empty-p entry-type)
	    (if BibTeX-use-default-format
		(let ((entry-type (car BibTeX-default-format)))
		  (db-change-format entry-type)
		  (db-record-set-field record 'BibTeX-entry-type entry-type))
	      (BibTeX-set-format))
	  (progn
	    (db-change-format entry-type)
	    (setq BibTeX-default-format
		  (assoc entry-type
			 BibTeX-alternate-formats)))))))


;;}}}


;;}}}
;;{{{ Validating Entries

;;{{{ BibTeX-check-abbreviations

(defun BibTeX-check-abbreviations (record)
  (mapconcat
   (lambda (field)
     (let ((string (db-record-field record field))
           (pos 0))
       (if (BibTeX-field-not-empty-p string)
           (while (string-match "#\\([^#%\"'(),={}\t\n ]+\\)#" string pos)
             (setq pos (match-end 1))
             (let ((match (match-string 1 string)))
               (if (BibTeX-get-expansion match)
                   ""
                 (format "\nUndefined abbreviation %s in field %s"
                         match (BibTeX-get-print-name field))))
             ""))))
   BibTeX-fields ""))

;;}}}
;;{{{ BibTeX-validate-displayed-record

(defun BibTeX-validate-displayed-record (&optional record)
  "Validate the record currently on display."
  (interactive)
  (let* ((record (or record (dbf-displayed-record)))
	 (warnings (concat (BibTeX-validate-record record)
			   (BibTeX-check-abbreviations record))))
    (dbf-set-this-record-modified-p t)
    (dbf-displayed-record-set-field-and-redisplay
     'updated (current-time-string))
    (dbf-displayed-record-set-field-and-redisplay
     'warnings
     (if (string-equal warnings "") ; remove first newline if necessary
	 warnings
       (substring warnings 1)))))

;;}}}
;;{{{ BibTeX-validate-record


(defun BibTeX-validate-record (record)

  "Returns warnings if RECORD is not BibTeX conforming."

  (let ((entry-type (db-record-field record 'BibTeX-entry-type)))
    (cond ((string-equal entry-type "preamble"))
	  ;; Do nothing about preambles.

	  ((string-equal entry-type "string")
	   ;; Check for extraneous hash symbols and update
	   ;; BibTeX-string-alist if necessary.
	   (let ((abbreviation (db-record-field record 'BibTeX-var)))
	     (if (BibTeX-field-empty-p abbreviation)
		 nil
	       (if (string-match
		    "#?\\([^#%\"'(),={}\t\n ]+\\)#?"
		    abbreviation)
		   (progn
		     (db-record-set-field
                      record 'BibTeX-var
                      (match-string 1 abbreviation))
		     (dbf-set-this-record-modified-p t)))
	       (if (not (BibTeX-get-expansion abbreviation))
		   (setq BibTeX-string-alist
			 (cons
			  (cons abbreviation
                                (db-record-field record 'BibTeX-val))
			  BibTeX-string-alist))))))

	  (t
	   ;; This should be a standard record.
	   (let ((warnings
		  (concat
		   (if (BibTeX-field-empty-p
			(db-record-field record 'BibTeX-tag))
		       "\nThis record is not tagged."
		     "")
		   (mapconcat
		    (lambda (field)
		      (if (atom field)
			  (if (BibTeX-field-empty-p
			       (db-record-field record field))
			      (format "\nRequired field %s is empty."
				      (BibTeX-get-print-name field))
			    "")
			;; If it's not an atom, it has to be a list
			;; with exactly three elements.
			(let ((first (nth 1 field))
			      (second (nth 2 field)))
			  (if (eq (car field) 'xor)
			      (cond
			       ((and
				 (BibTeX-field-empty-p
				  (db-record-field record first))
				 (BibTeX-field-empty-p
				  (db-record-field record second)))
				(format
				 "\nField %s and field %s should not both be empty."
				 first second))
			       ((and
				 (BibTeX-field-not-empty-p
				  (db-record-field record first))
				 (BibTeX-field-not-empty-p
				  (db-record-field record second)))
				(format
				 "\nField %s and field %s should not both be set."
				 first second))
			       (t ""))
			    (if (and (BibTeX-field-empty-p
				      (db-record-field record first))
				     (BibTeX-field-empty-p
				      (db-record-field record second)))
				(format
				 "\nField %s and field %s should not both be empty."
				 first second))))))
		    (BibTeX-get-required-field-list entry-type) ""))))
	     warnings)))))

;;}}}



;;}}}

(provide 'edbibtex)

;;{{{ Local Variables


;; Local Variables:
;; mode: emacs-lisp
;; folded-file: t
;; End:


;;}}}
