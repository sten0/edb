;;; edb-t-places-usuk.el

;; Copyright (C) 2005,2006,2007,2008 Thien-Thi Nguyen

;; This file is part of EDB.
;;
;; EDB is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 3, or (at your option) any later
;; version.
;;
;; EDB is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with EDB; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301, USA.

;;; Commentary:

;; Places in the US/UK
;;
;; These variables and functions used to be in db-types.el,
;; without the "edb-t-places-usuk:" prefix.

;;; Code:

(defvar edb-t-places-usuk:UK-postal-code-regexp
  "[A-Z][0-9][A-Z] ?[0-9][A-Z][0-9]")

(defvar edb-t-places-usuk:zip-code-regexp
  "[0-9][0-9][0-9][0-9][0-9]\\(-?[0-9][0-9][0-9][0-9]\\)?")

(defvar edb-t-places-usuk:postal-code-regexp
  (mapconcat
   'identity
   (list
    ;; US zip code
    edb-t-places-usuk:zip-code-regexp
    ;; UK postal code
    edb-t-places-usuk:UK-postal-code-regexp
    ;; other
    "[0-9][0-9A-Z]*")
   "\\|"))

(defsubst edb-t-places-usuk:postal-code-p (s)
  (string-match edb-t-places-usuk:postal-code-regexp s))

;; Full state-alist and associated functions added by
;; Alan Stebbens, UCSB, Sep 18 '92.

(defvar edb-t-places-usuk:state-alist
  '(("AK" . "Alaska")
    ("AL" . "Alabama")
    ("AR" . "Arkansas")
    ("AZ" . "Arizona")
    ("CA" . "California")
    ("CO" . "Colorado")
    ("CT" . "Conneticut")
    ("DC" . "District of Columbia")
    ("DE" . "Deleware")
    ("FL" . "Florida")
    ("GA" . "Georgia")
    ("GU" . "Guam")
    ("HI" . "Hawaii")
    ("ID" . "Idaho")
    ("IL" . "Illinois")
    ("IN" . "Indiana")
    ("IA" . "Iowa")
    ("KS" . "Kansas")
    ("KY" . "Kentucky")
    ("LA" . "Louisiana")
    ("MA" . "Massachusetts")
    ("MD" . "Maryland")
    ("ME" . "Maine")
    ("MI" . "Michigan")
    ("MN" . "Minnessota")
    ("MS" . "Mississippi")
    ("MO" . "Missouri")
    ("MT" . "Montana")
    ("NE" . "Nebraska")
    ("NH" . "New Hampshire")
    ("NC" . "North Carolina")
    ("ND" . "North Dakota")
    ("NJ" . "New Jersey")
    ("NM" . "New Mexico")
    ("NV" . "Nevada")
    ("NY" . "New York")
    ("OH" . "Ohio")
    ("OK" . "Oklahoma")
    ("OR" . "Oregon")
    ("PA" . "Pennsylvania")
    ("PR" . "Puerto Rico")
    ("RI" . "Rhode Island")
    ("SC" . "South Carolina")
    ("SD" . "South Dakota")
    ("TX" . "Texas")
    ("TN" . "Tennessee")
    ("UT" . "Utah")
    ("VA" . "Virginia")
    ("VT" . "Vermont")
    ("WA" . "Washington")
    ("WI" . "Wisconson")
    ("WV" . "West Virginia")
    ("WY" . "Wyoming"))
  "An alist of ABBREV and FULLNAME of each of the United States, and its
territories.  Used by \"edb-t-places-usuk:full-state-name\" and
\"edb-t-places-usuk:abbreviate-state\".")

(defun edb-t-places-usuk:statep (obj)
  "Return non-nil if OBJ is a valid state abbreviation."
  (when (stringp obj) (edb-t-places-usuk:full-state-name obj)))

(defun edb-t-places-usuk:full-state-name (abbrev)
  "Return the full state name for ABBREV, or nil if not found."
  (and (= 2 (length abbrev))
       (cdr (assoc (upcase abbrev) edb-t-places-usuk:state-alist))))

(defun edb-t-places-usuk:abbreviate-state (state)
  "Return the postal abbreviation for STATE, or nil if not found."
  (car (rassoc (capitalize state) edb-t-places-usuk:state-alist)))


(provide 'edb-t-places-usuk)

;;; edb-t-places-usuk.el ends here
