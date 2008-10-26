;;; db-oldnames.el --- map old names to new funcs/vars if possible

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

;; Usage: (require 'db-oldnames)
;;
;; This is for EDB 1.x support; it will NOT be available w/ EDB 2.x.
;;
;; If you find yourself using a function or variable named in this file,
;; it's probably a good idea to replace it w/ the newer name so as to be
;; able to avoid loading this file in the first place.

;;; Code:

(defalias 'define-alternative-multi-char-displaytype 'define-enum-type)
(defalias 'string-lessp-ci 'db-string-lessp-ci)
(defalias 'string-order-ci 'db-string-order-ci)
(defalias 'make-regexp-pattern 'db-make-regexp-pattern)
(defalias 'regexp-pattern-regexp 'db-regexp-pattern-regexp)
(defalias 'make-string-pattern 'db-make-string-pattern)
(defalias 'string-pattern-regexp 'db-string-pattern-regexp)
(defalias 'string-pattern-string 'db-string-pattern-string)
(defalias 'string-match-function 'db-string-match-function)
(defalias 'string-match-display->actual 'db-string-match-display->actual)
(defalias 'string-match-actual->display 'db-string-match-actual->display)
(defalias 'string-or-nil->string 'db-string-or-nil->string)
(defalias 'string-or-nil-lessp-ci 'db-string-or-nil-lessp-ci)
(defalias 'string-or-nil-order-ci 'db-string-or-nil-order-ci)
(defalias 'string-or-nil-match-function 'db-string-or-nil-match-function)
(defalias 'string->nil-or-string 'db-string->nil-or-string)
(defalias 'make-n-line-sep-function 'db-make-n-line-sep-function)

(require 'edb-t-human-names)
(defalias 'order-last-names 'edb-t-human-names:order-last-names)
(defalias 'canonicalize-name 'edb-t-human-names:canonicalize-name)
(defalias 'same-first-name-p 'edb-t-human-names:same-first-name-p)
(defalias 'order-first-names 'edb-t-human-names:order-first-names)
(defalias 'nicknamep 'edb-t-human-names:nicknamep)
(defalias 'standardize-name 'edb-t-human-names:standardize-name)
(defalias 'name->name-jr 'edb-t-human-names:name->name-jr)
(defalias 'name->first-last-jr 'edb-t-human-names:name->first-last-jr)

(require 'edb-t-places-usuk)
(defalias 'postal-code-p 'edb-t-places-usuk:postal-code-p)
(defalias 'statep 'edb-t-places-usuk:statep)
(defalias 'full-state-name 'edb-t-places-usuk:full-state-name)
(defalias 'abbreviate-state 'edb-t-places-usuk:abbreviate-state)

(require 'edb-t-timedate1)
(defalias 'make-date 'edb-t-timedate1:make-date)
(defalias 'date-year 'edb-t-timedate1:date-year)
(defalias 'date-month 'edb-t-timedate1:date-month)
(defalias 'date-day 'edb-t-timedate1:date-day)
(defalias 'make-empty-date 'edb-t-timedate1:make-empty-date)
(defalias 'date-year-long 'edb-t-timedate1:date-year-long)
(defalias 'date->day-of-year 'edb-t-timedate1:date->day-of-year)
(defalias 'date->absolute-days 'edb-t-timedate1:date->absolute-days)
(defalias 'date->weekday-index 'edb-t-timedate1:date->weekday-index)
(defalias 'date->weekday-name 'edb-t-timedate1:date->weekday-name)
(defalias 'date-order-absolute 'edb-t-timedate1:date-order-absolute)
(defalias 'parse-date-string 'edb-t-timedate1:parse-date-string)
(defalias 'parse-date-string-or-nil 'edb-t-timedate1:parse-date-string-or-nil)
(defalias 'parse-date-default-function 'edb-t-timedate1:parse-date-default-function)
(defalias 'format-date 'edb-t-timedate1:format-date)
(defalias 'simple-format-date 'edb-t-timedate1:simple-format-date)
(defalias 'simple-format-date-or-nil 'edb-t-timedate1:simple-format-date-or-nil)
(defalias 'format-date-mmddyy 'edb-t-timedate1:format-date-mmddyy)
(defalias 'format-date-ddmmyy 'edb-t-timedate1:format-date-ddmmyy)
(defalias 'format-date-yymmdd 'edb-t-timedate1:format-date-yymmdd)
(defalias 'format-date-ddmmmyy 'edb-t-timedate1:format-date-ddmmmyy)
(defalias 'format-date-yyyymmdd 'edb-t-timedate1:format-date-yyyymmdd)
(defalias 'format-date-full 'edb-t-timedate1:format-date-full)
(defalias 'format-date-unix 'edb-t-timedate1:format-date-unix)
(defalias 'format-date-all 'edb-t-timedate1:format-date-all)
(defalias 'format-date-dec 'edb-t-timedate1:format-date-dec)
(defalias 'format-date-europe 'edb-t-timedate1:format-date-europe)
(defalias 'date-match-function 'edb-t-timedate1:date-match-function)
(defalias 'date-merge 'edb-t-timedate1:date-merge)
(defalias 'date->storage-string-mmddyyyy 'edb-t-timedate1:date->storage-string-mmddyyyy)
(defalias 'storage-string-mmddyyyy->date 'edb-t-timedate1:storage-string-mmddyyyy->date)
(defalias 'date->storage-string-lisp 'edb-t-timedate1:date->storage-string-lisp)
(defalias 'storage-string-lisp->date 'edb-t-timedate1:storage-string-lisp->date)
(defalias 'date-stored->actual 'edb-t-timedate1:date-stored->actual)
(defalias 'make-time 'edb-t-timedate1:make-time)
(defalias 'time-hours 'edb-t-timedate1:time-hours)
(defalias 'time-mins 'edb-t-timedate1:time-mins)
(defalias 'time-secs 'edb-t-timedate1:time-secs)
(defalias 'make-empty-time 'edb-t-timedate1:make-empty-time)
(defalias 'empty-time-p 'edb-t-timedate1:empty-time-p)
(defalias 'time-default-constraint 'edb-t-timedate1:time-default-constraint)
(defalias 'parse-time-string 'edb-t-timedate1:parse-time-string)
(defalias 'parse-time-default-function 'edb-t-timedate1:parse-time-default-function)
(defalias 'format-time-24 'edb-t-timedate1:format-time-24)
(defalias 'format-time-12 'edb-t-timedate1:format-time-12)
(defalias 'format-time-12-hhmm 'edb-t-timedate1:format-time-12-hhmm)
(defalias 'format-time-24-hhmm 'edb-t-timedate1:format-time-24-hhmm)
(defalias 'time-order 'edb-t-timedate1:time-order)
(defalias 'time-match-function 'edb-t-timedate1:time-match-function)
(defalias 'time->storage-string 'edb-t-timedate1:time->storage-string)
(defalias 'storage-string->time 'edb-t-timedate1:storage-string->time)
(defalias 'time-merge 'edb-t-timedate1:time-merge)
(defalias 'date->storage-string 'edb-t-timedate1:format-date-full)
(defalias 'storage-string->date 'edb-t-timedate1:date-stored->actual)
(defalias 'format-time-hhmm 'edb-t-timedate1:format-time-12-hhmm)

(provide 'db-oldnames)

;;; db-oldnames.el ends here
