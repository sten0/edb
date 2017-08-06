;;; edb-t-timedate1.el

;; Copyright (C) 2005-2017 Thien-Thi Nguyen

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
;; along with EDB.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Library of date and time types for EDB database fields.
;; This file is an extension of db-types.el.
;;
;; This file defines the date and time record types, plus several kinds of
;; date- and time-related display types, with variations on formatting.
;;
;; For efficiency, the types are defined in terms of the displayspec
;; abstraction instead of via format strings.
;;
;; These variables and functions used to be in db-time.el,
;; without the "edb-t-timedate1:" prefix.  The "1" is because
;; db-time.el originated with EDB 1.x (or perhaps even earlier).
;; Since then, Emacs has incorporated time/date functionality
;; that is incompatible w/ this interface.
;;
;; Performance note:
;; If the speed of reading and writing database files is very important to
;; you, consider using `fset' to set `edb-t-timedate1:date->storage-string'
;; `edb-t-timedate1:storage-string->date' to more efficient functions,
;; such as `edb-t-timedate1:date->storage-string-mmddyyyy' and
;; `edb-t-timedate1:storage-string-mmddyyyy->date', or
;; `edb-t-timedate1:date->storage-string-lisp'
;; and `edb-t-timedate1:storage-string-lisp->date'.

;;; Code:

(require 'edbcore)
(eval-when-compile (require 'cl))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Dates
;;;

;;;
;;; Abstraction:  dotted list of (year month . day), all integers.
;;;

(defsubst edb-t-timedate1:make-date (year month day)
  "Make an EDB 1.x date object from YEAR, MONTH and DAY."
  (cons year (cons month day)))

(defsubst edb-t-timedate1:date-year (date) (car date))
(defsubst edb-t-timedate1:date-month (date) (car (cdr date)))
(defsubst edb-t-timedate1:date-day (date) (cdr (cdr date)))

(defsubst edb-t-timedate1:make-empty-date ()
  "Return an EDB 1.x date object containing no information."
  (edb-t-timedate1:make-date nil nil nil))

(defun edb-t-timedate1:date-year-long (date)
  "Extract the year from an EDB 1.x DATE object as a four digit value."
  (let ((yy (edb-t-timedate1:date-year date)))
    (cond ((< yy 50) (+ 2000 yy))
          ((< yy 99) (+ 1900 yy))
          (t yy))))

;;; Years

(defun edb-t-timedate1:date->day-of-year (date)
  "Return the day number within the year for Gregorian DATE."
  ;;
  ;; An explanation of the calculation can be found in PascAlgorithms by
  ;; Edward and Ruth Reingold, Scott-Foresman/Little, Brown, 1988.
  ;;
  (let* ((month (edb-t-timedate1:date-month date))
         (day   (edb-t-timedate1:date-day   date))
         (year  (edb-t-timedate1:date-year  date))
         (day-of-year (+ day (* 31 (1- month)))))
    (if (> month 2)
        (progn
          (decf day-of-year (/ (+ 23 (* 4 month)) 10))
          (if (or (and (zerop      (% year   4))
                       (not (zerop (% year 100))))
                  (zerop (% year 400)))
              (incf day-of-year))))
    day-of-year))

(defun edb-t-timedate1:date->absolute-days (date)
  "Return the number of days elapsed between the Gregorian 12/31/1 BC and DATE.
The Gregorian date Sunday, December 31, 1 BC is imaginary."
  (let ((mm (edb-t-timedate1:date-month date))
        (dd (edb-t-timedate1:date-day date))
        (yy (1- (edb-t-timedate1:date-year-long date))))
    (+ (edb-t-timedate1:date->day-of-year date) ; + days in this year
       (* 365 yy)                               ; + days in prior years
       (/ yy 4)                                 ; + Julian leap years
       (- (/ yy 100))                           ; - century years
       (/ yy 400))))                            ; + Gregorian leap years

;;; Weekdays

(defsubst edb-t-timedate1:date->weekday-index (date)
  "Return the weekday index for DATE."
  (% (edb-t-timedate1:date->absolute-days date) 7))

(defsubst edb-t-timedate1:date->weekday-name (date)
  "Return the weekday name for the DATE."
  (aref
   ;; Sunday must come first -- absolute dates begin on Sunday, Dec 31, 1BC.
   ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"]
   (edb-t-timedate1:date->weekday-index date)))


;;; Months

;; These sub-alists aren't really necessary; they're only used to make the
;; associated arrays.  And the full alist is used, of course.  But it uses
;; different cons cells, which is a waste.
(defconst edb-t-timedate1:full-monthname-alist
  '(("January" . 1) ("February" . 2) ("March" . 3) ("April" . 4)
    ("May" . 5) ("June" . 6) ("July" . 7) ("August" . 8)
    ("September" . 9) ("October" . 10) ("November" . 11) ("December" . 12)))

(defconst edb-t-timedate1:monthabbrev-alist
  '(("Jan" . 1) ("Feb" . 2) ("Mar" . 3) ("Apr" . 4) ("May" . 5) ("Jun" . 6)
    ("Jul" . 7) ("Aug" . 8) ("Sep" . 9) ("Oct" . 10) ("Nov" . 11) ("Dec" . 12)))

(defconst edb-t-timedate1:monthname-alist
  (append edb-t-timedate1:monthabbrev-alist
          edb-t-timedate1:full-monthname-alist
          '(("Sept" . 9))))

;; Why do I need this in an array?  (Why not?)  (Well, it's extra space.)
(defconst edb-t-timedate1:monthabbrev-array
  (vconcat '("") (mapcar 'car edb-t-timedate1:monthabbrev-alist)))

(defconst edb-t-timedate1:monthname-array
  (vconcat '("") (mapcar 'car edb-t-timedate1:full-monthname-alist)))


;;; Ordering

(defun edb-t-timedate1:date-order-absolute (d1 d2)
  (let ((rv (db-number-or-nil-order-nil-greatest
             (edb-t-timedate1:date-year d1)
             (edb-t-timedate1:date-year d2))))
    (when (zerop rv)
      (setq rv (db-number-or-nil-order-nil-greatest
                (edb-t-timedate1:date-month d1)
                (edb-t-timedate1:date-month d2))))
    (when (zerop rv)
      (setq rv (db-number-or-nil-order-nil-greatest
                (edb-t-timedate1:date-day d1)
                (edb-t-timedate1:date-day d2))))
    rv))


;;;
;;; Regexps
;;;

(defconst edb-t-timedate1:date-regexps
  (let* ((monthname (concat "\\("
                            (mapconcat 'car
                                       edb-t-timedate1:monthname-alist
                                       "\\|")
                            "\\)\\.?"))
         (weekday (concat
                   "\\("
                   (mapconcat 'identity
                              '("Sunday" "Monday" "Tuesday"
                                "Wednesday" "Thursday"
                                "Friday" "Saturday"
                                "Tues" "Thurs"
                                "Sun" "Mon" "Tue" "Wed"
                                "Thu" "Fri" "Sat")
                              "\\|")
                   "\\)\\.?"))
         (monthno "\\(0?[1-9]\\|1[0-2]\\)")
         (monthno-two-char "\\(0[1-9]\\|1[0-2]\\)")
         (monthday "\\(0?[1-9]\\|[12][0-9]\\|3[01]\\)")
         (monthday-two-char "\\([0-2][0-9]\\|3[01]\\)")
         ;; NOTE: No surrounding ()!
         (full-year "[0-2][0-9][0-9][0-9]")
         (short-year "[0-9][0-9]")
         ;; NOTE: No internal grouping; is that intentional?  That is, this
         ;; only counts as one grouping when counting regexp matches, because
         ;; I didn't use any internal \\( \\).
         (year (concat "\\(" full-year
                       "\\|" short-year "\\)"))

         (sep "[ -.,/']+"))
    (list
     ;; MMDDYY
     (cons (concat monthname
                   sep
                   monthday
                   "\\("
                   sep
                   year
                   "\\)?")
           '(4 nil 1 2))
     (cons (concat monthno
                   sep
                   monthday
                   "\\("
                   sep
                   year
                   "\\)?")
           '(4 1 nil 2))
     ;; DDMMYY
     (cons (concat monthday
                   sep
                   monthname
                   "\\("
                   sep
                   year
                   "\\)?")
           '(4 nil 2 1))
     (cons (concat "\\("
                   monthday
                   sep
                   "\\)?"
                   monthname
                   sep
                   year)
           '(4 nil 3 2))
     (cons (concat monthday
                   sep
                   monthno
                   sep
                   "\\(" full-year "\\)")
           '(3 2 nil 1))
     ;; YYMMDD
     ;; Using year instead of full-year is ambiguous (consider
     ;; 11-11-11), but we already tried MMDDYY and it failed.
     (cons (concat year
                   sep
                   monthname
                   sep
                   monthday)
           '(1 nil 2 3))
     (cons (concat year
                   sep
                   monthno
                   sep
                   monthday)
           '(1 2 nil 3))
     ;; YYMMDD, no separators
     ;; This is ambiguous.
     (cons (concat year
                   monthno-two-char "?"
                   monthday-two-char "?")
           '(1 2 nil 3))
     ;; WWMMDDYY
     (cons (concat weekday
                   sep
                   monthname
                   sep
                   monthday
                   "\\("
                   sep
                   year
                   "\\)?")
           '(5 nil 2 3))
     ;; WWDDMMYY
     (cons (concat weekday
                   sep
                   monthday
                   sep
                   monthname
                   "\\("
                   sep
                   year
                   "\\)?")
           '(5 nil 3 2))
     ;; ctime
     (cons (concat
            weekday
            " "
            monthname
            "  ?"
            monthday
            ;; time of day
            " [0-9:]+ "
            "\\(" full-year "\\)")
           '(4 nil 2 3))))
  "Assoc list of regexps and match locators.
A match locator is a list of four numbers indicating which submatch of the
regexp contains the year, month number, month name, and day of the month.
The list elements may be nil if that information is not available.")


;;;
;;; Parsing dates
;;;

(defun edb-t-timedate1:parse-date-string (s)
  "Parse string S, and return an EDB 1.x date object.
Signal error if the parse is invalid.  If S contains only whitespace,
return a null date object.  If S is nil, use the result of calling
`edb-t-timedate1:parse-date-default-function' instead."
  (let ((rxls edb-t-timedate1:date-regexps)
        (son->non (lambda (s-o-n)
                    (when s-o-n (db-string->number-or-nil s-o-n))))
        rv mls)
    (setq s (if (null s)                ; provide default date for nil strings
                (edb-t-timedate1:parse-date-default-function)
              (db-string-trim-whitespace s)))
    (if (zerop (length s))                           ; if empty string,
        (edb-t-timedate1:make-empty-date)            ; return empty date
      ;; rxls is nulled if a match is found
      (while rxls
        (if (string-match (concat "^" (car (car rxls)) "$") s)
            ;; Bug in version 18 save-match-data:  it's impossible
            ;; to have a marker at 0, so this gets converted to 1.
            (setq mls (mapcar (lambda (m)
                                (when m (match-string m s)))
                              (cdr (car rxls)))
                  ;; mls is year, monthnumber, monthname, day
                  rv (edb-t-timedate1:make-date
                      (funcall son->non (car mls))
                      (or (funcall son->non (car (cdr mls)))
                          (and (car (cdr (cdr mls)))
                               ;; match is non-nil; don't check match-beginning
                               ;; At one time this clobbered the match-data.
                               (cdr (assoc (capitalize (car (cdr (cdr mls))))
                                           edb-t-timedate1:monthname-alist))))
                      (funcall son->non (nth 3 mls)))
                  rxls nil)
          ;; string-match failed
          (setq rxls (cdr rxls))))
      (if rv
          (if (if (edb-t-timedate1:date-day rv)
                  (if (edb-t-timedate1:date-month rv)
                      (<= (edb-t-timedate1:date-day rv)
                          (aref [0 31 28 31 30 31 30 31 31 30 31 30 31]
                                (edb-t-timedate1:date-month rv)))
                    (error "Date has a day but no month"))
                t)
              (if (and (let ((y (edb-t-timedate1:date-year rv)))
                         (or (not y) (zerop y)))
                       (let ((m (edb-t-timedate1:date-month rv)))
                         (or (not m) (zerop m)))
                       (let ((d (edb-t-timedate1:date-day rv)))
                         (or (not d) (zerop d))))
                  (edb-t-timedate1:make-empty-date)
                rv)
            (error "There is no such day as %s %d!"
                   (aref edb-t-timedate1:monthname-array
                         (edb-t-timedate1:date-month rv))
                   (edb-t-timedate1:date-day rv)))
        (error "`%s' is not a valid date." s)))))

(defun edb-t-timedate1:parse-date-string-or-nil (s)
  "Like `edb-t-timedate1:parse-date-string',
but return null date in case arg S is nil."
  (if s
      (edb-t-timedate1:parse-date-string s)
    (edb-t-timedate1:make-empty-date)))

(defvar edb-t-timedate1:parse-date-default 'empty
  "A symbol: `empty' or `current-date', specifying what date string
`edb-t-timedate1:parse-date-default-function' should return, and
`edb-t-timedate1:parse-date-string' should use when passed a nil argument.")

(defun edb-t-timedate1:parse-date-default-function ()
  "Return a default value for `edb-t-timedate1:parse-date-string'
to use if its input is nil."
  (case edb-t-timedate1:parse-date-default
    ((empty)
     "")
    ((today current-date current-time current-time-string)
     (current-time-string))
    (t
     (error "Bad `edb-t-timedate1:parse-date-default' value: %S"
            edb-t-timedate1:parse-date-default))))



;; AKS, UCSB, 9/30/92
;;
;; General purpose date format routine


;; *WARNING*: If any new escape symbols are added, BE SURE that they are
;; placed in order of longest symbol first, so that the regexp computed
;; below works properly.

(defconst edb-t-timedate1:format-date-sub-syms-alist
  (eval-when-compile
    (let* ((Y '(edb-t-timedate1:date-year  date))
           (M '(edb-t-timedate1:date-month date))
           (D '(edb-t-timedate1:date-day   date))
           (raw (list
                 ;; days
                 "day" D
                 '(substring (edb-t-timedate1:date->weekday-name date) 0 3)
                 "dd" D
                 `(format "%02d" ,D)
                 "d" D
                 D
                 ;; months
                 "month" M
                 `(aref edb-t-timedate1:monthname-array ,M)
                 "mon" M
                 `(aref edb-t-timedate1:monthabbrev-array ,M)
                 "mm" M
                 `(format "%02d" ,M)
                 "m" M
                 M
                 ;; years
                 "year" Y
                 '(edb-t-timedate1:date-year-long date)
                 "yy" Y
                 `(format "%02d" (% ,Y 100))
                 ;; misc
                 "jday" `(and ,D ,M ,Y)
                 '(edb-t-timedate1:date->day-of-year date)
                 "wday" `(and ,D ,M ,Y)
                 '(edb-t-timedate1:date->weekday-index date)
                 "weekday" `(and ,D ,M ,Y)
                 '(edb-t-timedate1:date->weekday-name date)))
           alist)
      (while raw
        (let ((name (pop raw))
              (test (pop raw))
              (doit (pop raw)))
          (push `(,name . ((lambda (date) ,test) . (lambda (date) ,doit)))
                alist)))
      (nreverse alist)))
  "An alist of (NAME . (TEST . DOIT)) used by `edb-t-timedate1:format-date'.
Each NAME is a string, which, when prefixed by \"%\", will be substituted by
the value of calling the associated DOIT but only if calling TEST returns
non-nil.  Calling is done with one arg, the date object.")

;; Build a regexp which matches the symbol names given above

(defconst edb-t-timedate1:format-date-sub-syms-regexp
  (concat "%\\("
          (mapconcat 'car edb-t-timedate1:format-date-sub-syms-alist "\\|")
          "\\)")
  "A regexp pattern to parse format strings for symbol substition strings;
computed from the variable `edb-t-timedate1:format-date-sub-syms-alist'.")

(defun edb-t-timedate1:format-date (fmtstr &optional date)
  "Using FMTSTR, format the DATE, which defaults to the current date
if nil.  FMTSTR can contain the following symbol strings, which are
substituted by their corresponding value from the date; other
characters are inserted as-is.

 String    Action
 ======    ======
  %d       day of month -- 1 to 31 (one or two digits)
  %dd      day of month -- 01 to 31 (always two digits)
  %m       month of year - 1 to 12 (one or two digits)
  %mm      month of year - 01 to 12 (always two digits)
  %mon     month name (abbreviated) - Jun
  %month   full month name - June
  %yy      last 2 digits of year - 00 to 99
  %year    year as 4 digits -- 0000 to 9999?
  %jday    Julian day of year -- 1 to 366
  %wday    day of week -- 0 to 6 (Sunday = 0)
  %day     day of week name -- \"Sun\" to \"Sat\"
  %weekday full day of week name -- \"Sunday\" to \"Saturday\"

See the variables `edb-t-timedate1:format-date-sub-syms-alist' and
`edb-t-timedate1:format-date-sub-syms-regexp'.

A special case: if an element of DATE is nil, its field is hidden.  A
DATE object of all nils is thus formatted as the empty string."

  (when (null date)
    (setq date (edb-t-timedate1:parse-date-string nil)))
  (let* ((ofs 0) (buf "") sym-alist x)
    (while (setq x (string-match edb-t-timedate1:format-date-sub-syms-regexp
                                 fmtstr ofs))
      (if (not (setq sym-alist
                     (assoc (match-string 1 fmtstr)
                            edb-t-timedate1:format-date-sub-syms-alist)))
          (error "Not in `edb-t-timedate1:format-date-sub-syms-alist': %s"
                 (match-string 1 fmtstr))
        ;; Does TEST work?
        (when (funcall (car (cdr sym-alist)) date)
          ;; Yes; insert its prefix string and its value.
          (setq buf (format "%s%s%s"
                            buf
                            (if (string= buf "")
                                buf
                              (substring fmtstr ofs x))
                            (funcall (cdr (cdr sym-alist)) date)))))
      ;; Skip past the variable.
      (setq ofs (match-end 0)))
    (concat buf (substring fmtstr ofs))))


(defconst edb-t-timedate1:simple-format-date-default "%month %d, %year"
  "*A default format used by edb-t-timedate1:simple-format-date.")

;; Note only one argument
(defun edb-t-timedate1:simple-format-date (date)
  "Format the DATE using a default format, defined by the variable
`edb-t-timedate1:simple-format-date-default'.  If DATE is nil,
use the value of `edb-t-timedate1:parse-date-default-function'."
  (edb-t-timedate1:format-date
   edb-t-timedate1:simple-format-date-default
   date))

(defun edb-t-timedate1:simple-format-date-or-nil (date)
  "Format the DATE using a default format, defined by the variable
`edb-t-timedate1:simple-format-date-default'.  If DATE is nil,
return the empty string."
  (if date
      (edb-t-timedate1:format-date
       edb-t-timedate1:simple-format-date-default
       date)
    ""))


;; AKS, edb-t-timedate1:format-date-xxx routines
;; The following display functions are used below
;; in the displayspec definitions:
;;
;;  Function                             Sample     Default
;;  ==================================== ========== =======
;;  edb-t-timedate1:format-date-mmddyy   06/10/54   "/"
;;  edb-t-timedate1:format-date-yymmdd   540610     ""
;;  edb-t-timedate1:format-date-ddmmyy   10.06.54   "."
;;  edb-t-timedate1:format-date-ddmmmyy  10 Jun 54  " "
;;  edb-t-timedate1:format-date-yyyymmdd 1954/06/10 "/"
;;
;;  -europe   10.06.54
;;  -dec      10-Jun-54
;;  -full     June 10, 1954
;;  -unix     Sun Jun 10 1954
;;  -all      Sunday, June 10, 1954

(defun edb-t-timedate1:format-date-mmddyy (date &optional separator)
  "Format the DATE into a \"%mm/%dd/%yy\" format.
Optional arg SEPARATOR is a string to use instead of \"/\".
If DATE is nil, return the empty string."
  (if date
      (let ((sep (or separator "/")))
        (edb-t-timedate1:format-date (format "%%mm%s%%dd%s%%yy" sep sep) date))
    ""))

(defun edb-t-timedate1:format-date-ddmmyy (date &optional separator)
  "Format the DATE into a \"%dd.%mm.%yy\" format.
Optional arg SEPARATOR a string to use instead of \".\".
If DATE is nil, return the empty string."
  (if date
      (let ((sep (or separator ".")))
        (edb-t-timedate1:format-date (format "%%dd%s%%mm%s%%yy" sep sep) date))
    ""))

(defun edb-t-timedate1:format-date-yymmdd (date &optional separator)
  "Format the DATE into a \"%yy%mm%dd\" format.
Optional arg SEPARATOR is a string to use instead of \"\" (empty string).
If DATE is nil, return the empty string."
  (if date
      (let ((sep (or separator "")))
        (edb-t-timedate1:format-date (format "%%yy%s%%mm%s%%dd" sep sep) date))
    ""))

(defun edb-t-timedate1:format-date-ddmmmyy (date &optional separator)
  "Format the DATE into a \"%dd %mon %yy\" format.
Optional arg SEPARATOR is a string to use instead of \" \" (space).
If DATE is nil, return the empty string."
  (if date
      (let ((sep (or separator " ")))
        (edb-t-timedate1:format-date (format "%%dd%s%%mon%s%%yy" sep sep) date))
    ""))

(defun edb-t-timedate1:format-date-yyyymmdd (date &optional separator)
  "Format the DATE into a \"%year/%mm/%dd\" format.
Optional arg SEPARATOR is a string to use instead of \"/\".
If DATE is nil, return the empty string."
  (if date
      (let ((sep (or separator "/")))
        (edb-t-timedate1:format-date (format "%%year%s%%mm%s%%dd" sep sep) date))
    ""))

(defun edb-t-timedate1:format-date-full (date)
  "Format the DATE into the full format: %month %d, %year.
If DATE is nil, return the empty string."
  (if date
      (edb-t-timedate1:format-date "%month %d, %year" date)
    ""))

(defun edb-t-timedate1:format-date-unix (date)
  "Format the DATE into the standard Unix format: %day %mon %d %year.
If DATE is nil, return the empty string."
  (if date
      (edb-t-timedate1:format-date "%day %mon %d %year" date)
    ""))

(defun edb-t-timedate1:format-date-all (date)
  "Format the DATE using all components, without abbreviations, in the
format: %weekday, %month %d, %year.  If DATE is nil, return the empty string."
  (if date
      (edb-t-timedate1:format-date "%weekday, %month %d, %year" date)
    ""))

;; Some common variations on a theme

(defun edb-t-timedate1:format-date-dec (date)
  "Format the DATE into the standard DEC format: dd-mmm-yy.
If DATE is nil, return the empty string."
  (edb-t-timedate1:format-date-ddmmmyy date "-"))

(defun edb-t-timedate1:format-date-europe (date)
  "Format the DATE into the European standard, which is DD.MM.YY.
If DATE is nil, return the empty string."
  (edb-t-timedate1:format-date-ddmmyy date "."))

(defun edb-t-timedate1:format-date-iso (date)
  "Format the DATE into a \"%year-%mm-%dd\" format.
If DATE is nil, return the empty string."
  (edb-t-timedate1:format-date-yyyymmdd date "-"))


;;;
;;; EDB type definitions
;;;

(edb-define-displaytype 'date nil
  :indent           nil
  :actual->display 'edb-t-timedate1:simple-format-date
  :display->actual 'edb-t-timedate1:parse-date-string)

(edb-define-recordfieldtype 'date nil
  :type           'date
  :default-value  (edb-t-timedate1:make-empty-date)
  :actual->stored 'edb-t-timedate1:date->storage-string
  :stored->actual 'edb-t-timedate1:storage-string->date
  :merge-function 'edb-t-timedate1:date-merge
  :order-fn       'edb-t-timedate1:date-order-absolute
  :match-function 'edb-t-timedate1:date-match-function
  :help-info      "A date.")

(edb-define-recordfieldtype 'date-efficient-storage 'date
  :stored->actual 'edb-t-timedate1:storage-string->date)

(edb-define-displaytype 'date-or-nil 'date
  :actual->display 'edb-t-timedate1:simple-format-date-or-nil
  :display->actual 'edb-t-timedate1:parse-date-string-or-nil)

(edb-define-recordfieldtype 'date-or-nil 'date
  :type           'date-or-nil
  :default-value   nil
  :stored->actual 'edb-t-timedate1:parse-date-string-or-nil
  :help-info      "(Optional) date.")


(dolist (type '("mmddyy"
                "yymmdd"
                "ddmmyy"
                "ddmmmyy"
                "yyyymmdd"
                "iso"
                "europe"
                "full"
                "all"
                "unix"
                "dec"))                 ; RIP (snif) --ttn
  (edb-define-displaytype
      (intern (concat "date-" type))
      'date
    :actual->display (intern (concat "edb-t-timedate1:format-date-" type))))


(defun edb-t-timedate1:date-match-function (pat targ)
  (and (or (not (edb-t-timedate1:date-year    pat))
           (and (edb-t-timedate1:date-year    targ)
                (= (edb-t-timedate1:date-year pat)
                   (edb-t-timedate1:date-year targ))))
       (or (not (edb-t-timedate1:date-month    pat))
           (and (edb-t-timedate1:date-month    targ)
                (= (edb-t-timedate1:date-month pat)
                   (edb-t-timedate1:date-month targ))))
       (or (not (edb-t-timedate1:date-day    pat))
           (and (edb-t-timedate1:date-day    targ)
                (= (edb-t-timedate1:date-day pat)
                   (edb-t-timedate1:date-day targ))))))

(defun edb-t-timedate1:date-merge (d1 d2)
  ;; If an item of a date is nil, use the other date's value.
  (let ((mm (or (edb-t-timedate1:date-month d1)
                (edb-t-timedate1:date-month d2)))
        (dd (or (edb-t-timedate1:date-day d1)
                (edb-t-timedate1:date-day d2)))
        (yy (or (edb-t-timedate1:date-year d1)
                (edb-t-timedate1:date-year d2))))
    (edb-t-timedate1:make-date yy mm dd)))

;; File representation
(fset 'edb-t-timedate1:date->storage-string 'edb-t-timedate1:format-date-full)
(fset 'edb-t-timedate1:storage-string->date 'edb-t-timedate1:date-stored->actual)

(defun edb-t-timedate1:date->storage-string-mmddyyyy (date)
  ;; This is fairly human-readable, but ambiguous.
  (if (edb-t-timedate1:date-year date)
      (format "%02d/%02d/%02d"
              (or (edb-t-timedate1:date-month date) 0)
              (or (edb-t-timedate1:date-day   date) 0)
              (or (edb-t-timedate1:date-year  date) 0))
    (if (not (or (edb-t-timedate1:date-month date)
                 (edb-t-timedate1:date-day   date)))
        ""
      (format "%02d/%02d"
              (or (edb-t-timedate1:date-month date) 0)
              (or (edb-t-timedate1:date-day   date) 0)))))

(defun edb-t-timedate1:storage-string-mmddyyyy->date (s)
  (let ((mm (db-string->integer (substring s 0 2)))
        (dd (db-string->integer (substring s 3 5)))
        (yyyy (and (> (length s) 5)
                   (db-string->integer (substring s 6)))))
    (edb-t-timedate1:make-date (if (not (zerop mm)) mm)
                               (if (not (zerop dd)) dd)
                               (if (and yyyy (not (zerop yyyy))) yyyy))))

(defun edb-t-timedate1:date->storage-string-lisp (date)
  ;; This is quite fast, but not very human-readable.
  (format "%s" date))

(defun edb-t-timedate1:storage-string-lisp->date (s)
  (car (read-from-string s)))

(defun edb-t-timedate1:date-stored->actual (s)
  ;; Don't do anything if S is already a date.  The point is to DTRT even when
  ;; the field didn't appear in the database file, so the record field got set
  ;; to the empty date rather than the empty string.
  (if (stringp s)
      ;; This is slow but general.
      (edb-t-timedate1:parse-date-string s)
    s))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Times
;;;
;;; Much of this was lifted from Dates
;;; Otherwise it was written by Alan K. Stebbens, UCSB <aks@hub.ucsb.edu>
;;;
;;; Display types:   time, time-12, time-24, time-12-hhmm, time-24-hhmm,
;;;                  and time-hhmm.
;;; Field Types:     time, time-12, time-24, and time-arb-storage
;;; Functions:       (all prefixed w/ "edb-t-timedate1:")
;;;                  parse-time-string, format-time-12, format-time-24
;;;                  time->storage-string, storage-string->time,
;;;                  time-order, time-merge, time-match-function,
;;;                  db-time-default-constraint
;;;

;;; Abstraction:  TIME object is a 3-elt list (this allows `nth' to work)
(defsubst edb-t-timedate1:make-time (hours mins secs)
  "Return a new EDB 1.x time object made from HOURS, MINS and SECS."
  (list hours mins secs))

(defsubst edb-t-timedate1:time-hours (time) (car time))
(defsubst edb-t-timedate1:time-mins (time) (car (cdr time)))
(defsubst edb-t-timedate1:time-secs (time) (car (cdr (cdr time))))

(defsubst edb-t-timedate1:make-empty-time ()
  "Return a new EDB 1.x time object containing no information."
  (edb-t-timedate1:make-time nil nil nil))

(defun edb-t-timedate1:empty-time-p (time)
  "Return t if all the TIME object's slots contain nil, nil otherwise."
  (and (null (edb-t-timedate1:time-hours time))
       (null (edb-t-timedate1:time-mins time))
       (null (edb-t-timedate1:time-secs time))))

(defun edb-t-timedate1:time-default-constraint (time rec idx db)
  "Enforce proper EDB 1.x time object values."
  (if (or (edb-t-timedate1:empty-time-p time)
          (and (< (edb-t-timedate1:time-hours time) 23)
               (< (edb-t-timedate1:time-mins time) 59)
               (< (edb-t-timedate1:time-secs time) 59)))
      t
    (error "Invalid time value.")))


;; edb-t-timedate1:parse-time-string
;;
;; State-driven time parser; I converted this from my Perl time parser.
;; Alan K. Stebbens, UCSB <aks@hub.ucsb.edu>

(defconst edb-t-timedate1:parse-time-regexp-array
  [ "\\([0-9]?[0-9]\\)\\(:\\| ?[ap]m\\)"
    "\\([0-5][0-9]\\)\\(:\\| ?[ap]m\\|\\Sw\\|$\\)"
    "\\([0-5][0-9]\\)\\( *[ap]m\\|\\Sw\\|$\\)" ]
  "An array of regexps used by `edb-t-timedate1:parse-time-string',
indexed by the current parse state to obtain the appropriate regexp.")


(defun edb-t-timedate1:parse-time-string (s)
  "Parse the first occurrence of hh:mm:ss in string S; return a time object.
If \":ss\" is hidden in S, the seconds default to zero.
If S contains only whitespace, return an empty time object.
If S is nil, use instead the result of
`edb-t-timedate1:parse-time-default-function'."
  (setq s (if (not s)                   ; provide default time for nil strings
              (edb-t-timedate1:parse-time-default-function)
            (db-string-trim-whitespace s)))
  (if (zerop (length s))
      (edb-t-timedate1:make-empty-time)
    (let ((case-fold-search t)
          ;; Initial regexp matches HH: or HHpm
          (hh 0) (mm 0) (ss 0) pm
          (vars '[hh mm ss])
          (ofs 0)
          (state 0))
      (while (and (< state 3)
                  (< ofs (length s))
                  (string-match
                   (aref edb-t-timedate1:parse-time-regexp-array state)
                   s ofs))
        (set (aref vars state) (string-to-number (match-string 1 s)))
        (setq state (if (equal ":" (match-string 2 s))
                        (1+ state)
                      3))
        (setq ofs (match-end 0)))
      (edb-t-timedate1:make-time
       (if (and (setq pm (match-string 2 s))
                (string-match "[ap]m" pm))
           (+ (% hh 12) (if (string-match "pm" pm) 12 0))
         hh)
       mm ss))))

(defvar edb-t-timedate1:parse-time-default 'empty
  "One of the symbols `empty' or `current-time' or `now' (these last two are
equivalent), specifying what `edb-t-timedate1:parse-time-default-function'
should return, and `edb-t-timedate1:parse-time-string' should use when passed
a nil argument.")

(defun edb-t-timedate1:parse-time-default-function ()
  "Return a default value for `edb-t-timedate1:parse-time-string'
to use if its input is nil."
  (case edb-t-timedate1:parse-time-default
    ((empty) "")
    ((current-time now) (current-time-string))
    (t (error "Unrecognized value `%s' for variable `%s'"
              edb-t-timedate1:parse-time-default
              'edb-t-timedate1:parse-time-default))))

;; test routine for above

'(defun test-time-parser ()
   (interactive)
   (dolist (time '("1am"
                   "1pm"
                   "1 pm"
                   "1 PM"
                   "1:01am"
                   "1:02pm"
                   "12:01:01am"
                   "12:01:01pm"
                   "12:34:45"
                   " 12:34:45"
                   " 12:34:45 "
                   "12:34:45 "
                   "May 12 14:34 1992"
                   "May 19 10:17:23pm 1992"
                   "May 19 10:17:23 pm 1992"))
     (message "In = \"%s\" Out = \"%s\" (CR for next)"
              time
              (edb-t-timedate1:parse-time-string time))
     (read-char)))


(defun edb-t-timedate1:format-time-24 (time)
  "Format TIME (a three element list) into a 24 hour time string in the
format HH:MM:SS.  If an element of the list is nil, that component is
omitted.  Typically, the seconds element is hidden or set to nil to
produce a time format with only HH:MM.
See `edb-t-timedate1:format-time-24-hhmm'."
  (let ((hh (edb-t-timedate1:time-hours time))
        (mm (edb-t-timedate1:time-mins time))
        (ss (edb-t-timedate1:time-secs time)))
    (concat
     (and hh (format "%d" hh))
     (and hh mm ":")
     (and mm (format "%02d" mm))
     (and mm ss (format ":%02d" ss)))))


(defun edb-t-timedate1:format-time-12 (time)
  "Format TIME (a 3 element list) into a 12 hour time string in the
format HH:MM:SS PM.  If an element of the list is nil, that component is
omitted.  Typically, the seconds element is hidden or set to nil to
produce a time format with only HH:MM.
See `edb-t-timedate1:format-time-12-hhmm'."
  (let ((hh (edb-t-timedate1:time-hours time))
        (mm (edb-t-timedate1:time-mins time))
        (ss (edb-t-timedate1:time-secs time)))
    (concat
     (and hh (format "%d" (cond ((zerop hh) 12)
                                ((< hh 13) hh)
                                (t (- hh 12)))))
     (and hh mm ":")
     (and mm (format "%02d" mm))
     (and mm ss (format ":%02d" ss))
     (and hh (if (>= hh 12) "pm" "am")))))

(fset 'edb-t-timedate1:format-time-hhmm 'edb-t-timedate1:format-time-12-hhmm)

(defun edb-t-timedate1:format-time-12-hhmm (time)
  "Format TIME in HH:MM PM format."
  (edb-t-timedate1:format-time-12
   (edb-t-timedate1:make-time
    (edb-t-timedate1:time-hours time)
    (edb-t-timedate1:time-mins time)
    nil)))

(defun edb-t-timedate1:format-time-24-hhmm (time)
  "Format TIME in 24-hour format without seconds."
  (edb-t-timedate1:format-time-24
   (edb-t-timedate1:make-time
    (edb-t-timedate1:time-hours time)
    (edb-t-timedate1:time-mins time)
    nil)))

;;;
;;; EDB type specifications
;;;


(edb-define-displaytype 'time nil
  :indent           nil
  :actual->display 'edb-t-timedate1:format-time-12
  :display->actual 'edb-t-timedate1:parse-time-string)

(dolist (type '("12"                    ; am/pm time
                "24"                    ; military time
                "hhmm"                  ; synonym for time-12-hhmm
                "12-hhmm"               ; am/pm w/o secs
                "24-hhmm"))             ; military w/o secs
  (edb-define-displaytype
      (intern (concat "time-" type))
      'time
    :actual->display (intern (concat "edb-t-timedate1:format-time-" type))))

(edb-define-recordfieldtype 'time nil
  :type                'time
  :default-value       (edb-t-timedate1:make-empty-time)
  :constraint-function 'edb-t-timedate1:time-default-constraint
  :actual->stored      'edb-t-timedate1:time->storage-string
  :stored->actual      'edb-t-timedate1:storage-string->time
  :merge-function      'edb-t-timedate1:time-merge
  :order-fn            'edb-t-timedate1:time-order
  :match-function      'edb-t-timedate1:time-match-function
  :help-info           "A time.")

(edb-define-recordfieldtype 'time-12 'time
  :help-info "A 12-hour time, with AM/PM.")

(edb-define-recordfieldtype 'time-24 'time
  :help-info "A 24-hour time (a.k.a. military time).")

(edb-define-recordfieldtype 'time-arb-storage 'time
  :stored->actual 'edb-t-timedate1:parse-time-string)

(defun edb-t-timedate1:time-order (t1 t2)
  (let ((result 0) (n 0))
    (while (and (<= n 2) (zerop result))
      (setq result
            (db-number-or-nil-order-nil-greatest
             (nth n t1) (nth n t2))))
    result))

(defun edb-t-timedate1:time-match-function (pat targ)
  (and (or (not (edb-t-timedate1:time-hours    pat))
           (and (edb-t-timedate1:time-hours    targ)
                (= (edb-t-timedate1:time-hours pat)
                   (edb-t-timedate1:time-hours targ))))
       (or (not (edb-t-timedate1:time-mins    pat))
           (and (edb-t-timedate1:time-mins    targ)
                (= (edb-t-timedate1:time-mins pat)
                   (edb-t-timedate1:time-mins targ))))
       (or (not (edb-t-timedate1:time-secs    pat))
           (and (edb-t-timedate1:time-secs    targ)
                (= (edb-t-timedate1:time-secs pat)
                   (edb-t-timedate1:time-secs targ))))))

(defun edb-t-timedate1:time->storage-string (time)
  (format "%02d:%02d:%02d"
          (or (edb-t-timedate1:time-hours time) 0)
          (or (edb-t-timedate1:time-mins  time) 0)
          (or (edb-t-timedate1:time-secs  time) 0)))

(defun edb-t-timedate1:storage-string->time (s)
  (edb-t-timedate1:make-time
   (db-string->integer (substring s 0 2))
   (db-string->integer (substring s 3 5))
   (db-string->integer (substring s 6 8))))

(defun edb-t-timedate1:time-merge (t1 t2)
  ;; If an item in one time is nil, take the other time's value.
  (let ((hh (or (edb-t-timedate1:time-hours t1)
                (edb-t-timedate1:time-hours t2)))
        (mm (or (edb-t-timedate1:time-mins t1)
                (edb-t-timedate1:time-mins t2)))
        (ss (or (edb-t-timedate1:time-secs t1)
                (edb-t-timedate1:time-secs t2))))
    (edb-t-timedate1:make-time hh mm ss)))

(provide 'edb-t-timedate1)

;;; edb-t-timedate1.el ends here
