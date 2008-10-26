;;; db-types.el --- part of EDB, the Emacs database

;; Copyright (C) 2004,2005,2006,2007,2008 Thien-Thi Nguyen

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

;; Library of types for EDB database fields.

;; This file contains predefined types.  For efficiency, they're defined
;; in terms of the displayspec abstraction instead of via format strings.
;; Improvements and additions are welcome.  Predefined types for dates and
;; times can be found in db-time.el.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Contents
;;;

;;; Contents
;;; Variables
;;; Displaytype
;;; Enumeration Displaytypes
;;; Numbers
;;; Booleans
;;; Strings


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;

(:edb1 :1displaytypes (make-hash-table :test 'eq))

(:edb1 :1recordfieldtypes (make-hash-table :test 'eq))

(defvar db-enum-ignore-case t
  "If non-nil, assume that any association lists created from
enumeration types input names are to include both upper and lower case
versions of the name, if distinct.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Displaytype
;;;

;;;###badname
(defalias 'define-displaytype-from-displayspec 'edb-define-displaytype)
(put 'edb-define-displaytype 'lisp-indent-hook 2)
(defun edb-define-displaytype (name source &rest override)
  "Define a displaytype NAME (a symbol) with SOURCE.
SOURCE may be a displayspec, the name (a symbol) of a currently
defined type, or nil.  In all cases, a new displayspec object
is created and then modified by OVERRIDE, a sequence of alternating
keywords and values, and finally added to a global list.  Return NAME."
  (unless (symbolp name)
    (error "Not a symbol: %s" name))
  (let ((ds (cond ((edb--1ds-p source) (edb--copy-1ds source))
                  ((not source) (apply 'edb--make-1ds override))
                  ((and (symbolp source)
                        (let ((ds (db-dspec<-dtype source)))
                          (when ds (edb--copy-1ds ds)))))
                  (t (error "Not a displayspec or known displaytype: %s"
                            source)))))
    (when override
      (let ((ls override) (kwidx (get 'edb--1ds 'kwidx)))
        (while ls
          (aset ds (or (cdr (assq (car ls) kwidx))
                       (error "No such keyword: %s" (car ls)))
                (cadr ls))
          (setq ls (cddr ls)))))
    (puthash name ds (edb--G :1displaytypes))
    name))

;;;###badname
(defalias 'define-recordfieldtype-from-recordfieldspec 'edb-define-recordfieldtype)
(put 'edb-define-recordfieldtype 'lisp-indent-hook 2)
(defun edb-define-recordfieldtype (name source &rest override)
  "Define a recordfieldtype NAME (a symbol) with SOURCE.
SOURCE may be a recordfieldspec, the name (a symbol) of a currently
defined type, or nil.  In all cases, a new recordfieldspec object
is created and then modified by OVERRIDE, a sequence of alternating
keywords and values, and finally added to a global list.  Return NAME."
  (unless (symbolp name)
    (error "Not a symbol: %s" name))
  (let ((rs (cond ((edb--v1-rs-p source) (edb--copy-v1-rs source))
                  ((not source) (apply 'edb--make-v1-rs override))
                  ((and (symbolp source)
                        (let ((rs (db-rfspec<-rftype source)))
                          (when rs (edb--copy-v1-rs rs)))))
                  (t (error "Unknown recordfieldspec or recordfieldtype: %s"
                            source)))))
    (when override
      (let ((ls override) (kwidx (get 'edb--v1-rs 'kwidx)))
        (while ls
          (aset rs (or (cdr (assq (car ls) kwidx))
                       (error "No such keyword: %s" (car ls)))
                (cadr ls))
          (setq ls (cddr ls)))))
    (puthash name rs (edb--G :1recordfieldtypes))
    name))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Enumeration displaytypes
;;;

;; An enumeration displaytype is used for fields whose values are one of a
;; fixed set of alternatives.

;; For each alternative, four pieces of information are specified
;; (see documentation for `define-enum-type' for details).  We have the
;; following options for storing the relationships between those pieces of
;; information, which must be used by such functions as the displayspec's
;; display->actual function:
;;  * in global variables keyed by enum-type name and looked up when needed,
;;  * in database-local variables similar to the above, or
;;  * in the functions themselves (the functions are created when the
;;    enum-type is defined and must in any event contain some specific
;;    information such as the enum-type name).

;; Where to Keep Info: If any of the information is kept in database-local
;;    variables, all of it should be, because database-local variables are
;;    saved when the database is stored in internal file layout and restored
;;    when it is read in again.  However, other information is kept in global
;;    variables such as :1displaytypes, so it is problematic to keep any
;;    information in database-local variables; it is especially bad to have
;;    the type half-defined so that an error is produced if the type is
;;    redefined but the existing information is insufficient to provide the
;;    full enum type functionality.  If all the information is stored in
;;    database-local variables, then it is unnecessary for databases stored in
;;    internal file layout to have calls to define-enum-type in their
;;    auxiliary files.


;;;###badname
(defalias 'define-enum-type 'edb-define-enumtype)
(defun edb-define-enumtype (typename alternatives &optional optstring)
  "Make TYPENAME (a symbol or string) an enumerated type.
Both a displaytype and a recordfieldtype are created.

ALTERNATIVES is a list.  Each alternative is a list of up to four components:
 the internal representation, any constant Lisp object, often a string;
 the input representation typed by the user to specify this alternative,
   a string or list of strings (for multiple input representations);
 the display representation, a string; and
 the file storage representation, a string.

If the input representation is omitted and the internal representation is a
string, that string is used.  If the display representation is omitted, it
defaults to the first input representation.  The display representation is
automatically also a valid input representation.  If the file storage
representation is omitted, it defaults to the display representation.
If all the other components are omitted, the internal representation string
may be used in place of a one-element list containing just it.

Optional argument OPTSTRING is a displayspec option string."

  (let ((type (if (stringp typename)
                  (intern typename)
                typename))
        d->a dinput->a a->d a->s a-s-differ
        internal input display storage)
    (dolist (alt alternatives)
      (unless (listp alt)
        (setq alt (list alt)))
      (setq internal (car alt)
            input (or (car (cdr alt)) internal)
            input (if (listp input) input (list input))
            display (or (car (cdr (cdr alt))) (car input))
            storage (or (nth 3 alt) display)

            d->a (nconc d->a (mapcar (lambda (irep)
                                       (cons irep internal))
                                     input))
            a-s-differ (or a-s-differ (not (equal internal storage))))
      (unless (member display input)
        (push (cons display internal) dinput->a))
      (push (cons internal display) a->d)
      (push (cons internal storage) a->s))
    ;; The order is significant in db-enum-make-help-info.
    ;; [It's not clear whether that's a feature or a bug.]
    (setq d->a (nconc d->a (nreverse dinput->a))
          a->d (nreverse a->d)
          a->s (and a-s-differ (nreverse a->s)))

    (flet ((enum-do-completions
            (rep type d->a)
            ;; Given a string REP and an enum TYPE, return REP if it is a
            ;; valid input representation, otherwise see if it completes to a
            ;; valid one, and show the possible completions.
            (let ((hit (assoc rep d->a)))
              (if hit
                  (cdr hit)
                (let* ((completion-ignore-case db-enum-ignore-case)
                       (try (try-completion rep d->a)))
                  (if (and try (setq hit (assoc try d->a)))
                      (cdr hit)
                    (dbf-set-this-field-text (or try ""))
                    (setq rep
                          (completing-read
                           (format "Enter \"%s\" enum value (? for list): "
                                   type)
                           d->a nil t (or try "")))
                    (cdr (assoc rep d->a)))))))
           (make-enum-member-orderer
            (type a->d v< v= v>)
            ;; Given an enum type TYPE, create an ordering function which
            ;; compares two items in the enum type's alternatives list.  The
            ;; resulting function returns V< if its first argument precedes
            ;; its second argument in the alternative list, V= if they're
            ;; equal or neither appears, and V> if the first follows the
            ;; second.
            ;;
            ;; If one of the arguments doesn't appear in the alternatives
            ;; list, the other is considered to precede it.
            `(lambda (o1 o2)
               (if (equal o1 o2)
                   ,v=
                 (let ((ls ',a->d)
                       (rv ,v=)
                       alt-o)
                   (while ls
                     (setq alt-o (car ls))
                     (cond ((equal (car alt-o) o1)
                            (setq rv ,v<
                                  ls nil))
                           ((equal (car alt-o) o2)
                            (setq rv ,v>
                                  ls nil))
                           (t
                            (setq ls (cdr ls)))))
                   rv)))))

      (edb-define-displaytype type
        (db-dspec<-type/opts nil (when optstring
                                   (split-string optstring ","))
                             t)
        :indent            nil
        :display->actual `(lambda (input-rep)
                            (,(symbol-function 'enum-do-completions)
                             input-rep ',type ',d->a))
        :actual->display `(lambda (enum-val)
                            (cdr (assoc enum-val ',a->d))))

      (edb-define-recordfieldtype type nil
        :type           type
        :default-value  ""
        :order-fn       (make-enum-member-orderer type a->d -1 0 1)
        :sort-fn        (make-enum-member-orderer type a->d t nil nil)
        :match-function 'db-string-match-function
        :help-info      (with-temp-buffer
                          (let ((standard-output (current-buffer)))
                            (princ (format "%s:  an enumerated type.  " type))
                            (display-completion-list (mapcar 'car d->a))
                            (buffer-string)))
        :actual->stored (when a->s
                          `(lambda (enum-val)
                             (cdr (assoc enum-val ',a->s))))
        :stored->actual (when a->s
                          `(lambda (stored-val)
                             (car (rassoc stored-val ',a->s))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Numbers
;;;


;;;
;;; Integers
;;;

(edb-define-displaytype 'integer nil
  :indent           nil
  :actual->display 'int-to-string
  :display->actual 'string-to-int)

(edb-define-recordfieldtype 'integer nil
  :type           'integer
  :default-value   0
  :actual->stored 'int-to-string
  :stored->actual 'string-to-int
  :order-fn       'db-number-order
  :sort-fn        '<
  :match-function '=
  :help-info      "An integer.")

(edb-define-displaytype 'integer-or-nil nil
  :indent           nil
  :actual->display 'db-number-or-nil->string
  :display->actual 'db-string->integer-or-nil)

(edb-define-recordfieldtype 'integer-or-nil nil
  :type           'integer-or-nil
  :default-value   0
  :actual->stored 'db-number-or-nil->string
  :stored->actual 'db-string->integer-or-nil
  :order-fn       'db-number-or-nil-order-nil-greatest
  ;; :sort-fn     '<
  :match-function 'equal
  :help-info      "An integer, or nil.")



;;;
;;; Numbers (a number is an integer or a float)
;;;

(let ((correct-string-to-number
       ;; Emacs 19.28 bug: isfloat_string fails when arg has trailing spaces,
       ;; so (s2n "5.4") => 5.4 but (s2n "5.4 ") => 5 .
       (if (= 5 (string-to-number "5.4 "))
           (lambda (string)
             (if (string-match " " string)
                 ;; chop everything after the first space
                 (string-to-number (substring string 0 (match-beginning 0)))
               (string-to-number string)))
         'string-to-number)))

  (edb-define-displaytype 'number nil
    :indent           nil
    :actual->display 'number-to-string
    :display->actual  correct-string-to-number)

  (edb-define-recordfieldtype 'number nil
    :type           'number
    :default-value   0
    :actual->stored 'number-to-string
    :stored->actual  correct-string-to-number
    :order-fn       'db-number-order
    :sort-fn        '<
    :match-function '=
    :help-info      "A number."))

(edb-define-displaytype 'number-or-nil nil
  :indent           nil
  :actual->display 'db-number-or-nil->string
  :display->actual 'db-string->number-or-nil)

(edb-define-recordfieldtype 'number-or-nil nil
  :type           'number-or-nil
  :default-value   0
  :actual->stored 'db-number-or-nil->string
  :stored->actual 'db-string->number-or-nil
  :order-fn       'db-number-or-nil-order-nil-greatest
  ;; :sort-fn     '<
  :match-function 'equal
  :help-info      "A number, or nil.")


;;;
;;; Sorting and ordering
;;;

(defsubst db-number-order (a b)
  "Return -1, 0, or 1 depending on whether A < B, A = B, or A > B."
  (cond ((= a b) 0)
        ((< a b) -1)
        (t 1)))

(defun db-number-or-nil-order-nil-greatest (a b)
  "Like db-number-order, but nil is treated as greater than any integer.
This puts nil after integers in an increasing list."
  (cond ((and a b)
         (db-number-order a b))
        (a -1)
        (b 1)
        (t 0)))

(defun db-number-or-nil-order-nil-least (a b)
  "Like db-number-order, but nil is treated as smaller than any integer.
This puts nil after integers in a decreasing list."
  (cond ((and a b)
         (db-number-order a b))
        (a 1)
        (b -1)
        (t 0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Booleans
;;;

(edb-define-displaytype 'yes-no nil
  :indent           nil
  :min-width        3
  :max-width        3
  :actual->display 'db-boolean->yes-no-string
  :display->actual 'db-yes-no-string->boolean)

(edb-define-recordfieldtype 'boolean nil
  :type           'boolean
  :default-value   nil
  :actual->stored 'db-boolean->yn-string
  :stored->actual 'db-yn-string->boolean
  :order-fn       'db-boolean-order-function
  :sort-fn        'db-boolean-lessp
  :match-function 'eq
  :help-info      "A boolean value.")

(defsubst db-boolean->yes-no-string (b)
  (if b "Yes" "No"))

(defun db-yes-no-string->boolean (s)
  (let ((low (downcase s)))
    (cond ((string= "yes" low)
           t)
          ((string= "no" low)
           nil)
          (t
           (error "`%s' is not `Yes' or `No'." s)))))

(defsubst db-boolean->yn-string (b)
  (if b "Y" "N"))

(defsubst db-yn-string->boolean (s)
  (string= "y" (downcase s)))

(defun db-boolean-order-function (b1 b2)
  ;; NOTE: t < nil so that in the "increasing" ordering "true" things occur
  ;;       before false ones.  (This is somewhat arbitrary.)
  (cond ((or (and b1 b2)
             (not (or b1 b2)))
         0)
        (b1
         -1)
        (t
         1)))

(defsubst db-boolean-lessp (b1 b2)
  (and (not b1) b2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Strings and variants
;;;

;; Type: string

(edb-define-displaytype 'string nil
  :max-height             nil
  :indent                 t
  :match-actual->display 'db-string-match-actual->display
  :match-display->actual 'db-string-match-display->actual)

(edb-define-recordfieldtype 'string nil
  :type           'string
  :default-value  ""
  :order-fn       'db-string-order-ci
  :sort-fn        'db-string-lessp-ci
  :match-function 'db-string-match-function
  :help-info      "A string.")

;; Type: one-line-string

(edb-define-displaytype 'one-line-string 'string
  :min-height 1
  :max-height 1
  :indent     nil)

(edb-define-recordfieldtype 'one-line-string 'string
  :type 'one-line-string)

;; Type: string-or-nil

(edb-define-displaytype 'string-or-nil 'string
  :actual->display 'db-string-or-nil->string)

(edb-define-recordfieldtype 'string-or-nil 'string
  :type           'string-or-nil
  :order-fn       'db-string-or-nil-order-ci
  :sort-fn        'db-string-or-nil-lessp-ci
  :match-function 'db-string-or-nil-match-function)

;; Type: nil-or-string

(edb-define-displaytype 'nil-or-string 'string-or-nil
  :display->actual 'db-string->nil-or-string)

(edb-define-recordfieldtype 'nil-or-string 'string-or-nil
  :type 'nil-or-string)

;; Type: one-line-string-or-nil

(edb-define-displaytype 'one-line-string-or-nil 'one-line-string
  :actual->display 'db-string-or-nil->string)

(edb-define-recordfieldtype 'one-line-string-or-nil
    'one-line-string
  :type           'one-line-string-or-nil
  :order-fn       'db-string-or-nil-order-ci
  :sort-fn        'db-string-or-nil-lessp-ci
  :match-function 'db-string-or-nil-match-function)


;; Helping functions for type string

(defsubst db-string-lessp-ci (s1 s2)
  "Case-insensitive version of string-lessp."
  (let ((v (compare-strings s1 0 nil s2 0 nil t)))
    (when (numberp v)
      (> 0 v))))

(defun db-string-order-ci (s1 s2)
  "Return -1, 0, or 1 depending on whether string S1 is lexicographically
less than, equal to, or greater than S2.  Case-insensitive."
  (let ((v (compare-strings s1 0 nil s2 0 nil t)))
    (cond ((eq t v) 0)
          ((> 0 v) -1)
          (t        1))))

;;; Matching strings

;; Pattern is a list of 'regexp and a regexp or a list of 'string, a
;; regexp, and a string.
(defun db-make-regexp-pattern (rx)
  (list 'regexp rx))
(defun db-regexp-pattern-regexp (pat)
  (car (cdr pat)))

(defun db-make-string-pattern (s &optional rx)
  (list 'string (or rx (regexp-quote s)) s))
(defun db-string-pattern-regexp (pat)
  (car (cdr pat)))
(defun db-string-pattern-string (pat)
  (car (cdr (cdr pat))))

(defsubst db-string-match-function (pat s)
  ;; The second element of pat is a regexp whether the pat is a
  ;; string or a regexp.
  (string-match (if (stringp pat)       ; slack: handle string
                    pat
                  (car (cdr pat)))
                s))

(defun db-string-match-display->actual (s)
  ;; Return a pattern.
  (if (string-match
       "^[ \t]*\\(/\\|regexp[ \t]+\\)" ;;; was: dbm-string-regexp-prefix
       s)
      (db-make-regexp-pattern (substring s (match-end 0)))
    (db-make-string-pattern s)))

(defun db-string-match-actual->display (pat)
  (cond ((eq (car pat) 'string)
         (db-string-pattern-string pat))
        ((eq (car pat) 'regexp)
         (concat
          "/" ;;; was: dbm-string-regexp-prefix-string
          (db-regexp-pattern-regexp pat)))
        (t
         (error "db-string-match-actual->display: bad pat %s" pat))))

;; Helping functions for type string-or-nil

(defsubst db-string-or-nil->string (s-o-n)
  (or s-o-n ""))

(defsubst db-string-or-nil-lessp-ci (x y)
  (db-string-lessp-ci (db-string-or-nil->string x)
                      (db-string-or-nil->string y)))

(defsubst db-string-or-nil-order-ci (x y)
  (db-string-order-ci (or x "") (or y "")))

(defsubst db-string-or-nil-match-function (pat s-o-n)
  (string-match (if (stringp pat)
                    pat
                  (if pat (car (cdr pat)) ""))
                (or s-o-n "")))

;; Helping functions for type nil-or-string

(defsubst db-string->nil-or-string (s)
  (if (equal "" s)
      nil
    s))

;;; db-types.el ends here
