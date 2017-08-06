;;; db-tagged.el --- part of EDB, the Emacs database

;; Copyright (C) 2004-2017 Thien-Thi Nguyen

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

;; Provide support for files of tagged fields, such as
;;  Name:  John Doe
;;  Age:   42
;;  Salary:22000

;; This file adds to EDB basic support for parsing and generating
;; files where records consist of a group of lines with tags and
;; values.  All functions and variables defined here start with the
;; prefix "db-tagged-".

;; The basic way to use this is to call `db-tagged-setup' with a list
;; describing the fields.  There are hooks to parse out and generate
;; possible added entries that don't fit the basic model.  There are
;; variables to customize all the separator strings and regexps for
;; complete parsing.  These should be set by specifying overriding
;; attributes in the call to `db-tagged-setup'.

;; The field info passed to `db-tagged-setup' is a list with one entry
;; per field.  Each entry is a three-element list: the field name, the
;; tag used to identify it in the file, and a short description for
;; documentation (not presently used).  The field name can be a two
;; element list with a field name and type to use types other than the
;; default.  Two values in the tag position are special, nil means that
;; it will be a computed field and is never stored in the database (and
;; thus doesn't need a tag), and the empty string marks a field where
;; "comments" are collected, comments consist of lines starting with the
;; separator, i.e. empty tags.

;; The default is records separated by blank lines, tags separated from
;; fields by ":", white space around the separator is not significant on
;; input, and that the separator should be followed by one tab on
;; output, and continuation lines start with whitespace.  All of these
;; can be customized.

;;; Code:

(require 'database)
(eval-when-compile (require 'cl))

(eval-when-compile (defvar database))         ; used dynamically

;;;###autoload
(defun db-tagged-setup (fspecs &rest attrs)
  "Ready the database to read files in tagged format.
Argument FSPECS is a list of tagged-field specifications, one for each field
in a database record.  Each tagged-field specification is a three-element
list of the field name \(a symbol\), the tag used to identify it in the file
\(a string\), and a brief help string.  Instead of a symbol, the tagged-field
name may be a cons of the field name and its type.  To indicate that a field
is never found in the input file \(typically because it is computed on the
fly\), use nil for its tag.

ATTRS is a sequence of alternating keywords and values specifying overriding
attributes.  Here are the valid attributes and their default values:

 :tag-chars           \"-_A-Za-z\"
 :pre-tag             \"\"
 :pre-tag-regexp      nil
 :pre-tag-output      nil
 :separator           \":\"
 :separator-regexp    nil
 :separator-output    nil
 :continuation        \"\t\"
 :continuation-regexp \"[ \t]+\"
 :continuation-output nil
 :pre-parse-thunk     nil
 :pre-write-function  nil
 :post-write-function nil
 :index-function      nil
 :default-field       nil

Note: Do not call `database-set-fieldnames-to-list' if using this function."
  (let ((details (edb--1D database :tagged-details))
        (defaults (list :tag-chars           "-_A-Za-z"
                        :pre-tag             ""
                        :pre-tag-regexp      nil
                        :pre-tag-output      nil
                        :separator           ":"
                        :separator-regexp    nil
                        :separator-output    nil
                        :continuation        "\t"
                        :continuation-regexp "[ \t]+"
                        :continuation-output nil
                        :pre-parse-thunk     nil
                        :pre-write-function  nil
                        :post-write-function nil
                        :index-function      nil
                        :default-field       nil)))
    ;; Check attributes.
    (let ((ls attrs))
      (while ls
        (unless (memq (car ls) defaults)
          (error "No such db-tagged attribute: %S" (car ls)))
        (setq ls (cddr ls))))
    ;; First, try to detect whether this database was once in tagged file
    ;; layout, but is now in internal file layout.
    (if details
        (unless (gethash :converted-p details)
          (puthash :converted-p t details))
      (apply 'edb--mputhash
             (setq details
                   (edb--1D! database :tagged-details
                             (make-hash-table :test 'eq :size 19)))
             :tagged-field-spec fspecs
             defaults)
      (database-set-fieldnames-to-list database
        (mapcar 'car fspecs)
        ;; Most fields are strings (or missing, here represented as nil)
        'nil-or-string)

      ;; Save help strings.
      (let* ((ls (mapcar 'caddr fspecs))
             (walk ls)
             (fno 0))
        (while walk
          (setcdr walk (cons (car walk) (cdr walk)))
          (setcar walk fno)
          (incf fno)
          (setq walk (cddr walk)))
        (apply 'db-set-field-help database ls))

      ;; Elaborate recordfieldspecs.  FIXME: This should not be necessary.
      (let* ((all-rs (database-recordfieldspecs database))
             (count (length all-rs)))
        (do ((fno 0 (1+ fno)))
            ((= fno count))
          (aset all-rs fno (db-rfspec<-rftype (aref all-rs fno)))))

      (setf (database-read-record-from-region
             database) (lambda ()
                         (apply 'db-tagged-read
                                (edb--1D database :tagged-read-kargs)))
            (database-write-region-from-record
             database) (lambda (record)
                         (apply 'db-tagged-write record
                                (edb--1D database :tagged-write-kargs)))
            (sepinfo-sep-string (database-record-sepinfo database)) "\n\n"
            (sepinfo-post-last-string (database-record-sepinfo database)) "\n")
      (add-hook 'db-after-read-hooks 'db-tagged-convert/index)

      ;; Override the defaults.
      (apply 'edb--mputhash details attrs)

      (macrolet
          ((D   (k)   `(gethash ,k details))
           (D-! (k v) `(unless (D ,k) (setf (D ,k) ,v))))

        ;; Compute downstream defaults.
        (D-! :pre-tag-regexp (regexp-quote (D :pre-tag)))
        (D-! :pre-tag-output (D :pre-tag))
        (D-! :separator-regexp (concat "[ \t]*"
                                       (regexp-quote (D :separator))
                                       "[ \t]*"))
        (D-! :separator-output (concat (D :separator) "\t"))
        (D-! :continuation-regexp (regexp-quote (D :continuation)))
        (D-! :continuation-output (D :continuation))

        ;; Performance-motivated pre-computations.
        (cl-flet
            ((P! (k &rest ls) (edb--1D! database k ls)))
          (P! :tagged-read-kargs
              ;; trx
              (concat "^" (D :pre-tag-regexp)
                      "\\([" (D :tag-chars) "]*\\)"
                      (D :separator-regexp))
              ;; crx
              (D :continuation-regexp)
              ;; pre
              (D :pre-parse-thunk)
              ;; t2f
              (let ((ht (make-hash-table
                         :test 'equal
                         :size (length fspecs)))
                    tag field)
                (dolist (spec fspecs)
                  (when (setq tag (cadr spec))
                    (setq field (car spec))
                    (puthash tag (if (consp field)
                                     (car field)
                                   field)
                             ht)))
                ht)
              ;; default-field
              (D :default-field))
          (P! :tagged-write-kargs
              ;; pre
              (D :pre-write-function)
              ;; sep
              (D :separator-output)
              ;; contin
              (D :continuation-output)
              ;; all
              (cl-loop
               with nam
               with tag
               with nm2no = (edb--1D database :nm2no)
               with fno
               for spec in fspecs
               if (setq tag (cadr spec))
               collect (progn
                         (setq nam (car spec)
                               fno (gethash (if (consp nam)
                                                (car nam)
                                              nam)
                                            nm2no))
                         (vector tag fno
                                 (aref
                                  (db-rs-slice
                                   database
                                   'edb--1rs-actual->stored)
                                  fno))))
              ;; aft
              (D :post-write-function)))))))

(defun db-tagged-convert/index ()
  (let ((details (edb--1D database :tagged-details)))
    (macrolet
        ((D (k) `(gethash ,k details)))
      ;; Work with actual objects.
      (when (and (D :tagged-field-spec)
                 (not (D :converted-p)))
        (database-stored->actual database)
        (setf (D :converted-p) t))
      ;; Do any user indexing.
      (let ((fn (D :index-function)))
        (when fn (funcall fn database)))
      ;; Clean up ourselves.
      (remove-hook 'db-after-read-hooks 'db-tagged-convert/index))))

(defun db-tagged-read (trx crx pre t2f default-field)
  (when pre
    (funcall pre))
  (goto-char (point-min))
  (let (pl nam fld prev)
    (while (not (eobp))
      (unless (looking-at trx)
        (error "Unexpected end of record (position %d)" (point)))
      (setq nam (match-string 1)
            fld (gethash nam t2f))
      (end-of-line)
      (let ((val (buffer-substring (match-end 0) (point))))
        (unless (eobp) (forward-char 1))
        (while (looking-at crx)
          (end-of-line)
          (setq val (concat val "\n"
                            (buffer-substring (match-end 0)
                                              (point))))
          (forward-line 1))
        (when (null fld)
          ;; Should allow an escape hook here, and provide a
          ;; generic one that adds to an alist-like entry
          (db-message "Invalid field name `%s'" nam)
          (setq fld default-field))
        (setq prev (plist-get pl fld))
        (when fld
          (setq pl (plist-put pl fld
                              (if (null prev)
                                  val
                                (concat prev "\n" val)))))))
    pl))

(defun db-tagged-write (record pre sep contin all aft)
  (when pre
    (funcall pre record))
  (let (tag fno a->s v val i cnt j)
    (dolist (vec all)
      (setq tag  (aref vec 0)
            fno  (aref vec 1)
            a->s (aref vec 2)
            v    (aref record fno)
            val  (if a->s
                     (funcall a->s v)
                   v))
      (unless (zerop (length val))
        (setq i 0
              cnt (if (equal "" tag)
                      sep
                    contin))
        (insert tag sep)
        (while (setq j (string-match "\n" val i))
          (insert (substring val i j) "\n" cnt)
          (setq i (+ j 1)))
        (insert (substring val i) "\n"))))
  (delete-char -1)                      ; HACK: punt last newline;
                                        ; it will be added back later.
  (when aft
    (funcall aft record)))

(provide 'db-tagged)

;;; db-tagged.el ends here
