;;; db-rdb.el --- part of EDB, the Emacs database

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

;; Provide support for RDB files, which can be a special case of a tagged
;; file, or, a tabular-format file.
;;
;; The RDB header for both kinds of files contains the field names and
;; the field descriptors, which are its length, type, and a "help" string.
;;
;; There are Perl scripts (by Walter Hobbs) implementing RDB files, and this
;; file is an attempt to provide EDB access to these kinds of files.

;; This file began as a copy of db-tagged.el, but has since been almost
;; completely rewritten.

;; The basic way to use this is to call "db-rdb-setup" with a list
;; describing the fields.  The purpose of the fields is to provide the
;; Elisp programmer a handle on the external fields.  The usage is
;; documented in the function.

;;; Code:

(require 'database)
(eval-when-compile (require 'cl))

(eval-when-compile (defvar database))         ; used dynamically

;;; Functions

;;; Abstraction of the RDB field specification:  (HANDLE TAG HELP)
;;; HANDLE --> (NAME . TAG) or NAME

(defsubst db-rdb-fspec-handle (fspec) (car fspec))
(defsubst db-rdb-fspec-name (fspec) (if (consp (car fspec))
                                        (car (car fspec))
                                      (car fspec)))
(defsubst db-rdb-fspec-type (fspec) (if (consp (car fspec))
                                        (cdr (car fspec))))
(defsubst db-rdb-fspec-tag (fspec) (car (cdr fspec)))
(defsubst db-rdb-fspec-help (fspec) (car (cdr (cdr fspec))))

;;;###autoload
(defun db-rdb-setup (rfspecs &optional lock-flag)

  "Ready the database to read files in RDB format.
Creates database local variables and sets database slots.  Argument
RFSPECS is a list of rdb-field specifications, one for each
field in a database record.  Optional, second argument LOCK-FLAG should
be non-nil to lock the file for synchronized updates.  The locking and
unlocking is done with \"rdblock\" and \"rdbunlock\", which must be
available in the current PATH environment variable.

Each field specification is a three-element list of the field name \(a
symbol\), the tag used to identify it in the file \(a string\), and a
brief help string.  Instead of a symbol, the rdb-field name may be a
two-element list of the field name its type.  To indicate that a field
is never found in the input file \(typically because it is computed on
the fly\), use nil for its tag."

  (let ((details (edb--1D database :rdb-details))
        (defaults (list :file-type 'list
                        :list-entry-regexp (concat "[ \t]*\\(["
                                                   "a-zA-Z0-9_"
                                                   "]*\\)"
                                                   (regexp-quote " | ")
                                                   "[ \t]*")
                        :list-continuation-regexp "^[ \t]*|[ \t]+"
                        :list-continuation-output nil
                        :max-field-name-length 0
                        :continuation-output nil)))
    ;; First, try to detect whether this database was once in RDB file
    ;; layout, but is now in internal file layout.
    (if details
        (unless (gethash :converted-p details)
          (puthash :converted-p t details))
      (apply 'edb--mputhash
             (setq details
                   (edb--1D! database :rdb-details
                             (make-hash-table :test 'eq :size 19)))
             :rdb-field-defs rfspecs
             defaults)
      (let* ((db database)
             (rsi (database-record-sepinfo db))
             (efields (db-rdb-read-fields db))
             (fspecs (db-rdb-correlate-field-defs rfspecs efields))
             (len 0))
        (cond
         ;; table format
         ((eq 'table (gethash :file-type details))
          ;; table style is the same a regular file layout
          (error "db-rdb-setup: table format not implemented yet!"))

         ;; list format
         ((eq 'list (gethash :file-type details))
          ;; discover the maximum of the length of all the field tags
          (mapc (lambda (fld)
                  (let ((tag (db-rdb-fspec-tag fld)))
                    (when (> (length tag) len)
                      (setq len (length tag)))))
                fspecs)
          (edb--mputhash
           details
           :max-field-name-length len
           :continuation-output (concat (make-string len 32)
                                        " | "))
          (setf (database-read-record-from-region db) 'db-rdb-list-rrfr
                (database-write-region-from-record db) 'db-rdb-list-wrfr
                ;; set pre-first-regexp to remove the header, up to the
                ;; first pair of newlines
                (sepinfo-pre-first-regexp rsi) (concat "\\`\\("
                                                       "\\([^\n]\\|\n[^\n]\\)*"
                                                       "\n\n\\)")
                (sepinfo-pre-first-regexp-submatch rsi) 1
                (sepinfo-sep-string rsi) "\n\n"
                (sepinfo-post-last-string rsi) "\n"
                ;; Save the header as the pre-first-string so it will get
                ;; written back on output.  At some point, maybe we should allow
                ;; for dynamic changes to the field definitions.
                (sepinfo-pre-first-string rsi)
                (gethash :header-string details))))

        ;; tell EDB about the fields now
        (database-set-fieldnames-to-list db
          (mapcar 'db-rdb-fspec-handle fspecs)
          ;; Most fields are strings (or missing, here represented as nil)
          'nil-or-string)

        ;; Save help strings.
        (let* ((ls (mapcar 'db-rdb-fspec-help fspecs))
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

        (add-hook 'db-after-read-hooks 'db-rdb-database-stored->actual)))))

(defun db-rdb-database-stored->actual (&optional db)
  "Like `database-stored->actual', but only in RDB file layout."
  (let ((details (edb--1D db :rdb-details)))
    (when (and details (not (gethash :converted-p details)))
      (database-stored->actual db)
      (puthash :converted-p t details))))

;; Read RDB List format region record

(defun db-rdb-list-rrfr ()

  "With the current buffer narrowed to a single RDB List record, parse
it into field values."

  (let* ((db database)
         (details (edb--1D db :rdb-details))
         (srx "^[ \t]*\\([a-zA-Z0-9_]*\\)[ \t]*|[ \t]*")
         (crx "^[ \t]*|[ \t]*")
         tag fld old val pl)
    ;; run-hooks takes a SYMBOL as its argument
    (let ((prep (gethash :pre-parse-thunk details)))
      (if prep
          (funcall prep)
        ;; todo: zonk after EDB 1.27
        (run-hooks 'db-rdb-rrfr-hooks)))
    (goto-char (point-min))
    (while (not (eobp))
      (unless (looking-at srx)
        (error "This didn't look right to me (point = %d)" (point)))
      (setq tag (match-string 1)
            fld (let ((fields (gethash :rdb-field-defs details))
                      (sym nil))
                  (dolist (field fields)
                    (when (equal tag (car (cdr field)))
                      (setq sym (car field))))
                  (if sym               ; found it?
                      (when (consp sym) ; is it a node?
                        (setq sym (car sym))) ; return just the symbol
                    ;; Field not defined -- report it, then add it
                    (message "Field name % encountered, but not defined." tag)
                    (setq fields (gethash :rdb-field-defs details))
                    (nconc fields (list (list (cons (setq sym (intern tag))
                                                    'string-or-nil)
                                              tag "Undefined field"))))
                  sym)
            old (plist-get pl fld))
      (end-of-line)
      (setq val (buffer-substring (match-end 0) (point)))
      ;; Why isn't this (forward-char 1)?
      (forward-line 1)
      (while (looking-at crx)
        (end-of-line)
        (setq val (concat val "\n" (buffer-substring (match-end 0) (point))))
        (forward-line 1))
      (push (if old (concat old "\n" val) val) pl)
      (push fld pl))
    pl))

;; RDB write RDB List file record from database record

(defun db-rdb-list-wrfr (record)

  "Given an EDB RECORD, write convert to the RDB List format
representation and write to the file."
  (let* ((db database)
         (a->s-slice (db-rs-slice db 'edb--1rs-actual->stored))
         (nm2no (edb--1D db :nm2no))
         (details (edb--1D db :rdb-details))
         (len (gethash :max-field-name-length details))
         (contin (gethash :continuation-output details)))
    ;; run-hooks takes a SYMBOL as its argument
    (let ((prep (gethash :pre-write-function details)))
      (if prep
          (funcall prep record)
        ;; todo: zonk after EDB 1.27
        (run-hooks 'db-rdb-wrfr-before-hooks)))
    (mapc (lambda (def)
            (let ((nam (db-rdb-fspec-name def))
                  (tag (db-rdb-fspec-tag def)))
              (unless (null tag)
                ;; Fields without tags are computed, so don't get stored
                (let* ((fno (gethash nam nm2no))
                       (a->s (aref a->s-slice fno))
                       (val (let ((v (aref record fno)))
                              (if a->s
                                  (funcall a->s v)
                                v))))
                  ;; null or empty values don't get written
                  (unless (or (null val)
                              (equal "" val))
                    (let ((pad (- len (length tag)))
                          (i 0) j)
                      ;; right-align the RDB field names
                      (when (> pad 0)
                        (insert (make-string pad 32)))
                      (insert tag " | ")
                      (while (setq j (string-match "\n" val i))
                        (insert (substring val i j) "\n" contin)
                        (setq i (+ j 1)))
                      (insert (substring val i) "\n")))))))
          (gethash :rdb-field-defs details))
    ;; HACK: punt last newline, it'll be added back later
    (delete-char -1)
    (let ((post (gethash :post-write-function details)))
      (if post
          (funcall post record)
        (run-hooks 'db-rdb-wrfr-after-hooks)))))

(defun db-rdb-read-fields (db)
  "In DATABASE, read the RDB-style headers, creating a list of field
definitions, which is returned as the result."
  "Setup the field names from the current EDB database file, which is
assumed to be formatted as an RDB database.  The RDB database may be
either in List format or Table form.  Any updates will continue to
maintain the RDB file in its current form.  Any EDB format files for the
database will be used automatically.

The argument, FIELDDEFS, is a list of elements: \(\(SYMBOL FIELD-NAME
FIELD-HELP\)...\).  SYMBOL may itself be either an atom, a symbol, or a
cons cell of \(SYMBOL . TYPE\).  SYMBOL is used to identify the field by
name in elisp code.  TYPE, if given, must be a valid EDB fieldtype,
either preconfigured, or user-defined.  If TYPE is null or hidden, it
will be deduced from the corresponding RDB header, utimately defaulting
to \"string\".

The FIELD-NAME is a string which must match one of the corresponding
field names in the database.  FIELD-NAMES which do not exist in the RDB
file are shown in an error message, as are any unmentioned field names
read from the RDB header.

The FIELD-HELP is a string used for information when queried with \"?\"
interactively.  If FIELD-HELP is null, any comment in the field
definition from the RDB file is used instead.

Lastly, if FIELDDEFS is null, then all of the information will be
obtained entirely from the RDB file, with the SYMBOL defaulting to the
symbol with the same print-string as FIELD-NAME."
  (with-current-buffer db-buffer
    (goto-char (point-min))
    (while (looking-at "^#")
      (forward-line 1))
    ;; if the first non-comment line is empty, we're in List mode
    (if (looking-at "^[ \t]*\n")
        ;; Positioned at the blank line preceeding the field definitions in an
        ;; RDB List file, read the field definitions and return them as an
        ;; association list: ((FIELD-NAME FIELD-DEF FIELD-HELP)...).
        (let (fields name defn width help end)
          (forward-line 1)              ; go to the first real line
          (while (not (looking-at "^[ \t]*$"))
            (cond ((looking-at "^\#"))  ; ignore comments
                  ((looking-at "^[ \t]*\\(\\w+\\)[ \t]+|[ \t]+\\(.*\\)$")
                   (setq name (match-string 1)
                         defn (db-string-split-first-word (match-string 2))
                         help (car-safe (cdr-safe defn))
                         defn (car defn)
                         width (and (string-match "\\([0-9]+\\)" defn)
                                    (string-to-number (match-string 1 defn)))
                         defn (and (string-match "\\([SNDM<>]\\)" defn)
                                   (match-string 1 defn))
                         fields (append fields (list (list name width defn
                                                           help))))))
            (forward-line 1))
          ;; we're just past the header -- copy everything and save it in
          ;; a local variable, so our caller can stuff into a record sepinfo.
          (forward-line 1)              ; skip the header/record separator
          (edb--mputhash
           (edb--1D database :rdb-details)
           :header-string (buffer-substring (point-min) (point))
           :header-fields fields
           ;; leave some clues as to the kind of RDB file we've parsed
           :file-type 'list)
          ;; return with the fields
          fields)
      ;; Positioned at the field name line in an RDB Table file, read the
      ;; field definitions and setup EDB for regular-file format.  Leave the
      ;; buffer positioned before the first record, if any.
      (let ((db database))
        (error "Not implemented yet.")
        (puthash :file-type 'table (edb--1D db :rdb-details))))))

(defun db-rdb-correlate-field-defs (ifields efields)

  "Given INTERNAL-FIELDS and EXTERNAL-FIELDS, correlate the fields with
each other and produce a field list suitable for M-x db-rdb-setup.

INTERNAL-FIELDS is a list: \(\(HANDLE NAME HELP\)...\), where HANDLE is
either \(SYMBOL . TYPE\) or just SYMBOL.

EXTERNAL-FIELDS is a list: \(\(NAME WIDTH FORMAT HELP\)...\), produced
by the function rdb-read-field-defs.

If TYPE is hidden, it is deduced from the corresponding FORMAT.
Similarly, if the internal HELP is hidden, any external HELP is used.
The internal definition always overrides the external, since it is more
specific to the EDB implementation.

The resulting list format is: \(\(\(SYMBOL . TYPE\) NAME HELP\)...\)."

  (let ((errs 0) fieldspec inames)
    (when (null ifields)
      (setq ifields (mapcar (lambda (edef) ; make default be same names
                              (list (intern (car edef))))
                            efields)))
    ;; now correlate internal fields with the external fields
    ;; while we're mapping over the idefs, build an alist of names
    ;; to use later for the reverse correlation check.
    (setq fieldspec
          (mapcar (lambda (idef)
                    (let* ((handle (nth 0 idef))
                           (name (nth 1 idef)) ; field name (tag)
                           (ihelp (nth 2 idef))
                           (symbol (or (and (atom handle) handle)
                                       (car handle)))
                           (type (and (consp handle)
                                      (cdr handle)))
                           (edef (and name
                                      (assoc name efields)))
                           width format ehelp)
                      (setq inames (append inames (list (list name))))
                      (if (and name (not edef))
                          (progn
                            (message "Internal field `%s' %s"
                                     name
                                     "not externally defined")
                            (beep t)
                            (sit-for 3)
                            (incf errs))
                        ;; else name could be null because it's dynamic
                        (when edef
                          (setq width (nth 1 edef)
                                format (nth 2 edef)
                                ehelp (nth 3 edef))))
                      ;; do merge of field info and return:
                      ;; ( (SYMBOL . TYPE) NAME HELP )
                      (list
                       (cons symbol
                             (or type
                                 (cond
                                  ;; left-aligned string
                                  ((or (null format)
                                       (equal format "S")
                                       (equal format "<")) 'one-line-string)
                                  ;; fixme: turn on right-alignment in
                                  ;; associated field displayspec --ttn
                                  ((equal format ">")      'one-line-string)
                                  ((equal format "N")      'integer-or-nil)
                                  ;; dates
                                  ((or (equal format "D")
                                       (equal format "M")) 'date-mmddyy)
                                  ;; no other alternatives
                                  ;; an error report
                                  (t
                                   (message "Unknown field format: %S, %s"
                                            format
                                            "string assumed")
                                   (beep t)
                                   (sit-for 3)
                                   'one-line-string))))
                       name
                       (or ihelp ehelp))))
                  ifields))
    ;; Now make sure we got all the external fields named
    ;; using the inames alist we built in the mapcar above.
    (mapc (lambda (edef)
            (let* ((name (nth 0 edef))
                   (idef (assoc name inames)))
              (unless idef
                (message "External field %s not defined internally" name)
                (sit-for 3)
                (incf errs))))
          efields)
    (unless (zerop errs)
      (unless (y-or-n-p (format "%d errors occurred; continue? " errs))
        (error "db-rdb-setup: %d errors" errs)))
    fieldspec))

;;;---------------------------------------------------------------------------

(provide 'db-rdb)

;;; db-rdb.el ends here
