;;; db-file-io.el --- part of EDB, the Emacs database

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

;; Read and write database files.

;;; Code:


(require 'pp)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DB variables
;;;

(defvar db-format-file-path nil
  "List of directories (strings) to search, in order, for format files not
found in the directory with their associated databases.")

(defvar db-aux-file-path nil
  "List of directories (strings) to search, in order, for auxiliary files not
found in the directory with their associated databases.")

(put 'db-before-read-hooks 'safe-local-variable 'edb-hookp)
(defvar db-before-read-hooks nil
  "Normal hook run immediately before a database is first read
but after all local variables are set.
The hooks are run in the data display buffer with variable `database' bound.
Variable `db-buffer' is bound to a buffer containing the database file.

This is a global variable.  If you set it to be specific to a particular
database \(for instance, in the format or auxiliary file\), then consider
having its last action be to reset the variable to nil.")

(put 'db-after-read-hooks 'safe-local-variable 'edb-hookp)
(defvar db-after-read-hooks nil
  "Function or list of functions run after a database is completely read.
The hooks are run in the data display buffer with variable `database' bound.
For databases with nonregular layouts, you might put a call to
`database-stored->actual' here, for instance.

This is a global variable.  If you set it to be specific to a particular
database \(for instance, in the format or auxiliary file), then consider
having its last action be to reset the variable to nil.")

(defvar db-format-file-suffixes '(".dbf" ".fmt" "f")
  "List of format file suffixes; the basename is that of the database file.
The suffixes are tried in order; the default is \(\".dbf\" \".fmt\" \"f\").
The . that may precede the extension must be specified explicitly.")

(defvar db-aux-file-suffixes '(".dba" ".aux" "a")
  "List of auxiliary file suffixes; the basename is that of the database file.
The suffixes are tried in order; the default is \(\".dba\" \".aux\" \"a\").
The . that may precede the extension must be specified explicitly.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Internal variables
;;;

;; These have docstrings so docstring-substitute will recognize them.

(eval-when-compile
  (defvar db-buffer nil "Buffer containing a database file being read."))

(eval-when-compile
  (defvar database nil "Database being read from a file; also other uses."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Read a database file
;;;

(defun db-read-database-file (source &optional display-template confirm)
  ;; Return the database.  The data display buffer will be the first element
  ;; of the :ddbufs list in the global hash.
  ;;
  ;; SOURCE is either a filename (string), or a list: (FILENAME RECORD...),
  ;; the latter indicating inherent data.
  ;;
  ;; Optional arg DISPLAY-TEMPLATE is one of the following:
  ;; - buffer containing the format to use;
  ;; - filename that specifies the format file;
  ;; - nil, in which case the one computed from `db-format-file-path' and
  ;;   `db-format-file-suffixes' will be used, or the user will be prompted
  ;;   for one.
  ;;
  ;; If optional prefix arg CONFIRM is non-nil, then no default will be used
  ;; without confirmation from the user.
  (edb--G! :io-error-p nil)
  (with-temp-buffer
    (let ((buf (current-buffer))
          (known-records (when (consp source)
                           (cdr source)))
          (db-file (when (stringp source)
                     source))
          (format-file (when (stringp display-template)
                         display-template))
          ddb database coding)

      ;; sanity check: mutual exclusivity
      (assert (= 1 (logxor (if known-records 1 0) (if db-file 1 0))))

      (when db-file
        (if (file-exists-p db-file)
            (insert-file-contents db-file nil)
          (message "New database file.")))

      (setf database (or (db-read-database-internal-file-layout)
                         (edb--make-v1-monolithic-mess)))
      (edb--meta1D database)
      (edb--1D! database :file (if known-records
                                   (propertize "(inherent)" :file (car source))
                                 db-file))
      (edb--1D! database :modifiable-p (if known-records
                                           t ; hack
                                         (file-writable-p db-file)))

      (when (and (not (bufferp display-template))
                 (or (not format-file) confirm))
        (setq format-file (db-choose-format-file database format-file
                                                 confirm)))

      (setq ddb (db-setup-data-display-buffer
                 (or format-file display-template)
                 database t))
      (let ((ddbufs (edb--1D database :ddbufs)))
        (edb--1D! database :ddbufs (cons ddb ddbufs)))

      (with-current-buffer ddb
        ;; reread data if coding system is different
        (when (and (boundp 'edb-data-coding)
                   (setq coding (symbol-value 'edb-data-coding)))
          (db-message "edb-data-coding: %s" coding)
          (setq buffer-file-coding-system coding)
          (with-current-buffer buf
            (unless (eq buffer-file-coding-system coding)
              (erase-buffer)
              (let ((coding-system-for-read coding))
                (insert-file-contents db-file nil))
              ;; hack! -- fixme: separate internal layout check/read, to
              ;;                 avoid ugliness that is the undo/redo.  --ttn
              (when (edb--1D database :togp)
                (kill-buffer (car (edb--1D database :ddbufs)))
                (setf database (db-read-database-internal-file-layout))
                (edb--1D! database :file db-file)
                (edb--1D! database :modifiable-p (file-writable-p db-file))
                (setq ddb (db-setup-data-display-buffer
                           (or format-file display-template)
                           database t))
                (let ((ddbufs (edb--1D database :ddbufs)))
                  (edb--1D! database :ddbufs (cons ddb ddbufs)))
                (with-current-buffer ddb
                  (setq buffer-file-coding-system coding)))))))

      (with-current-buffer ddb
        (let ((db-buffer buf))
          (run-hooks 'db-before-read-hooks)))

      (if known-records
          (edb--snap! database (length known-records) known-records)
        (db-read-database-file-helper database))

      (with-current-buffer ddb
        (run-hooks 'db-after-read-hooks))

      database)))


(defun db-read-database-internal-file-layout ()
  ;; If the buffer contains a database in internal file layout, read and
  ;; return the header portion (not the records).  Otherwise, return nil.
  (when (db-skip-string-forward ";; Database file written by EDB")
    (let ((here (point))
          (emacs-lisp-mode-hook nil)
          cruft)
      (emacs-lisp-mode)
      ;; Update old formats in small increments.
      (when (db-skip-string-forward "; format 0.1")
        (delete-char -1)
        (insert "2")
        (forward-sexp)
        (backward-char 1)
        ;; add locals slot to database
        (insert " nil")
        (goto-char here))
      (when (db-skip-string-forward "; format 0.2")
        (delete-char -1)
        (insert "3")
        (forward-sexp)
        (backward-char 1)
        (backward-sexp 1)
        (backward-char 1)
        ;; add modified bit to database
        (insert " nil")
        (goto-char here))
      (when (db-skip-string-forward "; format 0.3")
        (delete-char -1)
        (insert "4")
        (down-list 1)
        (forward-sexp 16)
        ;; add "internal file layout" slot to database
        (insert " t")
        (forward-sexp 14)
        ;; add modifiable bit to database
        (insert " t")
        (goto-char here))
      (when (db-skip-string-forward "; format 0.4")
        (delete-char -1)
        (insert "5")
        ;; change "[database" to "[cl-struct-database"
        (down-list 1)
        (insert "cl-struct-")
        (forward-sexp 17)
        ;; change "[sepinfo" to "[cl-struct-sepinfo"
        (down-list 1)
        (insert "cl-struct-")
        (up-list 1)
        (down-list 1)
        (insert "cl-struct-")
        (up-list 1)
        (down-list 1)
        (insert "cl-struct-")
        (goto-char here))
      (cl-flet
          ((skip-zonk (s z) (let ((p (progn (forward-sexp s) (point))))
                              (forward-sexp z)
                              (delete-region p (point)))))
        (when (db-skip-string-forward "; format 0.5")
          (delete-char -1)
          (insert "6")
          ;; Remove five slots for quotation information.
          (down-list 1)
          (skip-zonk 24 5)
          (goto-char here))
        (when (db-skip-string-forward "; format 0.6")
          (delete-char -1)
          (insert "7")
          (down-list 1)
          ;; Remove structure tag.
          (skip-zonk 0 1)
          ;; Remove slots for data container, number of records, filename,
          ;; "file-local" variables, aux filename, buffer list, default format
          ;; filename (but keep as cruft), hide functions, number of fields.
          (skip-zonk 1 6)
          (let ((filename (read (current-buffer))))
            (when filename (push (cons :format-file filename) cruft)))
          (skip-zonk -1 3)
          ;; Remove slot for field name to number alist.
          (skip-zonk 1 1)
          ;; Remove slots for "hidden to end" and "internal file layout" bits.
          (skip-zonk 2 2)
          ;; Remove structure tags.
          (down-list 1)
          (skip-zonk 0 1)
          (backward-up-list 1)
          (forward-sexp 1)
          (down-list 1)
          (skip-zonk 0 1)
          (backward-up-list 1)
          ;; Remove slot for alternatives sepinfo.
          (skip-zonk 1 1)
          ;; Remove slots for mod bits.
          (skip-zonk 5 2)
          ;; Delete parens around record data.
          (re-search-forward "^($")
          (delete-region (1- (match-beginning 0)) (match-end 0))
          (save-excursion
            (re-search-forward "^).*$")
            (delete-region (1- (match-beginning 0)) (match-end 0)))
          ;; Add cruft.
          (insert (format "\n%S\n" cruft))
          (goto-char here)))

      ;; Don't forget to change `db-write-1' when updating the format number.
      (unless (db-skip-string-forward "; format 0.7")
        (db-message "I don't know if I can read the database, but I'll try"))
      (end-of-line)
      (let ((rv (read (current-buffer)))
            (cruft (read (current-buffer))))
        (edb--meta1D rv)
        (edb--1D! rv :togp t)
        (when cruft (edb--1D! rv :1CRUFT cruft))
        ;; Deconstruct so that `database-set-fieldnames-to-list'
        ;; can elaborate (after reconstructing).  Ugly shit!
                  (let* ((names (database-fieldnames rv))
                         (count (length names))
               (types (database-recordfieldspecs rv))
               name type)
          (setf (database-fieldnames rv) nil
                (database-recordfieldspecs rv) nil)
          (database-set-fieldnames-to-list
           rv (mapcar (lambda (fidx)
                        (if (setq name (aref names fidx)
                                  type (aref types fidx))
                            (cons name type)
                          name))
                      (number-sequence 0 (1- count)))))
        ;; TODO: Factor this and same in `db-setup-data-display-buffer'.
        (unless (database-field-priorities rv)
          (setf (database-field-priorities rv)
                (list (mapcar 'list (number-sequence
                                     0 (1- (length (database-fieldnames
                                                    rv))))))))
        rv))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Format file
;;;

(defun db-locate-format-file (filename)
  ;; Return a full pathname for the format file named FILENAME, or err.
  (or (db-locate-readable-file-prefer-cwd
       (file-name-nondirectory filename)
       db-format-file-path)
      (error "I can't find a format file named %s." filename)))

(defun db-choose-format-file (db ff-default confirm)
  ;; Return a format file according to DB or FF-DEFAULT.
  ;; Prompt if CONFIRM is set or if we can't get one from DB alone.
  (let* ((db-file (edb--1D db :file))
         (default (or ff-default
                      (cdr (assq :format-file (edb--1D db :1CRUFT)))
                      (db-locate-readable-file-prefer-cwd
                       (file-name-nondirectory
                        (file-name-sans-extension db-file))
                       (cons (file-name-directory db-file)
                             db-format-file-path)
                       db-format-file-suffixes))))
    (if (and default (not confirm))
        (let* ((dir (file-name-directory db-file))
               (default-directory (if dir
                                      (expand-file-name dir)
                                    default-directory)))
          (db-locate-format-file default))
      ;; Don't need `db-locate-format-file' because MUSTMATCH arg
      ;; to `read-file-name' is t.
      (expand-file-name
       (read-file-name
        (if default
            (format "Display format for %s: [default %s] "
                    (file-name-nondirectory db-file)
                    (file-name-nondirectory default))
          (format "Display format for %s: "
                  (file-name-nondirectory db-file)))
        (file-name-directory db-file)
        (when default
          (expand-file-name default (file-name-directory db-file)))
        t)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Read database file:  helper functions
;;;

(defun db-read-database-file-helper (db)
  ;; Read current buffer into database DB and return DB.
  (db-message "Reading database...")
  ;; The format and the auxiliary file should already be loaded; ie, all
  ;; special variables should be set.
  (if (edb--1D db :togp)
      (let* ((new (list nil))
             (tp new)
             (count 0))
        (while (progn (skip-chars-forward " \n")
                      (< (point) (point-max)))
          (setcdr tp (list (read (current-buffer))))
          (setq count (1+ count)
                tp (cdr tp)))
        (edb--snap! db count (cdr new)))

    (database-io-setup db t)

    ;; Remove pre-first-record- and post-last-record strings or regexps, then
    ;; add a record separator to the end so that every record is terminated by
    ;; one.  Try the {pre-first,post-last}-regexp first.  On match, save the
    ;; text in the associated {pre-first,post-last}-string for output later.
    (let ((rsep (database-record-sepinfo db))
          rgx str p)

      ;; Remove gubbish before first record.
      (goto-char (point-min))
      (when (setq p (cond ((setq rgx (sepinfo-pre-first-regexp rsep))
                           (unless (db-skip-regexp-forward rgx)
                             (error "Unmatched pre-first regexp"))
                           (match-end (sepinfo-pre-first-regexp-submatch rsep)))
                          ((setq str (sepinfo-pre-first-string rsep))
                           (unless (db-skip-string-forward str)
                             (error "Missing pre-first string"))
                           (point))
                          (t nil)))
        (if rgx
            (setf (sepinfo-pre-first-string rsep)
                  (delete-and-extract-region (point-min) p))
          (delete-region (point-min) p)))

      ;; Remove gubbish after last record.
      (goto-char (point-max))
      (when (setq p (cond ((setq rgx (sepinfo-post-last-regexp rsep))
                           (unless (re-search-backward rgx)
                             (error "Unmatched post-last regexp"))
                           (match-beginning
                            (sepinfo-post-last-regexp-submatch rsep)))
                          ((setq str (sepinfo-post-last-string rsep))
                           (unless (search-backward str)
                             (error "Missing post-last string"))
                           (point))
                          (t nil)))
        (if rgx
            (setf (sepinfo-post-last-string rsep)
                  (delete-and-extract-region p (point-max)))
          (delete-region p (point-max))))

      ;; If there isn't one there already, add a final record separator so
      ;; that every record is terminated by one.
      (let ((sep (sepinfo-sep-string rsep)))
        (unless (db-skip-string-backward sep)
          (insert sep))))

    (if (database-read-record-from-region db)
        (db-read-file-custom db)
      (db-read-file-delimited db)))
  (edb--meta1D db)
  (db-message "Reading database...done (%d records)" (edb--1D db :nrecords))
  db)


(defun edb--snap! (db len ls)
  ;; Take a list of records and set DB's `:vov' and `:nrecords'.
  (let ((vov (make-vector (+ len 10) nil)))
    (dotimes (i len)
      (aset vov i (pop ls)))
    (edb--1D! db :vov vov)
    (edb--1D! db :nrecords len)))


(defmacro db-reading-noninternal (condition &rest body)
  (declare (indent 1) (debug (form body)))
  `(let* ((new (list nil))
          (tp new)
          (count 0)                     ; binding used in BODY
          this-record)                  ; value set by BODY
     (while ,condition
       (setq count (1+ count))
       ;; Noisy, obscures warning messages.
       (if (and db-inform-interval
                (zerop (% count db-inform-interval)))
           (message "Reading database...%d" count))
       ,@body                           ; uses binding `database'
       (setcdr tp (list this-record))
       (setq tp (cdr tp)))
     (edb--snap! db count (cdr new))))


;; fixme: integrate and refactor db-read-file-* funcs. --ttn

(defun db-read-file-custom (db)
  (let ((rrfunc (database-read-record-from-region db))
        (nfields (length (database-fieldnames db)))
        (nm2no (edb--1D db :nm2no))
        (rsi (database-record-sepinfo db))
        ;; documented dynamic binding
        (database db))
    ;; Uses `database-read-record-from-region' and `sepinfo-sepfunc' from
    ;; `database-record-sepinfo'.
    ;; Check that the sepinfo has enough information.
    (if (sepinfo-sep-regexp rsi)
        (unless (sepinfo-sep-regexp-submatch rsi)
          (error "Missing submatch for sep-regexp `%s' of record sepinfo."
                 (sepinfo-sep-regexp rsi)))
      (unless (sepinfo-sep-function rsi)
        (let ((sep (sepinfo-sep-string rsi)))
          (when (or (not sep)
                    (equal "" sep))
            (error "Missing specification in record sepinfo."))
          (db-jam-si sep rsi))))

    (if (sepinfo-sep-function rsi)
        ;; Use sep-function to delimit the records

        ;; Return a pair of (end-pos . start-pos).
        (let ((delim (sepinfo-sep-function rsi))
              ;; Read-record-from-region might do insertions or deletions,
              ;; so just remembering the position isn't enough.
              next-start-marker
              (prev-end-pos 'nil)
              (end-start-pair '(nil)))
          (goto-char (point-min))
          (setq next-start-marker (point-marker))

          ;; Assume that there is at least one record in the file.
          ;; fixme: validate first. --ttn
          (db-reading-noninternal (marker-position next-start-marker)

            (setq end-start-pair (funcall delim prev-end-pos))
            (narrow-to-region (marker-position next-start-marker)
                              (car end-start-pair))
            ;; Writers of record->region often forget to do this.
            (goto-char (point-min))
            (setq this-record (let ((v (funcall rrfunc)))
                                (if (vectorp v)
                                    v
                                  (db--mkrec nfields nm2no v))))
            ;; Not `(car end-start-pair)' in case buffer was modified.
            (setq prev-end-pos (point-max))
            (widen)
            ;; In 18.55, `set-marker' can only make markers point to the
            ;; accessible portion of the buffer, so do this after widening.
            ;; (This may have changed by 18.59.)
            ;; Don't use just `(cdr end-start-pair)' because the buffer may
            ;; have been modified.
            (set-marker next-start-marker (and (cdr end-start-pair)
                                               (+ (- (cdr end-start-pair)
                                                     (car end-start-pair))
                                                  prev-end-pos)))))
      ;; Use regexp to delimit the records.
      (let ((rsep-rgx (sepinfo-sep-regexp rsi))
            (rsep-rgx-subm (sepinfo-sep-regexp-submatch
                            rsi))
            (next-start (point-min))
            ;; How many characters to skip before looking for the start of the
            ;; next record.  We can't use a position because rrfunc may insert
            ;; or delete, and we can't use markers because `narrow-to-region'
            ;; squeezes them.
            sep-len)
        ;; The caller also moved point.
        (goto-char (point-min))
        (db-reading-noninternal (< next-start (point-max))
          (goto-char next-start)
          ;; re-search-forward errs if it fails
          (re-search-forward rsep-rgx)
          (setq sep-len (- (match-end rsep-rgx-subm)
                           (match-beginning rsep-rgx-subm)))
          (narrow-to-region next-start (match-beginning rsep-rgx-subm))
          (goto-char (point-min))
          (setq this-record (let ((v (funcall rrfunc)))
                                (if (vectorp v)
                                    v
                                  (db--mkrec nfields nm2no v))))
          (setq next-start (+ (point-max) sep-len))
          (widen))))))



(defun db-read-file-delimited (db)
  ;; If there are any regexps set in the sepinfos at this point, then we
  ;; assume that the programmer specified them explicitly; we assume that if
  ;; any substitutions are requested, then the programmer knows that they
  ;; won't cause any ambiguities.

  (goto-char (point-min))

  ;; We have now cleared away the pre- and post- garbage.
  (if (or (sepinfo-sep-regexp (database-field-sepinfo db))
          (sepinfo-sep-regexp (database-record-sepinfo db)))
      ;; Regexps involved, so do no substitution on the separators, except for
      ;; those that the user has explicitly requested; then read the database.
      (progn
        (database-perform-substitutions db t)
        (db-read-file-delimrx db))

    ;; There are no regexps involved.
    (let* ((fsep-str (database-full-fieldsep-string db))
           (rsep-str (database-full-recordsep-string db))
           conftup)

      ;; Convert the database buffer from "complex" to "simple" form.
      (let* ((fsi (database-field-sepinfo db))
             (pre-str (sepinfo-pre-first-string fsi))
             (pre-rgx (sepinfo-pre-first-regexp fsi)))
        ;; Previously, we only did the record separators;
        ;; here we do the field separators as well.
        (goto-char (point-min))
        (cond (pre-rgx
               (if (db-skip-regexp-forward pre-rgx)
                   (delete-region (point-min) (point))
                 (error "Missing regexp `%s' before first field."
                        pre-rgx)))
              (pre-str
               (if (db-skip-string-forward pre-str)
                   (delete-region (point-min) (point))
                 (error "Didn't find string `%s' leading the first field."
                        pre-str))))

        (goto-char (point-max))
        ;; Don't get rid of post-last-field-regexp or post-last-field-string
        ;; because we look for them at the end of every record.

        ;; Make sure that record-sepinfo-sep-string appears at the end.
        (when (sepinfo-sep-string (database-record-sepinfo db))
          (if (db-skip-string-backward
               (sepinfo-sep-string (database-record-sepinfo db)))
              (goto-char (point-max))
            (insert (sepinfo-sep-string (database-record-sepinfo db)))))
        (when pre-str
          (insert pre-str)))

      ;; We're still inside the big let.

      ;; This pre-read confirmation should be optional.  And it should be
      ;; able to deal with regexps.
      ;; When would this test fail?  Won't fsep-str and
      ;; rsep-str always be non-nil when we get here?
      (when (and fsep-str rsep-str)
        (db-message "Confirming database format...")
        (setq conftup (db-confirm-seps fsep-str rsep-str
                                       (length (database-fieldnames db))
                                       nil))
        (if (or (db-conftup-bad-fsep conftup)
                (db-conftup-bad-rsep conftup))
            (progn
              (db-warning "The database file is malformed!")
              (when (db-conftup-bad-fsep conftup)
                (db-warning "Extra field separator %s found in data."
                            (pp-to-string fsep-str)))
              (when (db-conftup-bad-rsep conftup)
                (db-warning "Extra record separator %s found in data."
                            (pp-to-string rsep-str)))
              ;; show the db warning buffer
              (if (yes-or-no-p "Bad file format; try reading anyway? ")
                  (db-message "Damaged file; expecting circa %d records"
                              (db-conftup-reccount conftup))
                (kill-buffer nil)
                (error "Aborted attempt to read database.")))
          (db-message "Database looks OK from here; expecting %d records"
                      (db-conftup-reccount conftup))))

      ;; mernst sez: This also sets the io-sep variables.
      ;; ttn sez: ???

      ;; Convert field/record separators (perhaps choosing new ones), so
      ;; they won't get damaged by the substitution, and then doing the
      ;; substitution.  It also must set the sub-{field,record}sep slots,
      ;; because later on those fields are slavishly followed.  We can't
      ;; parse or do substitutions if either of the separators is a regexp.
      (if (or (sepinfo-sep-regexp (database-record-sepinfo db))
              (sepinfo-sep-regexp (database-field-sepinfo db)))
          (setf (database-sub-recordsep-string db)
                (or (sepinfo-sep-regexp (database-record-sepinfo db))
                    (database-sub-recordsep-string db)
                    rsep-str)
                (database-sub-fieldsep-string db)
                (or (sepinfo-sep-regexp (database-field-sepinfo db))
                    (database-sub-fieldsep-string db)
                    fsep-str))
        (unless rsep-str
          (error "No record separator specified."))
        (if (database-acceptable-delimiter-p db rsep-str)
            (setf (database-sub-recordsep-string db) rsep-str)
          (db-message "Substituting record delimiter for read...")
          (setf (database-sub-recordsep-string db)
                (database-generate-delimiter db))
          (goto-char (point-min))
          (let ((s (database-sub-recordsep-string db)))
            (while (search-forward rsep-str nil t)
              (replace-match s nil t)))
          (setq fsep-str (replace-regexp-in-string
                          rsep-str (database-sub-recordsep-string db)
                          fsep-str))
          (db-message "Substituting record delimiter for read...done"))
        (if (or (database-acceptable-delimiter-p db fsep-str)
                (database-read-record-from-region db))
            (progn
              (db-message "fsep-str `%s' acceptable because `(or %s %s)'"
                          (string-to-list fsep-str)
                          (database-acceptable-delimiter-p db fsep-str)
                          (database-read-record-from-region db))
              (setf (database-sub-fieldsep-string db) fsep-str))
          (unless fsep-str
            (error "No field separator specified."))
          (db-message "Substituting field delimiter for read...")
          (setf (database-sub-fieldsep-string db)
                (database-generate-delimiter db))
          (db-message "Substituting field delimiter... (`%s' for `%s')"
                      (string-to-list (database-sub-fieldsep-string db))
                      (string-to-list fsep-str))
          (goto-char (point-min))
          (let ((s (database-sub-fieldsep-string db)))
            (while (search-forward fsep-str nil t)
              (replace-match s nil t)))
          (db-message "Substituting field delimiter for read...done"))
        (db-message "sub-fieldsep: %s"
                    (string-to-list (database-sub-fieldsep-string db)))

        (database-perform-substitutions db t))

      (db-read-file-delimstr db))))


(defun db-read-file-delimstr (db)
  ;; When we call this, the database is in the following form:
  ;; Point at start of first field of first record.
  ;; Each field, except last, is ended by actual-fieldsep.
  ;; Each record, including last, is ended by actual-recordsep.
  ;; End of last recordsep-string = eob.
  ;; Field-sep and record-sep must be strings.
  (db-message "Reading database...")

  (goto-char (point-min))
  (let* ((fsep (database-sub-fieldsep-string db))
         (rsep (database-sub-recordsep-string db))
         (flen (length fsep))
         (rlen (length rsep))
         (nfields (length (database-fieldnames db)))
         (max-fno (1- nfields))
         (default-slice (db-rs-slice db 'edb--1rs-default-value))
         (here (point))
         fno
         end-of-rsep
         end-of-record)

    (db-reading-noninternal (not (or (eobp)
                                     ;; Does this cause any problems?
                                     ;; Special case for rsep = "\n\n",
                                     ;; extra newline at end
                                     (and (string= rsep "\n\n")
                                          (looking-at "\n\\'"))))
      (setq this-record (make-vector nfields nil))
      (if (search-forward rsep nil t)
          (setq end-of-rsep (point)
                end-of-record (- (point) rlen))
        (db-warning "Didn't find %s at end of last field of record %d, %s!"
                    (pp-to-string rsep) count "and I put it there myself")
        (setq end-of-rsep (point-max)
              end-of-record (point-max)))
      (goto-char here)

      (setq fno 0)
      (while (< fno max-fno)
        ;; fixme: trap errors, maybe check for rsep in field data.  --ttn
        (if (search-forward fsep end-of-record t)
            (progn
              (aset this-record fno (buffer-substring here (- (point) flen)))
              (setq here (point))
              (incf fno))

          (db-warning "%s %d fields of record %d (didn't find fsep %s)"
                      "Hit the end of the record after"
                      fno count (pp-to-string fsep))
          (aset this-record fno (buffer-substring here end-of-record))
          (setq here end-of-record)
          (incf fno)
          (while (<= fno max-fno)
            (aset this-record fno (aref default-slice fno))
            (incf fno))))
      ;; Weren't too few fields, so set the last one (else it's already set).
      (when (= fno max-fno)
        (when (search-forward fsep end-of-record t)
          (db-warning "Extra fields in record %d %s."
                      count "packed into the last field; beware when writing")
          (edb--G! :io-error-p t))
        (aset this-record max-fno (buffer-substring here end-of-record)))
      (goto-char end-of-rsep)
      (setq here (point)))

    ;; Convert from stored to actual format.
    (database-stored->actual db)

    ;; Function is called for side-effect, but return the database anyway.
    db))


(defun db-read-file-delimrx (db)

  (db-message "Reading database...")

  (goto-char (point-min))

  (let* ((fsi (database-field-sepinfo db))
         (rsi (database-record-sepinfo db))
         (frx (or (sepinfo-sep-regexp fsi)
                  (regexp-quote (sepinfo-sep-string
                                 (database-field-sepinfo db)))))
         (frx-subm (or (sepinfo-sep-regexp-submatch fsi)
                       0))
         (rrx (or (sepinfo-sep-regexp rsi)
                  (regexp-quote (sepinfo-sep-string rsi))))
         (rrx-subm (or (sepinfo-sep-regexp-submatch rsi)
                       0))

         ;; Do not fold into `rrx'; they may have submatches of their own.
         (pre-str (sepinfo-pre-first-string fsi))
         (pre-frx (or (sepinfo-pre-first-regexp fsi)
                      (and pre-str
                           (regexp-quote pre-str))))
         (pre-frx-subm (or (sepinfo-pre-first-regexp-submatch
                            fsi)
                           0))
         (post-str (sepinfo-post-last-string fsi))
         (post-frx (or (sepinfo-post-last-regexp fsi)
                       (and post-str
                            (regexp-quote post-str))))
         (post-frx-subm (or (sepinfo-post-last-regexp-submatch
                             fsi)
                            0))

         (nfields (length (database-fieldnames db)))
         (max-fno (1- nfields))
         (here (point))
         fno
         end-of-rrx
         end-of-record)

    (db-reading-noninternal (not (eobp))
      (setq this-record (make-vector nfields nil))
      (when pre-frx
        (if (db-skip-regexp-forward pre-frx)
            (progn
              (setq here (match-end pre-frx-subm))
              (goto-char here))
          (error "Didn't find pre-first stuff I expected.")))

      (setq fno 0)
      (while (< fno max-fno)
        ;; fixme: trap errors, maybe check for rrx in field data.  --ttn
        (if (re-search-forward frx nil t)
            (progn
              (aset this-record fno
                    (buffer-substring here (match-beginning frx-subm)))
              (setq here (match-end frx-subm))
              (incf fno))
          (db-warning "%s %d fields of record %d (didn't find frx `%s')"
                      "End of data after"
                      fno count frx)
          (setq fno max-fno)))
      (if (re-search-forward rrx nil t)
          (setq end-of-rrx (match-end rrx-subm)
                end-of-record (match-beginning rrx-subm))
        (db-warning "Didn't find %s at end of last field of record %d."
                    (pp-to-string rrx) count)
        (setq end-of-rrx (point-max)
              end-of-record (point-max)))
      (goto-char here)
      (when (re-search-forward frx end-of-record t)
        (db-warning "Too many fields in record %d; %s; beware when writing."
                    count "packing them all into the last field")
        (edb--G! :io-error-p t))
      (aset this-record max-fno (buffer-substring here end-of-record))
      (goto-char end-of-rrx)
      (setq here (point)))

    (database-stored->actual db)

    db))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Read database utilities
;;;


(defun database-stored->actual (&optional db)
  ;; Convert string values in a newly-read database to the actual format.  If
  ;; DB is not specified, use the value of the dynamic variable `database'.
  ;; This makes it possible to be put directly in `db-after-read-hook'.
  (unless db (setq db database))
  (let* ((no-of-fields (length (database-fieldnames db)))
         (convert (make-vector no-of-fields nil))
         (s->a-slice (db-rs-slice db 'edb--1rs-stored->actual))
         s->a todo val)
    (dotimes (fno no-of-fields)
      (when (setq s->a (aref s->a-slice fno))
        (push fno todo)
        (aset convert fno s->a)))
    (when todo
      (db-message "Converting from stored record format...")
      (db-maprecords (lambda (record)
                       (dolist (fno todo)
                         (setq val (aref record fno))
                         (aset record fno (if (stringp val)
                                              (funcall (aref convert fno) val)
                                            val))))
                     db nil "Converting record format...%s")
      (db-message "Converting record format...done"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Write database file
;;;


(defun db-write-1 ()
  (edb--G! :io-error-p nil)
  ;; This is called by `db-write-database-file' with the data display buffer
  ;; current, and we need the values of variables local to that buffer.
  (let ((orig-buf (current-buffer))
        (ofile (edb--1D dbc-database :file))
        (inherent (edb--1D dbc-database :inherent))
        (db dbc-database)
        (coding buffer-file-coding-system))
    (when (file-directory-p ofile)
      (edb--G! :io-error-p t)
      (error "Invalid filename: %s" ofile))
    (unless (file-writable-p ofile)
      (if (and (not noninteractive)
               (y-or-n-p (concat "File not writable: " ofile
                                 "\nMake it writable and continue? ")))
          (set-file-modes ofile (logior #o200 (file-modes ofile)))
        (edb--G! :io-error-p t)
        (error "File not writable: %s" ofile)))
    (with-temp-buffer
      (cond
       ;; Data inherent data to the connection.
       (inherent
        (cl-flet*
            ((iget (k) (gethash k inherent))
             (bget (k) (car (iget k)))
             (bput (k v) (setcar (iget k) v)))
          (let* ((all (db-maprecords 'identity db nil nil t))
                 (zonkp nil)
                 (cbuf (cond ((eq 'buffer (iget :control-type))
                              (get-buffer (iget :control-name)))
                             ((find-buffer-visiting (iget :control-fname)))
                             (t (setq zonkp t)
                                (find-file (iget :control-fname))))))
            (unless cbuf
              (error "Control no longer exists for %S"
                     (buffer-name orig-buf)))
            (with-current-buffer cbuf
              (save-excursion
                (funcall (edb--rget '(:edb1 :seq-funcs) (iget :seqw))
                         (bget :start-box)
                         (bget :finish-box)
                         all)
                (bput :finish-box (point)))
              (if (not (setq ofile (iget :control-fname)))
                  (db-message "Buffer %S updated (but not written to a file)"
                              (iget :control-name))
                (unless (file-writable-p ofile)
                  (error "File not writable: %s" ofile))
                (when (file-directory-p ofile)
                  (error "Invalid filename: %s" ofile))
                (write-file ofile)
                (when zonkp
                  (kill-buffer nil)))
              (database-set-modified-p db nil)))))
       ;; "Internal" representation -- sigh.
       ((edb--1D db :togp)
        (let ((standard-output (current-buffer)))
          (setq buffer-file-coding-system coding)
          (insert ";; Database file written by EDB; format 0.7")
          (let ((coding-string (symbol-name coding)))
            (unless (string-match "^undecided-" coding-string)
              (insert " -*- coding: " coding-string "; -*-")))
          (insert "\n")
          (let ((copy (edb--copy-v1-monolithic-mess db))
                (nfields (length (database-fieldnames db)))
                (prios (database-field-priorities db)))
            (setf (database-field-priorities copy)
                  (if (equal (list (mapcar 'list (number-sequence
                                                  0 (1- nfields))))
                             prios)
                      nil
                    prios))
            (pp copy))
          (let ((cruft (edb--1D db :1CRUFT)))   ; blech
            (if cruft                           ; even more blech
                (pp cruft)
              (insert "nil\n")))
          (db-maprecords 'pp db)
          ;; This is not catching the error raised by basic-save-buffer if
          ;; the destination is not writable.
          (condition-case error
              (let ((require-final-newline nil)
                    ;; get around write-file
                    (auto-save-default nil))
                (write-file ofile)
                (database-set-modified-p db nil))
            ;; Used to be `file-error', which didn't catch the error raised
            ;; by `basic-save-buffer' if the destination is not writable.
            (error
             (edb--G! :io-error-p t)
             ;; This must come before the buffer is killed.
             (db-warning "Error `%s' while writing buffer %s to file %s."
                         error (buffer-name orig-buf) ofile)))))
       ;; Don't use internal representation.
       (t
        (db-copy-buffer-local-variables orig-buf
                                        ;; We don't want to be *exactly*
                                        ;; like the data display buffer.
                                        'major-mode
                                        'buffer-read-only)
        (database-io-setup db)
        (let ((first-record t)
              (rsep (database-record-sepinfo db)))
          (when (sepinfo-pre-first-string rsep)
            (insert (sepinfo-pre-first-string rsep)))
          (if (database-write-region-from-record db)
              ;; Don't check separators for write-record-function, even for
              ;; recordsep; read-record-function's cleverness is unknown albeit
              ;; not unknowable.  Also, don't do substitution or quoting.
              (let ((record-sep-string (sepinfo-sep-string rsep))
                    (write-region-fn (database-write-region-from-record db))
                    ;; documented dynamic binding
                    (database db))
                (db-message "Writing database...")
                (db-maprecords (lambda (record)
                                 (if first-record
                                     (setq first-record nil)
                                   (insert record-sep-string))
                                 (funcall write-region-fn record))
                               db nil "Writing database...%d")
                (db-message "Writing database to disk..."))
            (db-write-intdelim db))
          (when (sepinfo-post-last-string rsep)
            (insert (sepinfo-post-last-string rsep)))

          (condition-case error
              (let ((require-final-newline nil)
                    ;; get around `write-file'
                    (auto-save-default nil))
                (write-file ofile)
                (database-set-modified-p db nil))
            ;; Used to be `file-error', which didn't catch the error raised
            ;; by `basic-save-buffer' if the destination is not writable.
            (error
             (edb--G! :io-error-p t)
             ;; This must come before the buffer is killed.
             (db-warning "Error `%s' while writing buffer %s to file %s."
                         error (buffer-name orig-buf) ofile))))))))
  ;; Update.
  (when (or (db-data-display-buffer-p)
            (db-summary-buffer-p))
    (force-mode-line-update)))


(defun db-write-intdelim (db)
  ;; Insert the records, fields separated by fieldsep and records separated by
  ;; recordsep, into the current buffer; it uses delimited format.  This is
  ;; called by `db-write-1', which arranges `unwind-protect' boundaries, calls
  ;; the -internal function that uses the proper output file layout, etc.
  (let* ((previous-point (point))
         (first-record t)
         (no-of-fields (length (database-fieldnames db)))
         (fsep (database-full-fieldsep-string db))
         (rsep (database-full-recordsep-string db))
         (sub-fsep (or (database-sub-fieldsep-string db) fsep))
         (sub-rsep (or (database-sub-recordsep-string db) rsep))
         (a->s-slice (db-rs-slice db 'edb--1rs-actual->stored))
         conftup)

    ;; Check the delimiters.
    (unless (database-acceptable-delimiter-p db sub-fsep)
      (setq sub-fsep (database-generate-delimiter nil)))
    (unless (database-acceptable-delimiter-p db sub-rsep)
      (setq sub-rsep (database-generate-delimiter nil)))

    (db-message "Writing database...")
    (db-maprecords
     (lambda (record)
       (dotimes (fno no-of-fields)
         (when (> fno 0) (insert sub-fsep))
         ;; This is `record-field-stored', inlined.
         (insert (let ((a->s (aref a->s-slice fno))
                       (val (aref record fno)))
                   (if a->s
                       (funcall a->s val)
                     val))))
       (insert sub-rsep))
     db nil "Writing database...%d")
    (db-message "Writing database...confirming")

    (narrow-to-region previous-point (point-max))
    (setq conftup (db-confirm-seps sub-fsep sub-rsep
                                   (length (database-fieldnames db))
                                   (edb--1D db :nrecords)))
    (if (or (db-conftup-bad-fsep conftup)
            (db-conftup-bad-rsep conftup))
        (progn
          (when (db-conftup-bad-fsep conftup)
            (db-warning "Unexpected field separator %s in data; %s."
                        (pp-to-string sub-fsep) "trying again")
            (setf (database-sub-fieldsep-string db)
                  (database-generate-delimiter db t)))
          (when (db-conftup-bad-rsep conftup)
            (db-warning "Unexpected record separator %s in data; %s."
                        (pp-to-string sub-rsep) "trying again")
            (setf (database-sub-recordsep-string db)
                  (database-generate-delimiter db t)))

          ;; We've chosen new separators; erase the work so far.
          (delete-region previous-point (point))

          ;; Call this function recursively.
          (db-write-intdelim db)
          ;; I've called this recursively; don't do any substitution
          ;; or quoting.
          )
      ;; Confirmation was OK:  correct number of field and record
      ;; separators found.

      ;; Put off adding the pre- and post- field strings until
      ;; after checking separators, as they may contain anomolous
      ;; field separators, for instance.

      ;; But do it before substitution so that all field pre- and
      ;; post- strings are treated identically.

      ;; The whole point of using io-separators is so they
      ;; appear exactly as the user specified, unaffected by
      ;; substitution.

      ;; But note that pre- and post- record strings will be added
      ;; later, after substitution.

      ;; Do the substitution, then, if field separators were changed in order
      ;; to prevent them from getting damaged by the substitution, convert
      ;; them back to the user-specified strings, which might contain
      ;; substrings that would have been substituted for in the previous
      ;; operation, had we not been careful.

      ;; Check that there are the correct number of fieldseps and recordseps
      ;; here; if wrong number, choose new fieldsep and/or recordsep and take
      ;; it from the top.

      (database-perform-substitutions db nil)

      (unless (equal sub-fsep fsep)
        (goto-char (point-min))
        (while (search-forward sub-fsep nil t)
          (replace-match fsep nil t)))
      (unless (equal sub-rsep rsep)
        (goto-char (point-min))
        (while (search-forward sub-rsep nil t)
          (replace-match rsep nil t)))

      ;; Now the buffer is ready to have the preceding and trailing junk
      ;; added and to be written to disk.

      ;; Convert from "simple" to "complex" form.
      (goto-char (point-min))
      (let ((pre-first (sepinfo-pre-first-string (database-field-sepinfo db))))
        (when pre-first
          (insert pre-first)))

      (goto-char (point-max))
      (if (and (db-skip-string-backward (or (sepinfo-pre-first-string
                                             (database-field-sepinfo db))
                                            ""))
               (db-skip-string-backward (or (sepinfo-sep-string
                                             (database-record-sepinfo db))
                                            "")))
          (delete-region (point) (point-max))
        (error "Didn't find expected trailing junk `%s' or `%s'."
               (sepinfo-pre-first-string (database-field-sepinfo db))
               (sepinfo-sep-string (database-record-sepinfo db)))))
    (widen)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; I/O utilities
;;;

(defun database-io-setup (db &optional norx)
  (cl-flet*
      ((bad (part s name)
            (error "Need a submatch to go with %s-regexp `%s' of %s."
                   part s name))
       (chk (si name sdef)
            ;; It's good that this setting of -regexp slots doesn't happen
            ;; until the last possible moment, when quotation-char and other
            ;; variables that these values depend on are already set to
            ;; their final values.

            ;; If the string is the empty string, then we must have just set
            ;; it that way, which means that we either also just set the
            ;; regexp to nil, or we set the regexp to something we care
            ;; about.  In either case don't mess further with the regexp.

            ;; The submatches could default to 0 if they're nil; but I want
            ;; to be more paranoid than that.
            (when (equal "" (sepinfo-pre-first-string si))
              (setf (sepinfo-pre-first-string si) nil))
            (cond ((sepinfo-pre-first-regexp si)
                   (unless (sepinfo-pre-first-regexp-submatch si)
                     (bad 'pre-first (sepinfo-pre-first-regexp si) name)))
                  ((and (sepinfo-pre-first-string si) (not norx))
                   (db-jam-si pre-first si)))

            ;; Don't test for sep-function because the sepinfo must be valid
            ;; for output as well as input.  On the other hand, we don't
            ;; need sep-string to be set if a wrfr function is in use, but
            ;; this function doesn't do any such checks.
            (when (or (not (sepinfo-sep-string si))
                      (and (equal "" (sepinfo-sep-string si))
                           (not (sepinfo-sep-regexp si))))
              (setf (sepinfo-sep-string si) sdef))
            (cond ((sepinfo-sep-regexp si)
                   (unless (sepinfo-sep-regexp-submatch si)
                     (bad 'sep (sepinfo-sep-regexp si) name)))
                  ((and (not (sepinfo-sep-function si)) (not norx))
                   (db-jam-si sep si)))

            (when (equal "" (sepinfo-post-last-string si))
              (setf (sepinfo-post-last-string si) nil))
            (cond ((sepinfo-post-last-regexp si)
                   (unless (sepinfo-post-last-regexp-submatch si)
                     (bad 'post-last (sepinfo-post-last-regexp si) name)))
                  ((and (sepinfo-post-last-string si) (not norx))
                   (db-jam-si post-last si)))))

    ;; Some of this information may already be correctly set (especially if
    ;; we're now writing), but just in case some of the database slots have
    ;; changed since reading.

    ;; When converting strings to regexps, must be careful to watch out for
    ;; substitution and quotation; don't get fooled.

    ;; When writing to the file, we take the previous version's local
    ;; variables section verbatim.

    ;; We're only setting the regexp variables.

    ;; When reading, we'll get the variables from the database, auxiliary,
    ;; and format files anew each time anyway.

    (chk (database-record-sepinfo db) "record" "\n")
    (chk (database-field-sepinfo db) "field" "\t")

    (edb--1D! db :substitution-no-no
              (apply 'concat
                     (mapcar (lambda (pair)
                               (concat (car pair) (cdr pair)))
                             (database-substitutions db))))))

(defmacro db-jam-si (afrag si)
  ;; AFRAG is a sepinfo accessor func name fragment (symbol).
  ;; SI is a variable (symbol) bound to a sepinfo.
  (let ((rx-acc (intern (format "sepinfo-%s-regexp" afrag)))
        (subrx-acc (intern (format "sepinfo-%s-regexp-submatch" afrag))))
    `(let ((str (,(intern (format "sepinfo-%s-string" afrag)) ,si)))
       (cond ((or (null str) (equal "" str))
              (setf (,rx-acc ,si) nil)
              (setf (,subrx-acc ,si) nil))
             (t
              (setf (,rx-acc ,si) (regexp-quote str))
              (setf (,subrx-acc ,si) 0))))))

(defun db-rfspec<-rftype (type)
  "Return the recordfieldspec associated with symbol TYPE."
  (let ((rv (gethash type (edb--G :1recordfieldtypes))))
    (when rv (if (symbolp rv)
                 (db-rfspec<-rftype rv)
               rv))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Substitution
;;;

;; When substitution is called, the rendered database is always in "simple"
;; format: rendered fields are separated by sub-fieldsep, and rendered records
;; (including the last one) are followed by sub-recordsep.

;; The "complicated" format includes pre-first-field at the beginning and
;; post-last-field (without record-sep-string or pre-first-field, which are
;; the other elements of sub-recordsep) at the end.

;; Simple format is nice because it requires no special-casing at the
;; beginning (because there's no gubbish there) or at the end (because
;; what's there is exactly what's between each pair of records).

(defun database-perform-substitutions (db backward)
  (when (database-substitutions db)
    (db-message "Substituting...")
    ;; Make replacements in the current buffer according to SUBS.
    ;; SUBS is list of pairs of strings; the cdr of each pair will be
    ;; substituted for the car, in order, unless optional argument BACKWARD is
    ;; non-nil, in which case the car is substituted for the cdr and the
    ;; substitutions are done in reverse order.
    ;;
    ;; Warn if any of the
    ;; substituted-in strings already appears in the buffer; such a
    ;; situation would make substitution, then unsubstitution, not yield
    ;; a result identical to the original buffer, since all instances of
    ;; the substituted-in string will be assumed on the reverse
    ;; substitution to have been the result of replacing a
    ;; substituted-for string.
    ;;
    ;; fixme: do all checking before any substitutions are done. --ttn
    (let ((subs (database-substitutions db))
          bef aft ambiguity ambiguities)
      (dolist (sub (if backward
                       (mapcar (lambda (pair)
                             (cons (cdr pair) (car pair)))
                           (reverse subs))
                     subs))
        (setq bef (car sub)
              aft (cdr sub))
        (goto-char (point-min))

        (when (search-forward aft nil t)
          (setq ambiguity sub)
          (goto-char (point-min)))

        (while (search-forward bef nil t)
          (replace-match aft nil t))

        ;; Don't complain if we didn't actually do any substitution.
        (when ambiguity
          (unless (= (point) (point-min))
            (push ambiguity ambiguities))
          (setq ambiguity nil)))

      (when ambiguities
        (error "Ambiguities: %s" ambiguities)))
    (db-message "Substituting...done")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Substitution Utilities
;;;

;; A "confirmation tuple" -- aka "conftup" -- has the form:
;;   [FIELDSEP-BAD-P RECORDSEP-BAD-P NO-OF-RECORDS]

(defsubst db-conftup-bad-fsep (conftup) (aref conftup 0))
(defsubst db-conftup-bad-rsep (conftup) (aref conftup 1))
(defsubst db-conftup-reccount (conftup) (aref conftup 2))

(defun db-confirm-seps (fsep rsep nfields nrecords)
  ;; Return a conftup.
  ;; Assume that recordsep appears at the end of the database as well.

  ;; here "-m" means "matches"
  (let* ((fsep-m (progn (goto-char (point-min))
                        (db-how-many-string-overlapping fsep)))
         (rsep-m (progn (goto-char (point-min))
                        (db-how-many-string-overlapping rsep)))
         (goal-fsep-m (and nrecords (* nrecords (1- nfields))))
         (goal-rsep-m nrecords))
    (if (equal fsep rsep)
        (progn
          (if nrecords
              (if (= rsep-m (+ goal-rsep-m goal-fsep-m))
                  (vector nil nil nrecords)
                (vector t t nil))
            ;; Remember that fsep = rsep when reading this code.
            (if (zerop (% rsep-m nfields))
                (vector nil nil (/ rsep-m nfields))
              (if (zerop (% (1+ rsep-m) nfields))
                  ;; Field separator at the end of data was misinterpreted
                  ;; as a record separator, so no record separator was added.
                  (progn (goto-char (point-max))
                         (insert rsep)
                         (vector nil nil (/ (1+ rsep-m) nfields)))
                (vector t t (/ rsep-m nfields))))))

      ;; fsep and rsep unequal; see if one is a substring of the other.

      ;; At least one of these must be zero.
      (let ((f-in-r (db-how-many-substring-overlapping fsep rsep))
            (r-in-f (db-how-many-substring-overlapping rsep fsep)))
        (if nrecords
            (progn
              ;; At most one of these is nonzero, so cond is OK.
              (cond ((> f-in-r 0)
                     (incf goal-fsep-m (* goal-rsep-m f-in-r)))
                    ((> r-in-f 0)
                     (incf goal-rsep-m (* goal-fsep-m r-in-f))))
              (vector (not (= fsep-m goal-fsep-m))
                      (not (= rsep-m goal-rsep-m))
                      nrecords))
          (incf nfields f-in-r)
          (let ((apparent-records (if (= 1 nfields)
                                      rsep-m
                                    (/ fsep-m (1- nfields)))))
            (setq goal-rsep-m
                  (if (zerop apparent-records)
                      1
                    (* apparent-records
                       (1+ (* r-in-f fsep-m)))))
            (vector (or
                     ;; Wrong field count; some record has too many or few.
                     (not (= fsep-m (* apparent-records (1- nfields))))
                     ;; too many fseps compared to rseps
                     (< rsep-m goal-rsep-m))
                    ;; too many rseps compared to fseps
                    (< goal-rsep-m rsep-m)
                    apparent-records)))))))


(defun database-acceptable-delimiter-p (db delimiter)
  ;; Return t iff no characters of DELIMITER appear in `:substitution-no-no'.
  (when delimiter
    (let ((result t)
          (idx 0)
          (len (length delimiter))
          (no-no (edb--1D db :substitution-no-no)))
      (while (and result (< idx len))
        (if (db-find-char (elt delimiter idx) no-no)
            (setq result nil)
          (incf idx)))
      result)))

(defun database-generate-delimiter (db &optional checkp)
  (let ((fsep (or (database-sub-fieldsep-string db)
                  (database-full-fieldsep-string db)))
        (rsep (or (database-sub-recordsep-string db)
                  (database-full-recordsep-string db)))
        (string (make-string 1 0))
        (c 0)
        rv)
    (while (and (< c 256) (not rv))
      (aset string 0 c)
      (if (and (database-acceptable-delimiter-p db string)
               (not (and checkp
                         (progn
                           (goto-char (point-min))
                           (search-forward string nil t))))
               (not (db-find-char c fsep))
               (not (db-find-char c rsep)))
          (setq rv string)
        (incf c)))
    (or rv (error "I can't find an acceptable delimiter!"))))

;;; db-file-io.el ends here
