;;; db-rep.el --- part of EDB, the Emacs database

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

;; Representation and basic operations for
;; database, recordfieldspec objects.

;;; Code:

(:edb1 :1singles (make-hash-table :test 'eq :weakness 'key))

(edb--define-child-hash 1D db (edb--G :1singles)
                        (let ((new (make-hash-table :size 7)))
                          (flet ((ahash () (make-hash-table
                                            :test 'eq
                                            :weakness 'key)))
                            (edb--mputhash new
                                           :markedp (ahash)
                                           :hiddenp (ahash))
                            new)))

(defun edb--1all-known-databases ()
  (edb--hashcollect :keys (edb--G :1singles)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Database abstraction
;;;

(put 'edb :databases-made 0)

(defstruct (edb--v1-monolithic-mess
            (:type vector)
            (:constructor edb--make-v1-monolithic-mess)
            (:conc-name database-)
            (:copier edb--copy-v1-monolithic-mess))
  (print-name (format "Unnamed Database %d" (incf (get 'edb :databases-made))))

  ;; field information
  fieldnames                            ; this is repeated in the recordpsecs
  recordfieldspecs                      ; vector: symbols or recordfieldspecs
                                        ; if symbol, search :1recordfieldtypes

  field-priorities                      ; maybe call this order-fields instead

  ;; For file i/o
  (record-sepinfo (edb--make-v1-sepinfo) :read-only t)
  (field-sepinfo (edb--make-v1-sepinfo) :read-only t)
  read-record-from-region
  write-region-from-record
  sub-fieldsep-string
  sub-recordsep-string

  substitutions                         ; associate ACTUAL with STORED strings

  locals                                ; associate SYMBOL with VALUE
  )

;;; Accessors

(defun database-set-modified-p (db val)
  (unless (eq val (edb--1D db :modp))
    (edb--1D! db :modp val)
    ;; Reflect the change in each data display buffer.
    (mapc (lambda (ddb)
            (with-current-buffer ddb
              (set-buffer-modified-p val)
              (force-mode-line-update)))
          (edb--1D db :ddbufs))))

(defun database-file (db)
  ;; EDB 1.x compatability; will NOT be available in EDB 2.x.
  (edb--1D db :file))

(defun database-data-display-buffers (db)
  ;; EDB 1.x compatability; will NOT be available in EDB 2.x.
  (edb--1D db :ddbufs))

(defun database-no-of-records (db)
  ;; EDB 1.x compatability; will NOT be available in EDB 2.x.
  (edb--1D db :nrecords))

;;; Non-primitive accessors

(defun db-rs-slice (db which)
  (let ((rs-slices (edb--1D db :rs-slices)))
    (or (gethash which rs-slices)
        (puthash which (let* ((all-rs (edb--1D db :elaborated-rfspecs))
                              (len (length all-rs))
                              (v (make-vector len nil)))
                         (do ((i 0 (1+ i)))
                             ((= len i))
                           (aset v i (funcall which (aref all-rs i))))
                         v)
                 rs-slices))))

;;; Database-local variables

(defun database-make-local (symbol db &optional value)
  "Declare a database-local variable named by SYMBOL for DATABASE.
Each such variable should only be declared once.
If optional argument VALUE is specified, the variable is set to it."
  (let ((lookup (assq symbol (database-locals db))))
    (if lookup
        (error "%s is already defined as a local variable in %s."
               symbol (database-print-name db))
      (push (cons symbol value) (database-locals db)))))

(defun database-set-local (symbol db value &optional no-error)
  "Set the value of database-local variable SYMBOL, in DB, to VALUE.
SYMBOL must have been declared by a previous call to `database-make-local'
unless optional argument NO-ERROR is supplied, in which case the function
does that automatically, if necessary."
  (let ((lookup (assq symbol (database-locals db))))
    (if lookup
        (setcdr lookup value)
      (if no-error
          (database-make-local symbol db value)
        (error "%s is not a database-local variable for %s."
               symbol (database-print-name db))))))

(defun database-get-local (symbol db &optional no-error)
  "Return the value of database-local variable SYMBOL for DATABASE.
If SYMBOL was not declared by a previous call to `database-make-local',
an error is signalled unless optional argument NO-ERROR is non-nil,
in which case nil is returned."
  (let ((lookup (assq symbol (database-locals db))))
    (cond (lookup
           (cdr lookup))
          (no-error
           nil)
          (t
           (error "%s is not a database-local variable for %s."
                  symbol (database-print-name db))))))

(defun database-local-p (symbol db)
  "Return non-nil if SYMBOL is a database-local variable for DATABASE."
  (assq symbol (database-locals db)))

;;; Non-primitive setters

(eval-when-compile (defvar db-default-field-type))

(defun database-set-fieldnames-to-list (db fspecs &optional dtype)
  "Set DB's fieldnames and record field types according to FSPECS.
But do nothing if DB's `fieldnames' slot is already set.

FSPECS is a list, each element of which can either be a single symbol,
the field name, or a cons (NAME . RECORDFIELDTPYE).  Optional third arg
DTYPE specifies the default recordfieldtype for single-symbol elements.
If DTYPE is not given, use the value of `db-default-field-type' (NOTE:
this variable is deprecated) if bound, or `string' otherwise."
  (unless (database-fieldnames db)
    (let* ((len (length fspecs))
           (fno 0)
           (rs  (make-vector len nil))
           (vec (make-vector len nil))
           (n2n (make-hash-table :size len :test 'eq))
           ;; We used to use a buffer-local var `db-default-field-type', but
           ;; that was a bug; a field's type is a property of the field, not
           ;; of its display, not of a buffer.  Generally, the concepts of
           ;; "local" and "default" together are difficult to explain w/o
           ;; detailed knowledge of the implementation and its precise timing
           ;; of events.  Better to specify the default together with the
           ;; non-defaults, such as is now possible w/ DTYPE, and let this
           ;; function worry about making the correct associations.
           ;;
           ;; For EDB 1.x, although we are obligated to check for use of this
           ;; var, we are free to drop the buffer-local var, as long as we
           ;; explain ourselves clearly in the documentation.
           (default-type (cond (dtype)
                               ((and (boundp 'db-default-field-type)
                                     db-default-field-type))
                               (t 'string))))
      (dolist (fname fspecs)
        (let ((type (if (consp fname)
                        (prog1 (cdr fname)
                          (setq fname (car fname)))
                      default-type)))
          (if (gethash type (edb--G :1recordfieldtypes))
              (aset rs fno type)
            (error "bad type %s" type))
          (aset vec fno fname)
          (puthash fname fno n2n)
          (incf fno)))
      (edb--1D! db :nm2no n2n)
      ;; elaborate
      (let ((elab (copy-sequence rs))
            one-rs)
        (do ((fno 0 (1+ fno)))
            ((= len fno))
          (when (symbolp (setq one-rs (aref elab fno)))
            (aset elab fno (db-rfspec<-rftype one-rs))))
        (edb--1D! db :elaborated-rfspecs elab))
      (edb--1D! db :rs-slices (make-hash-table :size 5 :test 'eq))
      (setf (database-fieldnames db) vec
            (database-recordfieldspecs db) rs))))

;;; Not quite so basic functions.

(defun database-full-fieldsep-string (db)
  ;; Return the string that really separates database record fields.
  (if (database-write-region-from-record db)
      nil
    (sepinfo-sep-string (database-field-sepinfo db))))

(defun database-full-recordsep-string (db)
  ;; Return the string that really separates database records.
  (let ((rsep (database-record-sepinfo db)))
    (if (database-write-region-from-record db)
        (sepinfo-sep-string rsep)
      (let ((fsep (database-field-sepinfo db)))
        (concat (sepinfo-post-last-string fsep)
                (sepinfo-sep-string rsep)
                (sepinfo-pre-first-string fsep))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Recordfieldspec abstraction
;;;

(defstruct (edb--v1-rs
            (:type vector) :named
            (:constructor edb--make-v1-rs)
            (:conc-name edb--1rs-)
            (:copier edb--copy-v1-rs))

  ;; datatype information
  type                                  ; e.g. 'string
  default-value
  common-form-function
  merge-function

  order-fn                              ; (A B) => {-1,0,1}
  sort-fn                               ; (A B) => bool
  match-function                        ; (PATTERN OBJ) => bool

  help-info                             ; perhaps should be help-string;
                                        ; or should be more complicated.

  actual->stored
  stored->actual

  ;; customizations
  constraint-function)

(put 'edb--v1-rs 'kwidx                 ; poor man's defsetf --ttn
     (let ((idx 0))
       (mapcar (lambda (ent)
                 (cons (intern (format ":%s" (symbol-name (car ent))))
                       (incf idx)))
               (cdr (get 'edb--v1-rs 'cl-struct-slots)))))


(defun db-rs-sortfunc (rs &optional reversep)
  "Return a sort function for records described by recordfieldspec RS.
If optional argument REVERSEP is non-nil, then the sort function goes in
the opposite order.
If the sort-fn slot of the appropriate recordfieldspec of `database' doesn't
contain one, one is made up on the fly from the order-fn slot.
If the order-fn slot is also empty, the resulting function always returns
nil, indicating that it is not the case that the first argument is less
than the second."
  (let ((sort-fn (edb--1rs-sort-fn rs)))
    (if sort-fn
        (if reversep
            `(lambda (value1 value2)
               ;; Maintain arg order and invert the result, so as
               ;; to properly handle the case: (eq value1 value2).
               (not (,sort-fn value1 value2)))
          sort-fn)
      (let ((order-fn (edb--1rs-order-fn rs)))
        (if order-fn
            `(lambda ,(if reversep '(value2 value1) '(value1 value2))
               (= -1 (funcall (function ,order-fn) value1 value2)))
          'nil-function)))))

(defun db-rs-ordfunc (rs &optional reversep)
  "Return an order function for records described by recordfieldspec RS.
If optional argument REVERSEP is non-nil, then the order function goes in
the opposite order.
If the order-fn slot of the appropriate recordfieldspec of `database' doesn't
contain one, one is made up on the fly from the sort-fn slot; `equal'
is used to determine whether two records are equal.
If the sort-fn slot is also empty, the resulting function always
returns 0, indicating equality."
  (let ((order-fn (edb--1rs-order-fn rs)))
    (if order-fn
        (if reversep
            `(lambda (value1 value2)
               (,order-fn value2 value1))
          order-fn)
      (let ((sort-fn (edb--1rs-sort-fn rs)))
        (if sort-fn
            `(lambda ,(if reversep '(value2 value1) '(value1 value2))
               (cond ((equal value1 value2)
                      0)
                     ((funcall (function ,sort-fn) value1 value2)
                      -1)
                     (t
                      1)))
          (lambda (value1 value2) 0))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Records
;;;

;; Abstraction

(defun db--mkrec (nfields nm2no init)
  (let ((rv (make-vector nfields nil)))
    (cond ((eq :alist (car init))       ; alist
           (dolist (pair (cdr init))
             (aset rv (gethash (car pair) nm2no)
                   (cdr pair))))
          (t                            ; plist
           (while init
             (aset rv (gethash (pop init) nm2no)
                   (pop init)))))
    rv))

(defun db-make-record (database init)
  "Return a DATABASE-specific record initialized with INIT.
INIT is either a list of alternating fieldnames (symbols) and values,
or a list whose car is the keyword `:alist' and whose cdr is an alist
mapping fieldnames to their values."
  (db--mkrec (length (database-fieldnames database))
             (edb--1D database :nm2no)
             init))

(defun db-copy-r2r (source target)
  "Copy the field values of the SOURCE record to the TARGET record."
  (dotimes (fno (length source))
    (aset target fno (aref source fno))))

;;; Fieldnames and record fieldnumbers

(defsubst db-fname<-fno (fieldnumber db)
  "Given a record FIELDNUMBER and DB, return a record fieldname."
  (aref (database-fieldnames db) fieldnumber))

;;; Retrieving field values

(defun db-record-field (record fieldname &optional database)
  "Return from RECORD the field with name FIELDNAME.
If RECORD is t, use the \"current record\".  Optional third
argument DATABASE specifies a database other than the current one."
  (let* ((db (or database dbc-database))
         (fno (or (gethash fieldname (edb--1D db :nm2no))
                  (error "No %s field in current record." fieldname)))
         (rec (if (eq t record)
                  (edb--S :under)
                record)))
    (aref rec fno)))

;;; Checking constraints

(defun db-check-constraint (value rec idx db)
  (let ((func (aref (db-rs-slice db 'edb--1rs-constraint-function) idx)))
    (when (and func (not (funcall func value rec idx db)))
      (error "The value `%s' does not satisfy the constraint for field %s."
             value (db-fname<-fno idx db)))))

;;; Setting field values

(defun db-record-set-field (record fieldname value &optional database nocheck)
  "Set, in RECORD, field FIELDNAME to VALUE.  Fourth argument is DATABASE.
Check constraints first unless optional fifth argument NOCHECK is non-nil."
  (let* ((db (or database dbc-database))
         (fno (or (gethash fieldname (edb--1D db :nm2no))
                  (error "No %s field in current record." fieldname))))
    (unless (or nocheck (not db))
      (db-check-constraint value record fno db))
    (aset record fno value)))


;;; Setting fields in :under

(defsubst dbf-this-record-set-field (fieldname value)
  "Set field with name FIELDNAME in `:under' to VALUE.
Causes the entire record to be redisplayed pretty soon.
You may want to use `dbf-displayed-record-set-field' instead."
  ;; fixme: check `(edb--S :utkmodp)' first. --ttn
  (db-record-set-field (edb--S :under) fieldname value)
  (setq dbf-redisplay-entire-record-p t))

;;; The displayed record

(defsubst dbf-displayed-record-field (fieldname)
  "Return the value of the field named FIELDNAME from the displayed record."
  (db-record-field (dbf-displayed-record) fieldname))

(defun dbf-displayed-record-set-field (fieldname value)
  "Set field with name FIELDNAME in displayed record to VALUE.
Cause the entire record to be redisplayed soon."
  ;; Make sure displayed-record = this-record.
  (dbf-set-this-record-modified-p t)
  (dbf-this-record-set-field fieldname value))

(defsubst dbf-displayed-record-set-field-and-redisplay (fieldname value)
  "Set field with name FIELDNAME in displayed record to VALUE.
Cause the entire record to be redisplayed immediately."
  ;; Is this correct?  Maybe displayed-record != this-record.
  (dbf-this-record-set-field fieldname value)
  (dbf-redisplay-entire-record-maybe))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sepinfo abstraction
;;;

;; This tells how a list of information appears in a file.

(defstruct (edb--v1-sepinfo
            (:type vector)
            (:constructor edb--make-v1-sepinfo)
            (:conc-name sepinfo-))
  pre-first-string
  pre-first-regexp
  pre-first-regexp-submatch
  sep-string
  sep-regexp
  sep-regexp-submatch
  sep-function                          ; return (end-pos . next-start-pos)
                                        ; takes prev-end-pos as an argument
                                        ; next-start-pos nil for last record
  post-last-string
  post-last-regexp
  post-last-regexp-submatch)


(defun db-make-n-line-sep-function (n)
  "Return a function useful when all records have exactly N lines on disk.
This is for use with the `:record-separator-function' control property."
  `(lambda (prev-end)
     (forward-line ,n)
     (cons (point) (unless (eobp) (point)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Manipulating database records
;;;

(defun database-add-record (record db &optional location)
  ;; Add RECORD to DATABASE.  If optional third argument LOCATION is a number,
  ;; insert immediately before that index; if it is a function, call it with
  ;; the current index to get the new index (modulo operation included); if it
  ;; is nil, insert at the end.
  (let* ((vov (edb--1D db :vov))
         (vsz (length vov))
         (new vov)
         (bno (edb--1D db :nrecords))   ; before
         (atz (cond ((numberp location) ; 0-based index to insert at
                     (1- location))
                    ((functionp location)
                     (% (1- (funcall location (edb--S :index))) bno))
                    ((not location)
                     bno))))
    ;; realloc if necessary
    (unless (< bno vsz)
      (setq new (make-vector (+ 10 bno) nil)) ; todo: parameterize
      (dotimes (i atz)
        (aset new i (aref vov i))))
    ;; shift forward to leave a hole
    (do ((i bno (1- i)))
        ((= atz i))
      (aset new i (aref vov (1- i))))
    ;; add it
    (aset new atz record)
    (unless (eq new vov)
      (edb--1D! db :vov new))
    (edb--1D! db :nrecords (1+ bno))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mapping over a database
;;;

(defvar db-inform-interval 100
  "When doing a lengthy computation, inform the user of progress every this
many records.  If nil, don't inform.")

;; Mapping functions dynamically bind `db-lmap-index'.

(defvar db-lmap-index nil
  "The current index in a call to `db-lmap'.")

(defun db-lmap (lmapfunc db &optional hide message accumulate)
  ;; Use LMAPFUNC instead of simply FUNC to avoid a dynamic binding conflict.
  ;; In the body, variable `db-lmap-index' is the index of the record.
  ;; See `db-maprecords'.
  (let* ((vov (edb--1D db :vov))
         (db-lmap-index 1)
         (htag (edb-tag :hiddenp db))
         (results (when accumulate (list nil)))
         (--hit (assq 'db-inform-interval (database-locals db)))
         (--infint (if --hit (cdr --hit) db-inform-interval))
         (tp results)
         lmaprecord v)
    (setq message (and --infint message))
    (dotimes (i (edb--1D db :nrecords))
      (setq db-lmap-index (1+ i)
            lmaprecord (aref vov i))
      (unless (and hide (edb-tagp htag lmaprecord))
        (setq v (funcall lmapfunc lmaprecord))
        (when accumulate
          (setcdr tp (list v))
          (setq tp (cdr tp))))
      (when (and message (zerop (% db-lmap-index --infint)))
        (db-message message db-lmap-index)))
    (when accumulate
      (cdr results))))

(defun db-maprecords (func &optional db hide message accumulate)
  "Apply FUNC to every record in current database in order of ascending index.
Optional second arg DB specifies a database to use other than the current one.
Optional third arg HIDE non-nil means apply FUNC only to unhidden records.
If optional fourth arg MESSAGE is non-nil, it should be a format string
containing one numeric \(%d\) specifier.  That message will be issued every
`db-inform-interval' records.  If optional fifth arg ACCUMULATE is non-nil,
return a list of the results; otherwise return nil."
  (db-lmap (lambda (record)
             (funcall func record))
           (or db dbc-database) hide message accumulate))

;;;###badname
(defalias 'maprecords 'db-maprecords)

;;; db-rep.el ends here
