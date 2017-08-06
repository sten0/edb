;;; connection.el

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

;;; Code:

(:edb1 :seq-funcs (make-hash-table :size 3 :test 'eq))

(edb--rput '(:edb1 :seq-funcs) 'read-line
  (lambda (finish)
    (let* ((recs (list nil))
           (tp recs))
      (while (< (progn
                  (nconc tp (list (read (current-buffer))))
                  (setq tp (cdr tp))
                  (skip-syntax-forward "^()")
                  (point))
                finish))
      (cdr recs))))

(edb--rput '(:edb1 :seq-funcs) 'write-line
  (lambda (b e recs)
    (goto-char b)
    (delete-region b e)
    (let ((out (current-buffer))
          (print-escape-newlines t))
      (dolist (r recs)
        (prin1 r out)
        (princ "\n" out)))))

;; Each entry is an alist that controls how a schema is handled.
;; Valid keys are:
;;
;; :valid-keys -- list of acceptable keys
;; :valid-options -- list of acceptable options
;; :elaborate-value -- function that takes two args: KEY and FORM,
;;    and returns FORM elaborated as a value (usually by `eval'ing
;;    FORM in a KEY-particular way)
;; :predicates -- list of symbol-predicate pairs; each predicate takes one
;;    arg: OBJECT, and returns non-nil if OBJECT fulfills the predicate
;;
(:edb1 :schema-schema (make-hash-table :size 3 :test 'eq))

(edb--rput '(:edb1 :schema-schema) 'single
  `((:valid-keys
     (:name stringp)
     (:fields vectorp)
     (:field-separator string-or-rxvec-p)
     (:record-separator string-or-rxvec-p)
     (:record-terminator stringp)
     (:record-separator-function functionp)
     (:read-record functionp)
     (:write-record functionp)
     (:record-defaults functionp)
     (:choose-display functionp)
     :display
     :report
     (:summary-format stringp)
     (:substitutions vectorp)
     (:every-change-function functionp)
     (:first-change-function functionp)
     (:field-order vectorp)
     (:tagged-setup consp)
     (:before-display functionp)
     (:locals vectorp)
     (:substitution-separators vector-of-two-strings-p) ; [fsep rsep]
     (:cruft vectorp)                   ; [[befrec aftrec] [beffld aftfld]]
     :data)
    (:valid-options)
    (:runtime-keys :wraparound :stay-in-edit-mode-p)
    (:elaborate-value
     . ,(lambda (key-ignored x)
          (cond ((and (consp x) (keywordp (car x))) x)
                ((and (consp x) (stringp (car x))) x)
                (t (eval x)))))
    (:predicates
     (string-or-rxvec-p
      . ,(lambda (object)
           (or (stringp object)
               (and (vectorp object)
                    (let ((len (length object)))
                      (and (memq len '(2 3))
                           (stringp (aref object 0))
                           (integerp (aref object 1))
                           (or (= 2 len)
                               (stringp (aref object 2)))))))))
     (vector-of-two-strings-p
      . ,(lambda (object)
           (and (vectorp object)
                (= 2 (length object))
                (let ((v (aref object 0)))
                  (or (not v) (stringp v)))
                (let ((v (aref object 1)))
                  (or (not v) (stringp v)))))))))

(defun edb--validate-schema (type options schema)
  (let* ((ent (or (edb--rget '(:edb1 :schema-schema) type)
                  (error "Invalid schema type: %S" type)))
         (key-specs (cdr (assq :valid-keys ent)))
         (valid-keys (mapcar (lambda (k-ent)
                               (if (consp k-ent)
                                   (car k-ent)
                                 k-ent))
                             key-specs))
         (elaborate-value (cdr (assq :elaborate-value ent)))
         (predicates (let ((local (cdr (assq :predicates ent)))
                           (ht (make-hash-table :test 'eq))
                           name)
                       (dolist (k-ent key-specs)
                         (when (consp k-ent)
                           (setq name (nth 1 k-ent))
                           (unless (gethash name ht)
                             (puthash name (or (cdr (assq name local))
                                               name)
                                      ht))))
                       ht))
         (valid-options (cdr (assq :valid-options ent))))
    ;; check plist form
    (let ((ls schema))
      (while ls
        (unless (keywordp (car ls))
          (error "Not a keyword: %S" (car ls)))
        (setq ls (cddr ls))))
    ;; check key membership
    (let ((ls schema))
      (while ls
        (unless (memq (car ls) valid-keys)
          (error "Not a valid key: %S" (car ls)))
        (setq ls (cddr ls))))
    ;; elaborate values
    (when elaborate-value
      (let ((ls schema) v new)
        (while ls
          (setq v (cadr ls)
                new (funcall elaborate-value (car ls) v))
          (unless (eq new v)
            (setcar (cdr ls) new))
          (setq ls (cddr ls)))))
    ;; check values
    (let ((ls schema) k v k-ent pred-name)
      (while ls
        (when (setq k (car ls)
                    v (cadr ls)
                    k-ent (assq k key-specs)
                    pred-name (and (consp k-ent) (nth 1 k-ent)))
          (unless (funcall (gethash pred-name predicates) v)
            (error "Wrong type for %s (not %s): %S" k pred-name v)))
        (setq ls (cddr ls))))
    ;; check option membership
    (let ((ls options))
      (while ls
        (unless (memq (car ls) valid-options)
          (error "Not a valid option: %S" (car ls)))
        (setq ls (cdr ls)))))
  schema)

(put 'edb--with-callable-connection 'lisp-indent-function 1)
(defmacro edb--with-callable-connection (name &rest body)
  `(cl-flet
       ((,name (&rest args) (apply ,name args)))
     ,@body))

(defun edb--connect (control-spec-source)
  ;; CONTROL-SPEC-SOURCE is either a filename,
  ;; or a buffer w/ appropriately formatted contents.
  (let ((conn (lexical-let (V)
                (lambda (command &rest args)
                  (case command
                    ;; it's not worthy of emacs if it's not extensible
                    (:V! (setq V (apply 'plist-put V args)))
                    (t (plist-get V command))))))
        schema)
    (edb--with-callable-connection conn
      (with-temp-buffer
        (let (emacs-lisp-mode-hook)
          (emacs-lisp-mode))
        ;; determine schema metainfo
        (let ((reality (cond ((bufferp control-spec-source)
                              (insert-buffer-substring control-spec-source)
                              (list (format "(internal from %S)"
                                            control-spec-source)
                                    (buffer-size)))
                             (t (insert-file-contents control-spec-source))))
              meta)
          (goto-char (point-min))
          ;; skip comments and blank lines before identifying cookie
          (while (and (< (point) (point-max)) (looking-at "[;\n]"))
            (forward-line 1))
          (unless (and (< 4 (cadr reality))
                       (string= ":EDB " (buffer-substring-no-properties
                                         (point) (+ 5 (point))))
                       (consp (setq meta (progn (goto-char (+ 5 (point)))
                                                (read (current-buffer))))))
            (error "Does not seem to be an EDB control file"))
          ;; maintain lameness for the present
          (unless (and (consp meta) (eq 'single (car meta)))
            (error "Not lame enough: %S" meta))
          (conn :V! :schema-type (car meta))
          (conn :V! :schema-options (cddr meta))
          (when (setq schema (cadr meta))
            (cl-flet
                ((bad (why) (error "Invalid (%s) schema basis: %S"
                                   why schema)))
              (unless (symbolp schema)
                (bad "not a symbol"))
              (unless (boundp schema)
                (bad "unbound variable"))
              (setq schema (symbol-value schema))
              (unless (or (not schema)
                          (consp schema))
                (bad "not a list"))
              (setq schema (reverse schema)))))
        ;; determine schema
        (let (sexp to-eval start kw val)
          (while (< (progn
                      (while (forward-comment 1))
                      (point))
                    (point-max))
            (setq start (point)
                  sexp (read (current-buffer)))
            ;; todo: convert into cond
            (when (consp sexp)
              (push sexp to-eval))
            (when (keywordp sexp)
              (push (setq kw sexp) schema)
              (setq val (read (current-buffer)))
              (if (memq kw '(:display :report :data))
                  (let* ((pls (if (eq t val)
                                  (list :name t :coding t :EOTB ":EOTB")
                                val))
                         (datap (eq :data kw))
                         (tb-start (progn (forward-line 1) (point)))
                         (name (plist-get pls :name))
                         (coding (or (plist-get pls :coding) t))
                         (EOTB (or (plist-get pls :EOTB) ":EOTB"))
                         tb-finish)
                    (unless (or
                             ;; data text blocks are anonymous
                             datap
                             (eq t name) (stringp name))
                      (error "Bad %S name: %S" kw name))
                    (unless (symbolp coding)
                      (error "Bad %S coding: %S" kw coding))
                    (unless (stringp EOTB)
                      (error "Bad %S EOTB: %S" kw EOTB))
                    (setq tb-finish (if (not (re-search-forward
                                              (concat "^" EOTB "$")
                                              (point-max) 1))
                                        (point-max)
                                      (forward-line 1)
                                      (match-beginning 0)))
                    (cl-flet
                        ((pl! (prop val) (setq pls (plist-put pls prop val))))
                      (if datap
                          (let* ((seqr (plist-get pls :seqr))
                                 (f (or (edb--rget '(:edb1 :seq-funcs) seqr)
                                        (error "Bad :seqr func: %S" seqr))))
                            (save-excursion
                              (goto-char tb-start)
                              (pl! :tb-start tb-start)
                              (pl! :tb-finish tb-finish)
                              (pl! :records (funcall f tb-finish))))
                        (pl! :text (buffer-substring-no-properties
                                    tb-start tb-finish))))
                    (if datap
                        (conn :V! (pop schema) pls)
                      (push pls schema)))
                (forward-comment 1)
                (push val schema))))
          ;; validate and stash
          (dolist (form (nreverse to-eval))
            (eval form))
          (conn :V! :schema (edb--validate-schema (conn :schema-type)
                                                  (conn :schema-options)
                                                  (nreverse schema))))))
    conn))

;;;---------------------------------------------------------------------------
;;; connection file cache

(defun edb--connection-file-cache (operation &rest args)
  (let* ((conn (with-current-buffer (edb--S :ddb-spec)
                 (get-text-property (point-min) :connection)))
         (schema (edb--with-callable-connection conn (conn :schema)))
         ;; prevent recursion
         (inhibit-file-name-handlers (cons 'edb--connection-file-cache
                                           inhibit-file-name-handlers))
         name desc)
    (cl-flet
        ((!! (s) (setq name (substring s (cdr (read-from-string s)))
                       desc (let ((ls schema) look)
                              (while (and (setq ls (memq :display ls)
                                                look (cadr ls))
                                          (not (string= (plist-get look :name)
                                                        name)))
                                (setq ls (cddr ls)))
                              look))))
      (case operation
        ((file-exists-p file-readable-p)
         (!! (car args))
         (not (not desc)))
        ((insert-file-contents)
         (!! (car args))
         (insert (plist-get desc :text)))
        ((file-name-directory file-name-nondirectory expand-file-name)
         (car args))
        (t
         (error "badness!"))))))

;;;---------------------------------------------------------------------------
;;; 1.x migration

(defun edb--1mm<-connection (conn)
  (let ((schema (funcall conn :schema))
        v)
    (cl-flet
        ((S (prop) (plist-get schema prop)))
      (edb--make-v1-monolithic-mess

       :print-name
       (S :name)

       ;; :fieldnames and :recordfieldspecs are set by other means.

       :field-priorities
       (when (setq v (S :field-order))
         (list (mapcar (lambda (spec)
                         (if (consp spec)
                             spec
                           (list spec)))
                       v)))

       :record-sepinfo
       (let* ((cruft (and (S :cruft) (aref (S :cruft) 0)))
              (bef (and cruft (aref cruft 0)))
              (brx (and (vectorp bef) (aref bef 0)))
              (sep (or (S :record-terminator)
                       (S :record-separator)))
              (srx (and (vectorp sep) (aref sep 0)))
              (aft (let ((c (and cruft (aref cruft 1))))
                     (cond ((not c) (S :record-terminator))
                           ((not (setq v (S :record-terminator))) c)
                           ((vectorp c) (vector (concat (regexp-quote v)
                                                        (aref c 0))
                                                (aref c 1)))
                           (t (concat v c)))))
              (arx (and (vectorp aft) (aref aft 0))))
         (edb--make-v1-sepinfo
          :pre-first-string (and (not brx) bef)
          :pre-first-regexp brx
          :pre-first-regexp-submatch (and brx (aref bef 1))
          :sep-string (if srx
                          (when (= 3 (length sep))
                            (aref sep 2))
                        sep)
          :sep-regexp srx
          :sep-regexp-submatch (and srx (aref sep 1))
          :sep-function (S :record-separator-function)
          :post-last-string (and (not arx) aft)
          :post-last-regexp arx
          :post-last-regexp-submatch (and arx (aref aft 1))))

       :field-sepinfo
       (let* ((cruft (and (S :cruft) (aref (S :cruft) 1)))
              (bef (and cruft (aref cruft 0)))
              (brx (and (vectorp bef) (aref bef 0)))
              (sep (S :field-separator))
              (srx (and (vectorp sep) (aref sep 0)))
              (aft (and cruft (aref cruft 1)))
              (arx (and (vectorp aft) (aref aft 0))))
         (edb--make-v1-sepinfo
          :pre-first-string (and (not brx) bef)
          :pre-first-regexp brx
          :pre-first-regexp-submatch (and brx (aref bef 1))
          :sep-string (if srx
                          (when (= 3 (length sep))
                            (aref sep 2))
                        sep)
          :sep-regexp srx
          :sep-regexp-submatch (and srx (aref sep 1))
          ;; sep-function never used, what about the rest?
          :sep-function nil
          :post-last-string (and (not arx) aft)
          :post-last-regexp arx
          :post-last-regexp-submatch (and arx (aref aft 1))))

       :read-record-from-region
       (S :read-record)

       :write-region-from-record
       (let* ((fun (S :write-record))
              ;; poor man's (bootstrapping) nm2no
              (rev (let ((n -1))
                     (mapcar (lambda (spec)
                               (cons (if (symbolp spec)
                                         spec
                                       (car spec))
                                     `(aref record ,(incf n))))
                             (S :fields))))
              (arglist (and fun (help-function-arglist fun)))
              use-let-p)
         (unless arglist
           (setq arglist (mapcar 'car rev)
                 use-let-p t))
         (cond ((not fun)
                nil)
               (use-let-p
                `(lambda (record)
                   (let ,(mapcar (lambda (fname)
                                   `(,fname ,(cdr (assq fname rev))))
                                 arglist)
                     (,fun))))
               (t
                `(lambda (record)
                   (,fun ,@(mapcar (lambda (fname)
                                     (cdr (assq fname rev)))
                                   arglist))))))

       :sub-fieldsep-string
       (when (setq v (S :substitution-separators))
         (aref v 0))

       :sub-recordsep-string
       (when (setq v (S :substitution-separators))
         (aref v 1))

       :substitutions
       (when (setq v (S :substitutions))
         (append v nil))

       :locals
       (mapcar (lambda (x)
                 (if (consp x)
                     (cons (car x) (cadr x))
                   (cons x nil)))
               (S :locals))))))

(defun edb--1format-buffer<-connection (conn)
  (let* ((schema (edb--with-callable-connection conn
                   (conn :schema)))
         (mm (edb--1mm<-connection conn))
         v form)
    (cl-flet*
        ((S (prop) (plist-get schema prop))
         (maybe-quote (v) (if (or (and (consp v)
                                       (memq (car v) '(function lambda)))
                                  (stringp v)
                                  (vectorp v))
                              v
                            (list 'quote v)))
         (ppcur (x) (pp x (current-buffer)) (when (symbolp x) (insert "\n")))
         (maybe-pp () (when form
                        (cond ((vectorp form)
                               (insert (format "%s:\n" (aref form 0)))
                               (ppcur (aref form 1)))
                              (t
                               ;; The local variables block must be 3000
                               ;; chars from the end, so printing things out
                               ;; to be handled "normally" loses on large
                               ;; eval forms.  Kludge around by listing all
                               ;; eval forms in a text property.  Sigh.
                               (let ((all (get-text-property 1 :eval-forms)))
                                 (insert
                                  "eval:\n"
                                  "(eval (nth "
                                  (format "%d" (length all))
                                  " (get-text-property 1 :eval-forms)))\n")
                                 (put-text-property
                                  (point-min) (point-max)
                                  :eval-forms (nconc all (list form)))))))))
      (set-buffer (get-buffer-create (format " FORMAT FILE: %S" (S :name))))
      (fundamental-mode)
      (erase-buffer)
      (insert (plist-get (S :display) :text))
      (insert "\n")
      (insert "Local" " " "Variables:\n")
      ;; fields
      (when (setq form nil v (S :fields))
        (setq form `(database-set-fieldnames-to-list database ',(append v nil))))
      (maybe-pp)
      (when (setq form nil v (S :tagged-setup))
        (setq form `(db-tagged-setup ',(cadr v) ,@(cddr v))))
      (maybe-pp)
      ;; monolithic mess
      (setq form nil)
      (dolist (slot (mapcar (lambda (x)
                              (intern (format "database-%s" (car x))))
                            (reverse (edb--struct-slot-info
                                      'edb--v1-monolithic-mess))))
        (unless (memq slot '(database-fieldnames
                             database-recordfieldspecs
                             database-locals
                             database-record-sepinfo
                             database-field-sepinfo))
          (when (setq v (funcall slot mm))
            (push (maybe-quote v) form)
            (push `(,slot database) form))))
      (when form
        (push 'setf form))
      (maybe-pp)
      ;; locals
      (dolist (local (database-locals mm))
        (setq form `(database-make-local ',(car local) database ,(cdr local)))
        (maybe-pp))
      ;; fsep and rsep
      (let ((sep-slots (mapcar (lambda (x)
                                 (intern (format "sepinfo-%s" (car x))))
                               (reverse (edb--struct-slot-info
                                         'edb--v1-sepinfo)))))
        ;; fsep
        (setq form nil)
        (dolist (slot sep-slots)
          (when (setq v (funcall slot (database-field-sepinfo mm)))
            (push (maybe-quote v) form)
            (push `(,slot sep) form)))
        (when form
          (setq form `(let ((sep (database-field-sepinfo database)))
                        (setf ,@form))))
        (maybe-pp)
        ;; rsep
        (setq form nil)
        (dolist (slot sep-slots)
          (when (setq v (funcall slot (database-record-sepinfo mm)))
            (push (maybe-quote v) form)
            (push `(,slot sep) form)))
        (when form
          (setq form `(let ((sep (database-record-sepinfo database)))
                        (setf ,@form))))
        (maybe-pp))
      ;; record defaults
      (when (setq form nil v (S :record-defaults))
        (setq form (vector
                    'db-new-record-function
                    `(lambda (rec db)
                       (let ((nm2no (edb--1D db :nm2no))
                             (plist (,v)))
                         (while plist
                           (aset rec (gethash (car plist) nm2no) (cadr plist))
                           (setq plist (cddr plist))))))))
      (maybe-pp)
      ;; before-display stuff (note: clobbered by :choose-display below)
      (when (setq form nil v (S :before-display))
        (setq form (vector
                    'dbf-before-display-record-function
                    `(lambda (record)
                       (,v ,@(mapcar
                              (lambda (field)
                                `(db-record-field record (quote ,field)))
                              (help-function-arglist v)))))))
      (maybe-pp)
      ;; first and every change
      (when (setq form nil v (S :first-change-function))
        (setq form (vector 'dbf-first-change-function v)))
      (maybe-pp)
      (when (setq form nil v (S :every-change-function))
        (setq form (vector 'dbf-every-change-function v)))
      (maybe-pp)
      ;; summary
      (when (setq form nil v (S :summary-format))
        (setq form `(dbf-set-summary-format ,v)))
      (maybe-pp)
      ;; multiple display formats
      (when (setq form nil v (let ((sch (edb--with-callable-connection conn
                                          (conn :schema)))
                                   acc)
                               (while (setq sch (cdr (memq :display sch)))
                                 (push (pop sch) acc))
                               acc))
        (setq form (vector
                    'dbf-format-name-spec-alist
                    (mapcar (lambda (pl)
                              (let ((name (plist-get pl :name)))
                                (cons name (or (plist-get pl :file)
                                               (format "(connection)%s"
                                                       name)))))
                            v))))
      (maybe-pp)
      ;; selecting display format
      (when (setq form nil v (plist-get (edb--with-callable-connection conn
                                          (conn :schema))
                                        :choose-display))
        (setq form (vector
                    'dbf-before-display-record-function
                    `(lambda (record)
                       (db-change-format
                        (,v ,@(mapcar
                               (lambda (field)
                                 `(db-record-field record (quote ,field)))
                               (help-function-arglist v))))))))
      (maybe-pp)
      ;; that's it
      (insert "End:\n")
      ;; remember your roots!
      (put-text-property (point-min) (point-max) :connection conn)
      ;; rv
      (current-buffer))))

(defun edb--1run-hooks (symbol)
  (let ((local (cdr (assq symbol (database-locals dbc-database)))))
    (run-hooks 'local symbol)))

(defun edb--1run-hook-with-arg (symbol &rest args)
  (let ((local (cdr (assq symbol (database-locals dbc-database)))))
    (apply 'run-hook-with-args 'local args))
  (apply 'run-hook-with-args symbol args))

;;; connection.el ends here
