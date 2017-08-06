;;; db-sort.el --- part of EDB, the Emacs database

;; Copyright (C) 2004-2017 Thien-Thi Nguyeny

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

;; Ordering-fields and field-priorities refer to the same thing.
;; Order funcs preferred over sort funcs.

;;; Code:


(defvar db-converted-ofields)           ; used dynamically


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;

(defvar db-sort-modifies-p nil
  "*If non-nil, then sorting a database marks it as modified too.")

;; Field priorities is a cons of two lists; the first list is those that
;; will be used, and the the second set is the ignored fields.  Ignorance
;; is only in the user interface; EDB always remembers everything.

;; Each list consists of (fieldnumber . order-info) pairs such that every
;; field is accounted for exactly once.  The order-info is 'increasing,
;; 'decreasing, or a cons of (type . value) where type is 'order-function or
;; 'sort-function.

;; Priorities are initially associated w/ the database.  On first use
;; it becomes format-local.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sort
;;;

;; fixme: Implement "hidden at end" as synthetic field (attribute).  --ttn

;;;###autoload
(defun database-sort (db &optional sorter hidden-records-at-end-p)
  "Sort and return DB, which is also side-effected.
If DB is nil, use the current database.
SORTER is either a field priorities list or a function which
takes two records as arguments and returns t if r1 < r2."
  (db-message "Sorting...")
  (unless db
    (setq db dbc-database))
  (unless (zerop (edb--1D db :nrecords))
    (let (db-converted-ofields)

      (unless (functionp sorter)
        (setq db-converted-ofields (db-convert-ordering-fields
                                    sorter db)
              sorter 'db-record-lessp))
      (edb--snap! db (edb--1D db :nrecords)
                  (sort (db-lmap 'identity db nil nil t)
                        (lambda (r1 r2)
                          (funcall sorter r1 r2))))))
  (let ((hit (assq 'db-sort-modifies-p (database-locals dbc-database))))
    (when (if hit (cdr hit) db-sort-modifies-p)
      (database-set-modified-p db t)
      (force-mode-line-update)))
  (db-message "Sorting...done")
  db)


(defun db-convert-ordering-fields (ofields db)
  (let ((all-rs (edb--1D db :elaborated-rfspecs))
        (nm2no (edb--1D db :nm2no))
        f fno sortinfo type value)
    (mapcar (lambda (spec)
              (setq f (car spec)
                    fno (if (integerp f)
                            f
                          (gethash f nm2no))
                    sortinfo (cdr spec))
              (cons fno
                    (if (atom sortinfo)
                        (db-rs-ordfunc (aref all-rs fno)
                                       (eq sortinfo 'decreasing))
                      (setq type (car sortinfo)
                            value (cdr sortinfo))
                      (cond ((eq 'order-function type)
                             value)
                            ((eq 'sort-function type)
                             ;; fixme: lame. --ttn
                             (error "Trying to order field `%s', %s"
                                    (if (symbolp f)
                                        f
                                      (db-fname<-fno f db))
                                    "but only sort function provided."))
                            (t (error "Unrecognized type in %s" sortinfo))))))
            (car (or ofields
                     (database-field-priorities db))))))


(defun db-make-ordering-fields-canonical (db)
  ;; Make sure that every car is a number (not a fieldname) and
  ;; that every field is represented.
  (let* ((ofields (database-field-priorities db))
         (nfields (length (database-fieldnames db)))
         (seen (make-vector nfields nil))
         (nm2no (edb--1D db :nm2no))
         fno sortinfo value
         (check-field
          (lambda (spec)
            (setq fno (if (numberp (car spec))
                          (car spec)
                        (gethash (car spec) nm2no))
                  sortinfo (cdr spec))
            (cond ((not fno)
                   (db-warning "`%s' not a valid field; ignoring it."
                               (car spec))
                   nil)
                  ((aref seen fno)
                   (db-warning "`%s' seen already; ignoring subsequent."
                               (db-fname<-fno fno db))
                   nil)
                  (t
                   (aset seen fno t)
                   (cons fno
                         (cond ((eq sortinfo 'decreasing)
                                'decreasing)
                               ((atom sortinfo)
                                (when (and sortinfo
                                           (not (eq sortinfo 'increasing)))
                                  (db-warning "%s %s being changed to `%s'."
                                              "Unrecognized ordering symbol"
                                              sortinfo
                                              "increasing"))
                                'increasing)
                               ;; sortinfo is a list
                               ((or (eq (car sortinfo) 'order-function)
                                    (eq (car sortinfo) 'sort-function))
                                (unless (functionp (cdr sortinfo))
                                  (db-warning "%s %s %s %s %s."
                                              (cdr sortinfo)
                                              "is claimed to be a"
                                              (car sortinfo)
                                              "but doesn't look like"
                                              "a function to me"))
                                sortinfo)
                               (t (error "Unrecognized type in %s"
                                         sortinfo))))))))
         (sig-fields (delq nil (mapcar check-field (car ofields))))
         (nonsig-fields (delq nil (mapcar check-field (cdr ofields))))
         missing-fields)
    (dotimes (fno nfields)
      (unless (aref seen fno)
        (db-warning "%s doesn't appear; adding it."
                    (db-fname<-fno fno db))
        (push (cons fno 'increasing) missing-fields)))
    (cons sig-fields
          (if missing-fields
              (append nonsig-fields missing-fields)
            nonsig-fields))))


;; Uses the `db-converted-ofields' dynamic variable.
(defun db-order-records (r1 r2)
  ;; Return -1, 0, or 1 depending on whether records R1 and R2 are <, =, or >.
  (let ((ofields db-converted-ofields)
        (result 0)
        fno ofunc)
    (while (and ofields (zerop result))
      (setq fno (car (car ofields))
            ofunc (cdr (car ofields))
            ofields (cdr ofields)
            result (funcall ofunc (aref r1 fno) (aref r2 fno))))
    result))


(defun db-record-lessp (r1 r2)
  ;; Return t if record R1 < R2.  Use `db-order-records'.
  (= -1 (db-order-records r1 r2)))


(defun database-sorted-p (db &optional sorter)
  ;; Return t if DB is sorted, nil otherwise.
  ;; Optional argument SORTER is used as the function.
  (db-with-sorter db
    (catch t
      (let ((vov (edb--1D db :vov))
            (frp t))                    ; first record
        ;; Don't do anything for the first record.
        ;; If some other record is less than its predecessor, complain.
        (db-lmap
         (lambda (record)
           (if frp
               (setq frp nil)
             (when (funcall sorter record (aref vov (1- (1- db-lmap-index))))
               (throw t nil))))
         db)
        t))))

(defun database-ordered-p (db &optional orderer)
  ;; Return t if DB is ordered, nil otherwise.
  ;; Optional argument ORDERER is used as the ordering function.
  (db-with-orderer db
    (catch t
      (let ((vov (edb--1D db :vov))
            (frp t))                    ; first record
        (db-lmap
         (lambda (record)
           (if frp
               (setq frp nil)
             (when (= 1 (funcall orderer
                                 (aref vov (1- (1- db-lmap-index)))
                                 record))
               (throw t nil))))
         db)
        t))))


(defmacro db-with-orderer (database &rest body)
  (declare (indent 1) (debug (sexp body)))
  `(let (db-converted-ofields)
     (unless orderer                    ; dynamically bound
       (setq orderer 'db-order-records
             db-converted-ofields
             ;; mernst sez: stupid way to call this function.
             ;; ttn sez: why?
             (db-convert-ordering-fields nil ,database)))
     ,@body))

(defmacro db-with-sorter (database &rest body)
  (declare (indent 1) (debug (sexp body)))
  `(let (db-converted-ofields)
     (if (not sorter)                   ; dynamically bound
         (setq sorter 'db-record-lessp
               db-converted-ofields
               ;; mernst sez: stupid way to call this function.
               ;; ttn sez: why?
               (db-convert-ordering-fields nil ,database)))
     ,@body))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sorting interface
;;;

;; Commands:
;;  cursor movement
;;  kill a line
;;  yank a line
;;  sort on this field only
;;  accept this ordering, sort with it, and make it the default
;;  accept this ordering and sort with it, but don't make it the default
;;  toggle ordering direction (increasing vs decreasing) or -to-end-p
;;  input custom ordering info:  a list or a function name

;;;###autoload
(defun database-sort-interface (db)
  ;; Assume current-buffer is data display buffer.
  (let ((canon (db-make-ordering-fields-canonical db))
        (ddb (current-buffer)))
    (switch-to-buffer (get-buffer-create
                       (concat "Ordering info for "
                               (database-print-name db))))
    (database-sort-interface-mode)

    (set (make-local-variable 'edb--bprops)
         (edb--rinit '(:edb1 :bprops) (current-buffer)
                     :size 5 :test 'eq :weakness 'key))
    (set (make-local-variable 'dbc-database) db)
    (edb--S! :ddb ddb)
    (edb--S! :hendp (edb--1D db :hendp))

    ;; Fill the buffer with info.
    (let (buffer-read-only)
      (erase-buffer)
      (insert "==== Significant fields:\n")
      (save-excursion
        (dolist (item (car canon))
          (dbsi-format item :sig))
        (insert (propertize "==== Nonsignificant fields:\n" :which :sig))
        (dolist (item (cdr canon))
          (dbsi-format item :non))
        (insert "==== Hidden records to end:  "
                (if (edb--S :hendp) "Yes" "No "))))
    (message "%s, %s, %s, %s, %s"
             "u = use"
             "RET = also make default"
             "! = this field only"
             "q = abort"
             "? = help")))

(defun database-sort-interface-mode ()
  "Sort interface buffer for choosing fields upon which to sort a database.
You can:

reorder fields with `dbsi-kill-line' and `dbsi-yank-line';

specify how a particular field should be ordered with `dbsi-increasing'
`dbsi-decreasing', `dbsi-ordering-function' and `dbsi-sorting-function';

change whether hidden records go at the end of the sorted order with
`dbsi-toggle-hidden-to-end';

exit the sort interface with one of the following:
`dbsi-use-ordering-make-database-default',
`dbsi-use-ordering-make-buffer-default', `dbsi-quit-clear-buffer-default'
`dbsi-use-ordering', `dbsi-this-field-only', `dbsi-quit'.

More specifically:
\\{database-sort-mode-map}"
  (setq major-mode 'database-sort-interface-mode)
  (setq mode-name "Database Sort Interface")

  (set-buffer-modified-p nil)
  (setq buffer-read-only t
        truncate-lines t)

  (use-local-map database-sort-mode-map)

  (setq mode-line-format
        '("--- %17b ["
          (-3 . "%p")
          "]-%-")))


(defvar database-sort-mode-map nil
  "Keymap for database data display buffer in view mode.")

(unless database-sort-mode-map
  (let ((m (make-sparse-keymap)))
    (suppress-keymap m)
    (mapc (lambda (key-def)
            (define-key m (car key-def) (cdr key-def)))
          '(("\C-k"     . dbsi-kill-line)
            ("\C-y"     . dbsi-yank-line)
            ("!"        . dbsi-this-field-only)
            ("\r"       . dbsi-use-ordering-make-database-default)
            ("\C-c\C-c" . dbsi-use-ordering-make-database-default)
            ("A"        . dbsi-use-ordering-make-buffer-default)
            ("U"        . dbsi-use-ordering-make-buffer-default)
            ("c"        . dbsi-quit-clear-buffer-default)
            ("a"        . dbsi-use-ordering)
            ("u"        . dbsi-use-ordering)
            ("t"        . dbsi-toggle-hidden-to-end)
            ("o"        . dbsi-ordering-function)
            ("s"        . dbsi-sorting-function)
            ("i"        . dbsi-increasing)
            ("d"        . dbsi-decreasing)
            ("q"        . dbsi-quit)
            ("\M-n"     . [?\C-k ?\C-n ?\C-y ?\C-p])
            ("\M-p"     . [?\C-k ?\C-p ?\C-u ?\C-y])
            ("?"        . describe-mode)))
    (setq database-sort-mode-map m)))


;; Insert item and trailing newline.
(defun dbsi-format (item which)
  (cl-flet
      ((pinsert (s) (insert (propertize s :item item :which which))))
    (pinsert (format "  %-16s    " (db-fname<-fno (car item) dbc-database)))
    (if (atom (cdr item))
        (pinsert (format "%s" (cdr item)))
      (pinsert (format "%s [%s]" (cdr (cdr item)) (car (cdr item)))))
    (pinsert "\n")))

(defun dbsi-reformat (item)
  (beginning-of-line)
  (let ((which (get-text-property (point) :which))
        buffer-read-only)
    (kill-line 1)
    (save-excursion (dbsi-format item which))))

;;; Moving fields around.

(defun dbsi-kill-line ()
  "Move field on current line to the top of the nonsignificant list."
  (interactive)
  (let (line beg buffer-read-only)
    (unless (get-text-property (point) :item)
      (error "No field on this line."))
    (beginning-of-line)
    (setq beg (point))
    (save-excursion
      (forward-line 1)
      (setq line (buffer-substring beg (point)))
      (delete-region beg (point))
      (goto-char (point-min))
      (search-forward "Nonsignificant fields:\n")
      (setq beg (point))
      (insert line)
      (put-text-property beg (point) :which :non))))

(defun dbsi-yank-line (afterp)
  "Yank the field from the top of the nonsignificant list.
Insert it before the current line.  Prefix arg means point doesn't move."
  (interactive "P")
  (let (eob which line b e buffer-read-only)
    (unless (eq :sig (get-text-property (point) :which))
      (error "Can't insert here."))
    (save-excursion
      (setq b (progn (goto-char (point-min))
                     (search-forward "Nonsignificant fields:\n")
                     (point))
            e (progn (forward-line 1)
                     (point)))
      (unless (get-text-property b :item)
        (error "Nothing to yank."))
      (setq line (propertize (buffer-substring b e) :which :sig))
      (delete-region b e))
    (unless (get-text-property (point) :item)
      (setq afterp nil))
    (beginning-of-line)
    (if afterp (save-excursion
                 (insert line))
      (insert line))))

;;; Changing info about hiding.

(defun dbsi-toggle-hidden-to-end ()
  "Toggle whether hidden records should all be placed at the end of
the sorted order or should be sorted according to the same criteria
as non-hidden records."
  (interactive)
  (save-excursion
    (let (buffer-read-only)
      (edb--S! :hendp (not (edb--S :hendp)))
      (goto-char (point-max))
      (backward-delete-char 3)
      (insert (if (edb--S :hendp) "Yes" "No ")))))

;;; Changing

(defun dbsi-increasing ()
  "Specify that the field at point should use an increasing ordering."
  (interactive)
  (let ((item (get-text-property (point) :item)))
    (when item
      (setcdr item 'increasing)
      (dbsi-reformat item))))

(defun dbsi-decreasing ()
  "Specify that the field at point should use a decreasing ordering."
  (interactive)
  (let ((item (get-text-property (point) :item)))
    (when item
      (setcdr item 'decreasing)
      (dbsi-reformat item))))

(defun dbsi-ordering-function ()
  "Specify an ordering function for the field at point.
An ``ordering function'' returns -1, 0, or 1 depending on whether its first
argument is less than, equivalent to, or greater than its second argument."
  (interactive)
  (let ((item (get-text-property (point) :item)))
    (when item
      (setcdr item (cons 'order-function
                         (read-from-minibuffer "Ordering function: "
                                               nil nil t)))
      (dbsi-reformat item))))

(defun dbsi-sorting-function ()
  "Specify a sorting function for the field at point.
A ``sorting function'' returns t if its first argument is less
than its second argument and nil otherwise."
  (interactive)
  (let ((item (get-text-property (point) :item)))
    (when item
      (setcdr item (cons 'sort-function
                         (read-from-minibuffer "Sorting function: "
                                               nil nil t)))
      (dbsi-reformat item))))

;;; Quitting

(defun dbsi-quit ()
  "Abort the sort and exit the sort interface."
  (interactive)
  (let ((ddb (edb--S :ddb)))
    (kill-buffer nil)
    (switch-to-buffer ddb)
    (db-message "Aborting sort")))

(defun dbsi-quit-clear-buffer-default ()
  "Clear the default sort order for this buffer and exit the sort interface
without sorting.
In the future, the default sort order will come from the database."
  (interactive)
  (let ((ddb (edb--S :ddb)))
    (kill-buffer nil)
    (switch-to-buffer ddb)
    (edb--S! :field-priorities nil)
    (db-message "Reset buffer-local default sort order; didn't sort")))

;;; Specifying an ordering.

(defun dbsi-this-field-only ()
  "Sort according to only the field at point.
All editing of other fields is ignored."
  (interactive)
  (let ((item (get-text-property (point) :item)))
    (unless item
      (error "No field on this line."))
    (dbsi-use-ordering (cons (list item) nil))))

(defun dbsi-ordering<-buffer ()
  (let (sig non p item which)
    (setq p (point-min))
    (while (setq p (next-single-property-change p :item))
      (when (setq item (get-text-property p :item))
        (if (eq :sig (get-text-property p :which))
            (push item sig)
          (push item non))))
    (cons (nreverse sig)
          (nreverse non))))

(defun dbsi-use-ordering-make-database-default ()
  "Use the current ordering to sort, and make it the default for future sorts
of this database."
  (interactive)
  (with-current-buffer (edb--S :ddb)
    (edb--S! :field-priorities nil))
  (edb--1D! dbc-database :hendp (edb--S :hendp))
  (setf (database-field-priorities dbc-database) (dbsi-ordering<-buffer))
  (dbsi-use-ordering (database-field-priorities dbc-database)))

(defun dbsi-use-ordering-make-buffer-default ()
  "Use the current ordering to sort, and make it the default for future sorts
in this data display buffer only."
  (interactive)
  (let ((ordering (dbsi-ordering<-buffer))
        (hendp (edb--S :hendp)))
    (with-current-buffer (edb--S :ddb)
      (edb--S! :field-priorities ordering)
      (edb--S! :hendp hendp))
    (dbsi-use-ordering ordering)))

(defun dbsi-use-ordering (&optional ordering)
  "Use the current ordering for this sort only."
  (interactive)
  (database-sort dbc-database
                 (or ordering (dbsi-ordering<-buffer))
                 (edb--S :hendp))
  (let ((ddb (edb--S :ddb)))
    (kill-buffer nil)
    (switch-to-buffer ddb)
    (dbf-finished-sorting)))

(provide 'db-sort)

;;; db-sort.el ends here
