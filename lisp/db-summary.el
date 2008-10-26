;;; db-summary.el --- part of EDB, the Emacs database

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

;; Patterned in part after rmail-new-summary.

;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;


;;;
;;; Hooks
;;;

(defvar database-summary-mode-hooks nil
  "Normal hook run when switching to Database Summary mode.")


;;;
;;; Summary variables
;;;

(defun dbs-set-index (index)
  (edb--S! :index index)
  (edb--S! :index-fraction (format "%d/%d" index (edb--S :nrecords))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macros for working in the correct buffer
;;;

(defmacro db-in-data-display-buffer (&rest body)
  (declare (indent 0) (debug body))
  `(db-in-buffer (or (edb--S :data-display-buffer)
                     (current-buffer))
     ,@body))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Creating the summary
;;;


(defun db-summary ()
  "Display a summary (or directory) of records in a separate buffer.
When called from the summary buffer, this updates the summary.
The displayed format can be set with `dbf-set-summary-format'."
  (interactive)
  (db-in-data-display-buffer
    (cond ((= 0 (edb--1D dbc-database :nrecords))
           (delete-windows-on (edb--S :sumbuf))
           (db-message "Database is empty"))
          (t
           (let ((ddb (current-buffer))
                 (sumbuf (edb--S :sumbuf)))
             (unless (edb--S :sumfun)
               (dbf-set-summary-format (edb--S :sumfmt)))
             (unless sumbuf
               (edb--S! :sumbuf
                        (with-current-buffer
                            (generate-new-buffer
                             (format "%s-summary" (buffer-name ddb)))
                          (set (make-local-variable 'edb--bprops)
                               (edb--rinit '(:edb1 :bprops) (current-buffer)
                                           :size 5 :weakness 'key))
                          (set (make-local-variable 'dbc-database)
                               (buffer-local-value 'dbc-database ddb))
                          (edb--S! :data-display-buffer ddb)
                          (database-summary-mode)
                          (edb--S! :summaries (make-hash-table :weakness 'key))
                          (setq sumbuf (current-buffer)))))
             (when (with-current-buffer sumbuf
                     (dbs-out-of-date-p))
               (dbf-fill-summary-buffer))
             (pop-to-buffer sumbuf)
             (edb--S! :data-display-buffer ddb)
             ;; Go to proper line.
             (dbs-move-to-proper-record))))))


;; This is spelled out instead of being db-summary-mode because it's a
;; "standalone" major mode, while db-edit-mode and db-view-mode are
;; "cooperating" major modes (unimportant distinction, really --ttn).
(defun database-summary-mode ()
  "Summary buffer for database mode.
Most keystrokes perform the same function they do in the data display buffer.

Key bindings:

\\{database-summary-mode-map}"

  (setq major-mode 'database-summary-mode)
  (setq mode-name "Database Summary")

  (set-buffer-modified-p nil)
  (setq buffer-read-only t)

  (use-local-map database-summary-mode-map)

  (setq mode-line-format
        (list
         "----- "
         (format "%-17s" (buffer-name (edb--S :data-display-buffer)))
         "   %[(" 'mode-name
         'minor-mode-alist
         '(:eval (concat (and (edb--S :hide-p) " Hide")
                         " "
                         (edb--S :index-fraction)))
         ")%]---"
         '(-3 . "%p")
         "-%-"))

  (edb--1run-hooks 'database-summary-mode-hooks)

  ;; Force an update.
  (edb--S! :nrecords -1))


(defsubst db-summary-buffer-p ()
  "T if this buffer is a database summary buffer."
  (eq major-mode 'database-summary-mode))


(defun db-summary-subset ()
  "Make EDB summary-listing based on hits of STRING search, of FIELD."
  (interactive)
  (db-mark-searched-records)
  (db-hide-unmarked-records)
  (db-toggle-show-hidden-records 0)
  (db-summary)
  (db-hiding-toggle 0)
  (db-hiding-toggle 1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Filling the summary
;;;

(defun dbs-insert-record-summary (record mhp mtag htag)
  ;; mhp -- mark-hidden-records-p
  (let ((one (if (edb-tagp mtag record) "+" " "))
        (two (if (and mhp (edb-tagp htag record)) "[" " "))
        (rest (substring (gethash record (edb--S :summaries)) 2)))
    (insert one two rest)))


(defun dbf-fill-summary-buffer (&optional movep)
  ;; fixme: check database nonempty. --ttn
  (let ((sum (edb--S :sumfun))
        (lines (edb--1D dbc-database :sum1lines))
        (hide (and (edb--S :invisible-hidden-p)
                   (edb--S :hide-p)))
        (mhp (edb--S :hide-p))
        (sumbuf (edb--S :sumbuf)))
    (when sumbuf
      (with-current-buffer sumbuf
        (let ((ht (edb--S :summaries))
              (mtag (edb-tag :markedp dbc-database))
              (htag (edb-tag :hiddenp dbc-database))
              buffer-read-only)
          (erase-buffer)
          (db-message "Computing summary...")
          (db-lmap
           (lambda (record)
             (unless (gethash record ht)
               (puthash record (funcall sum record) ht))
             (dbs-insert-record-summary record mhp mtag htag))
           dbc-database hide "Computing summary...%d")
          (db-message "Computing summary...done")
          ;; get rid of last newline.
          (backward-delete-char 1)
          (set-buffer-modified-p nil)
          (edb--S! :nrecords (edb--1D dbc-database :nrecords))
          (edb--S! :recompute-p nil)
          (edb--S! :index 0))
        (when movep
          (dbs-move-to-proper-record))))))

(defun dbf-update-summary-marks ()
  ;; Update just the marked and hidden summary markings efficiently.
  (when (edb--S :sumbuf)
    (with-current-buffer (edb--S :sumbuf)
      (let ((opoint (point))
            (hidep (not (or (not (edb--S :invisible-hidden-p))
                            (not (edb--S :hide-p)))))
            (mtag (edb-tag :markedp dbc-database))
            (htag (edb-tag :hiddenp dbc-database))
            line hiddenp)
        (unwind-protect
            (let ((sum1lines (edb--1D dbc-database :sum1lines))
                  buffer-read-only)
              (goto-char (point-min))
              (db-lmap
               (lambda (record)
                 (delete-char 1)
                 (insert (if (edb-tagp mtag record) "+" " "))
                 (backward-char 1)
                 (setq line 0
                       hiddenp (and (edb--S :hide-p) (edb-tagp htag record)))
                 ;; Each summary item spans exactly sum1lines screen lines.
                 (while (< line sum1lines)
                   (forward-char 1)
                   (delete-char 1)
                   (insert (if hiddenp "[" " "))
                   (forward-line 1)
                   (incf line)))
               dbc-database
               hidep))
          (goto-char opoint))))))


(defun dbf-update-summary-item (index)
  ;; Update just changes to one record in the summary efficiently.
  (let ((record (aref (edb--1D dbc-database :vov) (1- index)))
        (mtag (edb-tag :markedp dbc-database))
        (htag (edb-tag :hiddenp dbc-database))
        (sum (edb--S :sumfun))
        (sumbuf (edb--S :sumbuf)))
    (when (and sumbuf (or (not (edb-tagp htag record))
                          (not (edb--S :invisible-hidden-p))
                          (not (edb--S :hide-p))))
      (db-in-buffer sumbuf
        (let ((orig (edb--S :index)))
          (unwind-protect
              (let ((ht (edb--S :summaries))
                    buffer-read-only)
                (puthash record (funcall sum record) ht)
                (dbs-move-to-proper-record index)
                ;; assuming at beginning of line
                (delete-region (point)
                               (progn
                                 (forward-line
                                  (edb--1D dbc-database :sum1lines))
                                 (point)))
                (dbs-insert-record-summary record (edb--S :hide-p) mtag htag))
            ;; save old line and column instead.
            (dbs-move-to-proper-record orig)))))))


(defun db-format->lines/sforms (format db indent nlp vheightp)
  ;; Take a format and return a cons of two values: a number and a list.
  ;; The list is list of forms which, when evaluated with variable
  ;; `formatted-record' bound, evaluate to strings; these can be used as
  ;; argumentes to concat, insert, etc.  The number is the number of lines
  ;; occupied by the items when inserted.
  ;;
  ;; Signal an error if any displayspec has nonequal min-height and
  ;; max-height, unless VHEIGHTP is non-nil, in which case the number
  ;; returned is a minimum.  NLP non-nil means add a newline.
  (let ((ph (and (string-match "\\\\\\\\" format) ; backslash place holder
                 (db-unused-char-in-string format)))
        (lines 0)
        results beg end ds minh maxh)
    (when (and indent (> indent 0))
      (setq format (concat (make-string indent 32)
                           (replace-regexp-in-string
                            "\n"
                            (concat "\n" (make-string indent 32))
                            format))))
    (when ph
      (setq format (replace-regexp-in-string
                    (regexp-quote "\\\\")
                    (char-to-string ph)
                    format)))

    (while (string-match db-ds+opts-rx format)
      (setq beg (match-beginning 0)
            end (match-end 0)
            ds (db-dspec<-string format db)
            minh (edb--1ds-min-height ds)
            maxh (edb--1ds-max-height ds))
      ;; fixme: should not be necessary. --ttn
      (unless minh
        (setf minh (setf (edb--1ds-min-height ds) 1)))
      (unless (or vheightp maxh)
        (setf maxh (setf (edb--1ds-max-height ds) minh)))
      (if (or vheightp (= minh maxh))
          (incf lines (1- minh))
        (error "Min- (%s) and max (%s) heights differ in summary displayspec."
               minh maxh))
      (unless (zerop beg)
        (let ((literal (substring format 0 beg)))
          (when ph
            (subst-char-in-string ph ?\\ literal t))
          (push literal results)
          (incf lines (db-count-newlines literal))))
      (push `(db-ds-printed ,ds formatted-record) results)
      (setq format (substring format end)))
    (when nlp (setq format (concat format "\n")))
    (unless (equal "" format)
      (when ph
        (subst-char-in-string ph ?\\ format t))
      (push format results))
    (incf lines (db-count-newlines format))
    (cons lines (nreverse results))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Synching the format and summary buffers
;;;

(defsubst dbs-in-synch-p ()
  (= (with-current-buffer (edb--S :data-display-buffer)
       (edb--S :index))
     (edb--S :index)))

(defsubst dbs-out-of-date-p ()
  (or (edb--S :recompute-p)
      (not (= (edb--1D dbc-database :nrecords)
              (edb--S :nrecords)))))

(defsubst dbf-set-summary-out-of-date-p ()
  (when (edb--S :sumbuf)
    (with-current-buffer (edb--S :sumbuf)
      (edb--S! :recompute-p t))))

(defun dbs-synch-format-with-summary ()
  ;; Ensure that the data display and summary buffers
  ;; have the same current record.
  (cond ((dbs-out-of-date-p) (dbs-synch-summary-with-format))
        ((dbs-in-synch-p))
        (t (let ((sum-index (edb--S :index)))
             (db-in-buffer (edb--S :data-display-buffer)
               (db-select-record sum-index))))))

(defun dbs-synch-summary-with-format ()
  (when (dbs-out-of-date-p)
    (db-in-buffer (edb--S :data-display-buffer)
      (dbf-fill-summary-buffer)))
  ;; If we just did the above, it will clearly be out of synch.
  ;; But it might be even if it wasn't out of date.
  (unless (dbs-in-synch-p)
    (dbs-move-to-proper-record)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Moving about
;;;

(defsubst dbs-goto-nth-summary (n)
  ;; NOTE: If hidden records aren't shown in the summary (and that should be
  ;;       an option), then this is wrong.  And in that case it's better to do
  ;;       relative than absolute motion.
  (goto-line (1+ (* (edb--1D dbc-database :sum1lines) (1- n))))
  (edb--S! :point (point)))

(defun dbs-move-to-proper-record (&optional index)
  "Move point to the summary of the record shown in the format or to INDEX."
  ;; Make no assumptions about the current index.
  (if (with-current-buffer (edb--S :data-display-buffer)
        (or (not (edb--S :hide-p))
            (not (edb--S :invisible-hidden-p))))
      (let ((index (or index (with-current-buffer
                                 (edb--S :data-display-buffer)
                               (edb--S :index)))))
        (dbs-goto-nth-summary index)
        (dbs-set-index index))
    (let ((prev 0)
          (last nil)
          (pidx (or index               ; proper index
                    (with-current-buffer (edb--S :data-display-buffer)
                      (edb--S :index)))))
      (db-lmap
       (lambda (ignored-record)
         (if (<= db-lmap-index pidx)
             (setq prev (1+ prev)
                   last db-lmap-index)
           ;; Past it but still haven't found a nonhidden record.
           (unless last
             (setq prev 1
                   last db-lmap-index))))
       dbc-database
       t)
      ;; If there are no displayed records at all, this will fail.
      (unless (= last pidx)
        (db-message "Record %s does not appear in the summary buffer"
                    pidx))
      (dbs-goto-nth-summary prev)
      (dbs-set-index last))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Movement commands
;;;

(defsubst dbs-forward-record (arg)
  ;; Move point forward ARG records in the summary buffer,
  ;; remembering the start position of that record.
  (goto-char (edb--S :point))
  (db-forward-line-wrapping (* (edb--1D dbc-database :sum1lines) arg))
  (edb--S! :point (point)))

(defun dbs-next-record-ignore-hiding (arg)
  "Go to the ARGth next record, ignoring hiding.
That is, all records, even those which are hidden, are counted."
  (interactive "p")
  (if (not (with-current-buffer (edb--S :data-display-buffer)
             (not (edb--S :invisible-hidden-p))))
      (db-next-record-ignore-hiding arg)
    (dbs-synch-format-with-summary)
    (db-in-buffer (edb--S :data-display-buffer)
      (db-next-record-ignore-hiding arg))
    (dbs-forward-record arg)
    (dbs-set-index (with-current-buffer (edb--S :data-display-buffer)
                     (edb--S :index)))))

(defun dbs-previous-record-ignore-hiding (arg)
  "Go to the ARGth previous record, ignoring hiding.
That is, all records, even those which are hidden, are counted."
  (interactive "p")
  (dbs-next-record-ignore-hiding (- arg)))

(defun dbs-scroll-up ()
  (interactive)
  (scroll-up)
  (db-jump-to-point))

(defun dbs-scroll-down ()
  (interactive)
  (scroll-down)
  (db-jump-to-point))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Summary Mode commands
;;;

(defvar database-summary-mode-map nil
  "Keymap for database summary buffer.")

(unless database-summary-mode-map
  (let ((m (make-sparse-keymap))
        (meta (list meta-prefix-char)))
    (suppress-keymap m)
    (mapc (lambda (key-def)
            (define-key m
              (let ((key (car key-def)))
                (if (consp key)
                    (concat meta (car key))
                  key))
              (cdr key-def)))
          '(("t" . toggle-truncate-lines)

            ;; Moving around in the database
            ("n"      . db-next-record)
            ("p"      . db-previous-record)
            ("\C-n"   . db-next-record)
            ("\C-p"   . db-previous-record)
            ("<"      . db-first-record)
            (">"      . db-last-record)
            (("<")    . db-first-record)
            ((">")    . db-last-record)
            ("j"      . db-jump-to-record)
            (" "      . db-next-screen-or-record)
            ("\177"   . db-previous-screen-or-record)
            (("n")    . dbs-next-record-ignore-hiding)
            (("p")    . dbs-previous-record-ignore-hiding)
            (("\C-n") . db-next-marked-record)
            (("\C-p") . db-previous-marked-record)

            ;; Exiting summary mode
            ("e" . dbs-edit)
            ("v" . dbs-view)
            ("q" . dbs-quit)
            ("x" . dbs-exit)

            ;; Adding and removing records
            ("a" . db-add-record)
            ("i" . db-add-record)
            ("d" . dbs-delete-record)
            ("k" . dbs-delete-record)
            ("o" . db-output-record-to-db)
            ("c" . db-copy-record)

            ;; Sorting
            ("S" . db-sort)

            ;; Searching commands
            ("s"    . db-search)
            ;;("S"  . db-incremental-search)
            ("\C-s" . db-isearch-forward)
            ("\C-r" . db-isearch-backward)

            ;; Everything else
            ("?"      . describe-mode)
            ("O"      . db-hide-record)
            (("o")    . db-hiding-toggle)
            (("O")    . db-hiding-set)
            (("\C-o") . db-toggle-show-hidden-records)
            ("g"      . db-summary)
            ("h"      . db-summary)
            ("D"      . db-summary)
            ("m"      . db-mark-record)
            ("r"      . db-report)
            ("\C-xr"  . db-revert-database)
            ("\C-v"   . dbs-scroll-up)
            (("v")    . dbs-scroll-down)

            ("\C-x\C-q" . db-toggle-modifiable-p)

            ("\C-c\C-c" . dbs-exit)

            ("b"   . undefined)
            ("f"   . undefined)
            ("l"   . undefined)
            ;;("u" . db-revert-record)
            ("w"   . undefined)
            ("y"   . undefined)
            ("z"   . undefined)))
    (setq database-summary-mode-map m)))


(defun dbs-view ()
  "Manipulate this record in the data display buffer in View mode."
  (interactive)
  (pop-to-buffer (edb--S :data-display-buffer))
  (db-view-mode))

(defun dbs-edit ()
  "Manipulate this record in the data display buffer in Edit mode."
  (interactive)
  (pop-to-buffer (edb--S :data-display-buffer))
  (when (eq 'database-view-mode major-mode)
    (db-first-field)))

(defun dbs-quit ()
  "Quit the summary buffer, and return to its data display buffer.
Delete any windows showing the summary buffer prior to burying it."
  (interactive)
  (let ((ddb (edb--S :data-display-buffer))
        (sum (current-buffer)))
    (delete-windows-on sum)
    (bury-buffer sum)
    (pop-to-buffer ddb)))

(defun dbs-exit ()
  "Exit the summary buffer, and return to its data display buffer.
Delete any windows showing the summary buffer prior to killing it."
  (interactive)
  (let ((ddb (edb--S :data-display-buffer))
        (sum (current-buffer)))
    (delete-windows-on sum)
    (kill-buffer sum)
    (pop-to-buffer ddb)))

(defun dbs-delete-record (&optional force)
  "Delete the current record from the database.
With a prefix argument, doesn't verify."
  (interactive "P")
  (when (or force (y-or-n-p "Delete this record? "))
    (db-in-buffer (edb--S :data-display-buffer)
      (db-delete-record t))
    ;; hope we're at the beginning of the record
    (let ((buffer-read-only nil))
      (kill-line (edb--1D dbc-database :sum1lines))
      (when (eobp)
        (goto-char (point-min))))
    (edb--S! :nrecords (1- (edb--S :nrecords)))
    (db-message "Record deleted")
    (dbs-set-index (with-current-buffer (edb--S :data-display-buffer)
                     (edb--S :index)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Menus
;;;

(defvar database-summary-mode-menu
  '("Database"
    "SUMMARY Mode:"
    ["Update summary"     db-summary t]
    ["Report"             db-report  t]
    "----"
    ("Motion"
     ["jump  to record"  db-jump-to-record t]
     ["last     record"  db-last-record    t]
     ["first    record"  db-first-record   t]
     "---"
     ["next"               db-next-record                 t]
     ["next (screen)"      db-next-screen-or-record       t]
     ["next (marked)"      db-next-marked-record          t]
     ["next (ingore hiding)" db-next-record-ignore-hiding t]
     "---"
     ["prev"               db-previous-record                 t]
     ["prev (screen) "     db-previous-screen-or-record       t]
     ["prev (marked)"      db-previous-marked-record          t]
     ["prev (ingore hiding)" db-previous-record-ignore-hiding t]
     "---"
     ["Isearch Backward" db-isearch-backward t]
     ["Isearch Forward" db-isearch-forward t]
     )
    "----"
    ["View record" dbs-view t]
    ["Edit record" dbs-edit t]
    ["Delete Record" dbs-delete-record t]
    ["Add Record" db-add-record t]
    ["Mark Record" db-mark-record t]
    ["Hide Record" db-hide-record t]
    "----"
    ("Hiding"
     ["Hiding    on/off" db-hiding-toggle             t]
     ["Hiding hide/show (in summary)" db-toggle-show-hidden-records t]
     ["Un-hide      all" db-unhide-all                  t]
     ["Un-mark      all" db-unmark-all                  t]
     ["Mark   un-hidden" db-mark-unhidden-records      t]
     ["Hide   un-marked" db-hide-unmarked-records       t]
     )
    "----"
    ["Sort     database" db-sort                t]
    ["Revert   database" db-revert-database t]
    ["Save     database" db-save-database       t]
    ["Write    database" db-write-database-file t]
    "----"
    ["Quit" dbs-quit t]
    ["Exit" dbs-exit t]
    )
  "Menu for Database Summary mode.")

;; 'ignored for SYMBOL argument was giving me trouble.
;; Does this work in Lucid Emacs?
(easy-menu-define ignored
  database-summary-mode-map
  "ignored-doc-string"
  database-summary-mode-menu)

;;; db-summary.el ends here
