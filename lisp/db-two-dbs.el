;;; db-two-dbs.el --- part of EDB, the Emacs database

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

;; Support for actions on two databases.

;;; Code:


(eval-when-compile (require 'cl))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Process two databases
;;;

;;;###autoload
(defun db-process-two-databases (db1 db2 lone1 lone2 corr &optional orderer)
  (db-with-orderer db1
    (unless (database-ordered-p db1 orderer)
      (db-message "Sorting %S..." (database-print-name db1))
      (database-sort db1 orderer))
    (unless (database-ordered-p db2 orderer)
      (db-message "Sorting %S..." (database-print-name db2))
      (database-sort db2 orderer))

    ;; Perhaps check for identical keys here.

    (db-message "Databases are properly ordered")
    (let* ((rno1 (edb--1D db1 :nrecords))
           (vov1 (edb--1D db1 :vov))
           (zix1 0)
           (rno2 (edb--1D db2 :nrecords))
           (vov2 (edb--1D db2 :vov))
           (zix2 0)
           (done1 (zerop rno1))
           (done2 (zerop rno2))
           r1 r2 r-order)
      (while (not (or done1 done2))
        (setq r1 (aref vov1 zix1)
              r2 (aref vov2 zix2)
              r-order (funcall orderer r1 r2))
        (cond ((= -1 r-order)
               (funcall lone1 r1)
               (incf zix1)
               (when (= zix1 rno1) (setq done1 t)))
              ((= 1 r-order)
               (funcall lone2 r2)
               (incf zix2)
               (when (= zix2 rno2) (setq done2 t)))
              ((zerop r-order)
               (funcall corr r1 r2)
               (incf zix1)
               (incf zix2)
               (when (= zix1 rno1) (setq done1 t))
               (when (= zix2 rno2) (setq done2 t)))
              (t
               (error "Bad result %s from orderer." r-order))))
      (while (not done1)
        (funcall lone1 (aref vov1 zix1))
        (incf zix1)
        (when (= zix1 rno1) (setq done1 t)))
      (while (not done2)
        (funcall lone2 (aref vov2 zix2))
        (incf zix2)
        (when (= zix2 rno2) (setq done2 t)))
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Merge
;;;

;; Want to display both records in full and, for each differing field,
;; ask how to set the merged database: from one, from the other, or by
;; entering something particular.  It should also be possible to edit
;; the merged record before proceeding to the next one.

;; Should check for the consistency of the two databases.  Should permit
;; conversion for one or both databases as part of this procedure.

;;;###autoload
(defun db-merge ()
  "Merge two databases chosen with completion among read-in databases."
  (interactive)
  (let ((avail (mapcar (lambda (db)
                         (cons (database-print-name db) db))
                       (edb--1all-known-databases)))
        db1 db2 db3 buf1 buf2 buf3)
    (cond ((< (length avail) 2)
           (error "Less than two read-in databases."))
          ((= 2 (length avail))
           (setq db1 (cdar avail)
                 db2 (cdadr avail)))
          (t
           ;; Could check that a data display buffer exists.
           (setq db1 (assoc (completing-read
                             "First database to merge? [? for options] "
                             avail nil t nil)
                            avail))
           (setq avail (delq db1 avail))
           (setq db1 (cdr db1))
           ;; fixme: err if not compatabile.
           (setq db2 (cdr (assoc (completing-read
                                  "Second database to merge? [? for options] "
                                  avail nil t nil)
                                 avail)))))
    (setq buf1 (car (edb--1D db1 :ddbufs))
          buf2 (car (edb--1D db2 :ddbufs)))
    (setq db3 (let ((result (edb--copy-v1-monolithic-mess db1)))
                (callf2 concat "Copy of " (database-print-name db1))
                (edb--1D! result :vov nil) ; hmmm
                (edb--1D! result :nrecords 0)
                ;; Should this go after db-choose-format-file?
                (edb--1D! result :file (concat (edb--1D db1 :file) "-COPY"))
                (edb--1D! result :ddbufs
                          (list (db-setup-data-display-buffer
                                 (db-choose-format-file result nil nil)
                                 result
                                 t)))
                result)
          buf3 (car (edb--1D db3 :ddbufs)))
    (setf (database-print-name db3)
          (concat "Merge of `" (database-print-name db1)
                  "' and `" (database-print-name db2) "'"))
    (let ((idx1 (with-current-buffer buf1 (edb--S :index)))
          (idx2 (with-current-buffer buf2 (edb--S :index))))
      (delete-other-windows)
      (switch-to-buffer buf1)
      (split-window-vertically (/ (window-height) 2))
      (other-window 1)
      (switch-to-buffer buf2)
      (split-window-vertically)
      (other-window 1)
      (switch-to-buffer buf3)
      (set-buffer buf1)
      (db-process-two-databases
       db1 db2
       (lambda (r) (database-add-record r db3))
       (lambda (r) (database-add-record r db3))
       (lambda (r1 r2)
         (database-add-record (db-merge-records r1 r2 db3 buf1 buf2 buf3)
                              db3)))
      (db-in-buffer buf1 (db-jump-to-record idx1))
      (db-in-buffer buf2 (db-jump-to-record idx2))
      (db-in-buffer buf3 (db-jump-to-record 1))))
  (message "Done merging."))


(defun db-merge-records (r1 r2 db buf1 buf2 buf3)
  (if (equal r1 r2)
      r1
    (set-buffer buf1)
    (db-display-record r1 t)
    (set-buffer buf2)
    (db-display-record r2 t)
    (set-buffer buf3)
    (let* ((nfields (length (database-fieldnames db)))
           (r3 (make-vector nfields nil))
           (ds1-all (with-current-buffer buf1 (edb--S :displayspecs)))
           (ds2-all (with-current-buffer buf2 (edb--S :displayspecs)))
           (cffuncs-slice (db-rs-slice db 'edb--1rs-common-form-function))
           v1 v2 ds1 ds2 cf fname)
      (dotimes (fno nfields)
        (setq ds1 (aref ds1-all fno)
              ds2 (aref ds2-all fno)
              cf (or (aref cffuncs-slice fno) 'identity)
              v1 (funcall cf (aref r1 fno))
              v2 (funcall cf (aref r2 fno)))
        (if (equal v1 v2)
            (aset r3 fno v1)
          (setq fname (db-fname<-fno fno db))
          ;; Problem: nil might not be valid in some places.
          ;; For now, ignore that.
          (db-display-record r3 t fno)
          ;; Does no constraint checking; is this the right thing to do?
          ;; Help for one-char-question is not entirely satisfactory here.
          (aset r3 fno
                (cond ((y-or-n-p
                        (format "Use first value for %s field? [%s] "
                                fname
                                (db-callconvert
                                 (edb--1ds-actual->display ds1)
                                 v1
                                 nil nil)))
                       v1)
                      ((y-or-n-p
                        (format "Use second value for %s field? [%s] "
                                fname
                                (db-callconvert
                                 (edb--1ds-actual->display ds2)
                                 v2
                                 nil nil)))
                       v2)
                      (t
                       (unless (equal (edb--1ds-display->actual ds1)
                                      (edb--1ds-display->actual ds2))
                         (error "Unequal display->actual functions"))
                       (db-callconvert
                        (edb--1ds-display->actual ds1)
                        (read-from-minibuffer
                         (format "Enter value for %s field: " fname))
                        nil nil nil))))))
      r3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compare
;;;

;;;###autoload
(defun databases-compatible (db1 db2)
  "Return t if the database records have the same field names and type,
nil otherwise."
  ;; Should eventually check types as well.
  (let ((rv t)
        (fno 0)
        (count1 (length (database-recordfieldspecs db1)))
        (count2 (length (database-recordfieldspecs db2)))
        (types1-slice (db-rs-slice db1 'edb--1rs-type))
        (types2-slice (db-rs-slice db2 'edb--1rs-type)))
    (when (= count1 count2)
      (while (and rv (< fno count1))
        (setq rv (eq (aref types1-slice fno)
                     (aref types2-slice fno)))
        (incf fno))
      rv)))

(defun database-compare-hack (db1 db2)
  ;; Check that fieldnames and types are the same.
  (unless (databases-compatible db1 db2)
    (error "Incompatible databases."))
  (let* ((name1 (or (database-print-name db1) "First Database"))
         (name2 (or (database-print-name db2) "Second Database"))
         (loners1 (get-buffer-create (concat "Loners for " name1)))
         (loners2 (get-buffer-create (concat "Loners for " name2)))
         (discrep (get-buffer-create (concat "Discrepancies between "
                                             name1 " and " name2))))
    (dolist (buf (list loners1 loners2 discrep))
      (with-current-buffer buf (erase-buffer)))
    (cl-flet*
        ((print-r (r db)
                  (let ((names (database-fieldnames db)))
                    (princ "\n")
                    (dotimes (fno (length (database-fieldnames db)))
                      (princ (format "%s:  %s\n" (aref names fno)
                                     (aref r fno))))))
         (print-l1 (r) (db-in-buffer loners1 (print-r r db1)))
         (print-l2 (r) (db-in-buffer loners2 (print-r r db2))))
      (db-process-two-databases
       db1 db2 'print-l1 'print-l2
       (lambda (r1 r2)
         ;; We already know that order-function considers the two
         ;; records the same; now we need to check whether any of
         ;; their fields differ and if so, report it.
         (unless (equal r1 r2)
           (db-in-buffer discrep
             (let ((all-rs (edb--1D db1 :elaborated-rfspecs))
                   fname rs ofunc v1 v2)
               (princ "\n")
               (dotimes (fno (length (database-fieldnames db1)))
                 (setq fname (db-fname<-fno fno db1)
                       ofunc (db-rs-ordfunc (aref all-rs fno))
                       v1    (aref r1 fno)
                       v2    (aref r2 fno))
                 (cond ((equal v1 v2)
                        (princ (format "  %s:  %s\n" fname v1)))
                       ((and ofunc (zerop (funcall ofunc v1 v2)))
                        (princ (format ". %s:  %s\n. %s:  %s\n"
                                       fname v1 fname v2)))
                       (t
                        (princ (format "* %s:  %s\n* %s:  %s\n"
                                       fname v1 fname v2)))))))))))))

(provide 'db-two-dbs)

;;; db-two-dbs.el ends here
