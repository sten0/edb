;;; db-nosetf.el --- EDB programming interface that does not use setf

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

;; Internally, EDB uses `defstruct' which normally provides for each slot a
;; TYPE-SLOT function which is setf-able.  Thus:
;;
;;        (TYPE-SLOT object)            ; getter
;;  (setf (TYPE-SLOT object) value)     ; setter
;;
;; EDB 1.x defines aliases for the setter of the form:
;;
;;  (TYPE-set-SLOT object value)
;;
;; This file provides all those aliases, which although unused internally,
;; are documented in the EDB Manual, and will continue to be supported for
;; the 1.x series.  2.x is another matter, however...

;;; Code:

(defsubst database-set-print-name (db val) (setf (database-print-name db) val))
(defsubst database-set-fieldnames (db val) (setf (database-fieldnames db) val))
(defsubst database-set-recordfieldspecs (db val) (setf (database-recordfieldspecs db) val))
(defsubst database-set-field-priorities (db val) (setf (database-field-priorities db) val))
(defsubst database-set-read-record-from-region (db val) (setf (database-read-record-from-region db) val))
(defsubst database-set-write-region-from-record (db val) (setf (database-write-region-from-record db) val))
(defsubst database-set-sub-fieldsep-string (db val) (setf (database-sub-fieldsep-string db) val))
(defsubst database-set-sub-recordsep-string (db val) (setf (database-sub-recordsep-string db) val))
(defsubst database-set-substitutions (db val) (setf (database-substitutions db) val))
(defsubst database-set-locals (db val) (setf (database-locals db) val))
(defsubst sepinfo-set-pre-first-string (si val) (setf (sepinfo-pre-first-string si) val))
(defsubst sepinfo-set-pre-first-regexp (si val) (setf (sepinfo-pre-first-regexp si) val))
(defsubst sepinfo-set-pre-first-regexp-submatch (si val) (setf (sepinfo-pre-first-regexp-submatch si) val))
(defsubst sepinfo-set-sep-string (si val) (setf (sepinfo-sep-string si) val))
(defsubst sepinfo-set-sep-regexp (si val) (setf (sepinfo-sep-regexp si) val))
(defsubst sepinfo-set-sep-regexp-submatch (si val) (setf (sepinfo-sep-regexp-submatch si) val))
(defsubst sepinfo-set-sep-function (si val) (setf (sepinfo-sep-function si) val))
(defsubst sepinfo-set-post-last-string (si val) (setf (sepinfo-post-last-string si) val))
(defsubst sepinfo-set-post-last-regexp (si val) (setf (sepinfo-post-last-regexp si) val))
(defsubst sepinfo-set-post-last-regexp-submatch (si val) (setf (sepinfo-post-last-regexp-submatch si) val))

(provide 'db-nosetf)

;;; db-nosetf.el ends here
