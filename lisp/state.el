;;; state.el

;; Copyright (C) 2005,2006,2007,2008 Thien-Thi Nguyen

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

;;; Code:

(defvar edb--global-state (make-hash-table :test 'eq) "EDB internals.")

(defsubst edb--arrrr! (loc)
  (let ((par edb--global-state))
    (while loc
      (setq par (gethash (car loc) par)
            loc (cdr loc)))
    par))

(defun edb--rinit (loc key &rest init)
  (assert loc)
  (let ((par (edb--arrrr! loc)))
    ;; Return the new hash table.
    (puthash key (apply 'make-hash-table init) par)))

(defun edb--rget (loc key)
  (let ((par (edb--arrrr! loc)))
    (gethash key par)))

(defun edb--rput (loc key value)
  (let ((par (edb--arrrr! loc)))
    (puthash key value par)))

(defun edb--rforget (loc key)
  (let* ((par (edb--arrrr! loc))
         (v (gethash key par)))
    (when (hash-table-p v)
      (clrhash v))
    (remhash key par)))

:further-init edb--global-state         ; (munge this line and suffer, fool!)

;;;---------------------------------------------------------------------------
;;; Access utilities

(defun edb--mputhash (ht &rest init)
  "Initialize hashtable HT with key1 value1 key2 value2... of INIT."
  (while init
    (puthash (pop init)                 ; DWR: relies on L->R OOE
             (pop init)
             ht)))

(defun edb--hashcollect (what table)
  (let (acc)
    (maphash (case what
               (:keys (lambda (k v) (setq acc (cons k acc))))
               (:vals (lambda (k v) (setq acc (cons v acc))))
               (:cons (lambda (k v) (setq acc (acons k v acc)))))
             table)
    acc))

(defmacro edb--define-child-hash (get key par init)
  `(progn
     (defun ,(intern (format "edb--%s" get)) (,key property)
       (gethash property (gethash ,key ,par)))
     (defun ,(intern (format "edb--%s!" get)) (,key property value)
       (puthash property value (gethash ,key ,par)))
     (defun ,(intern (format "edb--meta%s" get)) (,key &optional command)
       (let* ((par ,par) (me (or (gethash ,key par) (puthash ,key ,init par))))
         (case command
           (:forget (clrhash me) (remhash ,key par))
           (t me))))))

;;;---------------------------------------------------------------------------
;;; Tags

(defun edb-tag (name db)
  "Return the tag object named NAME associated with database DB."
  (edb--1D db name))

(defun edb-tagp (tag record)
  "Return t if RECORD has its TAG set."
  (gethash record tag))

(defun edb-tagx (tag record)
  "Set TAG for RECORD."
  (puthash record t tag))

(defun edb-tag- (tag record)
  "Clear TAG for RECORD."
  (remhash record tag))

;;; state.el ends here
