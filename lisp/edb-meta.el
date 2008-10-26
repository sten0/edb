;;; edb-meta.el

;; Copyright (C) 2006,2007,2008 Thien-Thi Nguyen

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

;; This file provides the single command `edb-meta',
;; as well as the feature by the same name.

;;; Code:

(eval-when-compile (require 'cl))

(defun edb-meta ()
  "Summarize EDB state in a new buffer, and switch to it.
Display the version number, and `features' related to EDB in parens.
Next, display a tree of key/value pairs starting w/ `edb--global-state'.
Display each key in `font-lock-builtin-face', abutted to its value.
Recursion occurs through hash table values.

Here is a list of how objects of various types are represented:
  hash-table                -- #
  marker                    -- m
  vector                    -- vLEN
  string                    -- \"\"LEN
  symbol                    -- NAME (as key), or 'NAME (as value)
  buffer                    -- <NAME>
  (lambda ...)              -- (f)
  compiled-function         -- {f}
  cons                      -- ()LEN
  monolithic-mess structure -- [mm: \"NAME\"]
  displayspec structure     -- [ds]
  recordfieldtype structure -- [rs]

LEN is the length of the vector or string or list.  `nil' is never
shown as a value; absence of a displayed value means the value is `nil'.
Unrecognized objects display as \"???\" in `font-lock-warning-face'.

Next, display a list of global variables that have non-nil default
bindings (either as a variable or as a function).  This list is taken
from the property `globals-to-check' of symbol `edb-meta'."
  (interactive)
  (switch-to-buffer (get-buffer-create "*EDB Meta*"))
  (buffer-disable-undo)
  (erase-buffer)
  (setq major-mode 'edb-meta-mode
        mode-name "EDB Meta"
        truncate-lines t)
  (insert "EDB " edb-version)
  (let (acc)
    (dolist (f features)
      (when (string-match "\\(^database$\\)\\|\\(^edb-\\|^db-\\)"
                          (symbol-name f))
        (push f acc)))
    (insert (format " %s\n" acc)))
  (flet ((w/face (string face)
                 (propertize string 'face face))
         (fo (o &optional keyp)
             (case (type-of o)
               (vector (cond ((edb--1ds-p o) "[ds]")
                             ((edb--v1-rs-p o) "[rs]")
                             ((= (length (get 'edb--v1-monolithic-mess
                                              'cl-struct-slots))
                                 (length o))
                              (format "[mm%s]" (if (stringp (aref o 0))
                                                   (format ": %S" (aref o 0))
                                                 "")))
                             (t (format "v%d" (length o)))))
               (symbol (cond ((not o) "")
                             ((or keyp (keywordp o)) (symbol-name o))
                             (t (format "'%s" o))))
               (hash-table "#")
               (integer (format "%d" o))
               (compiled-function "{f}")
               (marker "m")
               (string (format "%S%d" "" (length o)))
               (buffer (format "<%s>" (buffer-name o)))
               (cons (if (eq 'lambda (car o))
                         "(f)"
                       (format "()%d" (safe-length o))))
               (t (w/face "???" 'font-lock-warning-face))))
         (fk (k)
             (if (symbolp k)
                 (symbol-name k)
               (fo k)))
         (d (level header ht)
            (let (kids)
              (insert "\n" (make-string (* 2 level) 32) header)
              (maphash (lambda (k v)
                         (let ((k-str (fo k t)))
                           (insert " " (w/face k-str 'font-lock-builtin-face)
                                   (fo v))
                           (when (hash-table-p v)
                             (push (cons k-str v) kids))))
                       ht)
              (insert "\n")
              (dolist (kid (nreverse kids))
                (d (1+ level) (car kid) (cdr kid))))))
    (d 0 (fo 'edb--global-state t) edb--global-state))
  (insert "\n")
  (dolist (sym (get 'edb-meta 'globals-to-check))
    (let ((var (and (boundp sym) (symbol-value sym)))
          (fun (and (fboundp sym) (symbol-function sym))))
      (when (or var fun)
        (insert (if var "(v)" "")
                (if fun "(f)" "")
                "\t" (symbol-name sym)
                "\n"))))
  (goto-char (point-min))
  (set (make-local-variable 'revert-buffer-function)
       (lambda (ignore-auto-IGNORED noconfirm-IGNORED)
         (message "Redoing edb-meta...")
         (edb-meta)
         (message "Redoing edb-meta... done")))
  (buffer-enable-undo))

;; Some of the following are no longer supported but we check for them anyway;
;; if they are bound, that indicates the program needs to be updated.
(put 'edb-meta 'globals-to-check
     '(db-before-read-hooks
       db-after-read-hooks
       db-view-mode-hooks
       db-edit-mode-hooks
       database-summary-mode-hooks
       dbf-enter-field-hook
       db-tagged-rrfr-hooks
       db-tagged-wrfr-before-hooks
       db-tagged-wrfr-after-hooks
       ))

(provide 'edb-meta)

;;; edb-meta.el ends here
