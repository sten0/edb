;;; database.el --- EDB, the Emacs database

;; Copyright (C) 2004-2017 Thien-Thi Nguyen

;; Keywords: EDB, database, forms
;; Version: 1.33
;; Release-Date: 2017-06-17

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

;; EDB is a flexible, customizable database program for Emacs.

;;; Code:

(when load-file-name
  (add-to-list 'load-path (expand-file-name
                           "edb" (file-name-directory
                                  load-file-name))))

(require 'edbcore)

(defconst edb-version "1.33")

(defun edb-version ()
  "Return a string describing the version of EDB that is running.
When called interactively, display the string in the echo area."
  (interactive)
  (let ((v (format "EDB %s of 2017-06-17" edb-version)))
    (when (called-interactively-p 'interactive)
      (message "%s" v))
    v))

(defun edb-interact (control data)
  "Open a connection to the CONTROL (.edb) and DATA files.
This should be behave roughly similarly to calling `db-find-file'
on the DATA file.

CONTROL may be a buffer whose contents are to be parsed as a .edb file.
DATA may be nil, in which case the EDB searches for a file whose stem is
the same as CONTROL's (or CONTROL's `buffer-file-name' if CONTROL is a
buffer), with extension one of \".data\", \".dat\", or \"\" (empty string),
in that order."
  (interactive
   (cl-flet
       ((r (p1 p2) (let ((filename (read-file-name
                                    (format "%s file (type RET %s): "
                                            p1 p2)
                                    nil "")))
                     (unless (string= "" filename)
                       filename))))
     (list (or (r "Control" "for this buffer")
               (current-buffer))
           (r "Data" "to have EDB search for it"))))
  (let* ((conn (edb--connect control))
         (bufferp-control (bufferp control))
         (c-type (if bufferp-control
                     'buffer
                   'file))
         (c-name (if bufferp-control
                     (buffer-name control)
                   control))
         (c-fname (if bufferp-control
                      (buffer-file-name control)
                    control))
         (ddb-spec (edb--1format-buffer<-connection conn))
         (inherent (edb--with-callable-connection conn
                     (conn :data)))
         (inh-name (when inherent (or c-fname c-name)))
         priorp database data-display-buffer)
    (when inherent
      (if data
          (db-message "Ignoring inherent data, using instead: %s" data)
        (setq data (plist-get inherent :records)))
      (plist-put inherent :records nil))
    (unless data
      (setq data (when c-fname
                   (let ((stem (file-name-sans-extension c-fname)))
                     (cl-flet
                         ((try (ext) (let ((f (concat stem ext)))
                                       (when (file-exists-p f)
                                         f))))
                       (or (try ".data")
                           (try ".dat")
                           (try "")))))))
    (unless data
      (kill-buffer ddb-spec)
      (error "Could not find data source"))
    (when (and (setq database (db-find-read-in-database
                               (if inherent inh-name data)))
               (not (edb--1D database :ddbufs)))
      (setq database nil))
    (when (and database ddb-spec)
      (let ((bufs (edb--1D database :ddbufs)))
        (while bufs
          (if (eq ddb-spec (with-current-buffer (car bufs)
                             (edb--S :ddb-spec)))
              (setq data-display-buffer (car bufs)
                    bufs nil)
            (setq bufs (cdr bufs))))))
    (if database
        (setq priorp t)
      (setq database (db-read-database-file (if inherent
                                                (cons inh-name data)
                                              data)
                                            ddb-spec)))
    (unless data-display-buffer
      (setq data-display-buffer (car (edb--1D database :ddbufs))))
    (switch-to-buffer data-display-buffer)
    (edb--S! :ddb-spec ddb-spec)
    (setq dbc-database database)
    (if priorp
        (message "(Re-using established connection.)")
      (when inherent
        (edb--mputhash
         (edb--rinit (list :edb1 :1singles database) :inherent
                     :size 7 :test 'eq)
         :control-type c-type
         :control-name c-name
         :control-fname c-fname
         :seqw (plist-get inherent :seqw)
         :start-box (cdr (plist-member inherent :tb-start))
         :finish-box (cdr (plist-member inherent :tb-finish)))
        (rename-buffer (concat (if (eq 'file c-type)
                                   (file-name-nondirectory c-name)
                                 c-name)
                               " (inherent)")))
      (db-first-record))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Autoloads
;;;

;;; db-rdb.el
(autoload 'db-rdb-setup "db-rdb"
  "Prepare EDB to read files in RDB format." t)

;;; db-sort.el
(autoload 'database-sort "db-sort"
  "Sort and return DATABASE, which is also side-effected." t)
(autoload 'database-sort-interface "db-sort")

;;; db-two-dbs.el
(autoload 'db-process-two-databases "db-two-dbs")
(autoload 'db-merge "db-two-dbs"
  "Merge two read-in databases." t)
(autoload 'databases-compatible "db-two-dbs")

;;; db-search.el
(autoload 'db-parse-match-pattern "db-search") ; should be called first
(autoload 'db-print-match-pattern "db-search")
(autoload 'db-match "db-search")

;;; db-tagged.el
(autoload 'db-tagged-setup "db-tagged"
  "Prepare EDB to read files in tagged format." t)

;;; lisp/edb-meta.el
(autoload 'edb-meta "edb-meta" "Summarize EDB state in a new buffer." t)

;;; lisp/edb-1int-to-single.el
(autoload 'edb-1int-to-single "edb-1int-to-single"
  "Translate EDB 1.x \"internal format\" to a \"single\" schema-schema."
  t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; One-time setup
;;;

(when (string-match "Lucid" emacs-version)
  (put 'emacs-version 'edb-running-lemacs t)
  (load "db-lemacs"))

(add-hook 'kill-buffer-hook 'db-kill-buffer-hook)

(provide 'database)

;;; database.el ends here
