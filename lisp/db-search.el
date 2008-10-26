;;; db-search.el --- part of EDB, the Emacs database

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

;;; Code:

(defvar database-search-mode-map (copy-keymap database-edit-mode-map)
  "Keymap for database data display buffer in edit mode.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Searching primitives
;;;

;;;###autoload
(defun db-parse-match-pattern (string ds)
  ;; These connectives and prefixes are listed in order of precedence.
  ;; [That order is rather unconventional.  Also, no grouping. --ttn]
  ;; The latter prefixes used to be preceded by [ \t]* and followed by [ \t]+;
  ;; for instance, (defvar dbm-<-prefix "^[ =t]*<[ \t]+").
  (let ((c-and "[ \t]+and[ \t]+")
        (c-or  "[ \t]+or[ \t]+")
        (p-not "^[ \t]*not[ \t]+")
        (p-<   "^<")
        (p->   "^>")
        (p-=   "^="))
    (flet ((try (x) (string-match x string))
           (sub (x) (db-parse-match-pattern x ds))
           (p1o (x) (db-callconvert (edb--1ds-display->actual ds)
                                    x nil nil nil))
           (b () (substring string 0 (match-beginning 0)))
           (e () (substring string (match-end 0))))
      (cond ((try c-and) `(db-match-and ,(sub (b)) (sub (e))))
            ((try c-or)  `(db-match-or  ,(sub (b)) (sub (e))))
            ;; no infix connectives in string
            ((try p-not) `(db-match-not ,(sub (e))))
            ((try p-<)   `(db-match-<   ,(p1o (e))))
            ((try p->)   `(db-match->   ,(p1o (e))))
            ((try p-=)   `(db-match-=   ,(p1o (e))))
            (t (db-callconvert
                (or (edb--1ds-match-display->actual ds)
                    (edb--1ds-display->actual ds))
                string
                nil nil nil))))))

;;;###autoload
(defun db-print-match-pattern (pat ds)
  (flet ((sub (x) (db-print-match-pattern x ds))
         (p1o (x) (db-callconvert (edb--1ds-actual->display ds)
                                  x nil nil)))
    (let* ((type (when (listp pat) (car pat)))
           (one (and type (cadr pat)))
           (two (and type (caddr pat))))
      (case type
        ;; extra space around AND to emphasize its low precedence
        (db-match-and (concat (sub one) "  AND  " (sub two)))
        (db-match-or  (concat (sub one)   " OR "  (sub two)))
        (db-match-not (concat "NOT " (sub one)))
        (db-match-<   (concat   "< " (p1o one)))
        (db-match->   (concat   "> " (p1o one)))
        (db-match-=   (concat   "= " (p1o one)))
        ;; pat was not a list or the type wasn't recognized
        (t (db-callconvert
            (or (edb--1ds-match-actual->display ds)
                (edb--1ds-actual->display ds))
            pat
            nil nil))))))

;;;###autoload
(defun db-match (pat targ rs)
  (flet ((m (x) (db-match x targ rs))
         (rso (x) (funcall (db-rs-ordfunc rs) x targ))
         (rsm (x) (funcall (edb--1rs-match-function rs) x targ)))
    (if (listp pat)
        (let ((one (cadr pat))
              (two (caddr pat)))
          (case (car pat)
            (db-match-and (and (m one) (m two)))
            (db-match-or  (or  (m one) (m two)))
            (db-match-not (not (m one)))
            (db-match-<   (=  1 (rso one)))
            (db-match->   (= -1 (rso one)))
            (db-match-=   (=  0 (rso one)))
            (t            (rsm pat))))
      (rsm pat))))

(provide 'db-search)

;;; db-search.el ends here
