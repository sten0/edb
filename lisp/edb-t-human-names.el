;;; edb-t-human-names.el

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

;; Names for human beings
;;
;; These variables and functions used to be in db-types.el,
;; without the "edb-t-human-names:" prefix.
;;
;; TODO: Internationalize (remove/reduce English bias).

;;; Code:

;;
;; Last names
;;

(defun edb-t-human-names:order-last-names (n1 n2)
  "Return -1, 0, or 1 depending on whether last name N1 is lexographically
less than, equal to, or greater than N2."
  ;; Note stringency of equality test:  capitalization and spacing matter.
  (cond ((equal n1 n2)
         0)
        ((string-lessp (edb-t-human-names:canonicalize-name n1)
                       (edb-t-human-names:canonicalize-name n2))
         -1)
        (t
         1)))

(defun edb-t-human-names:canonicalize-name (s)
  ;; Remove spaces and quotation marks, and ignore capitalization.
  (let ((result ""))
    (while (string-match "[ ']+" s)
      (setq result (concat result (substring s 0 (match-beginning 0)))
            s (substring s (match-end 0))))
    (downcase (concat result s))))


;;
;; First names
;;

(defconst edb-t-human-names:nicknames
  '(("Abraham" "Abe")
    ("Charles" "Charlie")
    ("David" "Dave")
    ("Elizabeth" "Beth" "Liz")
    ("Gerald" "Gerry")
    ("James" "Jamey" "Jamie")
    ("Leonard" "Lenny")
    ("Michael" "Mike")
    ("Richard" "Dick" "Dicky" "Rick" "Ricky" "Rico")
    ("Robert" "Bob")
    ("Theodore" "Ted"))
  "List of nicknames which are not prefixes of the full name.
It is by no means comprehensive.")

(defun edb-t-human-names:same-first-name-p (n1 n2)
  (or (edb-t-human-names:nicknamep n1 n2)
      (edb-t-human-names:nicknamep n2 n1)))

(defun edb-t-human-names:order-first-names (n1 n2)
  (let ((m1 (db-string-split-first-word n1))
        (m2 (db-string-split-first-word n2)))
    (cond ((or
            (edb-t-human-names:same-first-name-p (car m1) (car m2))
            (edb-t-human-names:same-first-name-p n1 n2))
           0)
          ((string-lessp (car m1) (car m2))
           -1)
          (t
           1))))

(defun edb-t-human-names:nicknamep (nickname fullname)
  (or (string-match (concat "^" (regexp-quote nickname)) fullname)
      (member nickname (cdr (assoc (car (db-string-split-first-word fullname))
                                   edb-t-human-names:nicknames)))))


;;
;; Full names
;;

(defun edb-t-human-names:standardize-name (name)
  ;; remove leading honorifics
  (when (string-match (concat "^\\(\\(Dr\\|Mrs?\\|Prof\\|1?Lt\\)\\.?"
                              "\\|Miss\\|Ensign\\|Doctor\\) ") name)
    (setq name (substring name (match-end 0))))
  ;; remove trailing gubbish
  (setq name (substring
              name 0 (string-match ",? \\(PhD\\|MD\\|Esq\\.?\\|Esquire\\)$"
                                   name)))
  ;; add periods after initials
  (while (string-match "\\(^\\| \\)[A-Z]\\([ ,]\\|$\\)" name)
    (setq name (concat (substring name 0 (match-beginning 2))
                       "."
                       (substring name (match-beginning 2)))))
  name)

(defvar edb-t-human-names:jr-assoc-list ; preferred suffixes
  '(("3d" . "3rd")))

(defun edb-t-human-names:name->name-jr (name)
  ;; Return (suffixless-name suffix).
  (if (string-match ",? +\\(jr\\.?\\|sr\\.?\\|3r?d\\|[45]th\\|I+\\)$" name)
      (list (substring name 0 (match-beginning 0))
            (let ((jr (substring name (match-beginning 1))))
              (or (cdr (assoc (downcase jr) edb-t-human-names:jr-assoc-list))
                  jr)))
    (list name "")))

(defun edb-t-human-names:name->first-last-jr (fullname)
  ;; Return (fname lname jr).
  (let* ((jr (edb-t-human-names:name->name-jr
              (edb-t-human-names:standardize-name fullname)))
         (ls (db-string-split-last-word (car jr) "\\(La\\|de\\) [A-Z-]+")))
    (append ls (cdr jr))))


(provide 'edb-t-human-names)

;;; edb-t-human-names.el ends here
