;;; db-isbn.el --- part of EDB, the Emacs database

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

;; ISBN type for EDB.
;; An ISBN number uniquely identifies a book; it is a string, one of
;;  * null string (unknown ISBN)
;;  * a dash (this book has not been assigned an ISBN)
;;  * a 13-character string computed of numbers, dashes, and X.
;;    The last digit is a check digit.


;;; Code:

(provide 'db-isbn)

;;;
;;; Display type
;;;

(edb-define-displaytype 'isbn 'string-or-nil
  :max-height 1)

;;;
;;; Field type
;;;

(edb-define-recordfieldtype 'isbn 'string-or-nil
  :type                'isbn
  :default-value       ""
  :help-info           "International Standard Book Number"
  :constraint-function 'db-isbn-constraint)

;;;
;;; Constraints
;;;

(defun db-isbn-constraint (val rec fno db)
  "Check an isbn."
  (unless (stringp val)
    (error "ISBN val %s should be a string." val))
  ;; Return t if all the tests succeed.
  (or (string-equal val "")
      (string-equal val "-")
      (progn
        (unless (= (length val) 13)
          (error "Bad length %d for ISBN %s; should be 13"
                 (length val) val))
        (unless (string-match "\\`[0-9]+-[0-9]+-[0-9]+-[0-9xX]\\'" val)
          (error "Malformed ISBN %s" val))
        ;; Verify the check digit.
        (let ((check-digit
               ;; From _Numbers, Groups and Codes_ by J.F. Humphreys and
               ;; M. Y. Prest, Cambridge University Press, Avon: 1989. page
               ;; 233.  Example: A well-known example of error-correction is
               ;; provided by the ISBN (International Standard Book Number) of
               ;; published books. This is a sequence of nine digits
               ;; a1a2...a9, where each ai is one of the numbers 0,1,...,9,
               ;; together with a check digit which is one of the symbols
               ;; 0,1,2,3,4,5,6,7,8,9, or X (with X representing 10). This
               ;; last digit is included so as to give a check that the
               ;; previous 9 digits have been correctly transcribed, and is
               ;; calculated as follows.  Form the integer 10a1 + 9a2 + 8a3
               ;; +... +2a9 and reduce this modulo 11 to obtain an integer b
               ;; between 1 and 11 inclusive. The check digit is obtained by
               ;; subtracting b from 11.
               (let ((factor 10)
                     (checksum 0)
                     (index 0)
                     digit)
                 ;; factor is the polynome factor for each digit. Since we
                 ;; already checked the format of the isbn, we don't need to
                 ;; do extensive tests here. Factor begins at 10 and goes down
                 ;; to 2 at the last (non-dash) digit excluding the check
                 ;; digit. checksum is the check sum and index is the current
                 ;; digit in the isbn string.
                 (while (>= factor 2)
                   ;; Extract the digit
                   (setq digit (aref val index))
                   (incf index)
                   ;; If the digit is not a dash then it's a part of the
                   ;; polynome.
                   (unless (= digit ?-)
                     (incf checksum (* factor (- digit ?0)))
                     (decf factor)))

                 ;; Last part of the computation.
                 (setq checksum (- 11 (% checksum 11)))
                 ;; Hack due to difference in the modulo function.
                 (when (= checksum 11)
                   (setq checksum 0))

                 ;; Convert to a string, if the sum is 10 then the string is
                 ;; "X".  The string is the value of the computation.
                 (if (= checksum 10)
                     "X"
                   (number-to-string checksum)))))
          (unless (string= check-digit (upcase (substring val 12 13)))
            (error "Check digit should be %s in ISBN %s" check-digit val)))
        t)))

;;; db-isbn.el ends here
