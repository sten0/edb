;;; edb-fixes-1.21.el -- Keeps bugfixes to EDB 1.21 in a central place.

;; Simply (require 'edb-fixes-1.21) and your EDB 1.21 will be fixed.

;; Written by Matt Swift <swift@alum.mit.edu>.
;;
;; The original copyright of EDB probably applies to this file, since it
;; contains EDB functions with only minor changes.  I disclaim any additional
;; copyright.

;; For each load file F, define `fix-edb-F-1.21'.  Following that, ensure that
;; it is executed only after F is loaded.  This way, you can `require'
;; edb-fixes-1.21 either before or after requiring EDB files.

(provide 'edb-fixes-1.21)

;;; Version: $Id: edb-fixes-1.21.el,v 1.1 2000/04/25 08:15:49 kawamura Exp $

(defun fix-edb-db-rep-1.21 ()
  ;; Fixes a bug in db-rep for the case when REVERSEP is non-nil and the items
  ;; compared are the same.
  (defun recordfieldspec-sort-function (recordfieldspec &optional reversep)
    "Return a sort function for records described by RECORDFIELDSPEC.
If optional argument REVERSEP is non-nil, then the sort function goes in
the opposite order.
If the sort-fn slot of the appropriate recordfieldspec of database doesn't
contain one, one is made up on the fly from the order-fn slot.
If the order-fn slot is also empty, the resulting function always returns
nil, indicating that it is not the case that the first argument is less
than the second."
    (let ((sort-fn (recordfieldspec-sort-fn recordfieldspec)))
      (if sort-fn
	  (if reversep
	      (` (lambda (value1 value2)
		   (not ((, sort-fn) value1 value2))))
	    ;;	    (` (lambda (value1 value2)
	    ;;		 ((, sort-fn) value2 value1)))
	    sort-fn)
	(order->sort (recordfieldspec-order-fn recordfieldspec) reversep))))

  ;; This redefinition fixes a bug in db-rep.
  (defmacro mapfields-macro (body record database)
    "Execute BODY for each field of RECORD, a record of DATABASE,
with variables `mapfields-field' and `mapfields-index' bound."
    (` (let ((mapfields-index 0)
	     (mapfields-record (, record))
	     (mapfields-fields (database-no-of-fields (, database)))
	     mapfields-field)
	 (while (< mapfields-index mapfields-fields)
	   (setq mapfields-field
		 (record-field-from-index mapfields-record mapfields-index))
	   ;; BODY must be a single form since it's the first argument
	   (, body)
	   (setq mapfields-index (1+ mapfields-index))))))
  )

(if (featurep 'db-rep)
    (funcall 'fix-edb-db-rep-1.21)
  (eval-after-load "db-rep" '(fix-edb-db-rep-1.21)))

(defun fix-edb-db-convert-1.21 ()
  ;; This fixes two bugs in the function.
  (defun db-canonicalize-creation-method (creation-method database)
    (cond
     ;; Literals
     ((null creation-method)
      ;; I don't think this clause is ever entered, but just in case...
      ;; The null test must precede the symbolp test, below.
      '(literal ""))
     ((stringp creation-method)
      (list 'literal creation-method))
     ((creation-method-literal-p creation-method)
      creation-method)
     ;; Field references
     ((symbolp creation-method)
      (or (fieldname->fieldnumber creation-method database)
	  (error "%s isn't a fieldname in database %s."
		 creation-method (database-print-name database))))
     ((numberp creation-method)
      (if (and (> creation-method 0)
	       (< creation-method (database-no-of-fields database)))
	  creation-method
	(error "%d isn't a valid field number in database %s."
	       creation-method (database-print-name database))))
     ;; Functions
     ((not (consp creation-method))
      (error "Ill-formed creation method %s." creation-method))
     (t
      (let ((function-spec (car creation-method))
	    function args result-no)
	(if (consp function-spec)
	    (setq function (car function-spec)
		  args (cdr function-spec)
		  result-no (car (cdr creation-method)))
	  (setq function function-spec
		args (cdr creation-method)
		result-no nil))
	(if (not (fboundp function))
	    (error "%s has no function definition." function))
	(setq args (mapcar (function (lambda (name-or-number)
				       (if (numberp name-or-number)
					   name-or-number
					 (fieldname->fieldnumber name-or-number database))))
			   args))
	(list (cons function args) result-no)))))
  )

(if (featurep 'db-convert)
    (funcall 'fix-edb-db-convert-1.21)
  (eval-after-load "db-convert" '(fix-edb-db-convert-1.21)))

;;;; EMACS FILE VARIABLES
;;;
;;; Local Variables:
;;; outline-regexp: ";;;[;]+"
;;; End:
;;;
