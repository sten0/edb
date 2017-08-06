;;; system.el                                                | need-trim

;; Copyright (C) 2005-2017 Thien-Thi Nguyen

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

;;; Code:

(require 'cl)
(require 'easymenu)
(require 'help-fns)

;;; !cfg: featurep-cl-macs
(eval-and-compile (require 'cl-macs))

;;; cfg: with-selected-window
;; snarfed 2005-01-18 from GNU Emacs subr.el (CVS revision 1.432);
;; modified to call `select-window' with only one arg
(defmacro with-selected-window (window &rest body)
  "Execute the forms in BODY with WINDOW as the selected window.
The value returned is the value of the last form in BODY.
This does not alter the buffer list ordering.
This function saves and restores the selected window, as well as
the selected window in each frame.  If the previously selected
window of some frame is no longer live at the end of BODY, that
frame's selected window is left alone.  If the selected window is
no longer live, then whatever window is selected at the end of
BODY remains selected.
See also `with-temp-buffer'."
  (declare (indent 1) (debug t))
  ;; Most of this code is a copy of save-selected-window.
  `(let ((save-selected-window-window (selected-window))
         ;; It is necessary to save all of these, because calling
         ;; select-window changes frame-selected-window for whatever
         ;; frame that window is in.
         (save-selected-window-alist
          (mapcar (lambda (frame) (list frame (frame-selected-window frame)))
                  (frame-list))))
     (unwind-protect
         (progn (select-window ,window)
                ,@body)
       (dolist (elt save-selected-window-alist)
         (and (frame-live-p (car elt))
              (window-live-p (cadr elt))
              (set-frame-selected-window (car elt) (cadr elt))))
       (if (window-live-p save-selected-window-window)
           (select-window save-selected-window-window)))))

;;; cfg: number-sequence
;; snarfed 2005-10-27 from GNU Emacs subr.el (CVS revision 1.484)
(defun number-sequence (from &optional to inc)
  "Return a sequence of numbers from FROM to TO (both inclusive) as a list.
INC is the increment used between numbers in the sequence and defaults to 1.
So, the Nth element of the list is \(+ FROM \(* N INC)) where N counts from
zero.  TO is only included if there is an N for which TO = FROM + N * INC.
If TO is nil or numerically equal to FROM, return \(FROM).
If INC is positive and TO is less than FROM, or INC is negative
and TO is larger than FROM, return nil.
If INC is zero and TO is neither nil nor numerically equal to
FROM, signal an error.

This function is primarily designed for integer arguments.
Nevertheless, FROM, TO and INC can be integer or float.  However,
floating point arithmetic is inexact.  For instance, depending on
the machine, it may quite well happen that
\(number-sequence 0.4 0.6 0.2) returns the one element list \(0.4),
whereas \(number-sequence 0.4 0.8 0.2) returns a list with three
elements.  Thus, if some of the arguments are floats and one wants
to make sure that TO is included, one may have to explicitly write
TO as \(+ FROM \(* N INC)) or use a variable whose value was
computed with this exact expression.  Alternatively, you can,
of course, also replace TO with a slightly larger value
\(or a slightly more negative value if INC is negative)."
  (if (or (not to) (= from to))
      (list from)
    (or inc (setq inc 1))
    (when (zerop inc) (error "The increment can not be zero"))
    (let (seq (n 0) (next from))
      (if (> inc 0)
          (while (<= next to)
            (setq seq (cons next seq)
                  n (1+ n)
                  next (+ from (* n inc))))
        (while (>= next to)
          (setq seq (cons next seq)
                n (1+ n)
                next (+ from (* n inc)))))
      (nreverse seq))))

;;; cfg: help-function-arglist
;; snarfed 2005-12-07 from GNU Emacs help-fns.el (CVS revision 1.80)
(defun help-function-arglist (def)
  ;; Handle symbols aliased to other symbols.
  (if (and (symbolp def) (fboundp def)) (setq def (indirect-function def)))
  ;; If definition is a macro, find the function inside it.
  (if (eq (car-safe def) 'macro) (setq def (cdr def)))
  (cond
   ((byte-code-function-p def) (aref def 0))
   ((eq (car-safe def) 'lambda) (nth 1 def))
   ((and (eq (car-safe def) 'autoload) (not (eq (nth 4 def) 'keymap)))
    "[Arg list not available until function definition is loaded.]")
   (t t)))

(unless (require 'cl-lib nil t)
  ;; Add other ‘cl-lib’ time travellers here.
  (defalias 'cl-loop 'loop)
  (defalias 'cl-labels 'labels)
  ;; The following are arguably sick and wrong.  So it goes.
  (defalias 'cl-flet* 'labels)
  (defalias 'cl-flet 'flet))

;;; system.el ends here
