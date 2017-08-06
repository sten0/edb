;;; db-format.el --- part of EDB, the Emacs database

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

;; Displaying and editing database records.

;;; Code:

(require 'easymenu)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;

;;
;; Location in the format (current field info)
;;

;; All of these variables include "this-" in their names.

(defsubst dbf-this-field-name (this-ds)
  (and this-ds (db-fname<-fno (edb--1ds-record-index this-ds)
                              dbc-database)))

(defsubst dbf-this-field-text ()
  ;; The text actually in the buffer.
  (buffer-substring-no-properties (edb--S :fbeg) (dbf-this-field-end-pos)))

(defsubst dbf-set-this-field-text (field-text)
  "Make the format display FIELD-TEXT in the current field."
  ;; Set the text actually in the buffer.
  (let ((beg (edb--S :fbeg)))
    (delete-region beg (dbf-this-field-end-pos))
    (goto-char beg))
  (insert field-text))

(defsubst dbf-this-field-modified-p ()
  (buffer-modified-p))
(defsubst dbf-set-this-field-modified-p (arg)
  (set-buffer-modified-p arg))

(:edb1 :1moving-mark (make-marker))


;;
;; The displayed record
;;

;;;###safe-file-local-variable
(defvar dbf-set-this-record-modified-function nil
  "A function called when the current record is marked as modified.
The function takes no arguments and its return value is ignored.
It is called after `:original' is copied to `:under' and after
`:utkmodp' is set to t.")

(defsubst dbf-set-this-record-modified-p (arg)
  "Set the value of `:utkmodp' to ARG.
If ARG is non-nil and `:utkmodp' is nil, also do the necessary
record-copying and call `dbf-set-this-record-modified-function'."
  (let ((cur (edb--S :utkmodp)))
    (edb--S! :utkmodp arg)
    (cond ((and arg (not cur))
           (db-copy-r2r (edb--S :original) (edb--S :under))
           (edb--1run-hooks 'dbf-set-this-record-modified-function)))))

(defsubst dbf-displayed-record ()
  "Return the record currently displayed in this data display buffer.
This is `:under' if `:utkmodp' is non-nil and `:original' otherwise."
  (if (edb--S :utkmodp)
      (edb--S :under)
    (edb--S :original)))

(defvar dbf-redisplay-entire-record-p nil
  "T if the whole record needs to be redisplayed.
This is often set by change functions.")


;;
;; Hooks
;;

;;; Minor mode hooks

(defvar db-view-mode-hooks nil
  "Function or list of functions called when Database View mode is entered.")

(defvar db-edit-mode-hooks nil
  "Function or list of functions called when Database Edit mode is entered.")

;;; Movement hooks

;;;###safe-file-local-variable
(defvar dbf-before-display-record-function nil
  "A function called before a record is displayed.
The function takes one argument, the record.

This is a good place to put calls to `db-change-format'.  Depending on
your function's implementation, however, you may silently override any user
calls to that function.")

;;;###safe-file-local-variable
(defvar dbf-enter-field-hook nil
  "A function (of no arguments) called whenever a display field is entered.")

;;; Change hooks

;;;###safe-file-local-variable
(defvar dbf-first-change-function nil
  "A function called the first time a record field is modified, or nil.
The function takes the fieldname and the old and new values as arguments,
and returns t if the record should be redisplayed.")

;;;###safe-file-local-variable
(defvar dbf-every-change-function nil
  "A function called whenever a record field is modified, or nil.
The function takes the fieldname and the old and new values as arguments,
and returns t if the record should be redisplayed.")

(defun dbf-set-change-function (fieldname function)
  "Set the change function for FIELDNAME to FUNCTION in the current database.
FUNCTION takes the fieldname and the old and new values as arguments,
and returns t if the record should be redisplayed."
  (aset (edb--S :change-functions)
        (gethash fieldname (edb--1D dbc-database :nm2no))
        function))

;;;###safe-file-local-variable
(defvar dbf-after-record-change-function nil
  "Function called whenever changes to a record are recorded semi-permanently
by `dbf-process-current-record-maybe'.  For convenience, the function
takes the record as an argument, which is guaranteed to be `:under'.
Its return value is ignored.")


;;
;; The format
;;

;; Some variables local to the data display buffer don't need to be changed
;; when the display format changes.  The ones appearing below do.

;;;###safe-file-local-variable
(defvar dbf-format-name-spec-alist nil
  "Association list of format names and format specifiers.
Each format name is an arbitrary string.
A format specifier is a filename or format file specifier, which is
a list of values for format variables.
The user sets the format specifier to a filename, and after that format file
has been read, EDB replaces the filename with a list of values for format
variables, so that the file need not be read again.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Constants
;;;

(defconst db-ds+opts-rx (concat
                         "\\\\"
                         "\\([[:alpha:]][[:alnum:]<>,=-]*\\)" ; 1
                         "\\(\\\\ \\)?"))                     ; 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Abstraction
;;;

;;
;; Displayspec
;;

;; Besides `record-index' all other information has to do with display only.

(defstruct (edb--1ds
            (:type vector) :named
            (:constructor edb--make-1ds)
            (:copier edb--copy-1ds))

  record-index                          ; zero-based backref

  ;; size and shape
  indent
  min-width
  max-width
  min-height                            ; default 1
  max-height                            ; default 1
  min-bytes
  max-bytes

  ;; other display info
  truncation-display-action
  padding-action
  actual->display
  display->actual
  ;; Is this where these belong?  Well, it lets me not make a new displayspec
  ;; for them...
  match-actual->display
  match-display->actual

  ;; editing info
  truncation-editing-action
  (reachablep t))

(put 'edb--1ds 'kwidx                   ; poor man's defsetf --ttn
     (let ((idx 0))
       (mapcar (lambda (ent)
                 (cons (intern (format ":%s" (symbol-name (car ent))))
                       (incf idx)))
               (cdr (edb--struct-slot-info 'edb--1ds)))))


;;
;; Optspecinfo
;;

;; An optspecinfo tells how to interpret optional parameters to a
;; display field specification.  An optspecinfo is a three-element list of
;;  * param-name: a string
;;  * settor: function taking a displayspec and a value and setting a slot
;;  * opt-param->value: a function converting the optional parameter (that
;;    is, the string that follows the equal sign) into the actual value.

(defconst db-optspec-list
  (mapcar (lambda (x)
            `(,(nth 0 x)
              ,(cl-flet
                   ((dset (frag) (let ((f (intern (format "edb--1ds-%s"
                                                          frag))))
                                   `(lambda (ds val)
                                      (setf (,f ds) val)))))
                 ;; Settor-or-accessor is either a settor function, a
                 ;; slotname, or a list of slotnames.  In the latter two
                 ;; cases, it's first converted into a settor.
                 (let ((settor-or-accessor (nth 1 x)))
                   (cond ((functionp settor-or-accessor)
                          settor-or-accessor)
                         ((symbolp settor-or-accessor)
                          (dset settor-or-accessor))
                         (t
                          `(lambda (displayspec value)
                             ,@(mapcar (lambda (frag)
                                         `(,(dset frag) displayspec value))
                                       settor-or-accessor))))))
              ,(nth 2 x)))
          '(("indent" indent (lambda (x) t))
            ("noindent" indent (lambda (x) nil))

            ("width" (min-width max-width) db-string->number)
            ("min-width" min-width db-string->number)
            ("max-width" max-width db-string->number)
            ("length" (min-width max-width) db-string->number)
            ("min-length" min-width db-string->number)
            ("max-length" max-width db-string->number)
            ("height" (min-height max-height) db-string->number)
            ("min-height" min-height db-string->number)
            ("max-height" max-height db-string->number)
            ("bytes" (min-bytes max-bytes) db-string->number)
            ("min-bytes" min-bytes db-string->number)
            ("max-bytes" max-bytes db-string->number)

            ("trunc-display" truncation-display-action intern)
            ("truncation-display-action" truncation-display-action intern)
            ("padding-action" padding-action intern)
            ("right-justify" padding-action (lambda (x) 'db-pad-left))
            ("actual->display" actual->display intern)
            ("a->d" actual->display intern)
            ("display->actual" display->actual intern)
            ("d->a" display->actual intern)

            ;; match-actual->display and match-display->actual,
            ;; fields 13 and 14  [??? --ttn]

            ("truncation-editing-action" truncation-editing-action intern)
            ("trunc-edit" truncation-editing-action intern)
            ("reachable" reachablep (lambda (x) t))
            ("unreachable" reachablep (lambda (x) nil)))))


(defun db-pad-left (min-width rep rep-length)
  (concat (make-string (- min-width rep-length) 32) rep))

(defun db-pad-right (min-width rep rep-length)
  (concat rep (make-string (- min-width rep-length) 32)))


(defun db-callconvert (convert fieldtext &rest args)
  (let ((res (if convert
                 (condition-case err
                     ;; Try calling it with one arg.
                     (funcall convert fieldtext)
                   (wrong-number-of-arguments
                    ;; Call it with all args.
                    (apply convert fieldtext args))
                   (error
                    ;; Otherwise resignal; "while t" makes this work
                    ;; under the debugger (see, eg, the code for the
                    ;; "error" function).
                    (while t
                      (signal (car err) (cdr err)))))
               fieldtext)))
    (if (= 3 (length args))
        ;; display->actual
        res
      ;; actual->display
      (if (stringp res)
          res
        "<ERROR>"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macros
;;;

;; If the user has deleted some of the leading spaces, they'll be restored.
;; Don't do anything about tabs, not even untabifying.

(defun db-unindentify (text)
  (let ((amt (dbf-this-field-indent)))
    (if amt
        (replace-regexp-in-string
         (concat "\n" (db-space-maybe-rx amt)) "\n" text)
      text)))

(defun db-space-maybe-rx (n)
  ;; Return a regexp matching N or fewer occurrences of the space character.
  ;; If N is nil, return the empty string, which is sometimes not a regexp you
  ;; want to search for by itself.
  (if n
      (if (> n
             ;; Emacs 19's regexp routines fix bugs in the Emacs 18 and Lucid
             ;; Emacs versions, but are sometimes much slower.  For deeply
             ;; indented fields, this can result in very slow editing.  We
             ;; disable some error-checking and correction for fields indented
             ;; more than 8 characters.
             8)
          (make-string (or n 0) 32)
        (let ((result (make-string (* 2 n) 32)))
          (setq n (1- (* 2 n)))
          (while (> n 0)
            (aset result n ??)
            (decf n 2))
          result))
    ""))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mode selection
;;;


(defvar database-view-mode-menu)        ; fixme: document. --ttn
(defvar database-edit-mode-menu)        ; fixme: document. --ttn

(defalias 'database-view-mode 'db-view-mode) ; for C-h m
(defun db-view-mode (&optional arg)
  "Switch to Database View mode.
With an argument, toggle between Database View and Database Edit modes."
  (interactive "P")

  (cond ((and arg (eq 'database-view-mode major-mode))
         (db-edit-mode))
        ;; If already in Database View mode, don't do anything.
        ((not (eq 'database-view-mode major-mode))
         (dbf-process-field-maybe t)
         (setq major-mode 'database-view-mode
               mode-name "Database View")
         (use-local-map database-view-mode-map)
         (when (edb--rget '(:edb1 :bprops) (current-buffer)) ; hmmm
           (edb--S! :this-fidx nil)
           (edb--S! :this-ds nil))
         (setq buffer-read-only t)
         (goto-char (point-min))
         (dbf-set-this-field-modified-p nil)
         (easy-menu-remove database-edit-mode-menu)
         (easy-menu-add database-view-mode-menu)
         (edb--1run-hooks 'db-view-mode-hooks)
         (force-mode-line-update))))

(defalias 'database-edit-mode 'db-edit-mode) ; for C-h m
(defun db-edit-mode (&optional arg)
  "Switch to Database Edit mode.
With an argument, toggle between Database Edit and Database View modes."
  (cond ((not (db-data-display-buffer-p))
         (error "Only call this in database mode."))
        ((and arg (eq 'database-edit-mode major-mode))
         (db-view-mode))
        (t
         (setq major-mode 'database-edit-mode
               mode-name "Database Edit")
         (use-local-map database-edit-mode-map)
         (if (edb--1D dbc-database :modifiable-p)
             (setq buffer-read-only nil)
           (message
            "%s" (substitute-command-keys
                  (concat "Database is not modifiable; "
                          "change that with \\[db-toggle-modifiable-p]"))))
         (easy-menu-add database-edit-mode-menu)
         (easy-menu-remove database-view-mode-menu)
         (edb--1run-hooks 'db-edit-mode-hooks)
         (force-mode-line-update))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Movement in the format
;;;

(defun db-parse-buffer-error (format-string &rest args)
  ;;(debug nil (apply 'format format-string args))
  (db-emergency-restore-format)
  (db-message "%s; %s"
              "I was confused about where I was"
              "changes to the field might have been lost"))

(defun db-next-line-or-field (arg)
  "Move to ARGth next line.  If that would move out of the current field,
move to the closest field, but not the current one, wrapping if necessary."
  (interactive "p")
  (let ((goal-column (current-column))
        goal-line)
    ;; Determine goal line.
    (db-forward-line-wrapping arg)
    (db-jump-to-point)
    (setq goal-line (db-current-line))
    ;; Move to proper column.
    (move-to-column goal-column)
    (db-jump-to-point)
    ;; Off goal line: move back and as near to the goal column as possible.
    (when (> (db-current-line) goal-line)
      (db-previous-field-internal 1)
      (goto-char (dbf-this-field-end-pos)))))

(defun db-move-to-field-exact (arg)
  "Move to the ARGth field in the display.  Ignores reachablep."
  (db-first-field-internal t)
  (db-next-field-internal arg t)
  (edb--1run-hooks 'dbf-enter-field-hook))

(defun db-next-field (arg)
  "Move to ARGth next reachable field, wrapping if necessary.
When called interactively, ARG defaults to 1."
  (interactive "p")
  (dbf-process-field-maybe t)
  (goto-char (edb--S :fbeg))
  (if (> arg 0)
      (db-next-field-internal arg)
    (db-previous-field-internal (- arg)))
  ;; We have just moved to a new field, which certainly isn't modified yet.
  (dbf-set-this-field-modified-p nil)
  (edb--1run-hooks 'dbf-enter-field-hook))

(defun db-next-field-internal (arg &optional exact)
  ;; Arg should be positive.  Assumes point is at the beginning of the field.
  ;; If EXACT is non-nil, reachablep is ignored.
  (let* ((displayspecs (edb--S :displayspecs))
         (iftxt (edb--S :iftxt))
         (this-fidx (edb--S :this-fidx))
         (shown (edb--S :shown))
         (len (length displayspecs)))
    (while (> arg 0)
      (if (not (db-skip-string-forward (aref shown this-fidx)))
          (db-parse-buffer-error
           "Didn't find field %s text `%s'."
           this-fidx (aref shown this-fidx))
        (edb--S! :this-fidx (incf this-fidx))
        (decf arg)
        (when (= len this-fidx)
          (unless (db-skip-string-forward (aref iftxt len))
            (db-parse-buffer-error
             "Didn't find trailing text `%s' after field %s."
             (aref iftxt len)
             (1- len)))
          (edb--S! :this-fidx (setq this-fidx 0))
          (goto-char (point-min)))
        (unless (db-skip-string-forward (aref iftxt this-fidx))
          (db-parse-buffer-error
           "Didn't find field separator `%s' before field %s."
           (aref iftxt this-fidx) this-fidx))
        ;; Implement reachablep.
        ;; fixme: handle infinite loop. --ttn
        (unless (or exact (edb--1ds-reachablep
                           (aref displayspecs this-fidx)))
          (incf arg))))
    (edb--S! :this-ds (aref displayspecs this-fidx))
    (edb--S! :fbeg (point)))
  (buffer-disable-undo)
  (buffer-enable-undo)

  (when (looking-at (regexp-quote (aref (edb--S :shown)
                                        (edb--S :this-fidx))))
    (let ((end-of-match (match-end 0)))
      (set-marker (edb--S :fend)
                  (if (= end-of-match (point-max))
                      nil
                    (1+ end-of-match))
                  (current-buffer)))))

(defun db-previous-line-or-field (arg)
  "Move to ARGth previous line.  If that would move out of the current field,
move to the closest field, but not the current one, wrapping if necessary."
  (interactive "p")
  (let ((goal-column (current-column))
        (vacated-line (db-current-line))
        this-line)
    (db-forward-line-wrapping (- arg))
    (move-to-column goal-column)
    (db-jump-to-point)
    (setq this-line (db-current-line))
    (when (= this-line vacated-line)
      ;; We moved to a line containing no field, so db-jump-to-point
      ;; put us in the field following point; ie, one on the line in
      ;; which we started.  This is not the desired behavior.
      ;; Get to a line containing a field.
      (db-previous-field-internal 1)
      (goto-char (dbf-this-field-end-pos))
      ;; Go to the correct column.
      (move-to-column goal-column)
      ;; Avoid getting dumped back into this field.
      (goto-char (min (point) (dbf-this-field-end-pos)))
      ;; And end up there.
      (db-jump-to-point))))

(defun db-previous-field (&optional arg)
  "Move to ARGth previous reachable field, wrapping if necessary.
When called interactively, ARG defaults to 1."
  (interactive "p")
  (dbf-process-field-maybe t)
  (goto-char (edb--S :fbeg))
  (if (> arg 0)
      (db-previous-field-internal arg)
    (db-next-field-internal (- arg)))
  (dbf-set-this-field-modified-p nil)
  (edb--1run-hooks 'dbf-enter-field-hook))

(defun db-previous-field-internal (arg)
  ;; Arg should be positive.  Assume point is at the beginning of the field.
  ;; pb = previous inter-field-text beginning
  (let* ((displayspecs (edb--S :displayspecs))
         (iftxt (edb--S :iftxt))
         (this-fidx (edb--S :this-fidx))
         (shown (edb--S :shown))
         (len (length displayspecs))
         (pb (marker-position (edb--S :fend))))
    (if pb
        (decf pb))
    (while (> arg 0)
      (unless (db-skip-string-backward (aref iftxt this-fidx))
        (db-parse-buffer-error
         "Didn't find field separator `%s' before field %s."
         (aref iftxt this-fidx) this-fidx))
      (setq pb (point))
      (edb--S! :this-fidx (decf this-fidx))
      (decf arg)
      (when (< this-fidx 0)
        (edb--S! :this-fidx (setq this-fidx (1- len)))
        (goto-char (point-max))
        (if (db-skip-string-backward (aref iftxt len))
            (setq pb (point))
          (db-parse-buffer-error
           "Didn't find trailing text `%s' after field %s."
           (aref iftxt len) this-fidx)))
      (unless (db-skip-string-backward (aref shown this-fidx))
        (db-parse-buffer-error
         "Didn't find field %s text `%s'."
         this-fidx (aref shown this-fidx)))
      ;; Implement reachablep.
      ;; fixme: handle infinite loop. --ttn
      (unless (edb--1ds-reachablep (aref displayspecs this-fidx))
        (incf arg)))
    (edb--S! :this-ds (aref displayspecs this-fidx))
    (edb--S! :fbeg (point))
    (buffer-disable-undo)
    (buffer-enable-undo)
    (set-marker (edb--S :fend)
                (and pb
                     (if (or (= 1 pb)
                             (= (point-max) pb))
                         nil
                       (1+ pb))))))

(defun db-first-field-internal (&optional exact)
  ;; Move to first field.  Optional EXACT means ignore reachability.
  (if (edb--S :this-fidx)
      (dbf-process-field-maybe t)
    (db-edit-mode))
  (edb--S! :this-fidx 0)
  ;; We need this even if field-index was nil, because someone might have
  ;; sneakily moved point.  (In fact, this is called after point is moved
  ;; via mouse.)
  (goto-char (point-min))
  (let ((iftxt (edb--S :iftxt))
        (this-fidx (edb--S :this-fidx)))
    (unless (db-skip-string-forward (aref iftxt 0))
      (db-parse-buffer-error
       "Didn't find field separator `%s' before field %s."
       (aref iftxt this-fidx) this-fidx))
    (db-next-field-internal 0)
    ;; Implement reachablep.
    (unless (or exact
                (edb--1ds-reachablep
                 (aref (edb--S :displayspecs) this-fidx)))
      (db-next-field-internal 1)))
  (dbf-set-this-field-modified-p nil))

(defun db-first-field ()
  "Move to first field."
  (interactive)
  (db-first-field-internal nil)
  (edb--1run-hooks 'dbf-enter-field-hook))

(defun db-last-field ()
  "Move to last field."
  (interactive)
  (db-first-field-internal nil)
  (db-previous-field 1))

(defun db-scroll-up ()
  "Like scroll-up, but also edits the nearest database field."
  (interactive)
  (scroll-up)
  (db-jump-to-point t))

(defun db-scroll-down ()
  "Like scroll-down, but also edits the nearest database field."
  (interactive)
  (scroll-down)
  (db-jump-to-point t))

(defun db-jump-to-point (&optional quietly)
  "In a data display buffer, move to the field containing or following point.
In a summary buffer, move to the record displayed around point."
  (cond ((db-data-display-buffer-p)
         (let* ((this-fidx (edb--S :this-fidx))
                (beg (and this-fidx (edb--S :fbeg))))
           (unless (and beg (and (<= beg (point))
                                 (<= (point) (dbf-this-field-end-pos))))
             ;; moving outside current field.
             (let ((new-point (point)))
               (set-marker (edb--G :1moving-mark) (point))
               ;; Go back to where we were:
               ;; if we were in a field, get back in it.
               (when this-fidx
                 (goto-char beg))
               (if (and this-fidx
                        (> (marker-position (edb--G :1moving-mark)) (point)))
                   ;; We are in a field and moving forward.
                   (progn
                     (dbf-process-field-maybe t)
                     (goto-char beg))
                 (db-first-field-internal nil))
               ;; If the dbf-process-field-maybe redisplays the entire record,
               ;; the marker gets wiped out (points to the beginning of the
               ;; buffer, because the buffer is cleared and refilled).
               (let ((moving-pos (marker-position (edb--G :1moving-mark))))
                 (unless (= 1 moving-pos)
                   (setq new-point moving-pos)))
               (set-marker (edb--G :1moving-mark) nil)
               (let ((len (length (edb--S :displayspecs))))
                 (while (and (> new-point (dbf-this-field-end-pos))
                             (< (edb--S :this-fidx) (1- len)))
                   ;; The EXACT argument is t so we don't infinite-loop when
                   ;; the last field is unreachable.
                   (db-next-field-internal 1 t)))
               (let ((this-ds (edb--S :this-ds)))
                 (unless (edb--1ds-reachablep this-ds)
                   ;; This message is getting wiped out by the
                   ;; mouse-button-up event.  How can I fix this?
                   ;; Hint:  Transposing the following two statements is
                   ;; not the answer.
                   (unless quietly
                     (db-message "Field `%s' is unreachable"
                                 (db-fname<-fno
                                  (edb--1ds-record-index this-ds)
                                  dbc-database)))
                   (db-next-field-internal 1)))

               (edb--1run-hooks 'dbf-enter-field-hook)
               ;; The max makes sure we're in a field, not beyond it.
               ;; The min is there only for the last field (because we could
               ;; be past it, in which case there's not a following field).
               (goto-char (min (max new-point (edb--S :fbeg))
                               (dbf-this-field-end-pos)))))
           ;; Check not in indentation even if didn't move to a new field.
           (when (let ((amt (dbf-this-field-indent)))
                   (and amt
                        (> amt 0)
                        (db-looking-back-at "^ +")
                        (< (current-column) amt)))
             (db-beginning-of-line-or-field))))
        ((db-summary-buffer-p)
         ;; This is wrong in the presence of hidden directory lines.
         (beginning-of-line)
         (let* ((p (edb--S :point))
                (lines (count-lines p (point)))
                (signed (if (< p (point)) lines (- lines))))
           (goto-char p)
           (dbs-next-record-ignore-hiding
            (/ signed (edb--1D dbc-database :sum1lines)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Movement within a field
;;;

;; These shouldn't be called if not on a field, so they don't check.

(defsubst dbf-this-field-end-pos ()
  (let ((pos (marker-position (edb--S :fend))))
    (if pos
        (1- pos)
      (point-max))))

(defun dbf-this-field-indent ()
  (let ((in (edb--1ds-indent (edb--S :this-ds))))
    (and in (if (numberp in)
                in
              (save-excursion
                (goto-char (edb--S :fbeg))
                (current-column))))))

;;;
;;; Checking
;;;

(defalias 'dbf-inform-outside-field
  ;; Function to call when point attempts to leave a field.
  ;; It should take one argument, a short message string.
  'error)

(defsubst dbf-check-if-outside-field (&optional quietly)
  ;; Move point to beginning of field if it's before that.
  (let ((beg-pos (edb--S :fbeg)))
    (when (< (point) beg-pos)
      (goto-char beg-pos)
      (unless quietly
        (db-message "Beginning of field"))))
  ;; Move point to end of field if it's beyond that.
  (let ((end-pos (dbf-this-field-end-pos)))
    (when (> (point) end-pos)
      (goto-char end-pos)
      (unless quietly
        (dbf-inform-outside-field "End of field.")))))


;;;
;;; Movement
;;;

(defsubst db-beginning-of-field ()
  "Move to the beginning of the current field."
  (interactive)
  (goto-char (edb--S :fbeg)))

(defsubst db-end-of-field ()
  "Move to the end of the current field."
  (interactive)
  (goto-char (dbf-this-field-end-pos)))

(defun db-beginning-of-line-or-field ()
  "Move to the beginning of the current line of the current field.
If invoked twice in succession, move to beginning of field."
  (interactive)
  (if (eq 'db-beginning-of-line-or-field last-command)
      (db-beginning-of-field)
    (beginning-of-line)
    (db-skip-regexp-forward (db-space-maybe-rx (dbf-this-field-indent)))
    (dbf-check-if-outside-field t)))

(defun db-end-of-line-or-field (arg)
  "Move to the end of the current line of the current field.
If invoked twice in succession, move to end of field."
  (interactive "p")
  (if (eq 'db-end-of-line-or-field last-command)
      (db-end-of-field)
    (end-of-line arg)
    (dbf-check-if-outside-field t)))

(defun db-forward-char (arg)
  "Like forward-char, but won't go outside field."
  (interactive "p")
  (if (< arg 0)
      (db-backward-char (- arg))
    (let ((indent (dbf-this-field-indent)))
      (while (> arg 0)
        (if (eobp)
            ;; This is so we get the error "End of field"
            ;; instead of "End of buffer".
            (progn
              (setq arg 0)
              (dbf-inform-outside-field "End of field."))
          (forward-char 1)
          (db-skip-regexp-forward (concat "^" (db-space-maybe-rx indent)))
          (decf arg)))
      (dbf-check-if-outside-field))))

(defun db-backward-char (arg)
  "Like backward-char, but won't go outside field."
  (interactive "p")
  (if (< arg 0)
      (db-forward-char (- arg))
    (let ((indent (dbf-this-field-indent)))
      (while (> arg 0)
        (if (bobp)
            ;; This is so we get the error "Beginning of field"
            ;; instead of "Beginning of buffer".
            (progn
              (setq arg 0)
              (dbf-inform-outside-field "Beginning of field."))
          (let ((rx (concat "^" (db-space-maybe-rx indent)))
                (here (point)))
            (when (re-search-backward rx nil t)
              (if (= here (match-end 0))
                  t
                (goto-char here)
                nil)))
          (backward-char 1)
          (decf arg)))
      (dbf-check-if-outside-field))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Editing
;;;

(defun db-delete-char (arg)
  (interactive "p")
  "Like delete-char, but won't delete outside the field."
  (delete-region (point) (progn (db-forward-char arg) (point))))

(defun db-backward-delete-char (arg)
  (interactive "p")
  "Like delete-backward-char, but won't delete outside the field."
  (delete-region (point) (progn (db-backward-char arg) (point))))

(defun db-forward-word (arg)
  "Like forward-word, but won't go outside field."
  (interactive "p")
  (forward-word arg)
  (dbf-check-if-outside-field))

(defun db-backward-word (arg)
  "Like backward-word, but won't go outside field."
  (interactive "p")
  (db-forward-word (- arg)))

(defun db-copy-region-as-kill (beg end)
  "Save the region as if killed, but don't kill it.
This calls `interprogram-cut-function' (if non-nil)
on the extracted text."
  (interactive "r")
  (let ((text (db-unindentify (buffer-substring beg end))))
    (if (eq last-command 'db-kill-region)
        (kill-append text (< end beg))
      (push text kill-ring)
      (when (> (length kill-ring) kill-ring-max)
        (setcdr (nthcdr (1- kill-ring-max) kill-ring) nil))
      ;; This only needs to be done in the else-clause of the
      ;; parent `if' expression since `kill-append' already DTRT.
      ;; TODO: Replace entire else-clause w/ `kill-new'.
      (when interprogram-cut-function
        (funcall interprogram-cut-function text))))
  (setq this-command 'db-kill-region)
  (setq kill-ring-yank-pointer kill-ring))

(defun db-kill-region (beg end)
  "Kill between point and mark.
The text is deleted but saved in the kill ring.
See `kill-region' for details."
  (interactive "*r")
  (db-copy-region-as-kill beg end)
  (delete-region beg end))

(defun db-kill-word (arg)
  "Like kill-word, but won't delete outside the field."
  (interactive "p")
  (db-kill-region (point) (progn (db-forward-word arg) (point))))

(defun db-backward-kill-word (arg)
  "Like backward-kill-word, but won't delete outside the field."
  (interactive "p")
  (db-kill-word (- arg)))

(defun db-kill-line (arg)
  "Like kill-line, but won't delete outside the field."
  (interactive "p")
  (let ((here (point)))
    (db-end-of-line-or-field arg)
    (when (< (point) (dbf-this-field-end-pos))
      (let ((indent (dbf-this-field-indent)))
        (cond (kill-whole-line
               (db-skip-regexp-forward
                (concat "[ \t]*\n" (db-space-maybe-rx indent))))
              ((and (eolp) (= indent (current-column)))
               (forward-char (1+ indent))))))
    (db-kill-region here (point))))

(defun db-kill-to-end ()
  "Kill from point to the end of the current field."
  (interactive)
  (db-kill-region (point) (dbf-this-field-end-pos)))

(defun db-newline (arg)
  "Insert a newline.  Will not make the current field too tall.
If current field's maximum height is 1 line, move to the next field instead."
  (interactive "p")
  ;; ignores the argument
  (let ((max-h (edb--1ds-max-height (edb--S :this-ds))))
    (if (or (not max-h)
            (< (count-lines (edb--S :fbeg) (dbf-this-field-end-pos))
               max-h))
        (let ((indent (dbf-this-field-indent)))
          (newline 1)
          (when indent (insert (make-string indent 32))))
      (if (= 1 max-h)
          (db-next-field 1)
        (db-message "Field is at maximum height already")))))

(defun db-open-line (arg)
  "Insert a newline and leave point before it.
Will not make the current field too tall."
  (interactive "p")
  (let ((here (point)))
    (db-newline arg)
    (goto-char here)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Value processing for fields and records
;;;

(defun dbf-process-field-maybe (set-text-p)
  ;; Set the value of the current record from the current field.
  ;; If arg SET-TEXT-P is non-nil, update the display as well.
  ;; Return t if field is unmodified or text is OK; nil otherwise.
  ;; May move point.
  (let ((fno (and (edb--rget '(:edb1 :bprops) (current-buffer))
                  (edb--S :this-fidx)))
        (shown (edb--S :shown)))
    (cond
     ((not fno))
     ((not (dbf-this-field-modified-p)))
     ((or (< (point) (edb--S :fbeg))
          (> (point) (dbf-this-field-end-pos)))
      (db-parse-buffer-error
       "Point was outside (%d) of current field (%d - %d)."
       (point) (edb--S :fbeg) (dbf-this-field-end-pos))
      ;; Return something in case `db-parse-buffer-error' returns.
      ;; TODO: Is this the right return value? --ttn
      nil)
     ((equal (dbf-this-field-text) (aref shown fno))
      ;; Field is unchanged, so mark it unmodified.
      (dbf-set-this-field-modified-p nil))
     (t
      ;; Field has been modified.
      (let* ((this-ds (edb--S :this-ds))
             (this-fname (dbf-this-field-name this-ds))
             (cur (db-callconvert
                   (edb--1ds-display->actual this-ds)
                   (let ((text (dbf-this-field-text)))
                     (db-unindentify text))
                   (aref (dbf-displayed-record) fno)
                   (dbf-displayed-record)
                   fno))
             (idx (edb--1ds-record-index this-ds))
             (old (aref (dbf-displayed-record) idx))
             (under (edb--S :under))
             (saved-modified-p (edb--S :utkmodp)))
        (unless (equal cur old)
          ;; The new value is different from the old.
          (dbf-set-this-record-modified-p t)
          (db-check-constraint cur under idx dbc-database)
          (aset under idx cur)

          (when set-text-p
            (aset shown fno
                  (let ((pr (db-ds-printed this-ds under))
                        (in (edb--1ds-indent this-ds)))
                    (if in
                        (if (numberp in)
                            (replace-regexp-in-string
                             "\n"
                             (concat "\n" (make-string in 32))
                             pr)
                          ;; Why can't I use (dbf-this-field-indent) even here?
                          (if (db-find-char ?\n pr)
                              (error "Don't know how much to indent.")
                            pr))
                      pr))))
          ;; No need to do redisplay before the change-hooks are
          ;; called since the user's version is already onscreen
          ;; and that will be very similar indeed to the display
          ;; text.
          (unless saved-modified-p
            (setq dbf-redisplay-entire-record-p
                  (or (and dbf-first-change-function
                           (funcall dbf-first-change-function
                                    this-fname old cur))
                      dbf-redisplay-entire-record-p)))
          (setq dbf-redisplay-entire-record-p
                (or (and dbf-every-change-function
                         (funcall dbf-every-change-function
                                  this-fname old cur))
                    dbf-redisplay-entire-record-p))
          (setq dbf-redisplay-entire-record-p
                (let ((change-function (aref (edb--S :change-functions) idx)))
                  (or (and change-function
                           (funcall change-function
                                    this-fname old cur))
                      dbf-redisplay-entire-record-p))))
        ;; The text is different; the value may or may not have differed.
        ;; Display the standard representation for this value, which has
        ;; already been computed.
        (when set-text-p
          (unless (dbf-redisplay-entire-record-maybe)
            ;; set-field-text always returns nil
            (dbf-set-this-field-text
             (aref shown fno))))
        (dbf-set-this-field-modified-p t))))))


(defun dbf-redisplay-entire-record-maybe ()
  ;; If `dbf-redisplay-entire-record-p' is non-nil, redisplay current record
  ;; and return t; otherwise return nil.
  (when dbf-redisplay-entire-record-p
    (setq dbf-redisplay-entire-record-p nil)
    (db-emergency-restore-format t)
    t))


(defun dbf-process-current-record-maybe (set-text-p)
  ;; Commit changes to the record being displayed and edited.  If the current
  ;; record (see `dbf-displayed-record') is a modified copy of a database
  ;; record, this copies it back to the original database record, modifying
  ;; the database by side effect.  Return t if successful, nil otherwise.
  ;; SET-TEXT-P non-nil means to also update the display.
  (when (edb--S :index)
    ;; Sets the field unmodified, if appropriate
    (dbf-process-field-maybe set-text-p)
    (when (edb--S :utkmodp)
      ;; Do any programmer-requested checking or postprocessing here.
      ;; This function may err, aborting out of whatever was trying to
      ;; process the current record and do something else.
      (edb--1run-hook-with-arg 'dbf-after-record-change-function
                               (dbf-displayed-record))
      (db-copy-r2r (edb--S :under) (edb--S :original))
      (dbf-update-summary-item (edb--S :index))
      ;; what about hiddenp and markedp? --ttn
      (database-set-modified-p dbc-database t)
      (edb--S! :utkmodp nil)
      (dbf-set-this-field-modified-p nil))
    ;; This function shouldn't have been called on a non-database record; how
    ;; did we get here?  It may not be the case that the info is about to be
    ;; abandoned.
    (or (not (edb--S :utkmodp))
        (y-or-n-p "Abandon the displayed information? ")
        (error "Don't abandon displayed information."))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Undoing changes
;;;

(defun db-revert-field (&optional quietly)
  "Undo any changes made since entering this field.
Replace the onscreen text in this field with that of the underlying record.

A similar effect can be had by invoking \\[advertised-undo] multiple times."
  (interactive)
  (let ((fname (unless quietly (dbf-this-field-name (edb--S :this-ds)))))
    (if (dbf-this-field-modified-p)
        (progn
          (dbf-set-this-field-text
           (aref (edb--S :shown) (edb--S :this-fidx)))
          (dbf-set-this-field-modified-p nil)
          (unless quietly
            (db-message "Reverted field `%s'" fname)))
      (unless quietly
        (db-message "Can't revert field `%s'; no changes since moving onto it"
                    fname)))))

(defun db-revert-record ()
  "Set the record to be the same as the corresponding one in the database.
In other words, undo any changes made since entering this record."
  (interactive)
  (db-revert-field t)
  (if (edb--S :utkmodp)
      (let ((buffer-read-only nil))
        (edb--S! :utkmodp nil)
        (db-display-record (dbf-displayed-record) t)
        (let ((this-fidx (edb--S :this-fidx)))
          (when this-fidx
            (db-move-to-field-exact this-fidx)))
        (db-message "Reverted record"))
    (db-message "Can't revert this record; no changes since selecting it")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Set displayspec from string
;;;


(defun db-dspec<-dtype (type)
  "Return a copy of the displayspec corresponding to string or symbol TYPE.
Return nil if there's no corresponding displayspec."
  (let ((ds (gethash (if (stringp type)
                         (intern type)
                       type)
                     (edb--G :1displaytypes))))
    (when ds (if (symbolp ds)
                 (db-dspec<-dtype ds)
               (edb--copy-1ds ds)))))


(defun db-dspec<-string (s db)
  ;; Assume match-data from `db-ds+opts-rx'.  S is nil if from the buffer.
  (let* ((ls (split-string (match-string 1 s) ","))
         (fname (intern (car ls)))
         (fidx (and db (gethash fname (edb--1D db :nm2no))))
         (type (and fidx (aref (db-rs-slice db 'edb--1rs-type) fidx)))
         ds)
    (when (and db (not fidx))
      (error "%s is not a field or field abbreviation."
             fname))
    (setq ds (db-dspec<-type/opts type (cdr ls)))
    (unless ds
      (error "Type %s in field %d (%s) not recognized."
             type
             fname fidx))
    (setf (edb--1ds-record-index ds) fidx)
    ds))


(defun db-dspec<-type/opts (type opts &optional notype-ok)
  ;; Either TYPE or OPTS (list of strings) must specify a type, unless
  ;; optional argument NOTYPE-OK is specified, in which case an empty
  ;; displayspec may be returned.

  ;; Ordinarily (for instance, when this is being called to parse part of a
  ;; format), NOTYPE-OK should not be specified, so that invalid
  ;; displaytypes aren't created.

  ;; A type in OPTS overrides TYPE.
  (if (not (setq opts (delete "" opts)))
      (if type
          (or (db-dspec<-dtype type)
              (error "No such displaytype as `%s'." type))
        (edb--make-1ds))
    (let (ds)
      ;; set the displayspec
      ;; note tricky sequencing
      (if (setq ds (db-dspec<-dtype (intern (car opts))))
          (pop opts)
        (if type
            (setq ds (db-dspec<-dtype type))
          (error "No type specified in `%s'." opts)))

      (while opts
        (let* ((pair (split-string (pop opts) "="))
               (opt (car pair))
               (val (or (cadr pair) ""))
               (spec (or (assoc opt db-optspec-list)
                         (error "Invalid optional field spec name or type: %s"
                                opt))))
          (funcall (nth 1 spec) ds (funcall (nth 2 spec) val))))
      ds)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Read a format file
;;;

(defun db-setup-data-display-buffer (template db new-p)
  ;; Create and return a data display buffer.  This is only called when a
  ;; brand-new data display buffer is being created, not when one is being
  ;; refreshed.  Arguments are TEMPLATE DB NEW-P.  TEMPLATE may either be
  ;; a buffer containing a display format, or the name of a format file.
  ;; If NEW-P is non-nil, then the database's auxiliary file is read and
  ;; its field variables are set.
  ;;
  ;; WARNING: If the format file's local variables set particular database
  ;; slots (such as fieldnames), and NEW-P is nil, then the database may be
  ;; left in an inconsistent state.  The "primary" format, which is read in
  ;; before the database is, should perform any such neccessary actions.
  (unless (bufferp template)
    (setq template (expand-file-name template))
    (unless (file-readable-p template)
      (error "Can't read format file `%s'." template)))

  (with-current-buffer (db-make-data-display-buffer db new-p)
    (assert (eq dbc-database db) t)
    (setq buffer-read-only nil)         ; (database-mode) set it to t

    (funcall (cond ((bufferp template)
                    'insert-buffer-substring)
                   (t
                    'insert-file-contents))
             template)
    (edb--S! :format-file (if (bufferp template)
                              "(internal)"
                            template))

    (when (and new-p (not (equal "(internal)" (edb--S :format-file))))
      (let ((aux (db-locate-readable-file-prefer-cwd
                  (file-name-sans-extension (edb--1D db :file))
                  (cons default-directory db-aux-file-path)
                  db-aux-file-suffixes)))
        (when aux
          ;; Note that the variable `database' is dynamically bound.
          (let ((database db))
            (load-file aux)))))
    ;; Note that the variable `database' is dynamically bound.
    (let ((database db))
      (db-really-hack-local-variables))

    ;; This is the second half of `insert-buffer-substring-no-properties'
    ;; that we used to use.  Unfortunately, we need the properties to be
    ;; around during local variables processing (ugh).
    (let ((inhibit-read-only t))
      (set-text-properties (point-min) (point-max) nil))

    ;; Initialize local variables.
    ;; Assume `dbc-database' already set.
    (let ((nfields (length (database-fieldnames dbc-database))))
      (unless (< 0 nfields)
        (error "Can't tell how many fields database has."))
      (edb--S! :change-functions (make-vector nfields nil))
      (edb--S! :under (make-vector nfields nil))

      (when new-p
        ;; Initialize database variables.  We didn't do this earlier because
        ;; they may depend on some values set in the format file.
        (unless (edb--1D db :togp)
          ;; Do this before the format is parsed but after the format's local
          ;; variables have been hacked.
          (unless (database-field-priorities db)
            (setf (database-field-priorities db)
                  (list (mapcar 'list (number-sequence 0 (1- nfields)))))))))

    (db-setup-ddb-parse-displayspecs db)

    (setq buffer-read-only t)
    (current-buffer)))

(defun db-make-data-display-buffer (db new-p)
  ;; Create and return a data display buffer; set some vars.
  (with-current-buffer (create-file-buffer (edb--1D db :file))
    (set (make-local-variable 'edb--bprops)
         (edb--rinit '(:edb1 :bprops) (current-buffer)
                     :size 23 :test 'eq :weakness 'key))
    (edb--S! :fend (make-marker))
    (edb--S! :wraparound 'delay)
    (edb--S! :stay-in-edit-mode-p t)
    (let ((dir (file-name-directory (edb--1D db :file))))
      (when dir
        (setq default-directory (expand-file-name dir))))
    (set (make-local-variable 'dbc-database) db)
    (dolist (var '(db-new-record-function
                   dbf-set-this-record-modified-function
                   dbf-redisplay-entire-record-p
                   dbf-before-display-record-function
                   dbf-enter-field-hook
                   dbf-first-change-function
                   dbf-every-change-function
                   dbf-after-record-change-function
                   dbf-format-name-spec-alist))
      (set (make-local-variable var) nil))
    (unless new-p
      ;; These are per-data-display-buffer variables.
      (let ((nfields (length (database-fieldnames db))))
        (edb--S! :change-functions (make-vector nfields nil))
        (edb--S! :under (make-vector nfields nil))))
    (database-mode)
    (current-buffer)))


(defun db-setup-ddb-parse-displayspecs (db)
  ;; Get rid of local variables.
  (goto-char (point-max))
  (search-backward "\n\^L" (point-min) 'move)
  (when (search-forward
         (concat "Local"                ; use `concat' to avoid
                 " "                    ; misinterpretation
                 "Variables:")
         nil t)
    (beginning-of-line 1)
    (delete-region (point) (point-max)))
  ;; Get rid of whitespace at end of buffer.
  (goto-char (point-max))
  (re-search-backward "[^ \t\n]")
  (delete-region (match-end 0) (point-max))
  ;; Get rid of whitespace at ends of lines.
  (goto-char (point-min))
  (while (re-search-forward  "[ \t]+$" nil t)
    (delete-region (match-beginning 0) (match-end 0)))

  (let ((prev-end (point-min))
        (backslash-placeholder (and (goto-char (point-min))
                                    (search-forward "\\\\" nil t)
                                    ;; assume this doesn't return nil
                                    (db-unused-char-in-buffer)))
        len
        beginning end ds
        ds-ls
        ift-ls)                         ; inter-field-text

    (when backslash-placeholder
      (setq backslash-placeholder (char-to-string backslash-placeholder))
      (goto-char (point-min))
      (while (search-forward "\\\\" nil t)
        (replace-match backslash-placeholder nil t)))

    (edb--S! :default-sumfmt nil)

    (goto-char (point-min))
    (while (re-search-forward db-ds+opts-rx nil t)
      (setq beginning (match-beginning 0)
            end (match-end 0)
            ;; Call "internal" version of function because match-data is set.
            ;; nil as first argument means make it from the buffer.
            ds (db-dspec<-string nil db))

      ;; Fix up backslash-replacement.  The buffer is fixed up instead of
      ;; just the ift-ls because of the call to current-column.
      (when backslash-placeholder
        (save-excursion
          (save-restriction
            (narrow-to-region prev-end beginning)
            (goto-char prev-end)
            (while (search-forward backslash-placeholder nil t)
              (replace-match "\\" nil t)))))

      (push (buffer-substring prev-end beginning) ift-ls)
      ;; Match about to be deleted; we just used the old value.
      (setq prev-end beginning)

      (unless (edb--S :default-sumfmt)
        (edb--S! :default-sumfmt (save-excursion
                                   (buffer-substring
                                    (progn (beginning-of-line 1)
                                           (point))
                                    (progn (end-of-line 1)
                                           (point)))))
        (unless (edb--S :sumfmt)
          (edb--S! :sumfmt (edb--S :default-sumfmt))))

      (delete-region beginning end)

      (when (eq t (edb--1ds-indent ds))
        (setf (edb--1ds-indent ds) (current-column)))

      (push ds ds-ls))

    ;; Fix up backslash-replacement for the post-last text.
    (when backslash-placeholder
      (save-excursion
        (save-restriction
          (narrow-to-region prev-end (point-max))
          (goto-char prev-end)
          (while (search-forward backslash-placeholder nil t)
            (replace-match "\\" nil t)))))

    (push (buffer-substring prev-end (point-max)) ift-ls)

    (edb--S! :iftxt (vconcat (nreverse ift-ls)))
    (edb--S! :displayspecs (vconcat (nreverse ds-ls)))
    (setq len (length (edb--S :displayspecs)))
    ;; The vector for :search-defaults is one element longer than the number
    ;; of fields; last element is the default for a search over all fields.
    (edb--S! :search-defaults (make-vector (1+ len) nil))
    (edb--S! :shown (make-vector len nil)))

  ;; Initialize more local variables.
  (edb--S! :fidx2dsidx (make-vector (length (database-fieldnames db)) nil))
  (let ((all-ds (edb--S :displayspecs))
        (fidx2dsidx (edb--S :fidx2dsidx)))
    (dotimes (fsno (length all-ds))
      (aset fidx2dsidx (edb--1ds-record-index (aref all-ds fsno)) fsno)))

  (dbf-set-summary-format
   (or (edb--S :sumfmt)
       (mapconcat (lambda (sym)
                    (format "\\%s" sym))
                  (append (database-fieldnames db) nil)
                  " ")))

  (set-buffer-modified-p nil))


(defun db-additional-data-display-buffer ()
  "Create another data display buffer in which to view this database."
  (interactive)
  (dbf-process-current-record-maybe t)
  (let* ((cur (current-buffer))
         (database dbc-database)
         (new (db-make-data-display-buffer database nil)))
    (let ((ddbufs (edb--1D database :ddbufs)))
      (edb--1D! database :ddbufs (cons new ddbufs)))
    (switch-to-buffer-other-window new)
    (db-copy-buffer-local-variables cur)
    (edb--rput '(:edb1 :bprops) (current-buffer)
               (copy-hash-table (edb--rget '(:edb1 :bprops) cur)))
    (edb--S! :sumbuf nil)
    ;; Here are the trampled-on variables that we really cared about.
    (edb--S! :under (make-vector (length (edb--S :original)) nil))
    (db-emergency-restore-format t)))


(defun db-change-format (&optional format-name filename)
  "Select and use an alternate display format to view the database.
If neither FORMAT-NAME nor FILENAME is specified (as is the case when this
is called interactively), the user is prompted for them.  In Emacs Lisp
code, if `dbf-format-name-spec-alist' has been been set, usually only one of
the arguments is specified.  If both are specified, then FORMAT-NAME
becomes a name for the format FILENAME specifies; if FORMAT-NAME is already
associated with a different format file, an error is signalled.

If the current format is unnamed, the user is prompted for a name
to give it, so that it can be conveniently restored if need be.  This
behavior is suppressed, and the record is not displayed, if the function is
not being called interactively.

The data display buffer is left in Database View mode.

Selecting the current format does not cause any work to be done.

Some databases automatically set the format of the record being displayed,
usually by setting `dbf-before-display-record-function' to a function that
overrides the format in effect when a record is about to be displayed.
This may cause this function to appear not to be doing any work.  In
actuality the format is being set, then reset."
  (interactive)

  (unless (and format-name
               (equal format-name (edb--S :format-name)))
    ;; We're not already in the requested format
    (db-view-mode)

    ;; If neither format- nor filename is specified, query for one of them.
    (unless (or format-name filename)
      (setq format-name
            (completing-read
             "Use which format? (? for options, RET to specify a file) "
             (cons '("") dbf-format-name-spec-alist)
             (lambda (elem)
               (stringp (car elem)))
             t))
      (when (equal "" format-name)
        (setq format-name nil
              filename (read-file-name "File for new format: " nil nil t))))

    ;; Either `format-name' or `filename' -- or possibly both,
    ;; if not called interactively -- is set.
    (when filename
      (setq filename (db-locate-format-file filename)))
    (when format-name
      (let ((file-name-handler-alist (cons
                                      (cons "^(connection)"
                                            'edb--connection-file-cache)
                                      file-name-handler-alist))
            (spec (cdr (assoc format-name dbf-format-name-spec-alist))))
        (if spec
            ;; successful format-name
            (let ((fs-filename (if (listp spec)
                                   (car spec)
                                 spec)))
              (if filename
                  (when (and fs-filename
                             ;; This test is required for interactive
                             ;; uses of `db-change-format'.
                             (not (db-same-file-p filename fs-filename)))
                    (error "Format name %s is associated with %s, not %s."
                           format-name fs-filename filename))
                (setq filename (db-locate-format-file fs-filename))))
          ;; unsuccessful format-name
          (if filename
              (push (cons format-name filename) dbf-format-name-spec-alist)
            ;; no filename, failed format-name
            (error "`%s' is not the name of a format." format-name)))))
    ;; Filename is now set.

    (cl-flet
        ((mkspec (sumfmt sumfun)
                 ;; All of these items vary from format to format within a
                 ;; particular data display buffer.
                 (list
                  (edb--S :format-file)
                  ;; These can vary between data display buffers which
                  ;; happen to be using the same format file to specify the
                  ;; layout of the record's fields.  That is, these are
                  ;; specific to a particular data display buffer, not to a
                  ;; format, because they have to do with what is actually
                  ;; being displayed and/or because we might expect the user
                  ;; to change them after reading in the format.  This is
                  ;; why we can't just associate this information with the
                  ;; format file, but have to save it on a
                  ;; per-data-display-buffer basis.
                  sumfmt
                  sumfun
                  (edb--S :shown)
                  (edb--S :search-defaults))))

      ;; First save away current format.
      ;; No need to do anything with filename.
      (let ((curname (edb--S :format-name)))
        (when (and (called-interactively-p 'any)
                   (not curname)
                   (y-or-n-p "Give the current format a name? "))
          (setq curname (read-string "Name for current format: "))
          (edb--S! :format-name curname))
        (when curname
          (let ((look (assoc curname dbf-format-name-spec-alist))
                (spec (mkspec (edb--S :sumfmt) (edb--S :sumfun))))
            (if look
                (setcdr look spec)
              (push (cons curname spec)
                    dbf-format-name-spec-alist)))))

      ;; Now install the new format.
      (let ((prev-format-file (edb--S :format-file))
            (fs (cdr (assoc filename (edb--S :fmtspec-stash)))))
        (edb--S! :format-name format-name)
        (edb--S! :format-file filename)
        (if fs
            (progn
              (mapc 'eval (edb--S! :always-forms (pop fs)))
              (edb--S! :displayspecs             (pop fs))
              (edb--S! :iftxt                    (pop fs))
              (edb--S! :fidx2dsidx               (car fs))
              (let ((spec (cdr (assoc (or (edb--S :format-name)
                                          (intern (edb--S :format-file)))
                                      dbf-format-name-spec-alist))))
                (edb--S! :format-file          (pop spec))
                (edb--S! :sumfmt               (pop spec))
                (edb--S! :sumfun               (pop spec))
                (edb--S! :shown                (pop spec))
                (edb--S! :search-defaults      (car spec))))
          ;; We didn't find `:format-file' in `:fmtspec-stash'; we probably
          ;; didn't find more than just a filename at `:format-name' in
          ;; dbf-format-name-spec-alist either.
          ;; This `let' is for the benefit of the new format file.
          (let ((file-name-handler-alist (cons
                                          (cons "^(connection)"
                                                'edb--connection-file-cache)
                                          file-name-handler-alist))
                (database dbc-database)
                (buffer-read-only nil))
            ;; Though we shamefully cover up the fact that the original format
            ;; file is being re-read (this time for caching purposes), this at
            ;; least allows us to no longer suggest setting the format name in
            ;; the format file.  All part of EDB 1.x janitorial services...
            ;; Also, don't mention anything for connection-bundled formats.
            (unless (or (memq (aref (edb--S :format-file) 0) '(?( ?)))
                        (equal prev-format-file (edb--S :format-file)))
              (db-message "Reading format from file: %s"
                          (edb--S :format-file)))
            (buffer-disable-undo)
            (erase-buffer)
            (insert-file-contents (edb--S :format-file))

            (db-really-hack-local-variables)

            (db-setup-ddb-parse-displayspecs dbc-database)

            ;; Save away the file-invariant stuff.
            (edb--S! :fmtspec-stash
                     (cons (list (edb--S :format-file)
                                 (edb--S :always-forms)
                                 (edb--S :displayspecs)
                                 (edb--S :iftxt)
                                 (edb--S :fidx2dsidx))
                           (edb--S :fmtspec-stash)))
            ;; Install the defaults under a symbol associated with the format
            ;; file (so it's not user-accessible).
            (push (cons (intern (edb--S :format-file))
                        (mkspec (edb--S :default-sumfmt)
                                (when (equal (edb--S :sumfmt)
                                             (edb--S :default-sumfmt))
                                  (edb--S :sumfun))))

                  dbf-format-name-spec-alist)
            (erase-buffer))))

      (when (called-interactively-p 'any)
        (db-display-record (dbf-displayed-record) t)))))


(defun db-emergency-restore-format (&optional recompute)
  "Throw away the contents of the format buffer; redisplay the current record.
Use this if the format gets munged.
Changes made to the current field since last moving onto it may be lost.
If optional argument RECOMPUTE is non-nil, the displayed text
is recomputed as well."
  (db-display-record (dbf-displayed-record) recompute)
  (let ((this-fidx (edb--S :this-fidx)))
    (when this-fidx
      (dbf-set-this-field-modified-p nil)
      (db-move-to-field-exact this-fidx)
      ;; If the hook changed formats, we'll be in Database View mode.
      (db-edit-mode))))

(defun dbf-set-summary-format (summary-format)
  "Specify the format used in the Database Summary buffer.
Argument SUMMARY-FORMAT is a string containing display specifications.
Call this in the data display buffer, or in a format file or auxiliary file."
  (interactive (list (let ((sumfmt (edb--S :sumfmt)))
                       (read-string "Summary format: "
                                    (cons sumfmt 0) nil sumfmt))))
  (unless (stringp summary-format)
    (error "Argument to dbf-set-summary-format should be a string, not %s"
           summary-format))
  (when (= ?\n (elt summary-format (1- (length summary-format))))
    (setq summary-format (substring summary-format 0 -1)))
  (edb--S! :sumfmt summary-format)
  (dbf-set-summary-out-of-date-p)
  (when (edb--S :sumbuf)
    (with-current-buffer (edb--S :sumbuf)
      (let ((ht (edb--S :summaries)))
        (when ht (clrhash ht)))))
  (let ((lasfl (db-format->lines/sforms summary-format dbc-database 2 t nil)))
    ;; The (constant) number of screen lines occupied by each record summary,
    ;; computed automatically from the summary format.  This doesn't depend
    ;; on the field values in the individual records because
    ;; db-format->lines/sforms errs if min-height is not equal to max-height
    ;; (unless variable-height is set).  That makes determining which summary
    ;; point is in, and getting to a particular summary, much easier.
    (edb--1D! dbc-database :sum1lines (car lasfl))
    (edb--S! :sumfun `(lambda (formatted-record)
                        (concat ,@(cdr lasfl))))))


(defmacro dbf-always (&rest body)
  "Execute forms in BODY, and arrange to execute them in the future
each time that this format replaces another."
  (declare (debug body))
  `(progn
     (edb--S! :always-forms (nconc (edb--S :always-forms) ,body))
     ,@body))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Display data in a format
;;;

(defvar db-display-record-redo-inter-field-text-function nil
  "Function taking two args called in two contexts during `db-display-record'.
The args are BUF and EXTENTS-LIST.  In the first call, done right before
erasing the buffer, EXTENTS-LIST is null.  In the second call, done after
text has been inserted in the buffer and point is at point-min, EXTENTS-LIST
is a list of pairs of buffer positions in BUF that define the \"inter-field
text\", that is the text specifically not representing the data, but instead
part of the display format.")

(defun db-display-record (record &optional recompute fieldno-limit)
  ;; Why take RECORD argument instead of using var `dbf-displayed-record'?
  ;; Joe Wells has used this feature, so don't remove it.
  "Display RECORD in the current buffer, which is a data display buffer.
If optional arg RECOMPUTE is non-nil, the display representations will be
computed first; RECOMPUTE is typically non-nil only the first time a record
is shown.  If optional third arg FIELDNO-LIMIT is non-nil, only
fieldnumbers strictly less than it will be displayed."
  (let* ((buffer-read-only nil)
         (is-displayed-record-p (eq record (dbf-displayed-record)))
         ;; If quitting occurs in the middle of this operation, EDB becomes
         ;; very confused.  WARNING: Inhibitting quitting is dangerous.
         (inhibit-quit t)
         displayspecs shown len iftxt ds rep ext-start ift-extents)
    (run-hook-with-args 'dbf-before-display-record-function record)
    ;; Do after in case `dbf-before-display-record-function' changes things.
    (setq displayspecs (edb--S :displayspecs)
          shown (edb--S :shown)
          len (length displayspecs)
          iftxt (edb--S :iftxt))
    ;; Allow dbf-before-display-record-function to do
    ;; dbf-set-this-record-modified-p if it wants to.
    (when is-displayed-record-p
      (setq record (dbf-displayed-record)))
    (buffer-disable-undo)
    (run-hook-with-args 'db-display-record-redo-inter-field-text-function
                        (current-buffer) nil)
    (erase-buffer)
    (dotimes (fidx len)
      (setq ds (aref displayspecs fidx))
      (setq ext-start (point))
      (insert (aref iftxt fidx))
      (push (cons ext-start (point)) ift-extents)
      (when recompute
        (aset shown fidx
              (if (and fieldno-limit (>= fidx fieldno-limit))
                  ;; fixme: handle min-height and min-bytes. --ttn
                  (make-string (or (edb--1ds-min-width ds) 0) 32)
                (setq rep (db-ds-printed ds record))
                (replace-regexp-in-string
                 "\n"
                 (concat "\n" (make-string (current-column) 32))
                 rep))))
      (insert (aref shown fidx)))
    (setq ext-start (point))
    (insert (aref iftxt len))
    (push (cons ext-start (point)) ift-extents)
    (dbf-set-this-field-modified-p nil)
    ;; This place is as good as any for leaving the cursor by default.
    (goto-char (point-min))
    (buffer-enable-undo (current-buffer))
    (run-hook-with-args 'db-display-record-redo-inter-field-text-function
                        (current-buffer)
                        (nreverse ift-extents))
    ;; If quitting occurred while this was happening, ignore it.
    (setq quit-flag nil)))

(defun db-ds-printed (ds record)
  (let* ((ridx (edb--1ds-record-index ds))
         (rep (db-callconvert
               (edb--1ds-actual->display ds)
               (aref record ridx)
               record
               ridx)))
    (let ((min-h (edb--1ds-min-height ds))
          (max-h (edb--1ds-max-height ds)))
      (when (or min-h max-h)
        (let ((rep-h (1+ (db-count-newlines rep))))
          (cond ((and min-h (< rep-h min-h))
                 ;; too short
                 (setq rep
                       (concat rep
                               (make-string (- min-h rep-h)
                                            ?\n))))
                ((and max-h (> rep-h max-h))
                 ;; too tall
                 (setq rep
                       (substring rep 0
                                  (db-find-char-from-end
                                   ?\n rep
                                   (- rep-h min-h)))))))))

    ;; These conditions are much too simplistic; they only work for one-line
    ;; representations.
    (let ((rep-w (length rep))
          (min-w (edb--1ds-min-width ds))
          (max-w (edb--1ds-max-width ds)))
      (cond ((and min-w (< rep-w min-w))
             ;; The display representation is too short
             (setq rep (funcall (or (edb--1ds-padding-action ds)
                                    'db-pad-right)
                                min-w rep rep-w))
             (unless (= (length rep) min-w)
               (error "Padding function %s returned \"%s\", %s %d, not %d."
                      (or (edb--1ds-padding-action ds)
                          'db-pad-right)
                      rep "which has length" (length rep) min-w))
             (setq rep-w min-w))
            ((and max-w (> rep-w max-w))
             ;; The display representation is too long.
             (funcall (or (edb--1ds-truncation-display-action ds)
                          (lambda (max-w rep rep-w)
                            (put-text-property max-w rep-w
                                               'invisible t
                                               rep)))
                      max-w rep rep-w)
             ;; Assume the truncation function did the right thing.
             (setq rep-w max-w)))
      rep)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Etc.
;;;

(defun database-mode ()
  "A mode for viewing and editing formatted data; a database front end.
In Database Edit mode, fields of the database may be changed.
In Database View mode, keystrokes are bound to database commands.
Typically, if point is on a field, the buffer is in Database Edit mode; if
point is at the beginning of the buffer, the buffer is in Database View mode.
The mode line indicates which mode the buffer is in.

Database View mode key bindings:

\\{database-view-mode-map}

Database Edit mode key bindings:

\\{database-edit-mode-map}"

  (setq mode-line-modified '(:eval (let ((meta (edb--meta1D dbc-database)))
                                     (if (not (gethash :modifiable-p meta))
                                         "%%%%"
                                       (string
                                        (if (gethash :modp meta) ?* ?-)
                                        (if (edb--S :utkmodp) ?* ?-)
                                        ))))
        mode-line-format '("-"
                           mode-line-modified
                           "%*"
                           "- %17b   %[("
                           mode-name
                           minor-mode-alist
                           (:eval (concat (and (edb--S :hide-p) " Hide")
                                          " "
                                          (edb--S :index-fraction)))
                           ")%]"
                           "---"
                           (-3 . "%p")
                           "-%-"))
  (make-local-variable 'require-final-newline)
  (setq require-final-newline nil)
  (db-view-mode))

(defsubst db-data-display-buffer-p ()
  "T if this buffer is a database data display buffer."
  (memq major-mode '(database-view-mode database-edit-mode)))

;;; db-format.el ends here
