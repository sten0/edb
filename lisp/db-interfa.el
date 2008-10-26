;;; db-interfa.el --- part of EDB, the Emacs database

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

;; Commands for operating on the current database.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;

(defvar dbc-database nil
  "The database associated with this format.
This variable is also set in the summary format.")

(defun dbc-set-hide-p (value)
  "Enable hiding if VALUE is non-nil, otherwise disable it.
This is done in both the data display buffer and summary buffer."
  (edb--S! :hide-p value)
  (let ((other (or (edb--S :data-display-buffer)
                   (edb--S :sumbuf))))
    (when other (with-current-buffer other (edb--S! :hide-p value)))))

;;; Etc.

;;;###safe-file-local-variable
(defvar db-new-record-function nil
  "Function called on empty records before they're inserted in the database.
Takes two arguments, the record and the database.  This variable is set
only in the data display buffer.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Keymaps
;;;

(defvar database-view-mode-map nil
  "Keymap for database data display buffer in view mode.")

(unless database-view-mode-map
  (let ((m (make-sparse-keymap))
        (meta (list meta-prefix-char)))
    (suppress-keymap m)
    (mapc (lambda (key-def)
            (define-key m (let ((key (car key-def)))
                            (if (consp key)
                                (concat meta (car key))
                              key))
              (cdr key-def)))
          '(("t" . toggle-truncate-lines)
            ("+" . db-additional-data-display-buffer)

            ;; Moving around in the database
            ("n"      . db-next-record)
            ("p"      . db-previous-record)
            ("<"      . db-first-record)
            (">"      . db-last-record)
            (("<")    . db-first-record)
            ((">")    . db-last-record)
            ("j"      . db-jump-to-record)
            (" "      . db-next-screen-or-record)
            ("\177"   . db-previous-screen-or-record)
            (("n")    . db-next-record-ignore-hiding)
            (("p")    . db-previous-record-ignore-hiding)
            (("\C-n") . db-next-marked-record)
            (("\C-p") . db-previous-marked-record)

            ;; Changing to edit mode
            ("\t"   . db-first-field)
            (("\t") . db-last-field)
            ("\C-n" . db-first-field)
            ("\C-p" . db-last-field)
            ("e"    . db-first-field)
            ;; These could be db-first-field and db-last-field, but that
            ;; wouldn't fit in: nowhere else are these keystrokes
            ;; inter-field-movement keystrokes.
            ("\C-f" . undefined)
            ("\C-b" . undefined)
            ("\C-a" . undefined)
            ("\C-e" . undefined)
            ;; ("v" . db-view-mode)
            ("\C-v" . db-scroll-up)
            ;; In view-mode, we're at the top of the buffer (not after
            ;; db-next-screen).
            (("v") . db-scroll-down)

            ;; Undoing changes
            ("\C-xu" . db-revert-record)
            ;;("u"   . db-revert-record)
            ("\C-xr" . db-revert-database)

            ;; Adding and removing records
            ("a" . db-add-record)
            ("i" . db-add-record)
            ("d" . db-delete-record)
            ("k" . db-delete-record)
            ("y" . db-yank-record)
            ("o" . db-output-record-to-db)
            ("c" . db-copy-record)

            ;; Sorting
            ("S" . db-sort)

            ;; Searching commands
            (("S")  . db-search)
            (("s")  . db-search)
            ("s"    . db-search)
            ("\C-s" . db-isearch-forward)
            ("\C-r" . db-isearch-backward)

            ;; Exiting database mode
            ("q" . db-quit)
            ("x" . db-kill-buffer)
            ("X" . db-kill-all-buffers)

            ("m" . db-mark-record)

            ("?" . describe-mode)

            ;; Gross key bindings.
            ("O"      . db-hide-record)
            (("o")    . db-hiding-toggle)
            (("O")    . db-hiding-set)
            (("\C-o") . db-toggle-show-hidden-records)

            ("F" . dbf-set-summary-format)
            ("D" . db-summary)          ; mnemonic for Directory
            ("h" . db-summary)          ; mnemonic for Headers
            ("H" . db-summary)          ; mnemonic for Headers

            ("r"  . db-report)
            ("\r" . db-accept-record)))
    (setq database-view-mode-map m)))


(defvar database-edit-mode-map nil
  "Keymap for database data display buffer in edit mode.")

(unless database-edit-mode-map
  (let ((m (make-sparse-keymap))
        (meta (list meta-prefix-char)))
    ;; Obviously don't do suppress-keymap on this one; we want to be
    ;; able to edit.  The view-mode commands should be available via C-c
    ;; and many (such as next-record) available via M- commands as well,
    ;; espcially those not ordinarily bound in text mode (eg M-n and
    ;; M-p).
    (mapc (lambda (key-def)
            (define-key m
              (let ((key (car key-def)))
                (if (consp key)
                    (concat meta (car key))
                  key))
              (cdr key-def)))
          '(;; Exiting edit mode
            ("\C-c\C-c" . db-view-mode)

            ;; Undoing changes
            ("\C-xU" . db-revert-field)

            ;; Moving from record to record
            (("n") . db-next-record)
            (("p") . db-previous-record)

            ;; Moving from field to field
            ("\t"   . db-next-field)
            (("\t") . db-previous-field)
            (("<")  . db-first-field)
            ((">")  . db-last-field)
            ("\C-v" . db-scroll-up)
            (("v")  . db-scroll-down)

            ;; Movement within a field
            ("\C-n" . db-next-line-or-field)
            ("\C-p" . db-previous-line-or-field)
            ;; almost-the-same-as-before commands
            ("\C-f" . db-forward-char)
            ("\C-b" . db-backward-char)
            (("f")  . db-forward-word)
            (("b")  . db-backward-word)
            ("\C-a" . db-beginning-of-line-or-field)
            ("\C-e" . db-end-of-line-or-field)

            ;; Editing a field
            ;;insertion
            ("\r"     . db-newline)
            ("\n"     . db-newline)
            ("\C-o"   . db-open-line)
            ;;deletion
            ("\C-d"   . db-delete-char)
            ("\177"   . db-backward-delete-char)
            (("d")    . db-kill-word)
            (("\177") . db-backward-kill-word)
            ("\C-k"   . db-kill-line)
            (("k")    . db-kill-to-end)
            ("\C-w"   . db-kill-region)
            (("w")    . db-copy-region-as-kill)

            ;; Other commands
            (("s")   . db-search-field)
            ;;(("S") . db-search-field)

            ("\C-s" . db-isearch-forward)
            ("\C-r" . db-isearch-backward)

            (("?") . db-field-help)))
    (setq database-edit-mode-map m)))


;;; Bindings for both keymaps

(mapc (lambda (&optional key-def)
        (apply 'define-key database-view-mode-map key-def)
        (apply 'define-key database-edit-mode-map key-def))
      '( ;; Saving the database
        ("\C-x\C-s" db-save-database)
        ("\C-x\C-w" db-write-database-file)

        ;; Toggling modifiable-p
        ("\C-x\C-q" db-toggle-modifiable-p)

        ;; Wipe out dangerous commands
        ("\C-xn" undefined)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Menus
;;;

;; Menus by Alastair Burt <burt@dfki.uni-kl.de>,
;;          Michael Ernst <mernst@theory.lcs.mit.edu>
;;          John Overton <overton@cs.uchicago.edu>

(defvar database-view-mode-menu
  '("Database"
    "VIEW Mode:"
    ["Edit"            db-first-field                 t]
    ["Report"          db-report                      t]
    ["Summary"         db-summary                     t]
    "---"
    ["Revert  record" db-revert-record t]
    ["Accept  record" db-accept-record t]
    ["Add     record" db-add-record t]
    ["Copy    record" db-copy-record t]
    ["Delete  record" db-delete-record t]
    ["Output  record" db-output-record-to-db t]
    ["Mark    record" db-mark-record t]
    ["Hide    record" db-hide-record t]
    "----"
    ("Hiding"
     ["Hiding    on/off" db-hiding-toggle             t]
     ["Hiding hide/show (in summary)" db-toggle-show-hidden-records t]
     ["Un-hide      all" db-unhide-all                  t]
     ["Un-mark      all" db-unmark-all                  t]
     ["Mark   un-hidden" db-mark-unhidden-records      t]
     ["Hide   un-marked" db-hide-unmarked-records       t]
     )
    ("Motion"
     ["jump to"  db-jump-to-record t]
     ["first" db-first-record   t]
     ["last"  db-last-record    t]
     "---"
     ["next"               db-next-record                     t]
     ["next (screen)"      db-next-screen-or-record           t]
     ["next (marked)"      db-next-marked-record              t]
     ["next (ignore hiding)" db-next-record-ignore-hiding     t]
     "---"
     ["prev"               db-previous-record                 t]
     ["prev (screen)"      db-previous-screen-or-record       t]
     ["prev (marked)"      db-previous-marked-record          t]
     ["prev (ignore hiding)" db-previous-record-ignore-hiding t]
     )
    "----"
    ["Create Report" db-report t]
    ["Toggle Hiding" db-hiding-toggle t]
    ["Summary" db-summary t]
    "----"
    ["Edit Mode" db-first-field t]
    "----"
    ["Sort    database" db-sort t]
    ["Revert  database" db-revert-database t]
    ["Save    database"  db-save-database       t]
    ["Write   database" db-write-database-file t]
    ["Internal Layout" db-toggle-internal-file-layout t]
    "----"
    ["Quit" db-quit t]
    )
  "Menu for Database View mode.")

;; 'ignored for SYMBOL argument was giving me trouble.
;; Does this work in Lucid Emacs?
(easy-menu-define ignored
  database-view-mode-map
  "ignored-doc-string"
  database-view-mode-menu)


(defvar database-edit-mode-menu
  '("Database"
    "EDIT Mode:"
    ["View mode"        db-view-mode      t]
    ["Report"           db-report         t]
    ["Summary"          db-summary        t]
    ["Summary   subset" db-summary-subset t]
    "---"
    ["Revert    record"  db-revert-record     t]
    "---"
    ["Revert     field"  db-revert-field   t]
    ["Help    on field" db-field-help t]
    ["Search  in field" db-search-field t]
    "---"
    ("Motion"
     ["Next       field"  db-next-field     t]
     ["Prev       field"  db-previous-field t]
     ["Last       field"  db-last-field     t]
     ["First      field"  db-first-field    t]
     ["Next      record" db-next-record t]
     ["Previous  record" db-previous-record t])
    "---"
    ["Revert  database" db-revert-database t]
    ["Search  database"  db-search-field        t]
    ["Save    database"  db-save-database       t]
    ["Write   database" db-write-database-file t]
    "---"
    ["Quit" db-quit t]
    )
  "Menu for Database Edit mode.")

;; 'ignored for SYMBOL argument was giving me trouble.
;; Does this work in Lucid Emacs?
(easy-menu-define ignored
  database-edit-mode-map
  "ignored-doc-string"
  database-edit-mode-menu)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Help
;;;

(defun db-set-field-help (db &rest spec)
  "Set field-specific help info for database DB from F1 H1, ...
F1, F2, ... should be field names (symbols) or field numbers,
while H1, H2, ... should be either a string or a form to be
evaluated that results in a string.  See `db-field-help'.

This should be called after the field names have been set up.

\(fn DB F1 H1 ...)"
  (let* ((len (length (database-fieldnames db)))
         (fh (or (edb--1D db :field-help)
                 (edb--1D! db :field-help (make-vector len nil))))
         (nm2no (edb--1D db :nm2no))
         field help-info)
    (while spec
      (setq field (pop spec)
            help-info (pop spec))
      (aset fh (if (symbolp field)
                   (gethash field nm2no)
                 field)
            help-info))))

(defun db-field-help ()
  "Display help for the current field in the echo area.
Help info comes from two places: field-specific help info, and
recordfieldtype-specific help info.  In both cases, if the info
is a string, display it.  Otherwise, `eval' it as a Lisp
expression and display the result (which should be a string).
When both sources provide the help info, the display the
field-specific info followed by two newlines followed by the
recordfieldtype-specific info."
  (interactive)
  (unless (edb--S :this-ds)
    (error "Not on a field."))
  (flet ((try (x) (when x
                    (if (stringp x)
                        (list "%s" x)
                      (condition-case err
                          (list "%s" (eval x))
                        (error
                         (list "Help form: %S\nfailed with error: %S"
                               x err)))))))
    (let* ((fidx (edb--1ds-record-index (edb--S :this-ds)))
           (one (let ((fh (edb--1D dbc-database :field-help)))
                  (and fh (try (aref fh fidx)))))
           (two (try (aref (db-rs-slice dbc-database 'edb--1rs-help-info)
                           fidx))))
      (if (or one two)
          (apply 'message (concat (car one) (and one two "\n\n") (car two))
                 (append (cdr one) (cdr two)))
        (message "No help available for `%s'."
                 (db-fname<-fno fidx dbc-database))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Quitting
;;;

;; These work in both the data display buffer and the summary buffer, for
;; folks who enjoy spending most of their time in the summary who will rebind
;; its keystrokes to call these functions instead of dbs-exit (for instance).

(defun db-quit ()
  "Quit editing the database for now; bury its buffers."
  (interactive)
  ;; Bury the data display and summary buffers.
  (let (dd sum)
    (db-in-data-display-buffer
      (setq dd  (current-buffer)
            sum (edb--S :sumbuf)))
    (when dd
      (delete-windows-on dd)
      (bury-buffer dd))
    (when sum
      (delete-windows-on sum)
      (bury-buffer sum))))

(defun db-kill-buffer (&optional noask)
  "Kill this data display buffer and any associated summary buffer.
Offer to save any changes.  Prefix arg means don't offer.
See also `db-kill-all-buffers'."
  (interactive "P")
  (unless noask
    (db-save-database t))
  (edb--S! :discard-changes! t)
  (kill-buffer nil))

(defun db-kill-all-buffers (&optional noask)
  "Kill all buffers associated with the current database.
Offer to save any changes.  Prefix arg means don't offer.
See also `db-kill-buffer'."
  (interactive "P")
  (unless noask
    (db-save-database t))
  (dolist (ddb (edb--1D dbc-database :ddbufs))
    (with-current-buffer ddb
      (edb--S! :discard-changes! t)
      (kill-buffer nil))))

(defun db-exit (&optional kill)
  "Be done with the database; like `db-quit', but offers to save any changes.
With prefix argument, kills the data display buffer, and the database, if that
was its only data display buffer."
  (interactive "P")
  (db-save-database t)
  (if kill
      (db-kill-all-buffers t)
    (db-quit)))

(defun db-kill-buffer-hook ()
  (when (edb--rget '(:edb1 :bprops) (current-buffer))
    (cond ((db-summary-buffer-p)
           (with-current-buffer (edb--S :data-display-buffer)
             (edb--S! :sumbuf nil)))
          ((db-data-display-buffer-p)
           (let ((left (remq (current-buffer)
                             (edb--1D dbc-database :ddbufs)))
                 buf)
             (unless (edb--S :discard-changes!)
               (when (and (or (edb--S :utkmodp)
                              (dbf-this-field-modified-p))
                          (y-or-n-p "Commit current record? "))
                 (dbf-process-current-record-maybe t))
               (db-save-database t left))
             (when (setq buf (edb--S :sumbuf))
               (delete-windows-on buf)
               (kill-buffer buf))
             (if left
                 (edb--1D! dbc-database :ddbufs left)
               ;; slightly suboptimal from maintenance pov --ttn
               (when (setq buf (edb--S :ddb-spec))
                 (kill-buffer buf))
               (edb--meta1D dbc-database :forget)))))
    (edb--rforget '(:edb1 :bprops) (current-buffer))))

(defun db-save-some-buffers (&optional quietly exiting)
  "Save some modified databases and file-visiting buffers.
Asks user about each one.  With argument, saves all with no questions."
  (interactive "P")
  (db-save-some-databases quietly)
  (save-some-buffers quietly exiting))

;; This isn't quite right because it should modify the ???.
(defun db-save-some-databases (&optional quietly)
  "Save some modified databases.  Asks user about each one.
With argument, saves all with no questions."
  (interactive "P")
  (let (buffers)
    (dolist (db (edb--1all-known-databases))
      (setq buffers (edb--1D db :ddbufs))
      (when buffers
        (dolist (buf buffers)
          (db-in-buffer buf (dbf-process-current-record-maybe t)))
        (db-in-buffer (car buffers) (db-save-database (not quietly)))
        (dolist (buf buffers)
          (with-current-buffer buf
            (force-mode-line-update)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File I/O
;;;

;;;###autoload
(defun db-find-file (filename &optional promptp)
  "Read a database from FILENAME; prompts when called interactively.
If the database file doesn't specify a format and the format file can't be
inferred from FILENAME, the user is prompted for it too.
The user is always prompted for the format if prefix argument
PROMPTP is non-nil.
If the database is already read in and PROMPTP is nil, the existing
database buffer is merely selected.
When called non-interactively, argument PROMPTP may be a string, the
name of a format file to use."
  (interactive "fDatabase file: \nP")
  ;; fixme: check whether the file is currently read in. --ttn
  (setq filename (expand-file-name filename))
  (let ((format-file (when (stringp promptp) promptp))
        database data-display-buffer)
    (when (stringp promptp)
      (setq promptp nil))

    (unless promptp
      (setq database (db-find-read-in-database filename))
      (when (and database (not (edb--1D database :ddbufs)))
        (setq database nil))
      ;; Find an appropriate data display buffer
      (when (and database format-file)
        (let ((bufs (edb--1D database :ddbufs))
              ddb-format-file)
          (while bufs
            (if (db-same-file-p format-file
                                (with-current-buffer (car bufs)
                                  (edb--S :format-file)))
                (setq data-display-buffer (car bufs)
                      bufs nil)
              (setq bufs (cdr bufs)))))))
    (unless database
      ;; Either promptp is non-nil, or we couldn't find an
      ;; appropriate read-in-database.
      (setq database (db-read-database-file filename format-file promptp)))
    (unless data-display-buffer
      (setq data-display-buffer (car (edb--1D database :ddbufs))))
    (switch-to-buffer data-display-buffer)
    (assert (eq (current-buffer) (variable-binding-locus 'dbc-database)))
    (setq dbc-database database))
  (db-first-record))

(defun db-find-read-in-database (filename)
  ;; Return the database most recently read in from FILENAME, or nil.
  (let ((db-ls (edb--1all-known-databases))
        result db maybe)
    (while (and (not result) db-ls)
      (setq db (car db-ls)
            maybe (edb--1D db :file)
            result (when (and (db-same-file-p
                               filename (if (equal "(inherent)" maybe)
                                            (get-text-property 0 :file maybe)
                                          maybe))
                              (car (edb--1D db :ddbufs)))
                     db)
            db-ls (cdr db-ls)))
    result))

(defun db-revert-database ()
  "Replace the database with the data on disk.
This undoes all changes since the database was last saved."
  (interactive)
  (when (yes-or-no-p (format "Revert database from file %s? "
                             (edb--1D dbc-database :file)))
    (let ((database dbc-database))
      (with-temp-buffer
        (insert-file-contents (edb--1D database :file) nil)
        (unless (setq database (db-read-database-internal-file-layout))
          (error "File no longer contains a database"))
        (db-read-database-file-helper database))

      (mapc (lambda (buf)
              (with-current-buffer buf
                ;; abandon any changes
                (dbf-set-this-field-modified-p nil)
                (edb--S! :utkmodp nil)
                (db-jump-to-record (edb--S :index) nil)))
            (edb--1D database :ddbufs))

      (db-message "Reverted database from disk"))))

(defun db-save-database (&optional query quietly)
  "Save the database to disk in the default save file.
Any changes to the current record are processed first.
The default save file is the file it was last saved to or read from.
If optional arg QUERY is specified, the user is asked first.
Optional second arg QUIETLY suppresses messages regarding the filename."
  (interactive)
  ;; fixme: check if file is more recent than buffer. --ttn
  (db-in-data-display-buffer
    (dbf-process-current-record-maybe t)
    (if (edb--1D dbc-database :modp)
        (when (or (not query)
                  (yes-or-no-p (concat "Save database "
                                       (database-print-name dbc-database)
                                       "? ")))
          (db-write-database-file (edb--1D dbc-database :file) quietly))
      (unless quietly
        (let ((name (database-print-name dbc-database)))
          (when (and (stringp name)
                     (or (string= "" name)
                         (string= "Unnamed Database "
                                  (substring name 0 (min 17 (length name))))))
            (setq name nil))
          (db-message "No changes need to be saved%s"
                      (if name
                          (format " in %S" name)
                        "")))))))

(defun db-write-database-file (&optional filename quietly)
  "Save the database to file FILENAME; it becomes the default save file.
Any changes to the current record are processed first.
If FILENAME is not specified, the user is prompted for it.
Optional second arg QUIETLY suppresses messages regarding the filename."
  (interactive)
  ;; Do this before asking for the filename.
  (dbf-process-current-record-maybe t)
  ;; Save even if the database is not modified.
  (unless filename
    (setq filename (read-file-name
                    (format "Save database %s into file: "
                            (database-print-name dbc-database)))))
  (unless (equal filename (edb--1D dbc-database :file))
    (edb--1D! dbc-database :file filename)
    ;; Rename the buffer.
    (rename-buffer (generate-new-buffer-name
                    (file-name-nondirectory filename))))
  (when (equal "(inherent)" filename)
    (setq quietly t)
    (db-message "Saving %s..." (buffer-name)))
  (let ((msg (format "Saving database to file %s..." filename)))
    (unless quietly (db-message "%s" msg))
    (db-write-1)
    (unless (or quietly (edb--G :io-error-p))
      (db-message "%sdone" msg))))

(defun db-toggle-internal-file-layout (&optional arg)
  "Toggle whether the database will be saved in EDB's internal file layout.
With a nonzero prefix argument, set it to use internal file layout.
With a zero prefix argument, set it not to use internal file layout."
  (interactive "P")
  (let ((v (if arg
               (not (zerop (prefix-numeric-value arg)))
             (not (edb--1D dbc-database :togp)))))
    (edb--1D! dbc-database :togp v)
    (when (interactive-p)
      (message "Use of internal file layout now %sabled"
               (if v "en" "dis")))))

(defun db-toggle-modifiable-p (&optional arg)
  "Toggle whether the database may be modified by the user.
With a nonzero prefix argument, set it modifiable.
With a zero prefix argument, set it non-modifiable."
  (interactive "P")
  (let ((modifiable-p (if arg
                          (not (zerop (prefix-numeric-value arg)))
                        (not (edb--1D dbc-database :modifiable-p)))))
    (edb--1D! dbc-database :modifiable-p modifiable-p)
    ;; fixme: handle mods in the presence of nil `modifiable-p'. --ttn
    (db-in-data-display-buffer
      (when (eq 'database-edit-mode major-mode)
        (setq buffer-read-only (not modifiable-p)))
      (set-buffer-modified-p (buffer-modified-p)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Record selection
;;;

;; This is the common body of `db-next-record' and `db-jump-to-record'.
(defmacro dbf-goto-record-internal (&rest record-selector-body)
  (declare (indent 0) (debug body))
  `(progn
     ;; NOTE: This line will cause problems if some hook function
     ;;       deliberately raises an error (like Joe Wells' do).
     (db-view-mode)
     (dbf-process-current-record-maybe nil)
     ,@record-selector-body
     (edb--S! :original (aref (edb--1D dbc-database :vov)
                              (1- (edb--S :index))))
     (db-display-record (dbf-displayed-record) t)))

(defun db-jump-to-record (arg &optional respect-hiding)
  "Show the database's ARGth record.  If ARG is a record, use its index.
Hiding is ignored unless optional argument RESPECT-HIDING is specified."
  ;; NOTE: Respect and ignorance have opposite nature.
  (interactive "NJump to record number: ")
  (unless (integerp arg)
    (let ((vov (edb--1D dbc-database :vov))
          (rno (edb--1D dbc-database :nrecords))
          (i 0))
      ;; linear search, blech
      (while (and (< i rno) (not (integerp arg)))
        (when (eq (aref vov i) arg)
          (setq arg (1+ i)))
        (incf i))
      (unless (integerp arg)
        ;; make `db-select-record' throw out-of-range error
        (setq arg -1))))
  (db-in-data-display-buffer
    (dbf-goto-record-internal
      (db-select-record arg (not respect-hiding))))
  (when (db-summary-buffer-p)
    (dbs-synch-summary-with-format)))

(defun db-first-record (&optional ignore-hiding)
  "Show the database's first record.
With optional prefix argument, ignores hiding."
  (interactive "P")
  (cond ((db-data-display-buffer-p)
         (db-jump-to-record 1 (not ignore-hiding)))
        ((db-summary-buffer-p)
         ;; fixme: handle hiding. --ttn
         (goto-char (point-min))
         (db-jump-to-point))
        (t
         (error "db-first-record called in wrong context."))))

(defun db-last-record (&optional ignore-hiding)
  "Show the database's last record.
With optional prefix argument, ignores hiding."
  (interactive "P")
  (cond ((db-data-display-buffer-p)
         (db-jump-to-record (edb--1D dbc-database :nrecords)
                            (not ignore-hiding)))
        ((db-summary-buffer-p)
         (goto-char (point-max))
         (db-jump-to-point))
        (t
         (error "db-last-record called in wrong context"))))

(defun db-next-record (arg &optional ignore-hiding markedp)
  "Go to the ARGth next record.
In that record, go to the current field, if any."
  (interactive "p")
  (when (db-summary-buffer-p)
    (dbs-synch-format-with-summary))
  (db-in-data-display-buffer
    (let ((this-fidx (edb--S :this-fidx)))
      (dbf-goto-record-internal
        (db-select-next-record arg ignore-hiding markedp))
      ;; If in edit mode, stay in edit mode in the same field.
      (when (and this-fidx (edb--S :stay-in-edit-mode-p))
        (db-move-to-field-exact this-fidx))))
  (when (db-summary-buffer-p)
    (let ((index (with-current-buffer (edb--S :data-display-buffer)
                   (edb--S :index))))
      ;; This might not be right, depending on what records are summarized.
      (dbs-forward-record (- index (edb--S :index)))
      (dbs-set-index index))))

(defsubst db-previous-record (arg &optional ignore-hiding markedp)
  "Go to the ARGth previous record.
In that record, go to the current field, if any."
  (interactive "p")
  (db-next-record (- arg) ignore-hiding markedp))

(defsubst db-next-record-ignore-hiding (arg)
  "Go to the ARGth next record, ignoring omissions.
That is, all records, even those which are hidden, are counted."
  (interactive "p")
  (db-next-record arg t))

(defsubst db-previous-record-ignore-hiding (arg)
  "Go to the ARGth previous record, ignoring omissions.
That is, all records, even those which are hidden, are counted."
  (interactive "p")
  (db-next-record-ignore-hiding (- arg)))

(defsubst db-next-marked-record (arg)
  "Go to the ARGth next marked record.
Hidden records are treated according to db-hide-p."
  (interactive "p")
  (db-next-record arg nil t))

(defsubst db-previous-marked-record (arg)
  "Go to the ARGth previous marked record.
Hidden records are treated according to db-hide-p."
  (interactive "p")
  (db-next-marked-record (- arg)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Moving from record to record (setting :index)
;;;

;; These don't display, but they do set `:index'.

;; Don't forget that when moving off a record, must check whether it has
;; been modified and, if so, call an update function.

(defun dbc-set-index (index &optional record)
  (unless record
    (setq record (aref (edb--1D dbc-database :vov) (1- index))))
  (edb--S! :index index)
  (edb--S! :index-fraction
           (let ((frac (format "%s%d/%d"
                               (if (edb-tagp
                                    (edb-tag :markedp dbc-database)
                                    record)
                                   "+" "")
                               index
                               (edb--1D dbc-database :nrecords))))
             (if (and (edb--S :hide-p) (edb-tagp
                                        (edb-tag :hiddenp dbc-database)
                                        record))
                 (concat "[" frac "]")
               frac))))

(defun db-select-next-record (n &optional ignore-hiding markedp)
  ;; Set new `:index' to point the Nth following, or if N is negative,
  ;; the -Nth preceding, record.  Respect `:hide-p' and `:wraparound'.
  (interactive "p")
  (let* ((up (< 0 n))
         (hidep (and (edb--S :hide-p) (not ignore-hiding)))
         (wrapp (or (eq t (edb--S :wraparound))
                    (and (eq 'delay (edb--S :wraparound))
                         (eq :failed last-command))))
         (idx (edb--S :index))
         (vov (edb--1D dbc-database :vov))
         (rno (edb--1D dbc-database :nrecords))
         (new (flet ((new-idx (delta) (1+ (% (+ rno delta -1 idx) rno))))
                (if (or hidep markedp)
                    ;; slow path
                    (let ((sign (if up 1 -1))
                          (good nil)
                          (stop (cond (wrapp idx)
                                      (up rno)
                                      (t 1)))
                          (htag (and hidep (edb-tag :hiddenp dbc-database)))
                          (mtag (and markedp (edb-tag :markedp dbc-database)))
                          record)
                      (while (not (or (zerop n)
                                      (if (not wrapp)
                                          (prog1 (= idx stop)
                                            (setq idx (new-idx sign)))
                                        (setq idx (new-idx sign))
                                        (= idx stop))))
                        (setq record (aref vov (1- idx)))
                        (unless (or (and htag (edb-tagp htag record))
                                    (and mtag (not (edb-tagp mtag record))))
                          (setq good idx)
                          (decf n sign)))
                      (or good (edb--S :index)))
                  ;; fast path
                  (new-idx (cond (wrapp n)
                                 (up (min n (- rno idx)))
                                 (t (max n (- 1 idx)))))))))
    (dbc-set-index (if (/= new (edb--S :index))
                       new
                     (setq this-command :failed)
                     new)
                   (aref vov (1- new)))
    (when (eq :failed this-command)
      (message "%s record" (if up "Last" "First")))))

(defun db-select-first-record (&optional ignore-hiding)
  ;; Select first record.  Does no display.
  ;; If hiding is in effect, select the first unhidden record, unless
  ;; optional argument IGNORE-HIDING is non-nil.
  (interactive)
  (if (and (edb--S :hide-p)
           (not ignore-hiding)
           (edb-tagp (edb-tag :hiddenp dbc-database)
                     (aref (edb--1D dbc-database :vov) 0)))
      (progn
        (edb--S! :index 1)
        (db-select-next-record 1))
    (dbc-set-index 1)))

(defun db-select-record (index &optional ignore-hiding)
  ;; Select record at INDEX.  Throw error if INDEX out of range.
  ;; If that record is hidden, select the first following non-hidden record,
  ;; unless optional argument IGNORE-HIDING is non-nil.
  (interactive "nRecord number: ")
  (let ((rno (edb--1D dbc-database :nrecords)))
    (unless (and (<= 1 index) (<= index rno))
      (error "Record number %d out of range 1..%d" index rno)))
  (db-select-first-record ignore-hiding)
  (db-select-next-record (1- index) ignore-hiding))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hybrid field/record movement commands
;;;

(defun db-next-screen-or-record (arg)
  "Go to the ARGth next screenful of this display, or to the ARGth
next record, if this is the last screenful of this display.
If point is in the summary buffer and the data display buffer is not visible,
then move to the next record."
  (interactive "p")
  (cond ((db-data-display-buffer-p)
         (dbf-next-screen-or-record arg))
        ((db-summary-buffer-p)
         (let ((w (get-buffer-window (edb--S :data-display-buffer))))
           (if w
               (progn
                 (with-selected-window w
                   (dbf-next-screen-or-record arg))
                 (dbs-synch-summary-with-format))
             (db-next-record arg))))))

(defun dbf-next-screen-or-record (arg)
  (let ((pmax (point-max)))
    (if (= pmax (window-end))
        (db-next-record arg)
      (while (and (> arg 0) (not (= pmax (window-end))))
        (scroll-up nil)
        (decf arg)))))

(defun db-previous-screen-or-record (arg)
  "Go to the ARGth previous screenful of this display, or to the ARGth
previous record, if this is the first screenful of this display.
If point is in the summary buffer and the data display buffer is not visible,
then move to the previous record."
  (interactive "p")
  (cond ((db-data-display-buffer-p)
         (dbf-previous-screen-or-record arg))
        ((db-summary-buffer-p)
         (let ((w (get-buffer-window (edb--S :data-display-buffer))))
           (if w
               (progn
                 (with-selected-window w
                   (dbf-previous-screen-or-record arg))
                 (dbs-synch-summary-with-format))
             (db-previous-record arg))))))

(defun dbf-previous-screen-or-record (arg)
  (let ((pmin (point-min)))
    (if (= pmin (window-start))
        (db-previous-record arg)
      (while (and (> arg 0) (not (= pmin (window-start))))
        (scroll-down nil)
        (decf arg))
      (when (= pmin (window-start))
        (goto-char (point-min))))))


(defun db-beginning-of-field-or-record ()
  "Move to the beginning of this field.
If already at its beginning, move to the first field."
  (interactive)
  (if (= (point) (edb--S :fbeg))
      (db-first-field)
    (db-beginning-of-field)))

(defun db-end-of-field-or-record ()
  "Move to the end of this field; if at its end, to the last field."
  (interactive)
  (if (= (point) (dbf-this-field-end-pos))
      (db-last-field)
    (db-end-of-field)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Adding and deleting records
;;;

(defun db-add-record (&optional append)
  "Add a new record to the database immediately before the current record.
Prefix arg APPEND means to add after the last record, instead.  After adding,
move point to the new record's first field and switch to Database Edit mode."
  (interactive "P")
  (when (db-summary-buffer-p)
    (pop-to-buffer (edb--S :data-display-buffer)))
  (let* ((nfields (length (database-fieldnames dbc-database)))
         (defaults-slice (db-rs-slice dbc-database 'edb--1rs-default-value))
         (new (make-vector nfields nil)))
    (dotimes (fno nfields)
      (aset new fno (aref defaults-slice fno)))
    (when db-new-record-function
      (funcall db-new-record-function new dbc-database))
    (database-add-record new dbc-database (unless append
                                            (edb--S :index))))
  (edb--S! :index (if append
                      (edb--1D dbc-database :nrecords)
                    (1+ (edb--S :index))))
  ;; Why doesn't this need to be (dbc-set-modified-p t)?
  (database-set-modified-p dbc-database t)
  ;; Probably unnecessary, as `database-add-record' has done the trick.
  (dbf-set-summary-out-of-date-p)
  ;; fixme: update incrementally instead of fully. --ttn
  (let ((sumbuf (edb--S :sumbuf)))
    (when (and sumbuf (get-buffer-window sumbuf))
      (db-in-buffer sumbuf
        (dbs-synch-summary-with-format))))
  (db-previous-record (if append 0 1))
  (db-message "%sed a new record" (if append "Append" "Insert"))
  ;; Begin editing the new record.  Don't use `db-edit-mode'.
  (db-first-field))

(defun db-delete-record (&optional force)
  "Remove the current record from the database.
With a prefix argument, doesn't verify."
  ;; fixme: handle "no more records" situation --ttn
  (interactive "P")
  (when (or force (y-or-n-p "Delete this record? "))
    (let ((cur (edb--S :index))
          (vov (edb--1D dbc-database :vov))
          (rno (edb--1D dbc-database :nrecords)))
      ;; stash
      (edb--1D! dbc-database :deleted-record (aref vov (1- cur)))
      ;; shift down and clear highest
      (do ((i (1- cur) (1+ i)))
          ((= (1- rno) i))
        (aset vov i (aref vov (1+ i))))
      (aset vov (decf rno) nil)
      (edb--1D! dbc-database :nrecords rno)
      (edb--S! :index (if (= 1 cur) ; ugh
                          rno
                        (1- cur))))
    ;; update the summary directly
    (database-set-modified-p dbc-database t)
    (db-message "Record deleted")
    (db-next-record 1)))

(defun db-yank-record (endp)
  "Insert, and make current, the most recently deleted record.
The deleted record is inserted before the current record.
With prefix argument ENDP, insert at end of database and don't select it."
  (interactive "P")
  (unless (edb--1D dbc-database :deleted-record)
    (error "No deleted record to yank."))
  (db-in-data-display-buffer
    ;; This is inelegant in the extreme, but the interaction of
    ;; `dbc-set-index' and db-{previous-next}-record and
    ;; `database-add-record' mystifies me.  --karl@owl.hq.ileaf.com (Karl Berry)
    (database-add-record (edb--1D dbc-database :deleted-record)
                         dbc-database (unless endp
                                        (edb--S :index)))
    (if endp
        ;; We go back to the current record below.
        (db-next-record 1)
      (dbc-set-index (1+ (edb--S :index)))
      (db-previous-record 1)
      (database-set-modified-p dbc-database t)))
  (when (db-summary-buffer-p)
    (dbs-synch-summary-with-format))
  (force-mode-line-update)
  (db-message "Record yanked"))

(defun db-copy-record (&optional arg)
  "Insert a copy of the current record in the database immediately after it.
The second of the two records is made the current record.
With a prefix argument, inserts that many copies."
  (interactive "p")
  (db-in-data-display-buffer
    (dbf-process-current-record-maybe t)
    (let* ((vov (edb--1D dbc-database :vov))
           (rec (aref vov (1- (edb--S :index)))))
      (while (> arg 0)
        (database-add-record (copy-sequence rec) dbc-database (edb--S :index))
        (dbc-set-index (1+ (edb--S :index)))
        (decf arg))))
  (when (db-summary-buffer-p)
    (dbs-synch-summary-with-format))
  (force-mode-line-update)
  (db-message "Record copied"))

(defun db-output-record-to-db (db)
  "Copy (output) the current record to database DB.
DB must be read in and compatible with the current database."
  ;; Make a list of databases compatible with this one.
  (interactive
   (list
    (let ((last (edb--1D dbc-database :db-for-output))
          choices sel)
      (dolist (db (edb--1all-known-databases))
        (when (and (not (eq db dbc-database))
                   (databases-compatible db dbc-database))
          (push (cons (or (database-print-name db)
                          (edb--1D db :file))
                      db)
                choices)))
      (unless choices
        (error "No compatible databases are currently read in!"))
      (unless (and last (member last (mapcar 'car choices)))
        (edb--1D! dbc-database :db-for-output (setq last nil)))
      (setq sel (completing-read
                 "Output record to which database (? for choices): "
                 choices nil t last))
      (unless (string= "" sel)
        (edb--1D! dbc-database :db-for-output sel))
      (cdr (assoc sel choices)))))
  (when db
    (when (db-summary-buffer-p)
      (dbs-synch-format-with-summary))
    (with-current-buffer (if (db-summary-buffer-p)
                             (edb--S :data-display-buffer)
                           (current-buffer))
      (let ((rec (aref (edb--1D db :vov) (1- (edb--S :index)))))
        (db-in-data-display-buffer
          (database-add-record rec db))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sorting
;;;

(defun db-sort (&optional dont-confirm)
  "Sort the database.  With a prefix argument, don't confirm the sort order."
  (interactive "P")
  (let ((cur (current-buffer)))
    (db-in-data-display-buffer
      (dbf-process-current-record-maybe t)
      ;; Save current record for the sake of `dbf-finished-sorting'.
      (edb--S! :record/before-sorting (aref (edb--1D dbc-database :vov)
                                            (1- (edb--S :index))))
      (edb--S! :buffer/before-sorting cur)
      (if dont-confirm
          (progn
            (database-sort dbc-database)
            (dbf-finished-sorting))
        (database-sort-interface dbc-database)))))

(defun dbf-finished-sorting ()
  ;; Recompute the current record's index, dropping the saved ref immediately.
  (let ((rec (prog1 (or (edb--S :record/before-sorting)
                        (aref (edb--1D dbc-database :vov) 0))
               (edb--S! :record/before-sorting nil))))
    (dbc-set-index (catch t (db-lmap (lambda (record)
                                       (when (eq rec record)
                                         (throw t db-lmap-index)))
                                     dbc-database))
                   rec))

  (let (sumbuf window)
    (when (setq sumbuf (edb--S :sumbuf))
      ;; Force summary refresh (due to re-ordering only) and maybe display.
      (with-current-buffer sumbuf (edb--S! :nrecords -1))
      (if (setq window (get-buffer-window sumbuf))
          (with-selected-window window (db-summary))
        (save-window-excursion
          (with-current-buffer sumbuf (db-summary))))))

  ;; The index shown in the mode line is correct, but the database may have
  ;; been marked as modified, and that change hasn't made it to the mode line.
  (force-mode-line-update)

  (switch-to-buffer (prog1 (or (edb--S :buffer/before-sorting)
                               (current-buffer))
                      (edb--S! :buffer/before-sorting nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Editing
;;;

(defun db-field-query-replace ()
  "Replace some instances of a value in this field with some other value.
Confirms before each replacement."
  (interactive)
  (unless (edb--S :this-fidx)
    (error "Call this when on a field."))
  (let* ((iftxt    (edb--S :iftxt))
         (cur-idx  (edb--S :index))
         (dispspec (edb--S :this-ds))
         (fsno     (edb--S :this-fidx))
         (ridx     (edb--1ds-record-index dispspec))
         (fname    (db-fname<-fno ridx dbc-database))
         (ordfunc  (db-rs-ordfunc
                    (aref (edb--1D dbc-database :elaborated-rfspecs) ridx)))
         ov                             ; original value
         ov-printed
         rv                             ; replacement value
         rv-printed)
    (dbf-process-current-record-maybe nil)
    (setq ov (db-callconvert
              (edb--1ds-display->actual dispspec)
              (read-string "Query replace: ")
              ;; No previous value or record.
              nil nil
              ridx))
    (db-check-constraint ov nil ridx dbc-database)
    ;; Must keep in mind that this is not necessarily what the user typed.
    (setq ov-printed (db-callconvert
                      (edb--1ds-actual->display dispspec)
                      ov nil ridx))

    (setq rv (db-callconvert
              (edb--1ds-display->actual dispspec)
              (read-string (format "Query replace %s with: "
                                   ov-printed))
              nil nil ridx))
    (db-check-constraint rv nil ridx dbc-database)
    (setq rv-printed (db-callconvert
                      (edb--1ds-actual->display dispspec)
                      rv nil ridx))

    (db-maprecords
     (lambda (record)
       (when (= 0 (funcall ordfunc ov (aref record ridx)))
         (db-display-record record t)
         (db-skip-string-forward (aref iftxt 0))
         (edb--S! :this-fidx 0)
         (db-next-field-internal fsno)
         ;; fixme: handle strings too big for minibuffer. --ttn
         (when (y-or-n-p (format "(%s) Replace `%s' with `%s'? "
                                 fname ov-printed rv-printed))
           ;; It's a bit extreme that this errs if the value
           ;; fails to meet the constraint.
           (db-check-constraint rv record ridx dbc-database)
           (aset record ridx rv)))))
    (db-message "Replacement done")
    (db-jump-to-record cur-idx t)))

(defun db-accept-record ()
  "Install the current record in the database; make any changes permanent."
  (interactive)
  (dbf-process-current-record-maybe t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Searching
;;;

(defun db-search-field (pattern &optional mark)
  "Search for occurrences of PATTERN in the current field of any record.
Finds the first match after the current record; wraps around automatically.
With prefix argument, marks all matches in addition to going to the first one.
If hiding is in effect, hidden records are ignored."
  (interactive
   (list (let ((this-fidx (edb--S :this-fidx)))
           (unless this-fidx
             (error "Not on a field in a data display buffer."))
           (read-string (format "Search in %s for: "
                                (dbf-this-field-name (edb--S :this-ds)))
                        (aref (edb--S :search-defaults) this-fidx)))
         current-prefix-arg))
  (if (equal "" pattern)
      (error "You didn't enter a pattern for which to search."))

  (let* ((cur-rec (aref (edb--1D dbc-database :vov) (1- (edb--S :index))))
         (mtag (edb-tag :markedp dbc-database))
         (this-ds (edb--S :this-ds))
         (pat (db-parse-match-pattern pattern this-ds))
         (pdisp (db-print-match-pattern pat this-ds))
         (ridx (edb--1ds-record-index this-ds))
         (rs (aref (edb--1D dbc-database :elaborated-rfspecs) ridx))
         (this-field-index (edb--S :this-fidx))
         this-field
         (fname (dbf-this-field-name this-ds))
         ;; success is t if we've already found some match.
         ;; The idea is that we'll move to the record at success-index when
         ;; we're done with the search; if success is nil then we're looking
         ;; for such a record.  This is either because we haven't found one or
         ;; because we have only found one before :index in the database.
         success success-record success-index
         (matches 0))
    (aset (edb--S :search-defaults) this-field-index pdisp)
    (if mark
        (db-message "Marking all %s in %s..." pdisp fname)
      (db-message "Searching in %s for %s..." fname pdisp))
    (db-lmap
     (lambda (record)
       (when (or mark (not success))
         (setq this-field (aref record ridx))
         (when (db-match pat this-field rs)
           (if (not success)
               (setq success-record record
                     success-index db-lmap-index
                     success t))
           (when mark
             (incf matches)
             (edb-tagx mtag record))))
       ;; We're looking for a match in some record besides the displayed
       ;; one and, preferrably, after it.  This permits the first success
       ;; succeeding the current record to overwrite the first success
       ;; preceding the current record.  This means that searches can't
       ;; abort after a success, since that success might be before the
       ;; current record.
       (when (eq record cur-rec)
         (setq success nil)))
     dbc-database
     (edb--S :hide-p))
    (if success-index
        (if (= (edb--S :index) success-index)
            (db-message "This record has the only match for %s" pdisp)
          ;; This takes care of committing any changes to the current record.
          (dbf-goto-record-internal
            (dbc-set-index success-index success-record))
          (db-move-to-field-exact this-field-index)
          (if mark
              (progn (dbf-set-summary-out-of-date-p)
                     (db-message "Searching for %s...marked %s matches"
                                 pdisp matches))
            (db-message "Searching for %s...found" pdisp)))
      (db-message "Couldn't find a match in %s for %s" fname pdisp))))


(defun db-search ()
  "`db-search' is not yet implemented; use `db-search-field' instead.
In a future version of EDB, `db-search' will permit searching on all fields
of a record simultaneously."
  (interactive)
  (error "Unimplemented; use db-search-field instead (M-s from Edit mode)."))


;; fixme: convert to after-command hooks. --ttn

(defun db-isearch-forward ()
  "Like isearch-forward, but maintains the correspondence between the format
and summary buffers."
  (interactive)
  (isearch-forward)
  (db-jump-to-point))

(defun db-isearch-backward ()
  "Like isearch-backward, but maintains the correspondence between the format
and summary buffers."
  (interactive)
  (isearch-backward)
  (db-jump-to-point))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hiding
;;;

(defun db-mark-record (&optional arg)
  "Toggle whether the current record is marked.
With a nonzero prefix argument, set it to be marked.
With a zero prefix argument, set it to be unmarked."
  (interactive "P")
  (db-in-data-display-buffer
    (let* ((mtag (edb-tag :markedp dbc-database))
           (idx (edb--S :index))
           (rec (aref (edb--1D dbc-database :vov) (1- idx))))
      (funcall (if (if arg
                       (not (zerop (prefix-numeric-value arg)))
                     (not (edb-tagp mtag rec)))
                   'edb-tagx
                 'edb-tag-)
               mtag rec)
      (dbf-update-summary-item idx)
      (dbc-set-index idx rec)
      (force-mode-line-update))))

(defun db-hide-record (&optional arg)
  "Change whether the current record is hidden.
With a nonzero prefix argument, set it to be hidden.
With a zero prefix argument, set it to be unhidden."
  (interactive "P")
  (db-in-data-display-buffer
    (let* ((htag (edb-tag :hiddenp dbc-database))
           (idx (edb--S :index))
           (rec (aref (edb--1D dbc-database :vov) (1- idx))))
      (funcall (if (if arg
                       (not (zerop (prefix-numeric-value arg)))
                     (not (edb-tagp htag rec)))
                   'edb-tagx
                 'edb-tag-)
               htag rec)
      (if (edb--S :hide-p)
          (dbf-update-summary-item idx)
        ;; Automatically turn on the effect of hiding.
        (dbc-set-hide-p t)
        ;; Update all marks, since potentially all have to be displayed now.
        (dbf-update-summary-marks))
      (dbc-set-index idx rec)
      (force-mode-line-update))))

(defun db-mark-searched-records ()
  "Mark all records found in search STRING of FIELD."
  (interactive)
  (setq current-prefix-arg "1")
  (call-interactively 'db-search-field "1")
  (let ((sumbuf (edb--S :sumbuf)))
    (when sumbuf
      (save-window-excursion
        (switch-to-buffer-other-window sumbuf)
        (dbs-synch-format-with-summary)))))


(defun db-hide-unmarked-records ()
  "Hide all unmarked records.  Also, clear mark bits and enable hiding."
  (interactive)
  (db-in-data-display-buffer
    (let ((mtag (edb-tag :markedp dbc-database))
          (htag (edb-tag :hiddenp dbc-database)))
      (db-lmap
       (lambda (record)
         (if (edb-tagp mtag record)
             (edb-tag- mtag record)
           (edb-tagx htag record)))
       dbc-database
       t))
    (dbc-set-hide-p t)
    (dbf-update-summary-marks)
    ;; fixme: redisplay? --ttn
    ))

(defun db-mark-unhidden-records ()
  "Mark all unhidden records.  Also clears all hide bits."
  (interactive)
  (db-in-data-display-buffer
    (let ((hiddenp (edb-tag :hiddenp dbc-database))
          (markedp (edb-tag :markedp dbc-database)))
      (db-lmap
       (lambda (record)
         (if (edb-tagp hiddenp record)
             (edb-tag- hiddenp record)
           (edb-tagx markedp record)))
       dbc-database))
    (dbc-set-hide-p t)
    (dbf-update-summary-marks)
    ;; fixme: redisplay? --ttn
    ))

(defun db-unhide-all ()
  "Clear the hide bit of every record."
  (interactive)
  (db-in-data-display-buffer
    (let ((htag (edb-tag :hiddenp dbc-database)))
      (db-lmap
       (lambda (record)
         (edb-tag- htag record))
       dbc-database))
    (dbc-set-index (edb--S :index))
    (force-mode-line-update)
    (dbf-update-summary-marks)))

(defun db-unmark-all ()
  "Clear the mark bit of every record."
  (interactive)
  (db-in-data-display-buffer
    (let ((mtag (edb-tag :markedp dbc-database)))
      (db-lmap
       (lambda (record)
         (edb-tag- mtag record))
       dbc-database))
    (dbc-set-index (edb--S :index))
    (force-mode-line-update)
    (dbf-update-summary-marks)))

(defun db-hiding-toggle (&optional arg)
  "Change whether hiding is in effect.
With a nonzero prefix argument, turn hiding on.
With a zero prefix argument, turn hiding off.

This does not change the current hide-function, and a hide bit is always
computed for each record, but hide bits have no effect on any operations
if hiding is not in effect."
  (interactive "P")
  (db-in-data-display-buffer
    (dbc-set-hide-p (if arg
                        (not (zerop (prefix-numeric-value arg)))
                      (not (edb--S :hide-p))))
    ;; Refill summary buffer whenever displayed set of records changes,
    ;; including when switching to no hiding and showing hidden records.
    (cond
     ((edb--S :invisible-hidden-p)
      ;; If the hidden records weren't being shown, the records that
      ;; should be displayed in the summary buffer just changed.
      (dbf-fill-summary-buffer t))
     (t
      (dbf-update-summary-marks)))
    (force-mode-line-update)
    (db-message "Hiding is now %sin effect" (if (edb--S :hide-p) "" "not "))))

(defun db-hiding-set ()
  "Set the criteria for automatically determining whether to hide a record.
This isn't implemented yet."
  (interactive)
  (error "db-hiding-set is not yet implemented."))

(defun db-toggle-show-hidden-records (&optional arg)
  "Toggle whether hidden records are shown in the summary.
With a nonzero prefix argument, show hidden records in the summary.
With a zero prefix argument, don't show hidden records in the summary."
  (interactive "P")
  (db-in-data-display-buffer
    (let ((inv-p (if arg
                     (zerop (prefix-numeric-value arg))
                   (not (edb--S :invisible-hidden-p))))
          (sumbuf (edb--S :sumbuf)))
      (edb--S! :invisible-hidden-p inv-p)
      (if (not inv-p)
          ;; If we weren't showing hidden records, we might as well start from
          ;; scratch in filling the summary buffer.
          (dbf-fill-summary-buffer t)
        (when sumbuf
          (db-in-buffer sumbuf
            (let ((buffer-read-only nil))
              (goto-char (point-min))
              (delete-matching-lines "^.\\[")
              (dbs-move-to-proper-record)))))
      (db-message "Hidden records will %snow be shown"
                  (if inv-p "not " "")))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Reporting
;;;

(defun db-report (filename &optional markedp)
  "Create a report according to format specifications in FILENAME.
Prefix argument MARKEDP, if non-nil, means report on only marked records.
If hiding is in effect, hidden records are not reported upon.
When called interactively, prompt for FILENAME."
  (interactive
   (list (let* ((ddb-spec (edb--S :ddb-spec))
                (conn (when ddb-spec
                        (with-current-buffer ddb-spec
                          (get-text-property (point-min) :connection))))
                (tspec (when conn
                         (edb--with-callable-connection conn
                           (plist-get (conn :schema) :report)))))
           (or tspec (read-file-name "Report format file: " nil nil t)))
         current-prefix-arg))
  (dbf-process-current-record-maybe t)
  (let* ((db dbc-database)
         (lasfl (db-format->lines/sforms
                 (cond ((consp filename)
                        (plist-get filename :text))
                       ((stringp filename)
                        (with-temp-buffer
                          (insert-file-contents filename)
                          (buffer-string))))
                 db nil nil t))
         (rfunc `(lambda (formatted-record)
                   (insert ,@(cdr lasfl))))
         (hide-p (edb--S :hide-p))
         (mtag (edb-tag :markedp db)))
    (switch-to-buffer (get-buffer-create "*Database Report*"))
    (setq buffer-read-only nil)
    (erase-buffer)
    (db-lmap
     (lambda (record)
       (when (or (not markedp) (edb-tagp mtag record))
         (funcall rfunc record)))
     db
     hide-p)
    (goto-char (point-min))))

;;; db-interfa.el ends here
