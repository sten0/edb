;;; db-util.el --- part of EDB, the Emacs database

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

;; Lisp utilities.
;; This file is largely cannibalized from util-mde.el and util-mdecl.el,
;; which are available from theory.lcs.mit.edu:/pub/emacs/.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Database messages
;;;

(defun db-message (fmtstr &rest args)
  ;; Format message, display it, and log it in buffer *Database-Log*.
  (let ((formatted (apply 'format fmtstr args)))
    (with-current-buffer (get-buffer-create "*Database-Log*")
      (buffer-disable-undo)
      (goto-char (point-max))
      (insert formatted "\n"))
    (message "%s" formatted)))

(defmacro db-warning (fmtstr &rest args)
  `(db-message ,(concat "Warning: " fmtstr) ,@args))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Searching, matching, and replacing
;;;

(defun db-unused-char-in-buffer ()
  "Return a character not used in the current buffer, or nil.
This function attempts to return a character that can be displayed in a single
screen column."
  (save-excursion
    (let ((maybe 32)
          (rv t))
      (while (eq rv t)
        (goto-char (point-min))
        (if (not (search-forward (char-to-string maybe) nil t))
            (setq rv maybe)
          (setq maybe (% (1+ maybe) 256))
          (when (eq maybe 32)
            (setq rv nil))))
      rv)))

(defun db-unused-char-in-string (s)
  "Return a character not used in string S, or nil.
This function attempts to return a character that can be displayed in a single
screen column."
  (with-temp-buffer
    (insert s)
    (db-unused-char-in-buffer)))

;;; Skipping

(defsubst db-skip-string-forward (s)
  (or (string= "" s)
      (let* ((beg (point))
             (len (length s))
             (end (+ beg len)))
        (when (and (<= end (point-max))
                   (string= s (buffer-substring-no-properties beg end)))
          (forward-char len)
          t))))

(defsubst db-skip-string-backward (s)
  (or (string= "" s)
      (let* ((end (point))
             (len (length s))
             (beg (- end len)))
        (when (and (<= (point-min) beg)
                   (string= s (buffer-substring-no-properties beg end)))
          (forward-char (- len))
          t))))

(defsubst db-skip-regexp-forward (rx)
  "If point is at regexp RX, move past it and return point;
otherwise return nil."
  (when (looking-at rx)
    (goto-char (match-end 0))))

;; From Robert Potter <potter@silver.lcs.mit.edu>
(defun db-looking-back-at (rx)
  "Return t when text before point matches regular expression RX."
  (save-excursion
    (save-restriction
      (narrow-to-region (point-min) (point))
      (re-search-backward (concat "\\(" rx "\\)\\'") (point-min) t))))

(defun db-how-many-string-overlapping (s)
  "Return number of matches for string S following point,
including overlapping ones."
  (let ((count 0))
    (save-excursion
      (while (search-forward s nil t)
        (goto-char (1+ (match-beginning 0)))
        (incf count)))
    count))

(defun db-how-many-substring-overlapping (small big)
  "Return number of matches for SMALL in BIG (both strings),
including overlapping ones."
  (let ((rx (regexp-quote small))
        (count 0)
        (start -1))
    (while (setq start (string-match rx big (1+ start)))
      (incf count))
    count))

(defun db-find-char (c s &optional count)
  "Look for char C in string S; return first index in S whose element is C.
If optional arg COUNT is specified, return the COUNTth occurrance."
  (unless count
    (setq count 1))
  (let ((idx 0)
        (len (length s))
        rv)
    (while (and (< idx len) (not rv))
      (when (char-equal c (aref s idx))
        (if (= count 1)
            (setq rv idx)
          (decf count)))
      (incf idx))
    rv))

(defun db-find-char-from-end (c s &optional count)
  "Look for char C in string S; return last index in S whose element is C.
Optional arg COUNT means return the COUNTth occurrance from the end."
  (unless count
    (setq count 1))
  (let ((idx (1- (length s)))
        rv)
    (while (and (> idx -1) (not rv))
      (when (char-equal c (aref s idx))
        (if (= count 1)
            (setq rv idx)
          (decf count)))
      (decf idx))
    rv))

(defsubst db-string-trim-whitespace (s)
  "Return a substring of S with whitespace removed from beginning and end."
  (let* ((beg (when (string-match "\\`[ \t\n\f\r]+" s)
                (match-end 0)))
         (end (string-match "[ \t\n\f\r]+\\'" s (or beg 0))))
    (if (or beg end)
        (substring s (or beg 0) end)
      s)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Variables
;;;

(defun edb--G  (key)       (edb--rget '(:edb1) key))
(defun edb--G! (key value) (edb--rput '(:edb1) key value))

(:edb1 :bprops (make-hash-table :size 11 :test 'eq :weakness 'key))

;; Cache for buffer-local control properties hash table.
(defvar edb--bprops nil)

(defmacro edb--S (property)
  `(gethash ,property edb--bprops))

(defmacro edb--S! (property newvalue)
  `(puthash ,property ,newvalue edb--bprops))

(defun edb-get (property &optional buffer)
  (unless (let* ((single (edb--rget '(:edb1 :schema-schema) 'single))
                 (runtime (cdr (assq :runtime-keys single))))
            (memq property runtime))
    (error "Bad property: %S" property))
  (if buffer
      (edb--rget (list :edb1 :bprops buffer) property)
    (edb--S property)))

(defun edb-put (property newvalue &optional buffer)
  (unless (let* ((single (edb--rget '(:edb1 :schema-schema) 'single))
                 (runtime (cdr (assq :runtime-keys single))))
            (memq property runtime))
    (error "Bad property: %S" property))
  (if buffer
      (edb--rput (list :edb1 :bprops buffer) property newvalue)
    (edb--S! property newvalue)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Files
;;;

(defun db-locate-readable-file-prefer-cwd (filename path &optional suffixes)
  "Return the full path of a readable file named FILENAME, located in
`default-directory' or on PATH, a list of directories (strings).
Optional arg SUFFIXES is a list of suffixes to append to FILENAME
when searching."
  (setq path (cons default-directory path))
  ;; The following is more or less equivalent to:
  ;;   (locate-file filename path suffixes 'readable)
  ;; but `locate-file' is not yet (2005-01-18) widely available.
  (catch 'full
    (dolist (dir path)
      (unless dir
        (setq dir default-directory))
      (dolist (suf (or suffixes '("")))
        (let ((try (expand-file-name (concat filename suf) dir)))
          (when (file-readable-p try)
            (throw 'full try)))))
    nil))

(defun db-same-file-p (n1 n2)
  "Return t if N1 and N2 are names for the same file.
Return nil if neither N1 nor N2 names an existing file."
  (setq n1 (file-chase-links n1)
        n2 (file-chase-links n2))
  (or (equal n1 n2)
      (equal n1 (file-name-nondirectory n2))
      (equal n2 (file-name-nondirectory n1))
      (and (file-exists-p n1) (file-exists-p n2)
           (equal (file-attributes n1)
                  (file-attributes n2)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Conversion
;;;

;; string-to-int is unacceptable because it returns 0 for unparseable values.

(defun db-string->integer-or-nil (s)
  (let ((rv (condition-case nil
                (car (read-from-string s))
              (error nil))))
    (and (integerp rv) rv)))
(defalias 'db-string->number-or-nil 'db-string->integer-or-nil)

(defun db-string->integer (s)
  "Return the integer represented by string S, or err."
  (or (db-string->integer-or-nil s)
      (error "db-string->integer: `%s' doesn't look like an integer." s)))
(defalias 'db-string->number 'db-string->integer)

(defun db-number-or-nil->string (n)
  (if (numberp n)
      (number-to-string n)
    ""))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buffers
;;;

(defmacro db-in-buffer (buffer &rest body)
  ;; This macro, which works when body moves point in a buffer
  ;; displayed in a window other than the selected window, is from
  ;; Joe Wells <jbw@cs.bu.edu>.  (If Lisp code moves point in a
  ;; buffer displayed in a window other than the selected window,
  ;; Emacs kindly restores point in the buffer to its window's
  ;; version of point.)
  (declare (indent 1) (debug (form body)))
  `(let ((targ ,buffer)
         (this-buffer (current-buffer))
         (db-in-buffer-thunk (lambda () ,@body)))
     (if (eq targ this-buffer)
         (funcall db-in-buffer-thunk)
       ;; Can't use save-excursion here because we only want to save the
       ;; current buffer, not its value for point.
       (unwind-protect
           (progn
             (set-buffer targ)
             (let* ((w (get-buffer-window targ))
                    (trackp (and (not (eq w (selected-window)))
                                 (eq (window-point w) (point)))))
               (prog1
                   (funcall db-in-buffer-thunk)
                 (when (and trackp
                            (eq (current-buffer) targ)
                            (eq w (get-buffer-window targ))
                            (not (eq w (selected-window))))
                   (set-window-point w (point))))))
         (if (and (bufferp this-buffer) (buffer-name this-buffer))
             (set-buffer this-buffer))))))


(defun db-copy-buffer-local-variables (buf &rest exclude)
  "Copy the values of most of BUF's local variables into the current buffer.
The var `enable-multibyte-characters' is handled by `set-buffer-multibyte'
and `buffer-undo-list' is excluded along with any of the other variable names
\(symbols\) listed in EXCLUDE."
  (let (symbol value)
    (dolist (pair (with-current-buffer buf (buffer-local-variables)))
      (unless (or (atom pair)
                  (memq (setq value (cdr pair)
                              symbol (car pair))
                        exclude))
        (case symbol
          ((0 nil buffer-undo-list))    ; do nothing
          (enable-multibyte-characters
           (set-buffer-multibyte value))
          (t
           (set (make-local-variable symbol) value)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Strings
;;;

(defun db-string-split-last-word (s &optional butnot)
  "Return list of two strings (all-but-last-word last-word).
If there is only one word, return (S \"\").
The result strings can be concatenated to return the original string,
with the exception of some number (at least one) of spaces and tabs,
and possibly a comma immediately preceding them.
Optional arg BUTNOT, if non-nil, is a regexp (containing spaces or tabs)
which, if found at the end of S, should be considered a single word."
  (if (let ((delim ",?[ \t]+"))
        (or (and butnot (string-match (concat delim "\\(" butnot "\\)$") s))
            (string-match (concat delim "\\([a-zA-Z0-9'-]+\\)$") s)))
      (list (substring s 0 (match-beginning 0))
            (substring s (match-beginning 1)))
    (list s "")))

(defun db-string-split-first-word (s)
  "Return list of strings (first-word remaining-words).
String S is split at the first sequence of spaces and tabs."
  (if (string-match "[ \t]+" s)
      (list (substring s 0 (match-beginning 0))
            (substring s (match-end 0)))
    (list s "")))

(defun db-count-newlines (s)
  "Return the number of newline characters found in string S."
  (let ((rv 0))
    (dotimes (idx (length s))
      (when (char-equal ?\n (aref s idx))
        (incf rv)))
    rv))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Cursor movement
;;;

(defun db-forward-line-wrapping (arg)
  "Like forward-line, but wrap around to the beginning of the buffer if
it encounters the end."
  (interactive "p")
  (let ((left (forward-line arg)))
    (cond ((or (> left 0) (not (bolp)))
           (goto-char (point-min))
           (db-forward-line-wrapping left))
          ((< left 0)
           (goto-char (point-max))
           (db-forward-line-wrapping (1+ left))))))

(defun db-current-line ()
  "Return the line number of the line containing point."
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (1+ (count-lines 1 (point))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; File local variables
;;;

(defun edb-true (&rest args)
  "Return t unconditionally, no matter the ARGS."
  t)

(defun edb-hookp (val)
  "Return t if VAL appears to be a hook.
That is, if it is satisfies `functionp', or it is a list of
elements each of which is either t or satisfies `functionp'."
  (or (functionp val)
      (and (consp val)
           (equal (make-list (length val) t)
                  (mapcar (lambda (v)
                            (or (eq t v)
                                (functionp v)))
                          val)))))

(defun db-really-hack-local-variables ()
  "Like `hack-local-variables', but without inhibitions."
  (let ((enable-local-eval t)
        (enable-local-variables t))
    (hack-local-variables)))


;;; db-util.el ends here
