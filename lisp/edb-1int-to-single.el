;;; edb-1int-to-single.el

;; Copyright (C) 2006-2017 Thien-Thi Nguyen

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

;; This file provides the single command `edb-1int-to-single',
;; as well as the feature by the same name.  See info file.

;;; Code:

(eval-when-compile (require 'cl))
(require 'edbcore)

(defun edb-1int-to-single (filename)
  "Translate contents of FILENAME to a \"single\" schema-schema.
If the contents are not in EDB 1.x \"internal file layout, format 0.7\",
signal an error.  Otherwise, leave the result of the translation in
a newly created output buffer named \":EDB (single) from FILENAME\".

The output buffer may have several \"<FIXME>\" tokens in it,
indicating places where further attention (a nice way to say
\"manual tweaking\") is required to complete the translation.

See info node `(edb) edb-1int-to-single' for the complete list
of possible <FIXME> occurances and suggested remedies."
  (interactive "fTranslate EDB 1.x \"internal file layout\": ")
  (let ((fixme "<FIXME>")
        ;; todo: for EDB 2.x, replace computation w/ (its) constant value
        (mm-idx (let ((slots (mapcar (lambda (ent)
                                       (intern (format ":%s" (car ent))))
                                     (edb--struct-slot-info
                                      'edb--v1-monolithic-mess))))
                  (map 'list 'cons slots (number-sequence 0 (length slots)))))
        mm extra records coding loc-block locals)
    (cl-flet
        ((elm () (let ((emacs-lisp-mode-hook nil))
                   (emacs-lisp-mode)))
         (mref (slot) (aref mm (cdr (assq slot mm-idx))))
         (cprop (prop &optional more) (insert (format "%s" prop)
                                              (if more
                                                  (format " %S" more)
                                                "")
                                              "\n"))
         (nlnl (&optional stuff) (insert (if stuff
                                             (format "%s" stuff)
                                           "")
                                         "\n\n"))
         (nl () (insert "\n"))
         (backslash-hat (c) (cond ((= ?\n c) (insert c))
                                  ((> 32 c) (insert "\\^" (+ c 64)))
                                  (t (insert c)))))
      (with-temp-buffer
        (elm)
        (insert-file-contents filename)
        (goto-char (point-min))
        (unless (looking-at ";; Database file written by EDB; format 0.7")
          (error "Not in \"internal file layout, format 0.7\": %s" filename))
        (forward-line 1)
        (setq mm (read (current-buffer))
              extra (read (current-buffer))
              records (buffer-substring-no-properties
                       (progn (forward-line 1) (point))
                       (point-max))
              coding coding-system-for-read))
      (switch-to-buffer
       (generate-new-buffer
        (concat ":EDB (single) from " filename)))
      (buffer-disable-undo)
      (setq default-directory (file-name-directory filename))
      (elm)
      (let ((standard-output (current-buffer))
            v)
        (insert ":EDB (single) ;;; -*- mode:emacs-lisp; coding:")
        (princ (or coding fixme))
        (nlnl "; -*-")
        (cprop :name)
        (prin1 (or (mref :print-name) fixme))
        (nlnl)
        (cprop :fields)
        (pp (map 'vector 'cons (mref :fieldnames) (mref :recordfieldspecs)))
        (nl)
        (when (setq v (car (mref :field-priorities)))
          (cprop :field-order)
          (pp (apply 'vector
                     (cl-flet
                         ((ok (x) (cond ((symbolp x)
                                         x)
                                        ((numberp x)
                                         (aref (mref :fieldnames) x))
                                        (t
                                         (error "badness")))))
                       (mapcar (lambda (spec)
                                 (cond ((and (consp spec)
                                             (not (consp (cdr spec))))
                                        (ok (car spec)))
                                       ((consp spec)
                                        (cons (ok (car spec)) (cdr spec)))
                                       (t
                                        (ok spec))))
                               v))))
          (nl))
        (let ((fsub (mref :sub-fieldsep-string))
              (rsub (mref :sub-recordsep-string)))
          (when (or rsub fsub)
            (cprop :substitution-separators)
            (let ((standard-output 'backslash-hat))
              (pp (vector fsub rsub)))
            (nl)))
        (when (setq v (mref :substitutions))
          (cprop :substitutions)
          (let ((standard-output 'backslash-hat))
            (pp (apply 'vector v)))
          (nl))
        (let (try limit)
          (cl-flet
              ((yes (c) (when c
                          (cprop :display t)
                          (setq limit (+ (cadr (insert-file-contents try))
                                         (point)))
                          (when (re-search-forward "\f*\nLocal Variables:\n"
                                                   limit 1)
                            (goto-char limit)
                            (setq loc-block (buffer-substring-no-properties
                                             (match-end 0) limit))
                            (delete-region (match-beginning 0) limit))
                          (unless (bolp)
                            (insert "\n"))
                          t)))
            (cond ((yes (and (setq try (assq :format-file extra))
                             (setq try (expand-file-name
                                        (cdr try)
                                        (file-name-directory filename)))
                             (file-readable-p try))))
                  ((yes (setq try (db-locate-readable-file-prefer-cwd
                                   (file-name-nondirectory
                                    (file-name-sans-extension filename))
                                   (cons (file-name-directory filename)
                                         db-format-file-path)
                                   db-format-file-suffixes))))
                  (t
                   (cprop :display (list (intern fixme)))
                   (insert fixme "\n"))))
          (nlnl :EOTB))
        (cprop :data '(:coding t :seqr read-line :EOTB ":EOTB"))
        (insert records ":EOTB")
        (when loc-block
          (goto-char (point-min))
          (forward-line 1)
          (insert "\n;;; From primary format file's local variables block:\n")
          (narrow-to-region (point) (point-max))
          (insert loc-block)
          (delete-region (point) (progn (search-backward "\nEnd:")
                                        (1+ (point))))
          (narrow-to-region (point-min) (point))
          (goto-char (point-min))
          (while (< (point) (point-max))
            (cond ((looking-at "^eval:")
                   (delete-char 5)
                   (let* ((opoint (point))
                          (form (read (current-buffer)))
                          (special (assq (car form)
                                         '((database-set-fieldnames-to-list
                                            "redundant")
                                           (dbf-set-summary-format
                                            :summary-format car)))))
                     (if special
                         (let* ((why (cadr special))
                                (blurb (if (stringp why)
                                           why
                                         (format "%s `%s' %s"
                                                 "translated to"
                                                 why
                                                 "control property")))
                                (xlat (caddr special)))
                           (delete-region opoint (progn (forward-line 1)
                                                        (point)))
                           (insert ";;*" blurb ":\n;; "
                                   (format "(%s ...)" (car form))
                                   "\n")
                           (when xlat
                             (save-excursion
                               (save-restriction
                                 (widen)
                                 (re-search-forward "^:data")
                                 (beginning-of-line)
                                 (cprop why)
                                 (let ((standard-output 'backslash-hat)
                                       (v (funcall xlat (cdr form))))
                                   (pp v)
                                   (if (stringp v)
                                       (nlnl)
                                     (nl)))))))
                       (goto-char opoint)
                       (delete-horizontal-space)
                       (indent-sexp)
                       (insert ";;*unhandled (" fixme "):\n")
                       (comment-region (point) (progn (forward-sexp 1)
                                                      (forward-line 1)
                                                      (point))))))
                  (t
                   (let* ((opoint (point))
                          (var (progn (looking-at "\\s-*\\([^:]+\\):\\s-*")
                                      (prog1 (intern (match-string 1))
                                        (goto-char (match-end 0)))))
                          (val (prog1 (read (current-buffer))
                                 (forward-line 1)))
                          (form `(set (make-local-variable ',var)
                                      ,(if (or (stringp val)
                                               (vectorp val))
                                           val
                                         (list 'quote val)))))
                     (case var
                       ((db-new-record-function
                         dbf-first-change-function
                         dbf-every-change-function
                         dbf-before-display-record-function
                         dbf-format-name-spec-alist
                         edb-data-coding)
                        (comment-region opoint (point))
                        (save-excursion
                          (goto-char opoint)
                          (insert ";;*special variable (" fixme "):\n")))
                       (t
                        (setq locals (acons var val locals))
                        (delete-region opoint (point))
                        (insert (format ";;*added to `:locals' %s: %s\n"
                                        "control property"
                                        var))))))))
          (widen)
          (delete-blank-lines))
        (goto-char (point-min))
        (when (setq locals (nconc (nreverse locals) (mref :locals)))
          (re-search-forward "^:data")
          (beginning-of-line)
          (cprop :locals)
          (pp (apply 'vector (mapcar (lambda (pair)
                                       (list (car pair) (cdr pair)))
                                     locals)))
          (nl))
        (goto-char (point-min))))))

(provide 'edb-1int-to-single)

;;; edb-1int-to-single.el ends here
