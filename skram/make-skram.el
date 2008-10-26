;;; make-skram.el

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

;;; Code:

(unless noninteractive
  (error "See GNUmakefile"))

(require 'cl)

(setq vc-handled-backends nil)          ; documented disabling method

(defun make-skram ()
  (emacs-lisp-mode)
  (let ((out (expand-file-name "skram.data"))
        indexed acc form type name bufs)
    (dolist (idx '(Function Variable))
      (with-temp-buffer

        ;; The next two forms are equivalent to:
        ;;   (info (format "(edb.info)%s Index" idx) (current-buffer))
        ;; but two-arg `info' is not yet (2005-01-18) widely available.
        (info (format "(%s)%s Index" (expand-file-name "edb.info" "../doc") idx))
        (setq buffer-read-only nil)

        (goto-char (point-min))
        (search-forward "* Menu:")
        (while (re-search-forward "^[*] \\([^: ]+\\)[: ]" (point-max) t)
          (pushnew (intern (match-string 1)) indexed))))
    (dolist (file (mapcar 'expand-file-name command-line-args-left))
      (push (find-file file) bufs)
      (goto-char (point-min))
      (while (ignore-errors (setq form (read (current-buffer))))
        (setq type (when (consp form) (symbol-name (car form)))
              name (when type (cadr form)))
        (when (and type name
                   (not (string-match "^define-" type))
                   (string-match "^def" type))
          (setq name (cond ((atom name)
                            name)
                           ((eq 'quote (car name))
                            (cadr name))
                           (t
                            (car name))))
          (pushnew (list name
                         (car form)
                         (when (memq name indexed) t)
                         (list (file-name-nondirectory file)))
                   acc :key 'car))))
    (dolist (buf bufs)
      (with-current-buffer buf
        (let ((file (file-name-nondirectory buffer-file-name))
              case-fold-search)
          (goto-char (point-min))
          (dolist (ent acc)
            (unless (member file (cadddr ent))
              (when (and (search-forward (symbol-name (car ent)) (point-max) t)
                         (memq (char-before (match-beginning 0))
                               '(?( ?) ?' 32)))
                (push file (cadddr ent))
                (goto-char (point-min))))))
        (kill-buffer nil)))
    (erase-buffer)
    (fundamental-mode)
    (dolist (ent acc)
      (insert (format "%s\n:%s\n:%s\n:future unclear\n:"
                      (car ent) (cadr ent)
                      (if (caddr ent) "ok" "maybe")))
      (dolist (file (cadddr ent))
        (insert file "\n"))
      (insert "\n"))
    (let (make-backup-files)
      (write-file out))
    (kill-buffer nil)))

;;; make-skram.el ends here
