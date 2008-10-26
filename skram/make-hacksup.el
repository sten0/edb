;;; make-hacksup.el

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

(fset 'vc-backend (lambda (file) nil))  ; a little too smart sometimes

(let ((files (mapcar 'expand-file-name command-line-args-left))
      form struct-names explicit indents indents-1)
  ;; first pass
  (dolist (file files)
    (find-file file)
    (goto-char (point-min))
    (while (setq form (ignore-errors (read (current-buffer))))
      (unless (consp form) (setq form nil))
      (case (car form)
        (defstruct
          (let ((conc (cadr form)))
            (setq conc
                  (if (consp conc)
                      (or (cadr (assq :conc-name (cdr conc)))
                          (format "%s-" (car conc)))
                    (format "%s-" conc)))
            (push (let ((s (format "%sset-" conc)))
                    (cons s (length s)))
                  struct-names)
            (dolist (slot (cddr form))
              (push (make-symbol (format "%sset-%s" conc
                                         (if (consp slot)
                                             (car slot)
                                           slot)))
                    indents-1))))
        (defmacro
          (let ((name (cadr form))
                (maybe-declare (cdddr form)))
            (setq maybe-declare (if (stringp (car maybe-declare))
                                    (cadr maybe-declare)
                                  (car maybe-declare)))
            (when (and (consp maybe-declare)
                       (eq 'declare (car maybe-declare)))
              (let ((n (cadr (assq 'indent (cdr maybe-declare)))))
                (when n (push `(put ',name 'lisp-indent-function ,n)
                              indents))))))
        (put
         (let ((name (cadr form))
               (prop (caddr form)))
           (when (and (consp name) (eq 'quote (car name)))
             (setq name (cadr name)))
           (when (and (consp prop) (eq 'quote (car prop)))
             (setq prop (cadr prop)))
           (when (memq prop '(lisp-indent-hook lisp-indent-function))
             (push form indents)
             (push name explicit)))))))
  ;; second pass
  (dolist (file files)
    (find-file file)
    (goto-char (point-min))
    (while (setq form (ignore-errors (read (current-buffer))))
      (unless (consp form) (setq form nil))
      (when (memq (car form) '(defun defsubst))
        (let* ((name (cadr form))
               (nstr (symbol-name name)))
          (unless (memq name explicit)
            (when (some (lambda (pair)
                          (eq t (compare-strings
                                 nstr       0 (cdr pair)
                                 (car pair) 0 (cdr pair))))
                        struct-names)
              (push name indents-1))))))
    (kill-buffer nil))
  ;; write it out
  (find-file "hacksup.el")
  (erase-buffer)
  (let ((standard-output (current-buffer)))
    (mapc 'princ '(";;; hacksup.el --- mellifluous munging\n"
                   ";;;    (this file is not installed)\n\n"
                   ";; indentation\n"))
    (pp `(mapc (lambda (func)
                 (put func 'lisp-indent-function 1))
               ',indents-1))
    (mapc 'pp indents)
    (princ "\n;;; hacksup.el ends here\n"))
  (save-buffer)
  (kill-buffer nil))

;;; make-hacksup.el ends here
