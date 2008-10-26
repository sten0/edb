;;; db-lemacs.el --- part of EDB, the Emacs database

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

;; This file collects all the previously scattered support for Lucid Emacs.
;; WARNING: Maintainer does not use Lucid Emacs so this code is not tested.

;;; Code:

(unless (get 'emacs-version 'edb-running-lemacs)
  (error "Do not load `db-lemacs' by itself; loading `database' is enough"))

;;
;; formerly in db-format.el
;;

(or (find-face 'db-inter-field-face)
    (make-face 'db-inter-field-face))

(or (face-differs-from-default-p 'db-inter-field-face)
    (copy-face 'bold 'db-inter-field-face))

(add-hook 'db-display-record-redo-inter-field-text-function
          (lambda (buf ls)
            (save-excursion
              (set-buffer buf)
              (if (null ls)
                  (map-extents (lambda (x y) (delete-extent x))
                               (current-buffer) (point-min) (point-max)
                               nil)
                (let (start end ext-start)
                  (dolist (pair ls)
                    (setq start (car pair)
                          end (cdr pair))
                    ;; This is a bit of a hack.  Leaving out the white space
                    ;; stops the field text from occassionally taking on the
                    ;; 'db-inter-field-face'.  If the user did not use white
                    ;; space the this would evidently not work.
                    (goto-char start)
                    (skip-chars-forward " \t\n")
                    (setq ext-start (point))
                    (goto-char end)
                    (skip-chars-backward " \t\n")
                    (when (< ext-start (point))
                      (set-extent-face
                       (make-extent ext-start (point))
                       'db-inter-field-face))))))))

;;
;; formerly in db-interfa.el
;;

(define-key database-view-mode-map [mouse1] 'db-lucid-mouse-jump-to-point)
(define-key database-view-mode-map [mouse3] 'database-view-mode-menu)

(define-key database-edit-mode-map [mouse1] 'db-lucid-mouse-jump-to-point)
(define-key database-edit-mode-map [mouse3] 'database-edit-mode-menu)

(define-key database-summary-mode-map [mouse1] 'db-lucid-mouse-jump-to-point)

(defun db-lucid-mouse-jump-to-point (e)
  "Move to the field or record nearest the mouse position.
See `db-jump-to-point' for more details."
  (interactive "@e")                    ; @ = select buffer, e = event
  (mouse-track e)                       ; set point to where the mouse is
  (db-jump-to-point))

(defun dbs-lucid-mouse-view (e)
  "Visit record under mouse in Database View mode."
  (interactive "@e")
  (mouse-set-point e)
  (db-jump-to-point)
  (dbs-view))

;;
;; formerly in db-summary.el
;;

(require 'mode-motion)
(add-hook 'database-summary-mode-hooks
          (lambda () (setq mode-motion-hook 'mode-motion-highlight-line)))
;; (db-lucid-summary-mode-menubar)

(define-key database-summary-mode-map [mouse1] 'db-lucid-mouse-jump-to-point)
(define-key database-summary-mode-map [mouse2] 'dbs-lucid-mouse-view)
(define-key database-summary-mode-map [mouse3] 'database-summary-mode-menu)

;;; db-lemacs.el ends here
