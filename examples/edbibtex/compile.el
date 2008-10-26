;; Load and compile edbibtex version 0.4


;; These only serve to avoid compilation warnings.

(defvar db-buffer nil)

(defvar database nil)

(defvar BibTeX-syntax-table (make-syntax-table))


;; Now do some real work:

(load-file "edbibtex.el")

(byte-compile-file "edbibtex.el")