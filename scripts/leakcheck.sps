;;; leakcheck.sps --- Do things that might trigger leaks.

;; Copyright (C) 2008 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(import (rnrs)
        (sbank support utils)
        (sbank gobject)
        (sbank typelib))

(gobject-setup!)
(typelib-import ("Everything" #f))

(define-syntax repeat
  (syntax-rules ()
    ((repeat n body ...)
     (do ((i 0 (+ i 1)))
         ((>= i n))
       body ...))))

(define N 1000)

(define (method-call)
  (repeat N (test-boolean #t))
  (repeat N (test-boolean #f)))

(define (obj-alloc)
  (repeat N (send <test-obj> (new/props))))

(define *tests*
  `((method-call . ,method-call)
    (obj-alloc . ,obj-alloc)))

(define (main argv)
  (let ((tests (append-map (lambda (arg)
                             (let ((sym (string->symbol arg)))
                               (case sym
                                 ((all) (map car *tests*))
                                 (else  (list sym)))))
                           (cdr argv))))
    (for-each (lambda (test)
                (cond ((assq-ref *tests* test) => (lambda (proc) (proc)))
                      (else (println "No code found for {0}, skipping" test))))
              tests)))

(main (command-line))
