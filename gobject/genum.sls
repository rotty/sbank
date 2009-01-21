;;; genum.sls --- support for enumerations and flags.

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
#!r6rs

(library (sbank gobject genum)
  (export
   genumerated?
   genumerated-lookup genumerated-values genumerated-symbols genumerated-gtype

   make-genum genum?

   make-gflags gflags?
   gflags->integer integer->gflags
   )
  (import (rnrs)
          (only (srfi :43 vectors) vector-fold)
          (spells tracing)
          (sbank support utils))

  (define-record-type genumerated
    (fields set? gtype symbols values)
    (protocol (lambda (p)
                (lambda (set? gtype alist)
                  (p set? gtype
                     (list->vector (map car alist))
                     (list->vector (map cdr alist)))))))

  (define (make-genum gtype alist)
    (make-genumerated #f gtype alist))

  (define (genum? thing)
    (and (genumerated? thing)
         (not (genumerated-set? thing))))

  (define (make-gflags gtype alist)
    (make-genumerated #t gtype alist))

  (define (gflags? thing)
    (and (genumerated? thing)
         (genumerated-set? thing)))

  ;; Note: this could be made more efficient by using sorted vectors
  ;; (but only in one direction)
  (define (genumerated-lookup enum sym-or-val)
    (if (symbol? sym-or-val)
        (cond ((vector-index eq? (genumerated-symbols enum) sym-or-val)
               => (lambda (i) (vector-ref (genumerated-values enum) i)))
              (else #f))
        (cond ((vector-index eqv? (genumerated-values enum) sym-or-val)
               => (lambda (i) (vector-ref (genumerated-symbols enum) i)))
              (else #f))))

  (define (gflags->integer flags lst)
    (fold-left
     (lambda (val elt)
       (bitwise-ior
        (cond ((and (symbol? elt) (genumerated-lookup flags elt))
               => values)
              ((integer? elt) elt)
              (else
               (error 'gflags->integer
                      "could not convert flag to integer" elt)))
        val))
     0
     lst))

  (define (integer->gflags flags n)
    (let ((val-vec (genumerated-values flags))
          (sym-vec (genumerated-symbols flags)))
      (vector-fold (lambda (i val sym result)
                     (if (= (bitwise-and n val) val)
                         (cons sym result)
                         result))
                   (genumerated-values flags)
                   (genumerated-symbols flags))))

  )
