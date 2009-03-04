;;; ptr-table.sls --- Pointer table

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

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

;; A table of values that are keyed by C pointers.

;;; Code:
#!r6rs

(library (sbank support ptr-table)
  (export make-ptr-table
          ptr-table-ref
          ptr-table-add!
          ptr-table-remove!
          ptr-table-drain-pool!)
  (import (rnrs base)
          (rnrs control)
          (rnrs records syntactic)
          (rnrs hashtables)
          (spells foreign))

  (define-record-type ptr-table
    (fields
     (immutable tbl)
     (mutable pool)
     (mutable id))
    (protocol (lambda (p)
                (lambda ()
                  (p (make-eqv-hashtable) '() 0)))))

  (define key-size 8)
  (define ptr-key-ref pointer-uint64-ref)
  (define ptr-key-set! pointer-uint64-set!)
  
  (define ptr-table-ref
    (let ((not-found (list 'not-found)))
      (lambda (tbl ptr default)
        (let ((val (hashtable-ref (ptr-table-tbl tbl)
                                  (ptr-key-ref ptr 0)
                                  not-found)))
          (if (eq? val not-found) default val)))))

  (define (ptr-table-add! tbl val)
    (let ((next-id (+ (ptr-table-id tbl) 1))
          (ptr (cond ((null? (ptr-table-pool tbl))
                      (malloc key-size))
                     (else
                      (let* ((pool (ptr-table-pool tbl))
                             (ptr (car pool)))
                        (ptr-table-pool-set! tbl (cdr pool))
                        ptr)))))
      (hashtable-set! (ptr-table-tbl tbl) next-id val)
      (ptr-table-id-set! tbl next-id)
      (ptr-key-set! ptr 0 next-id)
      ptr))

  (define (ptr-table-remove! tbl ptr)
    (hashtable-delete! (ptr-table-tbl tbl) (ptr-key-ref ptr 0))
    (ptr-table-pool-set! tbl (cons ptr (ptr-table-pool tbl)))
    (when (= (hashtable-size (ptr-table-tbl tbl)) 0)
      (ptr-table-id-set! tbl 0)))

  (define (ptr-table-drain-pool! tbl)
    (for-each free (ptr-table-pool tbl))
    (ptr-table-pool-set! tbl '()))
  
  )
