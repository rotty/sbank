;;; decorators.sls --- Decorator registry for typelib.

;; Copyright (C) 2008, 2009 Andreas Rottmann <a.rottmann@gmx.at>

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

(library (sbank typelib decorators)
  (export register-typelib-decorator
          lookup-typelib-decorator)
  (import (rnrs base)
          (rnrs hashtables)
          (rnrs arithmetic fixnums)
          (srfi :39 parameters))

  (define hash-bits (- (fixnum-width) 1))
  (define hash-mask (fxnot (fxarithmetic-shift -1 hash-bits)))
  
  (define (key-hash key)
    (fxxor (fxand (string-hash (car key)) hash-mask)
           (fxand (string-hash (cdr key)) hash-mask)))

  (define *typelib-decorators* (make-hashtable key-hash equal?))
  
  (define (lookup-typelib-decorator namespace name)
    (hashtable-ref *typelib-decorators* (cons namespace name) #f))

  (define (register-typelib-decorator namespace name decorator)
    (hashtable-update! *typelib-decorators*
                       (cons namespace name)
                       (lambda (old-decorator)
                         (if old-decorator
                             (lambda (obj)
                               (decorator (old-decorator obj)))
                             decorator))
                       #f)))
