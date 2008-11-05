;;; simple.sls --- Simple C type utilities.

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

;; These utilities are "simple" in the sense that they don't use GType
;; machinery or any shlibs.

;;; Code:

(library (sbank ctypes simple)
  (export pointer+

          utf8z-ptr->string
          string->utf8z-ptr
          ->utf8z-ptr/null

          pointer-utf8z-ptr-set!
          pointer-utf8z-ptr-ref)
  (import (rnrs)
          (spells foreign))

  (define (utf8z-ptr->string ptr)
    (let ((size (do ((i 0 (+ i 1)))
                    ((= (pointer-ref-c-unsigned-char ptr i) 0) i))))
      (utf8->string (memcpy (make-bytevector size) ptr size))))
  
  (define (->utf8z-ptr/null who s)
    (cond ((string? s) (string->utf8z-ptr s))
          ((eqv? s #f)
           (integer->pointer 0))
          (else
           (assertion-violation who "invalid argument" s))))

  (define (string->utf8z-ptr s)
    (let* ((bytes (string->utf8 s))
           (bytes-len (bytevector-length bytes))
           (result (malloc (+ bytes-len 1))))
      (memcpy result bytes bytes-len)
      (pointer-set-c-char! result bytes-len 0)
      result))

  (define (pointer-utf8z-ptr-set! ptr i val)
    (pointer-set-c-pointer! ptr i (if (pointer? val)
                                      val
                                      (string->utf8z-ptr val))))

  (define (pointer-utf8z-ptr-ref ptr i)
    (let ((utf8z-ptr (pointer-ref-c-pointer ptr i)))
      (if (= (pointer->integer utf8z-ptr) 0)
          #f
          (utf8z-ptr->string utf8z-ptr))))
  
  (define (pointer+ p n)
    (integer->pointer (+ (pointer->integer p) n))))