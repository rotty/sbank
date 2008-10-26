;;; type-data.sls --- Meta-data about types.

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


(library (sbank type-data)
  (export array-type?
          array-elements-pointers?
          array-is-zero-terminated?
          array-size
          array-element-type
          make-array-type
          array-length-index

          signature?
          make-signature
          signature-callout
          signature-callback
          
          make-rtype-info
          rtype-info?
          rtype-info-type
          rtype-info-is-pointer?
          rtype-info-null-ok?)
  (import (rnrs base)
          (rnrs records syntactic)
          (spells foreign)
          (spells tracing)
          (sbank utils))

  (define-record-type array-type
    (fields (immutable element-type array-element-type)
            (immutable elements-pointers? array-elements-pointers?)
            (immutable is-zero-terminated array-is-zero-terminated?)
            (immutable size array-size)
            (immutable length-index array-length-index)))

    (define-record-type rtype-info
    (fields (immutable type rtype-info-type)
            (immutable is-pointer? rtype-info-is-pointer?)
            (immutable null-ok? rtype-info-null-ok?)))

  (define-record-type signature
    (fields (mutable callout signature-callout% signature-set-callout%!)
            (mutable callback signature-callback% signature-set-callback%!))
    (protocol (lambda (p)
                (lambda (callout callback)
                  (p (make-lazy-entry callout) (make-lazy-entry callback))))))

  (define (lazy-forcer accessor setter)
    (lambda (obj)
      (if (lazy-entry? (accessor obj))
          (setter obj ((lazy-entry-proc (accessor obj)))))
      (accessor obj)))
  
  (define signature-callout (lazy-forcer signature-callout% signature-set-callout%!))
  (define signature-callback (lazy-forcer signature-callback% signature-set-callback%!)))