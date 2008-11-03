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
          array-is-zero-terminated?
          array-size
          array-element-type-info
          array-element-type
          make-array-type
          array-length-index

          signature?
          make-signature
          signature-rti
          signature-atis
          signature-callout
          signature-callback
          
          make-type-info
          type-info?
          type-info-type
          type-info-is-pointer?
          type-info-null-ok?

          make-property-info
          property-info?
          property-info-type-info
          property-info-type
          property-info-is-pointer?
          property-info-readable?
          property-info-writable?
          property-info-construct?
          property-info-construct-only?)
  (import (rnrs base)
          (rnrs records syntactic)
          (spells foreign)
          (spells tracing)
          (sbank utils))

  (define-record-type array-type
    (fields (immutable element-type-info array-element-type-info)
            (immutable zero-terminated array-is-zero-terminated?)
            (immutable size array-size)
            (immutable length-index array-length-index)))

  (define (array-element-type atype)
    (type-info-type (array-element-type-info atype)))
  
  (define-record-type type-info
    (fields type is-pointer? null-ok?))

  (define-record-type property-info
    (fields type-info readable? writable? construct? construct-only?))

  (define (property-info-type pinfo)
    (type-info-type (property-info-type-info pinfo)))

  (define (property-info-is-pointer? pinfo)
    (type-info-is-pointer? (property-info-type-info pinfo)))
  
  (define-record-type signature
    (fields (mutable rti signature-rti% signature-set-rti%!)
            (mutable atis signature-atis% signature-set-atis%!)
            (mutable callout signature-callout% signature-set-callout%!)
            (mutable callback signature-callback% signature-set-callback%!))
    (protocol (lambda (p)
                (lambda (rti atis callout callback)
                  (p (make-lazy-entry rti)
                     (make-lazy-entry atis)
                     (make-lazy-entry callout)
                     (make-lazy-entry callback))))))

  (define (lazy-forcer accessor setter)
    (lambda (obj)
      (let ((val (accessor obj)))
        (if (lazy-entry? val)
            (let ((new-val ((lazy-entry-proc val))))
              (setter obj new-val)
              new-val)
            val))))

  (define signature-rti (lazy-forcer signature-rti% signature-set-rti%!))
  (define signature-atis (lazy-forcer signature-atis% signature-set-atis%!))
  (define signature-callout (lazy-forcer signature-callout% signature-set-callout%!))
  (define signature-callback (lazy-forcer signature-callback% signature-set-callback%!)))
