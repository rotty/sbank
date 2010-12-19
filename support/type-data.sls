;;; type-data.sls --- Meta-data about types.

;; Copyright (C) 2008-2010 Andreas Rottmann <a.rottmann@gmx.at>

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

(library (sbank support type-data)
  (export free-spec-shift
          free-spec-set?
          free-spec-any?
          
          array-type?
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
          signature-gtype-lookup
          signature-cached-callback
          
          callback-destroy-notify

          make-type-info
          type-info?
          type-info-type
          type-info-is-pointer?
          type-info-allow-none?
          type-info-parameters
          type-info->prim-type

          type-tag-symbol->prim-type

          make-arg-info
          arg-info?
          arg-info-closure-index
          arg-info-destroy-index
          arg-info-scope
          arg-info-direction
          arg-info-ownership
          arg-info-free-spec

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
          (rnrs control)
          (rnrs lists)
          (rnrs enums)
          (rnrs arithmetic fixnums)
          (rnrs records syntactic)
          (rnrs hashtables)
          (srfi :8 receive)
          (only (spells misc) unspecific)
          (only (spells opt-args) lambda*)
          (spells foreign)
          (spells tracing)
          (sbank support utils)
          (sbank support callback-pool)
          (sbank gobject internals data)
          (sbank gobject genum)
          (sbank gobject gtype))

  ;;; Free specification
  
  (define (free-spec-set? free-spec)
    (fxbit-set? free-spec 0))

  (define (free-spec-any? free-spec)
    (not (fxzero? free-spec)))
  
  (define (free-spec-shift free-spec)
    (fxarithmetic-shift free-spec -1))

  ;;; Array types
  
  (define-record-type array-type
    (fields (immutable element-type-info array-element-type-info)
            (immutable zero-terminated array-is-zero-terminated?)
            (immutable size array-size)
            (immutable length-index array-length-index)))

  (define (array-element-type atype)
    (type-info-type (array-element-type-info atype)))

  
  ;;; Type information
  
  (define-record-type type-info
    (fields type is-pointer? allow-none? parameters))

  (define (type-tag-symbol->prim-type sym)
    (case sym
      ((gtype) gtype-ctype)
      ((size) 'size_t)
      ((ssize) 'ssize_t)
      ((boolean) 'uint)
      ((unichar) 'uint32)
      ((utf8 filename gvalue gslist glist ghash) 'pointer)
      (else sym)))

  (define (type-info->prim-type ti out?)
    (let ((type (type-info-type ti)))
      (cond ((or out?
                 (type-info-is-pointer? ti)
                 (array-type? type)
                 (gobject-class? type))
             'pointer)
            ((signature? type)
             'fpointer)
            ((genumerated? type)
             'int)
            ((symbol? type)
             (type-tag-symbol->prim-type type))
            (else
             (assertion-violation 'type-info->prim-type
                                  "argument/return type not yet implemented"
                                  type)))))

  
  ;;; Argument information
  
  (define-record-type arg-info
    (parent type-info)
    (fields direction
            ownership
            closure-index
            destroy-index
            scope)
    (protocol
     (lambda (n)
       (lambda* (type
                 pointer?
                 allow-none?
                 parameters
                 direction
                 (ownership #f)
                 (closure-index #f)
                 (destroy-index #f)
                 (scope #f))
         ((n type pointer? allow-none? parameters)
          direction ownership closure-index destroy-index scope)))))

  (define (arg-info-free-spec info)
    (case (arg-info-direction info)
      ((in in-out)
       (case (arg-info-ownership info)
         ((transfer-container-ownership) #b10)
         ((transfer-ownership)           #b00)
         (else                           #b11)))
      (else ;out
       (case (arg-info-ownership info)
         ((transfer-container-ownership) #b01)
         ((transfer-ownership)           #b11)
         (else                           #b00)))))
  
  (define-record-type property-info
    (fields type-info readable? writable? construct? construct-only?))

  (define (property-info-type pinfo)
    (type-info-type (property-info-type-info pinfo)))

  (define (property-info-is-pointer? pinfo)
    (type-info-is-pointer? (property-info-type-info pinfo)))

  ;; A `signature' record captures all data related to a given C
  ;; function type, including the return type, argument types, the
  ;; procedures to create Scheme wrappers around C function pointers
  ;; of the type indicated (callout) and the procedure to create C
  ;; function pointers from Scheme procedures (callback). Furthermore,
  ;; there is a pool of already-prepared callback C function pointers,
  ;; as there is no way to free the resources associated with these
  ;; (see https://bugs.launchpad.net/ikarus/+bug/336384).
  (define-record-type signature
    (fields (mutable rti signature-rti% signature-set-rti%!)
            (mutable atis signature-atis% signature-set-atis%!)
            (mutable get-cb signature-get-cb% signature-set-get-cb%!)
            (immutable gtype-lookup signature-gtype-lookup))
    (protocol (lambda (p)
                (lambda (rti atis gtype-lookup)
                  (p (make-lazy-entry rti) (make-lazy-entry atis) #f gtype-lookup)))))

  (define (signature-cached-callback signature callback-maker)
    (or (signature-get-cb% signature)
        (let ((get-cb (callback-pool-getter (make-callback-pool (callback-maker)))))
          (signature-set-get-cb%! signature get-cb)
          get-cb)))

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
  
  )

;; Local Variables:
;; scheme-indent-styles: ((type-flags-case 1))
;; End:
