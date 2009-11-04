;;; type-data.sls --- Meta-data about types.

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

(library (sbank support type-data)
  (export arg-flags
          list->arg-flags
          arg-flags-any?
          arg-flags-case
          arg-flags->free-spec
          free-spec-shift
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
          make-simple-signature
          signature-rti
          signature-atis
          signature-callout
          signature-callback
          callback-destroy-notify

          make-type-info
          type-info?
          type-info-type
          type-info-is-pointer?
          type-info-null-ok?
          type-info-closure-index
          type-info-destroy-index
          type-info-parameters
          type-info-scope

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
          (spells foreign)
          (spells tracing)
          (sbank support utils))

  (define-enumeration arg-flag
    (transfer-ownership
     transfer-container-ownership
     in
     out
     in-out)
    arg-flags)

  (define list->arg-flags (enum-set-constructor (arg-flags)))
  
  (define-syntax arg-flags-case
    (syntax-rules (else)
      ((arg-flags-case flags
         ((test-flag ...) clause-expr ...)
         ...
         (else else-expr ...))
       (let ((val flags))
         (cond ((arg-flags-any? val (arg-flags test-flag ...))
                clause-expr ...)
               ...
               (else else-expr ...))))
      ((arg-flags-case flags clause ...)
       (arg-flags-case flags clause ... (else (unspecific))))))

  (define (arg-flags-any? flags test-flags)
    (not (enum-set=? (enum-set-intersection flags test-flags)
                     (arg-flags))))

  (define (free-spec-set? free-spec)
    (fxbit-set? free-spec 0))

  (define (free-spec-any? free-spec)
    (not (fxzero? free-spec)))
  
  (define (free-spec-shift free-spec)
    (fxarithmetic-shift free-spec -1))
  
  (define (arg-flags->free-spec flags)
    (arg-flags-case flags
      ((in) (arg-flags-case flags
              ((transfer-container-ownership) #b10)
              ((transfer-ownership)           #b00)
              (else                           #b11)))
      (else (arg-flags-case flags
              ((transfer-container-ownership) #b01)
              ((transfer-ownership)           #b11)
              (else                           #b00)))))

  (define-record-type array-type
    (fields (immutable element-type-info array-element-type-info)
            (immutable zero-terminated array-is-zero-terminated?)
            (immutable size array-size)
            (immutable length-index array-length-index)))

  (define (array-element-type atype)
    (type-info-type (array-element-type-info atype)))

  (define-record-type type-info
    (fields type
            is-pointer?
            null-ok?
            closure-index
            destroy-index
            parameters
            scope)
    (protocol
     (lambda (p)
       (case-lambda
         ((type is-pointer? null-ok?)
          (p type is-pointer? null-ok? #f #f '() #f))
         ((type is-pointer? null-ok? parameters)
          (p type is-pointer? null-ok? #f #f parameters #f))
         ((type is-pointer? null-ok? closure-index destroy-index scope)
          (p type is-pointer? null-ok? closure-index destroy-index '() scope))))))

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
            (mutable callout signature-callout% signature-set-callout%!)
            (mutable callback signature-callback% signature-set-callback%!)
            (mutable cb-pool
                     signature-cb-pool
                     signature-set-cb-pool!))
    (protocol (lambda (p)
                (lambda (rti atis callout callback)
                  (p (make-lazy-entry rti)
                     (make-lazy-entry atis)
                     (make-lazy-entry callout)
                     (make-lazy-entry callback)
                     '())))))

  (define (make-simple-signature rtype atypes)
    (define (type->ti type)
      (case type
        ((pointer) (make-type-info 'void #t #t))
        (else
         (make-type-info type #f #f))))
    (make-signature (type->ti rtype)
                    (map type->ti atypes)
                    (lambda ()
                      (make-c-callout rtype atypes))
                    (lambda ()
                      (make-c-callback rtype atypes))))

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
  (define signature-callout
    (lazy-forcer signature-callout% signature-set-callout%!))

  ;; Here we work the "pool magic", by wrapping the actual callback
  ;; procedures inside a "reusable" wrapper. When the callback is done
  ;; with, we return the wrapper to the pool.

  (define-record-type pooled-cb
    (fields (immutable ptr)
            (mutable proc))
    (protocol (lambda (p)
                (lambda (prepare proc)
                  (letrec* ((outer (lambda args (apply (pooled-cb-proc self) args)))
                            (self (p (prepare outer) proc)))
                    self)))))

  (define (pooled-cb-reclaimer sig pooled-cb)
    (lambda ()
      (pooled-cb-proc-set! pooled-cb #f)
      (signature-set-cb-pool!
       sig
       (cons pooled-cb (signature-cb-pool sig)))))

  (define (signature-pop-pooled-cb! sig)
    (let ((pooled-cb (car (signature-cb-pool sig))))
      (signature-set-cb-pool! sig (cdr (signature-cb-pool sig)))
      pooled-cb))

  (define signature-callback
    (let ((force-cb
           (lazy-forcer signature-callback% signature-set-callback%!)))
      (lambda (sig)
        (let ((prepare (force-cb sig)))
          (lambda (proc)
            (let ((pooled-cb
                   (if (null? (signature-cb-pool sig))
                       (make-pooled-cb prepare proc)
                       (let ((pcb (signature-pop-pooled-cb! sig)))
                         (pooled-cb-proc-set! pcb proc)
                         pcb))))
              (values (pooled-cb-ptr pooled-cb)
                      (pooled-cb-reclaimer sig pooled-cb))))))))

  ;; We kindof cheat here to be more general, and don't specify any
  ;; argument types, but this is not a problem because of the C
  ;; calling convention (i.e. caller-pops-args).
  (define destroy-notify-signature
    (make-simple-signature 'void '()))

  (define destroy-notify-callback
    (signature-callback destroy-notify-signature))

  (define (callback-destroy-notify reclaim-callback)
    (let ((reclaim-destroy-notify #f))
      (receive (destroy-notify reclaim)
               (destroy-notify-callback
                (lambda ()
                  (reclaim-callback)
                  (reclaim-destroy-notify)))
        (set! reclaim-destroy-notify reclaim)
        destroy-notify)))

  )

;; Local Variables:
;; scheme-indent-styles: ((arg-flags-case 1))
;; End:
