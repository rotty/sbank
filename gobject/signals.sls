;;; signals.sls --- GObject signal support.

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


(library (sbank gobject signals)
  (export g-signal-connect g-signal-emit)
  (import (rnrs base)
          (rnrs control)
          (spells receive)
          (spells foreign)
          (spells tracing)
          (only (spells assert) cerr)
          (xitomatl srfi and-let*)
          (xitomatl srfi let-values)
          (sbank utils)
          (sbank shlibs)
          (sbank ctypes basic)
          (sbank typelib)
          (sbank type-data)
          (sbank gobject gvalue)
          (sbank gobject gtype)
          (sbank gobject internals))

  (define signal-destroy-notify-ptr
    ((make-c-callback 'void '(pointer pointer))
     (lambda (data closure)
       ;; TODO: we should somehow free the callback here, but the
       ;; current ikarus FFI doesn't allow us doing that
       #f)))

  (define g-signal-connect
    (let-callouts libgobject
        ((g-signal-connect-data
          'ulong "g_signal_connect_data" '(pointer pointer pointer pointer pointer int)))
      (define (lose msg . irritants)
        (apply error 'g-signal-connect msg irritants))
      (lambda (instance signal callback)
        (let*-values (((signal detail detailed-signal) (parse-signal-spec signal lose))
                      ((detailed-signal-ptr) (string->utf8z-ptr detailed-signal)))
          (let ((id (g-signal-connect-data
                     (ginstance-ptr instance)
                     detailed-signal-ptr
                     (cond ((gobject-class-get-signal-callback
                             (ginstance-class instance)
                             signal) => (lambda (wrap) (wrap callback)))
                           (else
                            (lose "no such signal" detailed-signal)))
                     (integer->pointer 0)
                     signal-destroy-notify-ptr
                     0)))
            (free detailed-signal-ptr)
            id)))))

  (define gquark-ctype 'uint32)

  (define g-signal-lookup
    (let-callouts libgobject ((lookup% 'uint "g_signal_lookup" `(pointer ,gtype-ctype)))
      (lambda (signal class)
        (let* ((name-ptr (string->utf8z-ptr (symbol->string signal)))
               (rv (lookup% name-ptr (gobject-class-gtype class))))
          (free name-ptr)
          rv))))

  (define g-signal-emit
    (let-callouts libgobject ((g-signal-emitv
                               'void "g_signal_emitv" `(pointer uint ,gquark-ctype pointer)))
      (lambda (instance signal . args)
        (define (lose msg . irritants)
          (apply error 'signal-emit msg irritants))
        (receive (signal detail detailed-signal) (parse-signal-spec signal lose)
          (let* ((signal-id (g-signal-lookup signal (ginstance-class instance)))
                 (signature (gobject-class-get-signal-signature (ginstance-class instance) signal))
                 (rti (signature-rti signature))
                 (atis (signature-atis signature))
                 (n-args (+ 1 (length atis)))
                 (arg-gvs (g-value-alloc n-args)))
            (unless (= n-args 1)
              (error 'g-signal-emit "signal arguments not yet implemented" instance signal args))
            (g-value-init! (pointer+ arg-gvs 0) (gobject-class-gtype (ginstance-class instance)))
            (g-value-set! (pointer+ arg-gvs 0) (ginstance-ptr instance))
            (cond ((eq? (type-info-type rti) 'void)
                   (g-signal-emitv arg-gvs signal-id detail (integer->pointer 0))
                   (do ((i 0 (+ i 1)))
                       ((>= i n-args))
                     (g-value-unset! (pointer+ arg-gvs (* i g-value-size))))
                   (free arg-gvs))
                  (else
                   (let ((ret-gv (g-value-new (type-info-gtype rti))))
                     (g-signal-emitv arg-gvs signal-id detail ret-gv)
                     (let ((rv (g-value-ref ret-gv)))
                       (g-value-free ret-gv)
                       rv)))))))))

  (define (parse-signal-spec signal lose)
    (cond ((string? signal)
           (let ((parts (string-split signal #\:)))
             (case (length parts)
               ((1) (values (string->symbol (car parts)) 0 signal))
               ((2) (values (string->symbol (car parts)) (cadr parts) signal))
               (else
                (lose "invalid signal name" signal)))))
          ((symbol? signal)
           (values signal 0 (symbol->string signal)))
          (else
           (lose "invalid signal specification" signal)))))
