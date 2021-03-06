;;; signals.sls --- GObject signal support.

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

(library (sbank gobject signals)
  (export g-signal-connect
          g-signal-disconnect
          g-signal-emit)
  (import (rnrs base)
          (rnrs control)
          (srfi :2 and-let*)
          (srfi :8 receive)
          (spells string-utils)
          (spells foreign)
          (spells tracing)
          (only (spells assert) cout)
          (sbank support utils)
          (sbank support shlibs)
          (sbank ctypes basic)
          (sbank ctypes call)
          (sbank typelib)
          (sbank support type-data)
          (sbank gobject gvalue)
          (sbank gobject gtype)
          (sbank gobject gquark)
          (sbank gobject internals))

  (define (g-signal-connect instance signal callback)
    (define (lose msg . irritants)
      (apply error 'g-signal-connect msg irritants))
    (let*-values
        (((signal detail detailed-signal) (parse-signal-spec signal lose))
         ((prepare) (gobject-class-get-signal-callback
                     (ginstance-class instance)
                     signal)))
      (unless prepare
        (lose "no such signal" detailed-signal))
      (receive (cb-ptr reclaim) (prepare callback)
        (let* ((detailed-signal-ptr (string->utf8z-ptr detailed-signal))
               (id (connect-data%
                    (ginstance-ptr instance)
                    detailed-signal-ptr
                    cb-ptr
                    (null-pointer)
                    (callback-destroy-notify reclaim)
                   0)))
          (free detailed-signal-ptr)
          id))))

  (define (g-signal-lookup signal class)
    (let* ((name-ptr (string->utf8z-ptr (symbol->string signal)))
           (rv (lookup% name-ptr (gobject-class-gtype class))))
      (free name-ptr)
      rv))

  (define (g-signal-disconnect obj handler-id)
    (disconnect% (ginstance-ptr obj) handler-id))

  (define (g-signal-emit instance signal . args)
    (define (lose msg . irritants)
      (apply error 'signal-emit msg irritants))
    (receive (signal detail detailed-signal) (parse-signal-spec signal lose)
      (let* ((signal-id (g-signal-lookup signal (ginstance-class instance)))
             (signature (gobject-class-get-signal-signature
                         (ginstance-class instance) signal))
             (rti (signature-rti signature))
             (atis (signature-atis signature))
             (n-args (length atis))
             ;;++ should check for right number of arguments here
             (arg-gvs (->g-value-array
                       (cons instance args)
                       #f
                       (map type-info-gtype atis))))
        (cond ((eq? (type-info-type rti) 'void)
               (emitv% arg-gvs signal-id detail (null-pointer))
               (free-g-value-array arg-gvs n-args))
              (else
               (let ((ret-gv (g-value-new (type-info-gtype rti))))
                 (emitv% arg-gvs signal-id detail ret-gv)
                 (free-g-value-array arg-gvs n-args)
                 (let ((rv (g-value-ref ret-gv)))
                   (g-value-free ret-gv)
                   rv)))))))

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
           (lose "invalid signal specification" signal))))


  (define-c-callouts libgobject
    (connect-data% 'ulong "g_signal_connect_data"
                   '(pointer pointer fpointer pointer fpointer int))
    (disconnect% 'void "g_signal_handler_disconnect" '(pointer ulong))
    (lookup% 'uint "g_signal_lookup" `(pointer ,gtype-ctype))
    (emitv% 'void "g_signal_emitv" `(pointer uint ,g-quark-ctype pointer)))

  )
