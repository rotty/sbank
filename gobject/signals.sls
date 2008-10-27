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
  (export signal-connect)
  (import (rnrs base)
          (spells receive)
          (spells foreign)
          (spells tracing)
          (only (spells assert) cout)
          (xitomatl srfi and-let*)
          (xitomatl srfi let-values)
          (sbank utils)
          (sbank shlibs)
          (sbank ctypes)
          (sbank typelib)
          (sbank gobject internals))

  (define signal-destroy-notify-ptr ((make-c-callback 'void '(pointer pointer))
                                     (lambda (data closure)
                                       (cout (list 'signal-destroyed: data closure) "\n"))))
  
  (define signal-connect
    (let ((g-signal-connect-data ((make-c-callout 'ulong
                                                  '(pointer pointer pointer pointer pointer int))
                                  (dlsym libgobject "g_signal_connect_data"))))
      (define (lose msg . irritants)
        (apply error 'signal-connect msg irritants))
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

  (define (parse-signal-spec signal lose)
    (cond ((string? signal)
           (let ((parts (string-split signal #\:)))
             (case (length parts)
               ((1) (values (string->symbol (car parts)) #f signal))
               ((2) (values (string->symbol (car parts)) (cadr parts) signal))
               (else
                (lose "invalid signal name" signal)))))
          ((symbol? signal)
           (values signal #f (symbol->string signal)))
          (else
           (lose "invalid signal specification" signal)))))
