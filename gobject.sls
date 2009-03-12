;;; gobject.sls --- Public interface to the GObject mapping.

;; Copyright (C) 2008, 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public
;; License along with this library. If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides the interface to the GObject to Scheme mapping.

;;; Code:
#!r6rs

(library (sbank gobject)
  (export gobject-class?
          gobject-class-decorate
          gobject-method-overrider
          ginstance? ginstance=? ginstance-is-a?
          genum? genumerated-lookup
          gflags? gflags->integer integer->gflags
          glist?
          gslist?
          ghash? ghash->alist
          gerror?
          send send-message
          gobject-setup!
          collect-gobjects)
  (import (rnrs base)
          (rnrs control)
          (rnrs lists)
          (only (spells assert) cout)
          (spells alist)
          (spells tracing)
          (sbank support utils)
          (sbank typelib decorators)
          (sbank glib)
          (sbank gobject gtype)
          (sbank gobject genum)
          (sbank gobject gvalue)
          (sbank gobject signals)
          (sbank gobject properties)
          (sbank gobject internals))


  (define (gobject-decorator typelib class)
    (gobject-class-decorate
     class
     values
     (gobject-method-overrider typelib
                               `((connect . ,g-object-connect)
                                 (disconnect . ,g-object-disconnect)
                                 (emit . ,g-object-emit)
                                 (get . ,g-object-get)
                                 (set . ,g-object-set)))
     values))

  (define (gvalue-decorator typelib class)
    'gvalue)

  (define (g-object-connect typelib next-method)
    (lambda (instance signal callback)
      (g-signal-connect instance signal callback)))

  (define (g-object-disconnect typelib next-method)
    (lambda (instance signal)
      (g-signal-disconnect instance signal)))

  (define (g-object-emit typelib next-method)
    (lambda (instance signal)
      (g-signal-emit instance signal)))

  (define (g-object-set typelib next-method)
    (lambda (instance . args)
        (do ((args args (cddr args)))
            ((null? args))
          (when (null? (cdr args))
            (error 'g-object-set "uneven number of arguments" args))
          (g-object-set-property instance (car args) (cadr args)))))

  (define (g-object-get typelib next-method)
    (lambda (instance . properties)
      (let loop ((vals '()) (props properties))
        (if (null? props)
            (apply values (reverse vals))
            (loop (cons (g-object-get-property instance (car props)) vals)
                  (cdr props))))))

  (define-setup-procedure (gobject-setup!)
    (glib-setup!)
    (g-type-init)
    (register-typelib-decorator "GObject" "Object" gobject-decorator)
    (register-typelib-decorator "GObject" "Value" gvalue-decorator)))
