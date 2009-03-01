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
          gobject-setup!)
  (import (rnrs base)
          (rnrs control)
          (rnrs lists)
          (only (spells assert) cout)
          (spells alist)
          (spells tracing)
          (sbank typelib decorators)
          (sbank gobject gtype)
          (sbank gobject genum)
          (sbank gobject gvalue)
          (sbank gobject signals)
          (sbank gobject properties)
          (sbank gobject internals))
  

  (define (gobject-decorator class)
    (gobject-class-decorate
     class
     values
     (gobject-method-overrider `((connect . ,g-object-connect)
                                 (emit . ,g-object-emit)
                                 (get . ,g-object-get)
                                 (set . ,g-object-set)))
     values))

  (define (gvalue-decorator class)
    'gvalue)
  
  (define (g-object-connect next-method)
    (lambda (instance signal callback)
      (g-signal-connect instance signal callback)))

  (define (g-object-emit next-method)
    (lambda (instance signal)
      (g-signal-emit instance signal)))
  
  (define (g-object-set next-method)
    (lambda (instance . args)
        (do ((args args (cddr args)))
            ((null? args))
          (when (null? (cdr args))
            (error 'g-object-set "uneven number of arguments" args))
          (g-object-set-property instance (car args) (cadr args)))))

  (define (g-object-get next-method)
    (lambda (instance . properties)
      (let loop ((vals '()) (props properties))
        (if (null? props)
            (apply values (reverse vals))
            (loop (cons (g-object-get-property instance (car props)) vals)
                  (cdr props))))))

  (define (gvalue-new class)
    (lambda (next-method)
      (lambda (val)
        (make-ginstance class (->g-value val)))))
  
  (define gobject-setup!
    (let ((installed? #f))
      (lambda ()
        (unless installed?
          (g-type-init)
          (register-typelib-decorator "GObject" "Object" gobject-decorator)
          (register-typelib-decorator "GObject" "Value" gvalue-decorator)
          (set! installed? #t))))))
