;;; gobject.sls --- Public interface to the GObject mapping.

;; Copyright (C) 2008 Andreas Rottmann <a.rottmann@gmx.at>

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


(library (sbank gobject)
  (export gobject-class?
          gobject-class-decorate
          gobject-method-overrider
          ginstance?
          genum? genum-lookup
          send send-message
          install-gobject-decorators)
  (import (rnrs base)
          (rnrs control)
          (rnrs lists)
          (only (spells assert) cout)
          (spells alist)
          (spells tracing)
          (sbank typelib decorators)
          (sbank gobject signals)
          (sbank gobject properties)
          (sbank gobject internals))
  

  (define (gobject-decorator class)
    (gobject-class-decorate class
                            values
                            (gobject-method-overrider `((connect . ,g-object-connect)
                                                        (emit . ,g-object-emit)
                                                        (get . ,g-object-get)
                                                        (set . ,g-object-set)))
                            values))
  
  (define (g-object-connect instance signal callback)
    (g-signal-connect instance signal callback))

  (define (g-object-emit instance signal)
    (g-signal-emit instance signal))
  
  (define (g-object-set instance . args)
    (do ((args args (cddr args)))
        ((null? args))
      (when (null? (cdr args))
        (error 'g-object-set "uneven number of arguments" args))
      (g-object-set-property instance (car args) (cadr args))))

  (define (g-object-get instance . properties)
    (let loop ((vals '()) (props properties))
      (if (null? props)
          (apply values (reverse vals))
          (loop (cons (g-object-get-property instance (car props)) vals) (cdr props)))))
  
  (define install-gobject-decorators
    (let ((installed? #f))
      (lambda ()
        (unless installed?
          (register-typelib-decorator "GObject" "Object" gobject-decorator)
          (set! installed? #t))))))
