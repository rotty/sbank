;;; internals.sls --- GObject mapping implementation.

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

(library (sbank gobject internals)
  (export make-gobject-class gobject-class?
          make-ginstance ginstance? ginstance-ptr ginstance-class
          send-message
          send
          make-genum genum? genum-lookup genum-values genum-symbols)
  (import (rnrs base)
          (rnrs control)
          (rnrs lists)
          (rnrs records syntactic)
          (rnrs mutable-pairs)
          (spells alist)
          (spells tracing)
          (sbank utils))

  ;;
  ;; Object system
  ;;
  
  (define-record-type ginstance
    (fields (immutable class ginstance-class)
            (immutable ptr ginstance-ptr)))

  (define-record-type gobject-class
    (fields (immutable namespace gobject-class-namespace)
            (immutable name gobject-class-name)
            (immutable parent gobject-class-parent)
            (immutable constructors gobject-class-constructors)
            (immutable methods gobject-class-methods)))

  (trace-define (lookup-method class name)
    (cond ((assq name (gobject-class-methods class))
           => (trace-lambda lookup-found (entry)
                (when (lazy-entry? (cdr entry))
                  (set-cdr! entry ((lazy-entry-proc (cdr entry)))))
                (cdr entry)))
          ((gobject-class-parent class) => (lambda (parent)
                                             (lookup-method parent name)))
          (else #f)))
  
  (trace-define (send-message obj msg . args)
    (if (ginstance? obj)
        (let ((method (send-message (ginstance-class obj) msg)))
          (apply method (ginstance-ptr obj) args))
        (cond ((assq msg (gobject-class-constructors obj))
               => (lambda (entry)
                    (when (lazy-entry? (cdr entry))
                      (set-cdr! entry ((lazy-entry-proc (cdr entry)))))
                    (apply (cdr entry) args)))
              ((lookup-method obj msg)
               => (lambda (proc)
                    (unless (null? args)
                      (error 'send-message "cannot send message with arguments to class" obj msg args))
                    proc))
              (else
               (error 'send-message "message not understood" obj msg args)))))

  (define-syntax send
    (syntax-rules ()
      ((send obj (msg arg ...) ...)
       (begin (send-message obj 'msg arg ...) ...))))


  ;;
  ;; Enumerations and flags
  ;;
  (define-record-type (genum make-genum% genum?)
    (fields
     (immutable syms genum-symbols)
     (immutable vals genum-values)))

  ;; Note: this could be made more efficient by using sorted vectors
  ;; (but only in one direction)
  (define (genum-lookup enum sym-or-val)
    (if (symbol? sym-or-val)
        (cond ((vector-index eq? (genum-symbols enum) sym-or-val)
               => (lambda (i) (vector-ref (genum-values enum) i)))
              (else #f))
        (cond ((vector-index eqv? (genum-values enum) sym-or-val)
               => (lambda (i) (vector-ref (genum-symbols enum) i)))
              (else #f))))

  (define (make-genum alist)
    (make-genum% (list->vector (map car alist)) (list->vector (map cdr alist)))))
