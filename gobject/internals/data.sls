;;; data.sls --- data types for the gobject bindings

;; Copyright (C) 2010 Andreas Rottmann <a.rottmann@gmx.at>

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

;;; Code:
#!r6rs

(library (sbank gobject internals data)
  (export make-ginstance
          make-ginstance/guarded
          ginstance?
          ginstance-ptr
          ginstance-class

          ginstance=?
          ginstance-is-a?

          gobject-collect
          
          make-gobject-class
          gobject-class?
          gobject-class-namespace
          gobject-class-name
          gobject-class-gtype
          gobject-class-parent
          gobject-class-interfaces
          gobject-class-load-members
          gobject-class-methods
          gobject-class-properties
          gobject-class-signals
          gobject-class-constructors
          gobject-class-force!
          make-gobject-class-lookup

          send
          send-message

          gobject-simple-class?
          gobject-simple-class-ref
          gobject-simple-class-unref
          
          make-gobject-record-class
          gobject-record-class?
          
          make-gobject-union-class
          gobject-union-class?

          gsequence?
          make-gsequence-class

          gslist?
          make-gslist-class

          glist?
          make-glist-class

          ghash?
          make-ghash-class
          ghash->alist

          gerror-type? gerror-type)
  (import (rnrs)
          (rnrs mutable-pairs)
          (srfi :2 and-let*)
          (srfi :8 receive)
          (spells gc)
          (spells foreign)
          (spells tracing) ;debug
          (sbank support utils)
          (sbank support shlibs)
          (sbank gobject gtype)
          (sbank gobject ghash))

  (define-record-type ginstance
    (fields (immutable class ginstance-class)
            (immutable ptr ginstance-ptr)))

  (define (ginstance=? x y)
    (cond ((and (ginstance? x) (ginstance? y))
           (pointer=? (ginstance-ptr x) (ginstance-ptr y)))
          ((and (eqv? x #f) (eqv? y #f))
           #t)
          ((or (and (eqv? x #f) (ginstance? y))
               (and (ginstance? x) (eqv? y #f)))
           #f)
          (else
           (error 'ginstance=? "invalid argument types" x y))))

  (define (ginstance-is-a? inst class)
    (let loop ((c (ginstance-class inst)))
      (cond ((eq? c class) #t)
            (else
             (gobject-class-force! c)
             (cond ((gobject-class-parent c) => loop)
                   (else                        #f))))))

  (define gobject-reaper
    (make-reaper (lambda (gi)
                   (g-object-unref (ginstance-ptr gi))
                   #t)))
  
  (define boxed-reaper
    (make-reaper (lambda (gi)
                   (g-boxed-free (gobject-class-gtype
                                  (ginstance-class gi))
                                 (ginstance-ptr gi))
                   #t)))

  (define simple-reaper
    (make-reaper (lambda (gi)
                   (let ((class (ginstance-class gi)))
                     ((gobject-simple-class-unref class) (ginstance-ptr gi))))))

  (define (reap-all reaper)
    (lambda ()
      (do ((o (reaper) (reaper)))
          ((eqv? o #f)))))
  
  (define (gobject-collect)
    (reap-all gobject-reaper)
    (reap-all boxed-reaper)
    (reap-all simple-reaper))
  
  (define (make-ginstance/guarded class ptr ref-type)
    (cond ((gobject-class-gtype class)
           => (lambda (gtype)
                (case (gtype->symbol gtype)
                  ((object)
                   (reap-all gobject-reaper)
                   (let ((o (make-ginstance class ptr)))
                     (case ref-type
                       ((ref)  (g-object-ref ptr))
                       ((sink) (when (g-object-is-floating? ptr)
                                 (g-object-ref-sink ptr)))
                       (else
                        (assertion-violation 'make-ginstance/guarded
                                             "invalid ref type" ref-type)))
                     (gobject-reaper o)
                     o))
                  ((boxed)
                   (reap-all boxed-reaper)
                   (let ((o (make-ginstance class ptr)))
                     (boxed-reaper o)
                     o))
                  (else
                   (make-plain-instance class ptr)))))
          (else
           (make-plain-instance class ptr))))

  ;; Construct a ginstance for a class without proper gtype
  (define (make-plain-instance class ptr)
    (or (and-let* (((gobject-simple-class? class))
                   (ref (gobject-simple-class-ref class))
                   (unref (gobject-simple-class-unref class)))
          (reap-all simple-reaper)
          (let ((o (make-ginstance class ptr)))
            (ref ptr)
            (simple-reaper o)
            o))
        (make-ginstance class ptr)))
  
  (define-record-type gobject-class
    ;;(opaque #t)
    (fields namespace
            name
            gtype
            (mutable load-members)
            (mutable parent)
            (mutable interfaces)
            (mutable constructors)
            (mutable methods)
            (mutable signals)
            (mutable properties))
    (protocol (lambda (p)
                (lambda (namespace name gtype load-members)
                  (p namespace name gtype load-members #f #f #f #f #f #f)))))

  (define (gobject-class-force! class)
    (cond ((gobject-class-load-members class)
           => (lambda (loader)
                (receive (parent interfaces constructors methods signals properties)
                         (loader class)
                  (gobject-class-parent-set! class parent)
                  (gobject-class-interfaces-set! class interfaces)
                  (gobject-class-constructors-set! class constructors)
                  (gobject-class-methods-set! class methods)
                  (gobject-class-signals-set! class signals)
                  (gobject-class-properties-set! class properties)
                  (gobject-class-load-members-set! class #f))))))

  (define (make-gobject-class-lookup accessor)
    (define (lookup class name)
      (cond ((assq name (accessor class))
             => (lambda (entry)
                  (when (lazy-entry? (cdr entry))
                    (set-cdr! entry ((lazy-entry-proc (cdr entry)) class)))
                  (cdr entry)))
            ((and-let* ((parent (gobject-class-parent class)))
               (gobject-class-force! parent)
               (lookup parent name)) => values)
            (else
             (let loop ((ifaces (gobject-class-interfaces class)))
               (cond ((null? ifaces) #f)
                     ((let ((iface (car ifaces)))
                        (gobject-class-force! iface)
                        (lookup iface name)) => values)
                     (else
                      (loop (cdr ifaces))))))))
    lookup)

  (define lookup-method (make-gobject-class-lookup gobject-class-methods))
  
  (define (send-message obj msg . args)
    (cond ((ginstance? obj)
           (let ((method (send-message (ginstance-class obj) msg)))
             (apply method obj args)))
          (else
           (gobject-class-force! obj)
           (cond ((assq msg (gobject-class-constructors obj))
                  => (lambda (entry)
                       (when (lazy-entry? (cdr entry))
                         (set-cdr! entry ((lazy-entry-proc (cdr entry)) obj)))
                       (apply (cdr entry) args)))
                 ((lookup-method obj msg)
                  => (lambda (proc)
                       (unless (null? args)
                         (error 'send-message
                                "cannot send message with arguments to class"
                                obj msg args))
                       proc))
                 (else
                  (error 'send-message "message not understood" obj msg args))))))

  (define-syntax send
    (syntax-rules ()
      ((send obj (msg arg ...) ...)
       (begin (send-message obj 'msg arg ...) ...))))
  
  ;; A simple class is one that is not part of the GObject class
  ;; hierarchy, e.g. plain structs and unions. It has therefore no
  ;; parent, no interfaces, no signals and no properties.
  (define-record-type gobject-simple-class
    (parent gobject-class)
    (fields ref unref)
    (protocol (lambda (n)
                (lambda (namespace name gtype ref unref load-members)
                  (let ((p (n namespace name gtype
                              (lambda (class)
                                (receive (constructors methods) (load-members class)
                                  (values #f '() constructors methods '() '()))))))
                    (p ref unref))))))


  (define (simple-protocol n)
    (lambda args
      ((apply n args))))

  (define-record-type gobject-record-class
    (parent gobject-simple-class)
    (protocol simple-protocol))

  (define-record-type gobject-union-class
    (parent gobject-simple-class)
    (protocol simple-protocol))

  (define-record-type gsequence-class
    (parent gobject-simple-class)
    (fields elt-out elt-back elt-cleanup)
    (protocol
     (lambda (n)
       (lambda (namespace name constructors methods
                          elt-out elt-back elt-cleanup)
         (let ((p (n namespace name #f #f #f (lambda (class)
                                               (values constructors methods)))))
           (p elt-out elt-back elt-cleanup))))))

  (define-record-type gmapping-class
    (parent gobject-simple-class)
    (fields key-out val-out key-back val-back key-cleanup val-cleanup)
    (protocol
     (lambda (n)
       (lambda (namespace name constructors methods
                          key-out val-out
                          key-back val-back
                          key-cleanup val-cleanup)
         (let ((p (n namespace name #f #f #f (lambda (class)
                                               (values constructors methods)))))
           (p key-out val-out key-back val-back key-cleanup val-cleanup))))))

  (define-record-type gslist-class
    (parent gsequence-class)
    (protocol (lambda (n)
                (lambda (elt-out elt-back elt-cleanup)
                  (let ((p (n "GLib" "SList" '() '() elt-out elt-back elt-cleanup)))
                    (p))))))

  (define-record-type glist-class
    (parent gsequence-class)
    (protocol (lambda (n)
                (lambda (elt-out elt-back elt-cleanup)
                  (let ((p (n "GLib" "List" '() '() elt-out elt-back elt-cleanup)))
                    (p))))))

  (define-record-type ghash-class
    (parent gmapping-class)
    (protocol
     (lambda (n)
       (lambda (key-out val-out key-back val-back key-cleanup val-cleanup)
         (let ((p (n "GLib" "HashTable"
                     '()
                     `((foreach . ,ghash-foreach)
                       (->alist . ,ghash->alist))
                     key-out val-out key-back val-back key-cleanup val-cleanup)))
           (p))))))

  (define (ghash-foreach ghash proc)
    (unless (ghash? ghash)
      (error 'ghash-foreach "need a GHash instance" ghash))
    (let* ((class (ginstance-class ghash))
           (key-back (gmapping-class-key-back class ))
           (val-back (gmapping-class-val-back class)))
      (g-hash-table-foreach (ginstance-ptr ghash)
                            (lambda (key val user-data)
                              (proc (key-back key) (val-back val)))
                            (null-pointer))))

  (define (ghash->alist ghash)
    (cond ((eqv? ghash #f) '())
          (else
           (let ((result '()))
             (ghash-foreach ghash (lambda (key val)
                                    (set! result (cons (cons key val) result))))
             result))))

  

  (define (gsequence? x)
    (and (ginstance? x)
         (gsequence-class? (ginstance-class x))))

  (define (gslist? x)
    (and (ginstance? x)
         (gslist-class? (ginstance-class x))))

  (define (glist? x)
    (and (ginstance? x)
         (gslist-class? (ginstance-class x))))

  (define (ghash? x)
    (and (ginstance? x)
         (ghash-class? (ginstance-class x))))

  (define-record-type (:gerror-type make-gerror-type gerror-type?))

  (define gerror-type (make-gerror-type))
  
  (define (g-object-is-floating? ptr)
    (not (= (g-object-is-floating ptr) 0)))

  (define-c-callouts libgobject
    (g-object-ref 'void "g_object_ref" '(pointer))
    (g-object-unref 'void "g_object_unref" '(pointer))
    (g-object-ref-sink 'void "g_object_ref_sink" '(pointer))
    (g-object-is-floating 'int "g_object_is_floating" '(pointer)))
  
  )
