(library (sbank typelib gobject)
  (export make-gobject-class
          gobject-class?
          send-message
          send)
  (import (rnrs base)
          (rnrs control)
          (rnrs lists)
          (rnrs records syntactic)
          (rnrs mutable-pairs)
          (spells alist)
          (spells tracing)
          (sbank utils))

  (define-record-type ginstance
    (fields (immutable class ginstance-class)
            (immutable ptr ginstance-ptr)))

  (define-record-type gobject-class
    (fields (immutable namespace gobject-class-namespace)
            (immutable name gobject-class-name)
            (immutable parent gobject-class-parent)
            (immutable constructors gobject-class-constructors)
            (immutable methods gobject-class-methods)))

  (define (lookup-method class name)
    (cond ((assq name (gobject-class-methods class))
           => (lambda (entry)
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
                    (make-ginstance obj (apply (cdr entry) args))))
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
       (begin (send-message obj 'msg arg ...) ...)))))
