(library (sbank typelib)
  (export open-typelib
          typelib-magic
          typelib-minor-version
          typelib-major-version
          typelib-get-entry-names)
  (import (rnrs base)
          (rnrs control)
          (rnrs lists)
          (rnrs bytevectors)
          (rnrs syntax-case)
          (rnrs io simple)
          (spells foreign)
          (spells tracing)
          (sbank stypes)
          (for (sbank typelib stypes) run expand))

  (define-syntax define-accessors (stype-accessor-definer (typelib-stypes)))

  (define-accessors "GTypelib"
    (tl-data "data"))
  
  (define-accessors "Header"
    (header-magic "magic")
    (header-major-version "major_version")
    (header-minor-version "minor_version"))
  
  (define-accessors "GError"
    (gerror-domain "domain")
    (gerror-code "code")
    (gerror-message "message"))

  
  (define libir (dlopen "libgirepository.so.0"))
  (define libgobject (dlopen "libgobject-2.0.so.0"))
  (define libglib (dlopen "libglib-2.0.so.0"))
  
  (define open-typelib
    (let ((g-ir-require ((make-callout 'pointer '(pointer pointer pointer unsigned-int pointer))
                         (dlsym libir "g_irepository_require"))))
      (lambda (namespace version flags)
        (let ((c-namespace (->c-string 'open-typelib namespace))
              (c-version (->c-string 'open-typelib version))
              (gerror (malloc (c-type-sizeof 'pointer))))
          (pointer-set-pointer gerror 0 (integer->pointer 0))
          (dynamic-wind
            (lambda () #t)
            (lambda ()
              (let ((result 
                     (g-ir-require (integer->pointer 0) c-namespace c-version flags gerror)))
                (cond ((= 0 (pointer->integer result))
                       (error 'open-typelib
                              "unable to require library"
                              (from-c-string (gerror-message (pointer-ref-pointer gerror 0)))))
                      (else
                       ;;(assert-typelib-sanity result)
                       result))))
            (lambda () (let ((e (pointer-ref-pointer gerror 0)))
                         (when (not (= 0 (pointer->integer e)))
                           (gerror-free e))
                         (free gerror))))))))

  (define (typelib-magic tl)         (header-magic (tl-data tl)))
  (define (typelib-minor-version tl) (header-minor-version (tl-data tl)))
  (define (typelib-major-version tl) (header-major-version (tl-data tl)))
  
  (define (typelib-get-entry-names typelib) '())

  (define gerror-free
    ((make-callout 'void '(pointer)) (dlsym libglib "g_error_free")))

  (define (from-c-string ptr)
    (let ((size (do ((i 0 (+ i 1)))
                    ((= (pointer-ref-unsigned-char ptr i) 0) i))))
      (utf8->string (memcpy (make-bytevector size) ptr size))))
  
  (define (->c-string who s)
    (cond ((string? s)
           (let* ((bytes (string->utf8 s))
                  (bytes-len (bytevector-length bytes))
                  (result (malloc (+ bytes-len 1))))
             (memcpy result bytes bytes-len)
             (pointer-set-char result bytes-len 0)
             result))
          ((eqv? s #f)
           (integer->pointer 0))
          (else
           (assertion-violation who "invalid argument" s))))


  ;; Initialize the GObject type system
  (((make-callout 'void '()) (dlsym libgobject "g_type_init" ))))
