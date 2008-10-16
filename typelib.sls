(library (sbank typelib)
  (export open-typelib
          typelib-magic
          typelib-minor-version
          typelib-major-version
          typelib-get-entry-names
          typelib-get-entry)
  (import (rnrs)
          (xitomatl srfi and-let*)
          (spells foreign)
          (spells tracing)
          (spells alist)
          (spells parameter)
          (spells table)
          (sbank stypes)
          (for (sbank typelib stypes) run expand))

  ;;
  ;; Typelib accessors and fetcher factories
  ;;
  
  (define-syntax define-accessors (stype-accessor-definer (typelib-stypes)))

  (define-accessors "GTypelib"
    (tl-data "data"))
  
  (define-accessors "Header"
    (header-magic "magic")
    (header-major-version "major_version")
    (header-minor-version "minor_version")
    (header-n-entries "n_entries")
    (header-directory "directory")
    (header-size "size")
    (header-entry-blob-size "entry_blob_size"))

  (define-accessors "DirEntry"
    (dir-entry-blob-type "blob_type")
    (dir-entry-local "local")
    (dir-entry-name "name")
    (dir-entry-offset "offset"))
  
  (define-accessors "GError"
    (gerror-domain "domain")
    (gerror-code "code")
    (gerror-message "message"))

  (define-syntax define-fetcher (stype-fetcher-factory-definer (typelib-stypes)))

  (define-fetcher header-fetcher "Header")
  
  (define libir (dlopen "libgirepository.so.0"))
  (define libgobject (dlopen "libgobject-2.0.so.0"))
  (define libglib (dlopen "libglib-2.0.so.0"))

  ;;
  ;; Public API
  ;;

  (define-record-type typelib
    (fields (immutable tl typelib-tl)
            (immutable directory typelib-directory)))

  (define open-typelib
    (let ((g-ir-require ((make-c-callout 'pointer '(pointer pointer pointer unsigned-int pointer))
                         (dlsym libir "g_irepository_require"))))
      (lambda (namespace version flags)
        (with-validation-context namespace
          (let ((c-namespace (->c-string 'open-typelib namespace))
                (c-version (->c-string 'open-typelib version))
                (gerror (malloc (c-type-sizeof 'pointer))))
            (pointer-set-c-pointer! gerror 0 (integer->pointer 0))
            (dynamic-wind
              (lambda () #t)
              (lambda ()
                (let ((result 
                       (g-ir-require (integer->pointer 0) c-namespace c-version flags gerror)))
                  (cond ((= 0 (pointer->integer result))
                         (error 'open-typelib
                                "unable to require library"
                                (from-c-string (gerror-message (pointer-ref-c-pointer gerror 0)))))
                        (else
                         (make/validate-typelib result)))))
              (lambda () (let ((e (pointer-ref-c-pointer gerror 0)))
                           (when (not (= 0 (pointer->integer e)))
                             (gerror-free e))
                           (free gerror)))))))))

  ;; Helper
  (define (typelib-header fetcher)
    (lambda (typelib)
      (fetcher (tl-data (typelib-tl typelib)))))

  (define (typelib-magic typelib)
    (memcpy (make-bytevector 16) ((typelib-header header-magic) typelib) 16))
  
  (define typelib-minor-version (typelib-header header-minor-version))
  (define typelib-major-version (typelib-header header-major-version))

  (define (typelib-get-entry-names typelib)
    (table-fold (lambda (key value result)
                  (cons key result))
                '()
                (typelib-directory typelib)))

  (define (typelib-get-entry typelib name)
    (and-let* ((loader (table-ref (typelib-directory typelib) name)))
      (loader)))
  
  ;;
  ;; Validation/Typelib construction
  ;;

  (define-condition-type &validation-error &error
    make-validation-error validation-error?
    (context validation-error-context))

  (define validation-context (make-parameter '()))
  
  (define (raise-validation-error message . irritants)
    (raise (condition (make-validation-error (validation-context))
                      (make-message-condition message)
                      (make-irritants-condition irritants))))

  (define-syntax with-validation-context
    (syntax-rules ()
      ((_ <context> <body> ...)
       (parameterize ((validation-context (cons <context> (validation-context))))
         <body> ...))))
  
  (define (make/validate-typelib tl)
    (define (lose msg . irritants)
      (apply raise-validation-error msg irritants))
    (let ((tld (tl-data tl)))
      (and-let* ((magic (memcpy (make-bytevector 16) (header-magic tld) 16))
                 ((not (bytevector=? magic (string->utf8 "GOBJ\nMETADATA\r\n\x1a;")))))
        (lose "invalid magic" magic))
      (or (and (= (header-major-version tld) 1)
               (= (header-minor-version tld) 0))
          (lose "version mismatch" (header-major-version tld) (header-minor-version tld)))
      (validate-blob-sizes tld)
      (make-typelib tl (make/validate-directory tld))))

  (define (validate-blob-sizes tld)
    (for-each (lambda (blob-name/size)
                (let ((actual-size ((header-fetcher
                                     (symbol-append (car blob-name/size) '- 'blob-size))
                                    tld)))
                  (or (= (cdr blob-name/size) actual-size)
                      (raise-validation-error "blob size mismatch" blob-name/size actual-size))))
              '((entry . 12)
                (function . 16)
                (callback . 12)
                (signal . 12)
                (vfunc . 16)
                (arg . 12)
                (property . 12)
                (field . 12)
                (value . 12)
                (constant . 20)
                (error-domain . 16)
                (annotation . 12)
                (signature . 8)
                (enum . 20)
                (struct . 20)
                (object . 32)
                (interface . 28)
                (union . 28))))

  (define (make/validate-directory tld)
    (with-validation-context 'directory
        (let ((entry-size (header-entry-blob-size tld))
              (n-entries (header-n-entries tld)))
          (do ((dir (make-table 'equal))
               (i 0 (+ i 1))
               (entry-ptr
                (validated-pointer+ tld (header-directory tld) (* n-entries entry-size))
                (pointer+ entry-ptr entry-size)))
              ((>= i n-entries) dir)
            (table-set! dir
                        (get/validate-string tld entry-ptr dir-entry-name)
                        (make-entry-loader tld entry-ptr))))))
  
  (define (make-entry-loader tld entry-ptr)
    (let ((content-ptr (validated-pointer+ tld (dir-entry-offset entry-ptr) 1)))
      (memoize-thunk/validation-context
       (if (= (dir-entry-local entry-ptr) 0)
           (lambda ()
             (list 'non-local-dir-entry (validation-context) (get/validate-string content-ptr)))
           (lambda ()
             (list 'local-dir-entry (validation-context) (dir-entry-offset entry-ptr)))))))

  (define (memoize-thunk/validation-context thunk)
    (let ((context (validation-context)))
      (lambda ()
        (let ((cache #f))
          (unless cache
            (parameterize ((validation-context context))
              (set! cache (thunk))))
          cache))))
  
  ;;
  ;; Helpers
  ;;

  (define (get/validate-string tld ptr offset-fetcher)
    (from-c-string (validated-pointer+ tld (offset-fetcher ptr) 1)))
  
  (define (validated-pointer+ tld offset size)
    (when (> (+ offset size) (header-size tld))
      (raise-validation-error "offset/size out of bounds" offset size (header-size tld)))
    (pointer+ tld offset))

  (define gerror-free
    ((make-c-callout 'void '(pointer)) (dlsym libglib "g_error_free")))

  (define (from-c-string ptr)
    (let ((size (do ((i 0 (+ i 1)))
                    ((= (pointer-ref-c-unsigned-char ptr i) 0) i))))
      (utf8->string (memcpy (make-bytevector size) ptr size))))
  
  (define (->c-string who s)
    (cond ((string? s)
           (let* ((bytes (string->utf8 s))
                  (bytes-len (bytevector-length bytes))
                  (result (malloc (+ bytes-len 1))))
             (memcpy result bytes bytes-len)
             (pointer-set-c-char! result bytes-len 0)
             result))
          ((eqv? s #f)
           (integer->pointer 0))
          (else
           (assertion-violation who "invalid argument" s))))

  (define (pointer+ p n)
    (integer->pointer (+ (pointer->integer p) n)))
  
  (define (symbol-append . syms)
    (string->symbol (apply string-append (map symbol->string syms))))
  
  ;; Initialize the GObject type system
  (((make-c-callout 'void '()) (dlsym libgobject "g_type_init" ))))
