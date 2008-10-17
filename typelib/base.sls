(library (sbank typelib base)
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
          (spells receive)
          (spells format)
          (only (spells lists) filter-map)
          (spells define-values)
          (only (spells misc) or-map)
          (only (spells assert) cerr)
          (sbank stypes)
          (sbank utils)
          (for (sbank typelib stypes) run expand))

  ;;
  ;; Typelib accessors and fetcher factories
  ;;
  
  (define-syntax define-accessors (stype-accessor-definer (typelib-stypes)))

  (define-accessors "GTypelib"
    (tl-data "data"))
  
  (define-accessors "GError"
    (gerror-domain "domain")
    (gerror-code "code")
    (gerror-message "message"))

  (define-accessors "Header"
    ;; This needs to be reasonably fast, so we don't use let-attribute/fetchers here
    (header-size "size")) 
  
  (define-syntax define-fetcher (stype-fetcher-factory-definer (typelib-stypes)))

  (define-fetcher header-fetcher "Header")
  (define-fetcher function-blob-fetcher "FunctionBlob")
  (define-fetcher dir-entry-fetcher "DirEntry")
  (define-fetcher signature-blob-fetcher "SignatureBlob")
  (define-fetcher interface-type-blob-fetcher "InterfaceTypeBlob")
  (define-fetcher simple-type-blob-fetcher "SimpleTypeBlob")
  
  (define-syntax let-attributes
    (syntax-rules ()
      ((let-attributes <fetcher> <pointer> (<name> ...) <body> ...)
       (let ((<name> ((<fetcher> '<name>) <pointer>)) ...)
         <body> ...))))
  
  (define libir (dlopen "libgirepository.so.0"))
  (define libgobject (dlopen "libgobject-2.0.so.0"))
  (define libglib (dlopen "libglib-2.0.so.0"))

  ;;
  ;; Public API
  ;;

  (define-condition-type &typelib-error &error
    make-typelib-error typelib-error?)

  (define (raise-typelib-error msg . irritants)
    (raise (condition (make-typelib-error)
                      (make-message-condition msg)
                      (make-irritants-condition irritants))))
  
  (define-record-type typelib
    (fields (immutable tl typelib-tl)
            (immutable namespace)
            (immutable directory typelib-directory)
            (immutable callouts typelib-callouts)
            (mutable shlibs %typelib-shlibs %typelib-set-shlibs!)))

  (define typelib-deprecation-handler
    (make-parameter (lambda (typelib name)
                      (cerr "DEPRECATED: item " name " of namespace "
                            (typelib-namespace typelib) "\n"))))

  ;; See comments in gtypelib.c (_g_typelib_init)
  (define (typelib-shlibs typelib)
    (or (%typelib-shlibs typelib)
        (let* ((tld (tl-data (typelib-tl typelib)))
               (names (get/validate-string tld (typelib-header typelib 'shared-library)))
               (shlibs (filter-map
                        (lambda (name)
                          (let ((shlib (or (dlopen name #t #t)
                                           (dlopen (string-append "lib" name ".so") #t #t))))
                            (unless shlib
                              (warning "failed to load shared library '~a' referenced by the typelib: ~a" (dlerror)))
                            shlib))
                        (string-split names #\,))))
          (let* ((app-dl (dlopen))
                 (shlibs (if app-dl (append shlibs (list app-dl)) shlibs)))
            (%typelib-set-shlibs! typelib shlibs)
            shlibs))))

  (define (typelib-dlsym typelib name)
    (or-map (lambda (shlib)
              (dlsym shlib name))
            (typelib-shlibs typelib)))
  
  (define (typelib-header typelib name)
    ((header-fetcher name) (tl-data (typelib-tl typelib))))
  
  (define *registered-typelibs* (make-table 'equal))
  
  (define (open-typelib namespace version flags)
    (or (table-ref *registered-typelibs* namespace)
        (let ((typelib (require-typelib namespace version flags)))
          (table-set! *registered-typelibs* namespace typelib)
          typelib)))

  (define require-typelib
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
                         (make/validate-typelib result namespace)))))
              (lambda () (let ((e (pointer-ref-c-pointer gerror 0)))
                           (when (not (= 0 (pointer->integer e)))
                             (gerror-free e))
                           (free gerror)))))))))

  
  (define (typelib-magic typelib)
    (memcpy (make-bytevector 16)
            ((header-fetcher 'magic) (tl-data (typelib-tl typelib))) 16))
  
  (define (typelib-minor-version typelib)
    ((header-fetcher 'minor-version) (tl-data (typelib-tl typelib))))
  
  (define (typelib-major-version typelib)
    ((header-fetcher 'major-version) (tl-data (typelib-tl typelib))))
  
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

  (define-condition-type &validation-error &typelib-error
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
  
  (define (make/validate-typelib tl namespace)
    (define (lose msg . irritants)
      (apply raise-validation-error msg irritants))
    (let ((tld (tl-data tl)))
      (let-attributes header-fetcher tld
                      (magic major-version minor-version)
        (and-let* ((magic (memcpy (make-bytevector 16) magic 16))
                   ((not (bytevector=? magic (string->utf8 "GOBJ\nMETADATA\r\n\x1a;")))))
          (lose "invalid magic" magic))
        (or (and (= major-version 1) (= minor-version 0))
            (lose "version mismatch" major-version minor-version))
        (validate-blob-sizes tld)
        (let ((typelib (make-typelib tl namespace (make-table 'equal) (make-table 'eqv) #f)))
          (fill/validate-directory! typelib tld)
          typelib))))

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

  (define (fill/validate-directory! typelib tld)
    (let-attributes header-fetcher tld
                    (entry-blob-size n-entries directory)
      (do ((dir (typelib-directory typelib))
           (i 0 (+ i 1))
           (entry-ptr
            (validated-pointer+ tld directory (* n-entries entry-blob-size))
            (pointer+ entry-ptr entry-blob-size)))
          ((>= i n-entries))
        (let-attributes dir-entry-fetcher entry-ptr
                        (name)
          (let ((name (get/validate-string tld name)))
            (table-set! dir name (make-entry-loader typelib tld name entry-ptr)))))))
  
  (define (make-entry-loader typelib tld name entry-ptr)
    (with-validation-context name
        (let-attributes dir-entry-fetcher entry-ptr
                        (offset)
          (let ((content-ptr (validated-pointer+ tld offset 1)))
            (thunk/validation-context
             (if (= ((dir-entry-fetcher 'local) entry-ptr) 0)
                 (lambda ()
                   (list 'non-local-dir-entry (validation-context) (get/validate-string tld offset)))
                 (case ((dir-entry-fetcher 'blob-type) entry-ptr)
                   ((1) (validated-function-loader typelib tld entry-ptr name))
                   (else
                    (lambda ()
                      (list 'local-dir-entry (validation-context) offset))))))))))

  (define (validated-function-loader typelib tld entry-ptr name)
    (let ((blob (validated-pointer+ tld
                                    ((dir-entry-fetcher 'offset) entry-ptr)
                                    ((header-fetcher 'function-blob-size) tld))))
      (lambda ()
        (let-attributes
            function-blob-fetcher blob
            (blob-type deprecated wraps-vfunc index name symbol signature)
          (unless (= blob-type 1)
            (raise-validation-error "invalid blob type for function entry" (blob-type blob)))
          (let ((callout (or (table-ref (typelib-callouts typelib) name)
                             (let ((callout (make/validate-callout tld signature)))
                               (table-set! (typelib-callouts typelib) signature callout)
                               callout))))
            (if (= wraps-vfunc 1)
                (make/validate-vfunc-caller tld index callout)
                (let ((proc (callout (typelib-dlsym typelib (get/validate-string tld symbol)))))
                  (if deprecated
                      (mark-deprecated typelib tld name proc)
                      proc))))))))

  (define (make/validate-callout tld signature-offset)
    (let-attributes signature-blob-fetcher
        (validated-pointer+ tld signature-offset ((header-fetcher 'signature-blob-size) tld))
        ;; FIXME: `arguments' are not handled correctly by GIR (no array size)
        (return-type n-arguments arguments)
      (receive (return-type pointer?) (simple-type-blob/type+pointer tld return-type)
        (unless (= n-arguments 0)
          (raise-validation-error "argument passing not yet implemented" n-arguments))
        (cond ((symbol? return-type)
               (case return-type
                 ((void) (make-c-callout 'void '()))
                 (else
                  (raise-validation-error "non-void return types not yet implemented" return-type))))
              (else
               (raise-validation-error "complex return types not yet implemented" return-type))))))
  
  (define (make/validate-vfunc-caller tld index callout)
    (raise-typelib-error "vfunc calling not yet implemented"))

  (define (thunk/validation-context thunk)
    (let ((context (validation-context)))
      (lambda ()
        (parameterize ((validation-context context))
          (thunk)))))

  (define (mark-deprecated typelib tld name proc)
    (let ((encountered? #f))
      (lambda args
        (cond (encountered? (apply proc args))
              (else
               (set! encountered? #t)
               ((typelib-deprecation-handler) typelib (get/validate-string tld name))
               (apply proc args))))))
  
  ;;
  ;; Helpers
  ;;
  (trace-define (simple-type-blob/type+pointer tld st-blob)
    (let-attributes simple-type-blob-fetcher st-blob
                    (offset reserved reserved2 pointer tag)
      (cond ((and (= reserved 0) (= reserved2 0))
             (when (>= tag type-tag-array)
               (raise-validation-error "wrong tag in simple type" tag))
             (when (and (>= tag type-tag-utf8) (= pointer 0))
               (raise-validation-error "pointer type expected for tag" tag))
             (values (type-tag->symbol tag) (bool pointer)))
            (else
             (let-attributes interface-type-blob-fetcher (validated-pointer+ tld offset)
                             (pointer tag)
               (raise-validation-error "non-simple types not yet implemented"))))))

  (define-syntax define-enum
    (syntax-rules ()
      ((define-enum val->symbol symbol->val (symbol ...))
       (define-values (val->symbol symbol->val)
         (let ((sym-vec '#(symbol ...)))
           (values (lambda (val) (vector-ref sym-vec val))
                   (lambda (sym) (vector-index eq? sym-vec sym))))))))

  (define-enum type-tag->symbol symbol->type-tag
    (void boolean int8 uint8 int16 uint16 int32 uint32
          int64 uint64 int uint long ulong ssize size
          float double time_t gtype utf8 filename
          array interface glist gslist ghash error))

  (define type-tag-utf8 (symbol->type-tag 'utf8))
  (define type-tag-array (symbol->type-tag 'array))
  
  (define (vector-index equal vec val)
    (let loop ((i 0))
      (cond ((>= i (vector-length vec)) #f)
            ((equal (vector-ref vec i) val) i)
            (else (loop (+ i 1))))))
  
  (define (get/validate-string tld offset)
    ;; FIXME: This should actually check we don't go beyond the data
    ;; when assembling the string
    (from-c-string (validated-pointer+ tld offset 1)))
  
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
  
  (define (bool i)
    (not (= 0 i)))

  (define (warning msg . args)
    (apply format (current-error-port) msg args)
    (newline (current-error-port)))

  ;; Initialize the GObject type system
  (((make-c-callout 'void '()) (dlsym libgobject "g_type_init" ))))
