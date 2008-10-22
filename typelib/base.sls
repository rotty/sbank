(library (sbank typelib base)
  (export require-typelib
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
          (only (spells lists) filter-map iota)
          (spells define-values)
          (only (spells misc) or-map)
          (only (spells assert) cerr cout)
          (sbank stypes)
          (sbank utils)
          (sbank typelib gobject)
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
  (define-fetcher object-blob-fetcher "ObjectBlob")
  (define-fetcher arg-blob-fetcher "ArgBlob")
  (define-fetcher constant-blob-fetcher "ConstantBlob")
  (define-fetcher enum-blob-fetcher "EnumBlob")
  (define-fetcher value-blob-fetcher "ValueBlob")
  (define-fetcher array-type-blob-fetcher "ArrayTypeBlob")
  
  (define-syntax let-attributes
    (syntax-rules ()
      ((let-attributes <fetcher> <pointer> (<name> ...) <body> ...)
       (let ((<name> ((<fetcher> '<name>) <pointer>)) ...)
         <body> ...))))

  (define-syntax let-accessors
    (syntax-rules ()
      ((let-accessors <fetcher> ((<accessor-name> <field>) ...) <body> ...)
       (let ((<accessor-name> (<fetcher> '<field>)) ...)
         <body> ...))))

  (define (checked-dlopen name)
    (or (dlopen name)
        (error 'checked-dlopen "unable to open shared library" name (dlerror))))
  
  (define libir (checked-dlopen "libgirepository.so.0"))
  (define libgobject (checked-dlopen "libgobject-2.0.so.0"))
  (define libglib (checked-dlopen "libglib-2.0.so.0"))

  ;;
  ;; Public API
  ;;

  (define-condition-type &typelib-error &error
    make-typelib-error typelib-error?)

  (define (raise-typelib-error msg . irritants)
    (raise (condition (make-typelib-error)
                      (make-message-condition msg)
                      (make-irritants-condition irritants))))
  
  (define-condition-type &typelib-callout-error &typelib-error
    make-typelib-callout-error typelib-callout-error?)

  (define (raise-typelib-callout-error msg . irritants)
    (raise (condition (make-typelib-callout-error)
                      (make-message-condition msg)
                      (make-irritants-condition irritants))))
  
  (define-record-type typelib
    (fields (immutable tl typelib-tl)
            (immutable namespace)
            (immutable name-table typelib-name-table)
            (immutable callouts typelib-callouts)
            (immutable directory typelib-directory)
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
  
  (define (require-typelib namespace version flags)
    (or (table-ref *registered-typelibs* namespace)
        (let ((typelib (require-typelib% 'require-typelib namespace version flags)))
          (table-set! *registered-typelibs* namespace typelib)
          typelib)))

  (define require-typelib%
    (let ((g-ir-require ((make-c-callout 'pointer '(pointer pointer pointer unsigned-int pointer))
                           (dlsym libir "g_irepository_require"))))
      (lambda (who namespace version flags)
        (with-validation-context namespace
          (let ((c-namespace (->utf8z-ptr/null who namespace))
                (c-version (->utf8z-ptr/null who version))
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
                                (utf8z-ptr->string (gerror-message (pointer-ref-c-pointer gerror 0)))))
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
                (typelib-name-table typelib)))

  (define (typelib-get-entry typelib name)
    (and-let* ((index (table-ref (typelib-name-table typelib) name)))
      (typelib-get-entry/index typelib index)))

  (define (typelib-get-entry/index typelib index)
    (let* ((dir (typelib-directory typelib))
           (entry (vector-ref dir (- index 1))))
      (if (lazy-entry? entry)
          (let ((val ((lazy-entry-proc entry))))
            (vector-set! dir index val)
            val)
          entry)))

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
                      (magic major-version minor-version n-entries)
        (and-let* ((magic (memcpy (make-bytevector 16) magic 16))
                   ((not (bytevector=? magic (string->utf8 "GOBJ\nMETADATA\r\n\x1a;")))))
          (lose "invalid magic" magic))
        (or (and (= major-version 1) (= minor-version 0))
            (lose "version mismatch" major-version minor-version))
        (validate-blob-sizes tld)
        (let ((typelib (make-typelib tl
                                     namespace
                                     (make-table 'equal)
                                     (make-table 'eqv)
                                     (make-vector n-entries)
                                     #f)))
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
                    (entry-blob-size n-entries n-local-entries directory)
      (do ((dir (typelib-directory typelib))
           (name-table (typelib-name-table typelib))
           (i 0 (+ i 1))
           (entry-ptr
            (validated-pointer+ tld directory (* n-local-entries entry-blob-size))
            (pointer+ entry-ptr entry-blob-size)))
          ((>= i n-entries))
        (let-attributes dir-entry-fetcher entry-ptr
                        (name)
          (let ((name (get/validate-string tld name)))
            (vector-set! dir i (make-lazy-entry (make-entry-loader typelib tld name entry-ptr)))
            (when (< i n-local-entries)
              (table-set! name-table name (+ i 1))))))))
  
  (define (make-entry-loader typelib tld entry-name entry-ptr)
    (with-validation-context entry-name
        (let-attributes dir-entry-fetcher entry-ptr
                        (name offset local blob-type)
          (let ((content-ptr (validated-pointer+ tld offset 1)))
            (thunk/validation-context
             (if (= local 0)
                 (lambda ()
                   (let ((namespace (get/validate-string tld offset))
                         (name (get/validate-string tld name)))
                     (or
                      (and-let* ((tl (require-typelib namespace #f 0)))
                        (typelib-get-entry tl name))
                      (raise-validation-error "invalid reference to other namespace" namespace name))))
                 (case blob-type
                   ((1) (make-function-loader typelib tld entry-ptr entry-name))
                   ((5) (make-enum-loader typelib tld entry-ptr entry-name))
                   ((7) (make-class-loader typelib tld entry-ptr entry-name))
                   (else
                    (lambda ()
                      (raise-validation-error "unknown entry type encountered" blob-type))))))))))

  (define (make-function-loader typelib tld entry-ptr name)
    (let ((blob (validated-pointer+ tld
                                    ((dir-entry-fetcher 'offset) entry-ptr)
                                    ((header-fetcher 'function-blob-size) tld))))
      (lambda ()
        (make/validate-function typelib tld blob #f))))

  (define (make/validate-function typelib tld blob method?)
    (let-attributes
            function-blob-fetcher blob
            (blob-type deprecated wraps-vfunc index name symbol signature)
      (let ((name (get/validate-string tld name)))
        (unless (= blob-type 1)
          (raise-validation-error "invalid blob type for function entry" (blob-type blob)))
        (let ((callout (or (table-ref (typelib-callouts typelib) signature)
                           (let ((callout (make/validate-callout typelib tld signature method?)))
                             (table-set! (typelib-callouts typelib) signature callout)
                             callout))))
          (if (= wraps-vfunc 1)
              (make/validate-vfunc-caller tld index callout)
              (let ((proc (callout (typelib-dlsym typelib (get/validate-string tld symbol)))))
                (if (= deprecated 1)
                    (mark-deprecated typelib tld name proc)
                    proc)))))))
  
  (define (make/validate-callout typelib tld signature-offset method?)
    (let-attributes signature-blob-fetcher
        (validated-pointer+ tld signature-offset ((header-fetcher 'signature-blob-size) tld))
        ;; FIXME: `arguments' are not handled correctly by GIR (no array size)
        (return-type n-arguments arguments)
      (let-values (((return-type pointer?)
                    (simple-type-blob/type+pointer typelib tld return-type))
                   ((prim-types setup collect cleanup vtypes flags)
                    (arg-blobs-values typelib tld arguments n-arguments method?)))
        (cond ((symbol? return-type)
               (case return-type
                 ((void) (make-callout 'void prim-types setup collect cleanup vtypes flags))
                 (else
                  (raise-validation-error "non-void return types not yet implemented" return-type))))
              ((gobject-class? return-type)
               (make-callout 'pointer prim-types setup collect cleanup vtypes flags))
              (else
               (raise-validation-error "complex return types not yet implemented" return-type))))))

  (define arg-blobs-values
    (let-accessors arg-blob-fetcher ((arg-in in)
                                     (arg-out out)
                                     (arg-null-ok null-ok)
                                     (arg-type arg-type)
                                     (arg-name name))
      (lambda (typelib tld arg-blobs n-args method?)
        (let* ((arg-blob-size ((header-fetcher 'arg-blob-size) tld))
               (length-indices
                (filter-map (lambda (arg-blob)
                              (receive (type pointer?)
                                  (simple-type-blob/type+pointer typelib tld (arg-type arg-blob))
                                (and (array-type? type) (array-length-index type))))
                     (make-array-pointers arg-blobs n-args arg-blob-size))))
          (let loop ((prim-types '())
                     (setup-steps '())
                     (collect-steps '())
                     (cleanup-steps '())
                     ;; "virtual" argument types, without the
                     ;; conversion to a pointer for out arguments and
                     ;; alias resolution
                     (vtypes '())
                     (flags '())
                     (i (- n-args 1)))
            (if (< i 0)
                (values (if method? (cons 'pointer prim-types) prim-types)
                        (args-setup-procedure n-args setup-steps)
                        (args-collect-procedure collect-steps)
                        (args-cleanup-procedure cleanup-steps)
                        vtypes
                        flags)
                (let* ((arg-blob (pointer+ arg-blobs (* i arg-blob-size)))
                       (name (get/validate-string tld (arg-name arg-blob)))
                       (in? (bool (arg-in arg-blob)))
                       (out? (bool (arg-out arg-blob)))
                       (null-ok? (bool (arg-null-ok arg-blob)))
                       (length? (memv i length-indices)))
                  (define (flag)
                    (cond ((and in? out?) 'in-out)
                          (out? 'out)
                          (in? 'in)
                          (else
                           (raise-validation-error "argument has no direction" i))))
                  (let*-values (((type pointer?)
                                 (simple-type-blob/type+pointer typelib tld (arg-type arg-blob)))
                                ((prim-type setup! collect cleanup)
                                 (arg/prim-type+setup+collect+cleanup type i null-ok? out?)))
                    (define (new-prim-types)
                      (cons (if out? 'pointer prim-type) prim-types))
                    (cond (length?
                           (loop (new-prim-types)
                                 (cons #f setup-steps)
                                 collect-steps
                                 cleanup-steps
                                 (cons type vtypes)
                                 (cons (flag) flags)
                                 (- i 1)))
                          (else
                           (loop (new-prim-types)
                                 (cons setup! setup-steps)
                                 (if collect
                                     (cons collect collect-steps)
                                     collect-steps)
                                 cleanup-steps
                                 (cons type vtypes)
                                 (cons (flag) flags)
                                 (- i 1))))))))))))

  (define (args-pre-call! arg-vec vtypes flags)
    (do ((i 0 (+ i 1))
         (vtypes vtypes (cdr vtypes))
         (flags flags (cdr flags)))
        ((>= i (vector-length arg-vec)))
      (case (car flags)
        ((in-out) (vector-set! arg-vec i (malloc/set! (car vtypes) (vector-ref arg-vec i))))
        ((out) (vector-set! arg-vec i
                            (malloc (c-type-sizeof (type-tag-symbol->prim-type (car vtypes)))))))))

  (define (args-post-call! arg-vec vtypes flags)
    (do ((i 0 (+ i 1))
         (vtypes vtypes (cdr vtypes))
         (flags flags (cdr flags)))
        ((>= i (vector-length arg-vec)))
      (case (car flags)
        ((in-out out) (vector-set! arg-vec i (deref-pointer (vector-ref arg-vec i) (car vtypes)))))))
  
  (define (arg/prim-type+setup+collect+cleanup type i null-ok? out?)
    (cond
     ((symbol? type)
      (let ((prim-type (type-tag-symbol->prim-type type)))
        (case type
          ((int8 uint8 int16 uint16 int32 uint32
                 int64 uint64 int uint long ulong ssize size
                 float double)
           (values prim-type i #f #f))
          ((utf8)
           (values prim-type
                   (converter-setup/null-ok i string->utf8z-ptr null-ok? #f)
                   (and out? (converter-collect i utf8z-ptr->string))
                   (cleanup-step free i)))
          (else
           (raise-typelib-callout-error "argument passing for this type not implemented" type)))))
     ((genum? type)
      ;; FIXME: Is signed-int always OK?
      (values 'signed-int
              (converter-setup i (lambda (val)
                                   (if (symbol? val)
                                       (or (genum-lookup type val)
                                           (raise-typelib-callout-error
                                            "invalid enumeration value" val (genum-symbols type)))
                                       val)))
              (and out? (converter-collect i (lambda (val)
                                               (or (genum-lookup type val) val))))
              #f))
     ((array-type? type)
      (values 'pointer
              (array-arg-setup type i)
              (array-arg-collect type i)
              (array-arg-cleanup type i)))
     (else
      (raise-validation-error "complex argument types not yet implemented" type))))

  (define (cleanup-step cleanup-proc i)
    (lambda (arg-vec)
      (cleanup-proc (vector-ref arg-vec i))))
  
  (define (converter-setup/null-ok i convert null-ok? null-val)
    (lambda (args arg-vec)
      (vector-set! arg-vec i (if (and (or *null-ok-always-on*
                                          null-ok?)
                                      (equal? (car args) null-val))
                                 (integer->pointer 0)
                                 (convert (car args))))
      (cdr args)))

  (define (converter-setup i convert)
    (lambda (args arg-vec)
      (vector-set! arg-vec i (convert (car args)))
      (cdr args)))
  
  (define (converter-collect i converter)
    (lambda (arg-vec)
      (converter (vector-ref arg-vec i))))
  
  (define (array-arg-setup type i)
    (lambda (args arg-vec)
      (let* ((vec (cond ((vector? (car args)) (car args))
                        ((list? (car args)) (list->vector (car args)))
                        (else (raise-typelib-callout-error
                               "cannot convert argument to array" (car args)))))
             (array (vector->c-array vec type)))
        (vector-set! arg-vec i array)
        (cond ((array-length-index type)
               => (lambda (l-index)
                    (vector-set! arg-vec l-index (vector-length vec))))))
      (cdr args)))

  (define (array-arg-collect atype i)
    (lambda (arg-vec)
      (c-array->vector (vector-ref arg-vec i) atype)))

  (define (array-arg-cleanup type i)
    (lambda (arg-vec)
      (free-c-array (vector-ref arg-vec i) type)))
  
  (define (args-setup-procedure n-args steps)
    (define (lose msg . irritants)
      (apply raise-typelib-callout-error msg irritants))
    (if (equal? steps (iota n-args))
        #f
        (lambda (in-args)
          (let ((arg-vec (make-vector n-args)))
            (let loop ((args in-args) (steps steps))
              (cond ((null? steps)
                     (unless (null? args)
                       (lose "unprocessed arguments" args))
                     arg-vec)
                    ((null? args)
                     (lose "too few arguments" in-args))
                    ((integer? (car steps))
                     (vector-set! arg-vec (car steps) (car args))
                     (loop (cdr args) (cdr steps)))
                    ((eqv? (car steps) #f)
                     (loop args (cdr steps)))
                    (else
                     (loop ((car steps) args arg-vec) (cdr steps)))))))))

  (define (args-collect-procedure steps)
    (if (null? steps)
        #f
        (lambda (arg-vec)
          (let loop ((out-vals '()) (steps steps))
            (if (null? steps)
                out-vals
                (loop (cons ((car steps) arg-vec) out-vals) (cdr steps)))))))

  (define (args-cleanup-procedure steps)
    (if (null? steps)
        #f
        (lambda (arg-vec)
          (for-each (lambda (step) (step arg-vec)) steps))))
  
  (define (make-callout prim-ret prim-args setup collect cleanup vtypes flags)
    (let ((prim-callout (make-c-callout prim-ret prim-args))
          (out-args? (exists (lambda (flag) (memq flag '(out in-out))) flags)))
      (cond ((and setup collect out-args?)
             (lambda (ptr)
               (let ((do-callout (prim-callout ptr)))
                 (lambda args
                   (let ((arg-vec (setup args)))
                     (args-pre-call! arg-vec vtypes flags)
                     (let ((ret-val (apply do-callout (vector->list arg-vec))))
                       (args-post-call! arg-vec vtypes flags)
                       (let ((out-vals (collect arg-vec)))
                         (if cleanup (cleanup arg-vec))
                         (if (eq? prim-ret 'void)
                             (apply values out-vals)
                             (apply values ret-val out-vals)))))))))
            (setup
             (assert (not (or collect out-args?)))
             (lambda (ptr)
               (let ((do-callout (prim-callout ptr)))
                 (lambda args
                   (let* ((arg-vec (setup args))
                          (ret-val (apply do-callout (vector->list arg-vec))))
                     (if cleanup (cleanup arg-vec))
                     (if (eq? prim-ret 'void)
                         (values)
                         ret-val))))))
            (else
             (assert (not (or setup collect out-args?)))
             prim-callout))))
  
  (define (make/validate-vfunc-caller tld index callout)
    (raise-typelib-error "vfunc calling not yet implemented"))

  (define (make-class-loader typelib tld entry-ptr name)
    (define (member-func-maker method?)
      (lambda (blob)
        (let ((name (scheme-ified-symbol
                     (get/validate-string tld ((function-blob-fetcher 'name) blob)))))
          (cons name
                (make-lazy-entry
                 (lambda ()
                   (make/validate-function typelib tld blob method?)))))))
    (let* ((offset ((dir-entry-fetcher 'offset) entry-ptr))
           (blob (validated-pointer+ tld offset ((header-fetcher 'object-blob-size) tld))))
      (lambda ()
        (let-attributes header-fetcher tld
                        (object-blob-size field-blob-size property-blob-size
                                          function-blob-size signal-blob-size
                                          vfunc-blob-size constant-blob-size)
          (let-attributes object-blob-fetcher blob
                          (blob-type deprecated name gtype-name gtype-init parent
                                     n-interfaces n-fields n-properties n-methods n-signals
                                     n-vfuncs n-constants)
            (unless (= blob-type 7)
              (raise-validation-error "invalid blob type for object entry" blob-type))
            (when (= deprecated 1)
              ((typelib-deprecation-handler) typelib (get/validate-string tld name)))
            (let ((ifaces-size (c-type-align 'uint32 (* 2 n-interfaces)))
                  (fields-size (* field-blob-size n-fields))
                  (properties-size (* property-blob-size n-properties))
                  (methods-size (* function-blob-size n-methods))
                  (vfuncs-size (* vfunc-blob-size n-vfuncs))
                  (signals-size (* signal-blob-size n-signals))
                  (constants-size (* constant-blob-size n-constants)))
              (validated-pointer+ tld offset (+ object-blob-size ifaces-size fields-size
                                                properties-size methods-size signals-size
                                                vfuncs-size constants-size))
              (let* ((ifaces (pointer+ blob object-blob-size))
                     (fields (pointer+ ifaces ifaces-size))
                     (properties (pointer+ fields fields-size))
                     (methods (pointer+ properties properties-size))
                     (signals (pointer+ methods methods-size))
                     (vfuncs (pointer+ signals signals-size))
                     (constants (pointer+ vfuncs vfuncs-size))
                     (parent (and (> parent 0)
                                  (typelib-get-entry/index typelib parent))))
                (receive (constructors methods)
                    (partition (lambda (m)
                                 (let-attributes function-blob-fetcher m
                                                 (constructor)
                                   (= constructor 1)))
                               (make-array-pointers methods n-methods function-blob-size))
                  (make-gobject-class (typelib-namespace typelib)
                                      name
                                      parent
                                      (filter-map (member-func-maker #f) constructors)
                                      (filter-map (member-func-maker #t) methods))))))))))

  (define (make-enum-loader typelib tld entry-ptr entry-name)
    (lambda ()
      (let ((blob (validated-pointer+ tld
                                      ((dir-entry-fetcher 'offset) entry-ptr)
                                      ((header-fetcher 'enum-blob-size) tld))))
        (let-attributes enum-blob-fetcher blob
                        (blob-type n-values values)
          (unless (= blob-type 5)
            (raise-validation-error "invalid blob type for enum entry" blob-type))
          (make-genum
           (map (lambda (val-ptr)
                  (let-attributes value-blob-fetcher val-ptr
                                  (name value)
                    (cons (scheme-ified-symbol (get/validate-string tld name))
                          value)))
                (make-array-pointers values n-values ((header-fetcher 'value-blob-size) tld))))))))
  
  (define (make-constant-loader typelib tld entry-ptr entry-name)
    (lambda ()
      (let ((blob (validated-pointer+ tld
                                      ((dir-entry-fetcher 'offset) entry-ptr)
                                      ((header-fetcher 'constant-blob-size) tld))))
        (let-attributes enum-blob-fetcher blob
                        (blob-type deprecated name type size offset)
          (unless (= blob-type 9)
            (raise-validation-error "invalid blob type for constant entry" blob-type))
          (receive (type pointer?) (simple-type-blob/type+pointer typelib tld blob-type)
            (when pointer?
              (raise-validation-error "pointer constants not yet supported"))
            (unless (symbol? type)
              (raise-validation-error "non-simple constanst not yet supported"))
            (unless (= (c-type-sizeof type) size)
              (raise-validation-error "constant size mismatch" type size))
            (fetch-constant typelib tld type offset))))))

  (define (fetch-constant typelib tld type offset)
    (let ((mem (validated-pointer+ tld offset (c-type-sizeof type))))
      ((make-pointer-c-getter (type-tag-symbol->prim-type type)) mem 0)))
  
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
               ((typelib-deprecation-handler) typelib name)
               (apply proc args))))))
  
  ;;
  ;; Helpers
  ;;
  (define (simple-type-blob/type+pointer typelib tld st-blob)
    (let-attributes simple-type-blob-fetcher st-blob
                    (offset reserved reserved2 pointer tag)
      (cond ((and (= reserved 0) (= reserved2 0))
             (when (>= tag type-tag-array)
               (raise-validation-error "wrong tag in simple type" tag))
             (when (and (>= tag type-tag-utf8) (= pointer 0))
               (raise-validation-error "pointer type expected for tag" tag))
             (values (type-tag->symbol tag) (bool pointer)))
            (else
             (let-attributes interface-type-blob-fetcher (validated-pointer+ tld offset 4)
                             (pointer tag)
               (values (make/validate-type typelib tld tag offset) (bool pointer)))))))

  (define (make/validate-type typelib tld tag offset)
    (case tag
      ((22)
       (let-attributes array-type-blob-fetcher (validated-pointer+ tld offset 4)
                       (pointer tag zero-terminated has-length has-size length size type)
         (when (and (= has-length 1) (= has-size 1))
           (raise-validation-error "array type has both length and size"))
         (receive (element-type elements-pointers?) (simple-type-blob/type+pointer typelib tld type)
             (make-array-type element-type
                              elements-pointers?
                              (bool zero-terminated)
                              (and (= has-size 1) size)
                              (and (= has-length 1) length)))))
      ((23)
       (let-attributes interface-type-blob-fetcher (validated-pointer+ tld offset 4)
                       (interface)
         (typelib-get-entry/index typelib interface)))
      (else
       (raise-validation-error "non-simple type of this kind not yet implemented" tag))))
  
  (define-record-type array-type
    (fields (immutable element-type array-element-type)
            (immutable elements-pointers? array-elements-pointers?)
            (immutable is-zero-terminated array-is-zero-terminated?)
            (immutable size array-size)
            (immutable length-index array-length-index)))
  
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
  
  (define (get/validate-string tld offset)
    ;; FIXME: This should actually check we don't go beyond the data
    ;; when assembling the string
    (utf8z-ptr->string (validated-pointer+ tld offset 1)))
  
  (define (validated-pointer+ tld offset size)
    (when (> (+ offset size) (header-size tld))
      (raise-validation-error "offset/size out of bounds" offset size (header-size tld)))
    (pointer+ tld offset))

  ;; Allocate memory as needed for the type @2, store a representation
  ;; of @1 in it, and return a pointer to the allocated memory
  (define (malloc/set! type val)
    (cond ((symbol? type)
           (let ((type (type-tag-symbol->prim-type type)))
             (let ((mem (malloc (c-type-sizeof type))))
               ((make-pointer-c-setter type) mem 0 val)
               mem)))
          ((array-type? type)
           (let ((mem (malloc (c-type-sizeof 'pointer))))
             (pointer-set-c-pointer! mem 0 val)
             mem))
          (else
           (error 'malloc/set! "not implemented" type val))))

  ;; Retrieve a Scheme representation of the memory pointed to by @1,
  ;; according to the type @2.
  (define (deref-pointer ptr type)
    (cond ((symbol? type)
           ((make-pointer-c-getter (type-tag-symbol->prim-type type)) ptr 0))
          ((array-type? type)
           (pointer-ref-c-pointer ptr 0))
          (else
           (error 'deref-pointer "not implemented for that type" ptr type))))
  
  (define gerror-free
    ((make-c-callout 'void '(pointer)) (dlsym libglib "g_error_free")))

  (define (utf8z-ptr->string ptr)
    (let ((size (do ((i 0 (+ i 1)))
                    ((= (pointer-ref-c-unsigned-char ptr i) 0) i))))
      (utf8->string (memcpy (make-bytevector size) ptr size))))
  
  (define (->utf8z-ptr/null who s)
    (cond ((string? s) (string->utf8z-ptr s))
          ((eqv? s #f)
           (integer->pointer 0))
          (else
           (assertion-violation who "invalid argument" s))))

  (define (string->utf8z-ptr s)
    (let* ((bytes (string->utf8 s))
           (bytes-len (bytevector-length bytes))
           (result (malloc (+ bytes-len 1))))
      (memcpy result bytes bytes-len)
      (pointer-set-c-char! result bytes-len 0)
      result))

  (define (array-type-values atype)
    (cond ((symbol? (array-element-type atype))
           (let* ((prim-type (type-tag-symbol->prim-type (array-element-type atype)))
                  (elt-size (c-type-sizeof prim-type)))
             (case (array-element-type atype)
               ((utf8)
                (values prim-type elt-size pointer-utf8z-ptr-ref pointer-utf8z-ptr-set!))
               (else
                (values prim-type
                        elt-size
                        (make-pointer-c-getter prim-type)
                        (make-pointer-c-setter prim-type))))))
          (else
           (raise-validation-error "non-simple array element types not yet supported"))))
  
  (define (vector->c-array vec atype)
    (let ((len (vector-length vec)))
      (receive (prim-type element-size element-ref element-set!) (array-type-values atype)
        (let ((mem (malloc (* element-size (+ len (if (array-is-zero-terminated? atype) 1 0))))))
          (do ((i 0 (+ i 1)))
              ((>= i len))
            (element-set! mem (* i element-size) (vector-ref vec i)))
          (when (array-is-zero-terminated? atype)
            (element-set! mem
                          (* element-size len)
                          (array-terminator prim-type)))
          mem))))

  (define (c-array->vector ptr atype)
    (receive (prim-type element-size element-ref element-set!) (array-type-values atype)
      (cond ((array-is-zero-terminated? atype)
             (let ((terminator?
                    (let ((terminator (array-terminator prim-type)))
                      (if (pointer? terminator)
                          (lambda (offset)
                            (= (pointer->integer (pointer-ref-c-pointer ptr offset))
                               (pointer->integer terminator)))
                          (lambda (offset)
                            (= (element-ref ptr offset) terminator))))))
               (let loop ((offset 0) (elts '()))
                 (if (terminator? offset)
                     (list->vector (reverse elts))
                     (loop (+ offset element-size) (cons (element-ref ptr offset) elts))))))
            ((array-size atype)
             => (lambda (size)
                  (do ((vec (make-vector size))
                       (i 0 (+ i 1)))
                      ((>= i size) vec)
                    (vector-set! vec i (element-ref ptr (* i element-size)))))))))


  (define (array-terminator prim-type)
    (case prim-type
      ((char uchar short ushort int uint long ulong) 0)
      ((pointer) (integer->pointer 0))
      (else
       (raise-typelib-callout-error
        "zero-termination of arrays of this type not implemented" prim-type))))
  
  (define (free-c-array ptr atype)
    (cerr "free-c-array not implemented, you are leaking memory :-P\n"))
  

  (define (pointer-utf8z-ptr-set! ptr i val)
    (pointer-set-c-pointer! ptr i (if (pointer? val)
                                      val
                                      (string->utf8z-ptr val))))

  (define (pointer-utf8z-ptr-ref ptr i)
    (let ((utf8z-ptr (pointer-ref-c-pointer ptr i)))
      (if (= (pointer->integer utf8z-ptr) 0)
          #f
          (utf8z-ptr->string utf8z-ptr))))
  
  (define (pointer+ p n)
    (integer->pointer (+ (pointer->integer p) n)))

  (define (make-array-pointers base n size)
    (do ((i (- n 1) (- i 1))
         (result '() (cons (pointer+ base (* i size)) result)))
        ((< i 0) result)))
  
  (define (bool i)
    (not (= 0 i)))

  (define (type-tag-symbol->prim-type sym)
    (case sym
      ((utf8) 'pointer)
      (else sym)))
  
  (define (warning msg . args)
    (apply format (current-error-port) msg args)
    (newline (current-error-port)))

  (define *null-ok-always-on* #f)
  
  ;; Initialize the GObject type system
  (((make-c-callout 'void '()) (dlsym libgobject "g_type_init" ))))
