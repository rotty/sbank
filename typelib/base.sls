;;; base.sls --- gobject-introspection typelib destructuring

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

;; This library provides access to the gobject-introspection typelib
;; information, in a high-level way.

;;; Code:


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
          (sbank utils)
          (sbank stypes)
          (sbank ctypes)
          (sbank gobject)
          (sbank conditions)
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

  (define (raise-sbank-error msg . irritants)
    (raise (condition (make-sbank-error)
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

  (define-condition-type &validation-error &sbank-error
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
                        (if method? (cons 0 setup-steps) setup-steps)
                        collect-steps
                        cleanup-steps
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
                                 (arg/prim-type+steps type i null-ok? out?)))
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

  (define (make/validate-vfunc-caller tld index callout)
    (raise-sbank-error "vfunc calling not yet implemented"))

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

  (define gerror-free
    ((make-c-callout 'void '(pointer)) (dlsym libglib "g_error_free")))

  (define (pointer+ p n)
    (integer->pointer (+ (pointer->integer p) n)))

  (define (make-array-pointers base n size)
    (do ((i (- n 1) (- i 1))
         (result '() (cons (pointer+ base (* i size)) result)))
        ((< i 0) result)))
  
  (define (bool i)
    (not (= 0 i)))

  (define (warning msg . args)
    (apply format (current-error-port) msg args)
    (newline (current-error-port)))

  ;; Initialize the GObject type system
  (((make-c-callout 'void '()) (dlsym libgobject "g_type_init" ))))
