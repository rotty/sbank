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
          (sbank type-data)
          (sbank stypes)
          (sbank ctypes)
          (sbank shlibs)
          (sbank gobject internals)
          (sbank conditions)
          (sbank typelib decorators)
          (for (sbank typelib stypes) run expand))

  (define-syntax debug (syntax-rules () ((debug <expr> ...) (begin))))
  #|
  (define-syntax debug
    (syntax-rules ()
      ((debug <expr> ...)
       (for-each display (list "DEBUG: " <expr> ... "\n")))))
  |#
  
  ;;
  ;; Typelib accessors and fetcher factories
  ;;
  
  (define-syntax define-accessors (stype-accessor-definer (typelib-stypes)))

  (define-accessors "GTypelib"
    (tl-data "data"))
  
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
  (define-fetcher error-type-blob-fetcher "ErrorTypeBlob")
  (define-fetcher object-blob-fetcher "ObjectBlob")
  (define-fetcher arg-blob-fetcher "ArgBlob")
  (define-fetcher constant-blob-fetcher "ConstantBlob")
  (define-fetcher enum-blob-fetcher "EnumBlob")
  (define-fetcher value-blob-fetcher "ValueBlob")
  (define-fetcher array-type-blob-fetcher "ArrayTypeBlob")
  (define-fetcher signal-blob-fetcher "SignalBlob")
  
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

  ;;
  ;; Public API
  ;;

  (define (raise-sbank-error msg . irritants)
    (raise (condition (make-sbank-error)
                      (make-message-condition msg)
                      (make-irritants-condition irritants))))
  
  (define-record-type typelib
    ;;(opaque #t)
    (fields (immutable tl typelib-tl)
            (immutable namespace)
            (immutable name-table typelib-name-table)
            (immutable signatures typelib-signatures)
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
    (let ((g-ir-require ((make-c-callout 'pointer '(pointer pointer pointer uint pointer))
                           (dlsym libgir "g_irepository_require"))))
      (lambda (who namespace version flags)
        (with-validation-context namespace
          (let ((c-namespace (->utf8z-ptr/null who namespace))
                (c-version (->utf8z-ptr/null who version))
                (gerror (malloc/set! 'pointer (integer->pointer 0))))
            (let ((result 
                   (g-ir-require (integer->pointer 0) c-namespace c-version flags gerror)))
              (cond ((= 0 (pointer->integer result))
                     (let ((e (pointer-ref-c-pointer gerror 0)))
                       (free gerror)
                       (raise-gerror/free 'require-typelib #f e namespace version)))
                    (else
                     (make/validate-typelib result namespace)))))))))

  
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
           (decorated-loader
            (typelib-namespace typelib)
            entry-name
            (if (= local 0)
                (lambda ()
                  (let ((namespace (get/validate-string tld offset))
                        (name (get/validate-string tld name)))
                    (or
                     (and-let* ((tl (require-typelib namespace #f 0)))
                       (typelib-get-entry tl name))
                     (raise-validation-error
                      "invalid reference to other namespace"
                      namespace name))))
                (case blob-type
                  ((1) (make-function-loader typelib tld entry-ptr entry-name))
                  ((3) (make-record-loader typelib tld entry-ptr entry-name))
                  ((5) (make-enum-loader typelib tld entry-ptr entry-name))
                  ((7) (make-class-loader typelib tld entry-ptr entry-name))
                  ((9) (make-constant-loader typelib tld entry-ptr entry-name))
                  (else
                   (lambda ()
                     (raise-validation-error "unknown entry type encountered" blob-type)))))))))))

  (define (make-function-loader typelib tld entry-ptr name)
    (let ((blob (validated-pointer+ tld
                                    ((dir-entry-fetcher 'offset) entry-ptr)
                                    ((header-fetcher 'function-blob-size) tld))))
      (lambda ()
        (make/validate-function typelib tld blob #f))))

  (define (get-signature typelib tld signature callout callback)
    (or (table-ref (typelib-signatures typelib) signature)
        (let ((result (make-signature callout callback)))
          (table-set! (typelib-signatures typelib) signature result)
          result)))
                                                  
  (define (make/validate-function typelib tld blob container)
    (let-attributes
            function-blob-fetcher blob
            (blob-type deprecated wraps-vfunc index name symbol signature
                       constructor getter setter)
      (let ((name (get/validate-string tld name)))
        (unless (= blob-type 1)
          (raise-validation-error "invalid blob type for function entry" blob-type))
        (when (and (bool constructor) (not container))
          (raise-validation-error "constructor without container"))
        (unless (or (= index 0) (bool setter) (bool getter) (bool wraps-vfunc))
          (raise-validation-error "indexed function blob must be setter getter or wrapper"))
        (let ((signature (get-signature typelib
                                        tld
                                        signature
                                        (lambda ()
                                          (make/validate-callout typelib
                                                                 tld
                                                                 signature
                                                                 (bool constructor)
                                                                 container))
                                        (lambda ()
                                          (make/validate-callback typelib
                                                                  tld
                                                                  signature
                                                                  (not (bool constructor))
                                                                  container)))))
          (cond ((= wraps-vfunc 1)
                 (make/validate-vfunc-caller tld index (signature-callout signature)))
                ((= getter 1)
                 (raise-validation-error "getters not yet supported"))
                ((= setter 1)
                 (raise-validation-error "setters not yet supported"))
                (else
                 (let ((proc ((signature-callout signature)
                              (typelib-dlsym typelib (get/validate-string tld symbol)))))
                   (if (= deprecated 1)
                       (mark-deprecated typelib tld name proc)
                       proc))))))))
  
  (define (make/validate-callout typelib tld signature-offset constructor? container)
    (let-attributes signature-blob-fetcher
        (validated-pointer+ tld signature-offset ((header-fetcher 'signature-blob-size) tld))
        (return-type n-arguments arguments may-return-null)
      (let-values (((rti)
                    (if constructor?
                        (make-type-info container #t (bool may-return-null))
                        (stblob-type-info typelib tld return-type (bool may-return-null))))
                   ((arg-types setup collect cleanup flags)
                    (arg-blobs-callout-values typelib
                                              tld
                                              arguments
                                              n-arguments
                                              constructor?
                                              container)))
        (when (and constructor? (not (gobject-class? (type-info-type rti))))
          (raise-validation-error "constructor does not return a class type"))
        (make-callout rti arg-types setup collect cleanup flags))))

  (define arg-blobs-callout-values
    (let-accessors arg-blob-fetcher ((arg-in in)
                                     (arg-out out)
                                     (arg-null-ok null-ok)
                                     (arg-type arg-type)
                                     (arg-name name))
      (lambda (typelib tld arg-blobs n-args constructor? container)
        (let* ((arg-blob-size ((header-fetcher 'arg-blob-size) tld))
               (type-infos (map (lambda (blob)
                                  (stblob-type-info typelib
                                                    tld
                                                    (arg-type blob)
                                                    (bool (arg-null-ok blob))))
                                (reverse (make-array-pointers arg-blobs n-args arg-blob-size))))
               (length-indices (filter-map (lambda (ti)
                                             (let ((type (type-info-type ti)))
                                               (and (array-type? type)
                                                    (array-length-index type))))
                                           type-infos))
               (has-self-ptr? (and container (not constructor?))))
          (let loop ((arg-types '())
                     (setup-steps '())
                     (collect-steps '())
                     (cleanup-steps '())
                     (flags '())
                     (tis type-infos)
                     (i (- n-args 1)))
            (if (< i 0)
                (if has-self-ptr?
                    (values (cons (make-type-info 'pointer #f #f) arg-types)
                            (cons 0 setup-steps)
                            collect-steps
                            cleanup-steps
                            (cons 'in flags))
                    (values arg-types setup-steps collect-steps cleanup-steps flags))
                (let* ((arg-blob (pointer+ arg-blobs (* i arg-blob-size)))
                       (name (get/validate-string tld (arg-name arg-blob)))
                       (in? (bool (arg-in arg-blob)))
                       (out? (bool (arg-out arg-blob)))
                       (length? (memv i length-indices)))
                  (define (flag)
                    (cond ((and in? out?) 'in-out)
                          (out? 'out)
                          (in? 'in)
                          (else
                           (raise-validation-error "argument has no direction" i))))
                  (receive (setup! collect cleanup)
                           (arg-steps (car tis) (if has-self-ptr? (+ i 1) i) out?)
                    (cond (length?
                           (loop (cons (car tis) arg-types)
                                 (cons #f setup-steps)
                                 collect-steps
                                 cleanup-steps
                                 (cons (flag) flags)
                                 (cdr tis)
                                 (- i 1)))
                          (else
                           (loop (cons (car tis) arg-types)
                                 (cons setup! setup-steps)
                                 (if collect
                                     (cons collect collect-steps)
                                     collect-steps)
                                 (if cleanup (cons cleanup cleanup-steps) cleanup-steps)
                                 (cons (flag) flags)
                                 (cdr tis)
                                 (- i 1))))))))))))

  (define (arg-blobs-callback-values typelib tld arguments n-arguments method? container)
    ;; IMPLEMENTME
    (values '() '() '() '()))
  
  (define (make/validate-callback typelib tld signature-offset method? container)
    (let-attributes signature-blob-fetcher
        (validated-pointer+ tld signature-offset ((header-fetcher 'signature-blob-size) tld))
        (return-type n-arguments arguments may-return-null)
      (receive (arg-types prepare collect flags)
               (arg-blobs-callback-values typelib
                                          tld
                                          arguments
                                          n-arguments
                                          method?
                                          container)
        (make-callback (stblob-type-info typelib tld return-type (bool may-return-null))
                       arg-types
                       prepare collect
                       flags))))
  
  (define (make/validate-vfunc-caller tld index callout)
    (raise-sbank-error "vfunc calling not yet implemented"))
  
  (define (make-class-loader typelib tld entry-ptr name)
    (define (member-func-maker container)
      (lambda (blob)
        (let ((name (scheme-ified-symbol
                     (get/validate-string tld ((function-blob-fetcher 'name) blob)))))
          (with-validation-context name
            (cons name
                  (make-lazy-entry
                   (thunk/validation-context
                    (lambda ()
                      (make/validate-function typelib tld blob container)))))))))
    (define (signal-maker blob)
      (let-attributes signal-blob-fetcher blob
                      (name signature)
        (let ((name (scheme-ified-symbol
                     (get/validate-string tld name))))
          (with-validation-context name
            (cons name
                  (make-signature
                   (thunk/validation-context
                    (lambda ()
                      (make/validate-callout typelib tld signature)))
                   (thunk/validation-context
                    (lambda ()
                      (make/validate-callback typelib tld signature)))))))))
    (let* ((offset ((dir-entry-fetcher 'offset) entry-ptr))
           (blob (validated-pointer+ tld offset ((header-fetcher 'object-blob-size) tld))))
      (lambda ()
        (make-gobject-class
         (typelib-namespace typelib)
         name 
         (lambda (class)
           (debug (list 'class-loader: class))
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
                     (let ((signals (map signal-maker
                                         (make-array-pointers signals n-signals signal-blob-size))))
                       (values parent
                               (map (member-func-maker class) constructors)
                               (map (member-func-maker class) methods)
                               signals))))))))))))

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
        (let-attributes constant-blob-fetcher blob
                        (blob-type deprecated name type size offset)
          (unless (= blob-type 9)
            (raise-validation-error "invalid blob type for constant entry" blob-type))
          (let* ((ti (stblob-type-info typelib tld type #f))
                 (type (type-info-type ti))
                 (pointer? (type-info-is-pointer? ti)))
            (unless (symbol? type)
              (raise-validation-error "non-simple constanst not yet supported"))
            (cond (pointer?
                   (unless (memq type '(utf8))
                     (raise-validation-error
                      "pointer constants of this type not yet supported" type)))
                  ((not (= (c-type-sizeof type) size))
                   (raise-validation-error "constant size mismatch" type size)))
            (fetch-constant typelib tld type offset))))))

  (define (fetch-constant typelib tld type offset)
    (case type
      ((utf8) (get/validate-string tld offset))
      (else
       (let ((mem (validated-pointer+ tld offset (c-type-sizeof type))))
         ((make-pointer-c-getter (type-tag-symbol->prim-type type)) mem 0)))))

  (define (make-record-loader typelib tld entry-ptr entry-name)
    (lambda ()
      (make-gobject-class (typelib-namespace typelib)
                          entry-name
                          (lambda (class)
                            (values #f '() '() '())))))
  
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

  (define (decorated-loader namespace name loader)
    (lambda ()
      (let ((decorator (lookup-typelib-decorator namespace name)))
        (debug (list 'decorated-loader: namespace name decorator))
        (if decorator (decorator (loader)) (loader)))))
  
  ;;
  ;; Helpers
  ;;
  (define (stblob-type-info typelib tld st-blob null-ok?)
    (let-attributes simple-type-blob-fetcher st-blob
                    (offset reserved reserved2 pointer tag)
      (cond ((and (= reserved 0) (= reserved2 0))
             (when (>= tag type-tag-array)
               (raise-validation-error "wrong tag in simple type" tag))
             (when (and (>= tag type-tag-utf8) (= pointer 0))
               (raise-validation-error "pointer type expected for tag" tag))
             (make-type-info (type-tag->symbol tag) (bool pointer) null-ok?))
            (else
             (make/validate-type-info typelib tld offset null-ok?)))))

  (define (make/validate-type-info typelib tld offset null-ok?)
    (let-attributes interface-type-blob-fetcher (validated-pointer+ tld offset 4)
                    (tag)
      (case (type-tag->symbol tag)
        ((array)
         (let-attributes array-type-blob-fetcher (validated-pointer+ tld offset 4)
                         (pointer tag zero-terminated has-length has-size length size type)
           (when (and (= has-length 1) (= has-size 1))
             (raise-validation-error "array type has both length and size"))
           (make-type-info
            (make-array-type (stblob-type-info typelib tld type #f)
                             (bool zero-terminated)
                             (and (= has-size 1) size)
                             (and (= has-length 1) length))
            #f
            null-ok?)))
        ((interface)
         (let-attributes interface-type-blob-fetcher (validated-pointer+ tld offset 4)
                         (interface)
           (let* ((entry (typelib-get-entry/index typelib interface))
                  (pointer? (not (genum? entry))))
             (debug (list "entry:" entry))
             (make-type-info entry pointer? null-ok?))))
        ((error)
         (let-attributes error-type-blob-fetcher (validated-pointer+ tld offset 4)
                         (tag n-domains domains)
           (make-type-info (make-gerror-type) #t #f)))
        (else
         (raise-validation-error "non-simple type of this kind not yet implemented"
                                 (type-tag->symbol tag))))))
  
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
