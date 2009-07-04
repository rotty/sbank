;;; base.sls --- gobject-introspection typelib destructuring

;; Copyright (C) 2008, 2009 Andreas Rottmann <a.rottmann@gmx.at>

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
#!r6rs

(library (sbank typelib base)
  (export require-typelib
          typelib-available?
          typelib-magic
          typelib-minor-version
          typelib-major-version
          typelib-get-entry-names
          typelib-get-entry
          typelib-dlsym)
  (import (rnrs)
          (srfi :2 and-let*)
          (srfi :14 char-sets)
          (spells foreign)
          (spells tracing)
          (spells alist)
          (srfi :39 parameters)
          (srfi :8 receive)
          (spells format)
          (spells string-utils)
          (only (srfi :13 strings) string-index)
          (only (srfi :1 lists) filter-map iota)
          (only (srfi :43 vectors) vector-fold)
          (spells define-values)
          (only (spells misc) or-map)
          (only (spells assert) cerr cout)
          (sbank support utils)
          (sbank support type-data)
          (for (sbank support stypes) run expand)
          (sbank ctypes basic)
          (sbank ctypes call)
          (sbank support shlibs)
          (sbank gobject internals)
          (sbank gobject gtype)
          (sbank support conditions)
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
    ;; This needs to be reasonably fast, so we don't use
    ;; let-attribute/fetchers here
    (header-size "size"))

  (define-accessors "ArgBlob"
    (arg-name "name")
    (arg-in "in")
    (arg-out "out")
    (arg-null-ok "allow_none")
    (arg-transfer-ownership "transfer_ownership")
    (arg-transfer-container-ownership "transfer_container_ownership")
    (arg-type "arg_type"))

  (define-accessors "FieldBlob"
    (field-name "name")
    (field-readable "readable")
    (field-writable "writable")
    (field-bits "bits")
    (field-struct-offset "struct_offset")
    (field-type "type"))

  (define-syntax define-fetcher
    (stype-fetcher-factory-definer (typelib-stypes)))

  (define-fetcher header-fetcher "Header")
  (define-fetcher function-blob-fetcher "FunctionBlob")
  (define-fetcher dir-entry-fetcher "DirEntry")
  (define-fetcher signature-blob-fetcher "SignatureBlob")
  (define-fetcher interface-type-blob-fetcher "InterfaceTypeBlob")
  (define-fetcher registered-type-blob-fetcher "RegisteredTypeBlob")
  (define-fetcher simple-type-blob-fetcher "SimpleTypeBlob")
  (define-fetcher error-type-blob-fetcher "ErrorTypeBlob")
  (define-fetcher object-blob-fetcher "ObjectBlob")
  (define-fetcher arg-blob-fetcher "ArgBlob")
  (define-fetcher constant-blob-fetcher "ConstantBlob")
  (define-fetcher enum-blob-fetcher "EnumBlob")
  (define-fetcher value-blob-fetcher "ValueBlob")
  (define-fetcher array-type-blob-fetcher "ArrayTypeBlob")
  (define-fetcher signal-blob-fetcher "SignalBlob")
  (define-fetcher property-blob-fetcher "PropertyBlob")
  (define-fetcher interface-blob-fetcher "InterfaceBlob")
  (define-fetcher parameter-type-blob-fetcher "ParamTypeBlob")
  (define-fetcher record-blob-fetcher "StructBlob")
  (define-fetcher callback-blob-fetcher "CallbackBlob")
  (define-fetcher field-blob-fetcher "FieldBlob")

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
    (fields tl
            namespace
            name-table
            signatures
            directory
            gtype-table
            (mutable shlibs %typelib-shlibs %typelib-set-shlibs!)))

  (define typelib-deprecation-handler
    (make-parameter (lambda (typelib name)
                      (cerr "DEPRECATED: item " name " of namespace "
                            (typelib-namespace typelib) "\n"))))

  (define (names->shlibs names)
    (filter-map
     (lambda (name)
       (let ((shlib (or (dlopen name #t #t)
                        (dlopen (string-append "lib" name ".so") #t #t))))
         (unless shlib
           (warning
            "failed to load shared library '~a' referenced by the typelib: ~a"
            name (dlerror)))
         shlib))
     (string-split names #\,)))

  ;; See comments in gtypelib.c (_g_typelib_init)
  (define (typelib-shlibs typelib)
    (or (%typelib-shlibs typelib)
        (let* ((tld (tl-data (typelib-tl typelib)))
               (names (get/validate-string
                       tld (typelib-header typelib 'shared-library)))
               (shlibs (names->shlibs names)))
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

  (define (typelib-gtype-lookup typelib)
    (lambda (gtype)
      (and-let* ((index (hashtable-ref (typelib-gtype-table typelib) gtype #f)))
        (typelib-get-entry/index typelib index))))

  (define *registered-typelibs* (make-hashtable string-hash string=?))

  (define (require-typelib namespace version flags)
    (or (hashtable-ref *registered-typelibs* namespace #f)
        (let ((typelib (require-typelib% 'require-typelib
                                         namespace version flags)))
          (hashtable-set! *registered-typelibs* namespace typelib)
          typelib)))

  (define (require-typelib% who namespace version flags)
    (with-validation-context namespace
      (let ((c-namespace (->utf8z-ptr/null who namespace))
            (c-version (->utf8z-ptr/null who version))
            (gerror (malloc/set! 'pointer (null-pointer))))
        (let ((result
               (g-ir-require% (null-pointer)
                              c-namespace
                              c-version
                              flags
                              gerror)))
          (cond ((null-pointer? result)
                 (let ((e (pointer-ptr-ref gerror 0)))
                   (free gerror)
                   (raise-gerror/free 'require-typelib #f e namespace version)))
                (else
                 (make/validate-typelib result namespace)))))))

  (define typelib-available?
    (case-lambda
      ((namespace version flags)
       (guard (c ((gerror? c) #f))
         (require-typelib namespace version flags)
         #t))
      ((namespace version)
       (typelib-available? namespace version 0))
      ((namespace)
       (typelib-available? namespace #f 0))))

  (define (typelib-magic typelib)
    (memcpy (make-bytevector 16)
            ((header-fetcher 'magic) (tl-data (typelib-tl typelib))) 16))

  (define (typelib-minor-version typelib)
    ((header-fetcher 'minor-version) (tl-data (typelib-tl typelib))))

  (define (typelib-major-version typelib)
    ((header-fetcher 'major-version) (tl-data (typelib-tl typelib))))

  (define (typelib-get-entry-names typelib)
    (vector-fold (lambda (i state name)
                  (cons name state))
                '()
                (hashtable-keys (typelib-name-table typelib))))

  (define (typelib-get-entry typelib name)
    (and-let* ((index (hashtable-ref (typelib-name-table typelib) name #f)))
      (typelib-get-entry/index typelib index)))

  (define (typelib-get-entry/index typelib index)
    (let* ((dir (typelib-directory typelib))
           (entry (vector-ref dir (- index 1))))
      (if (lazy-entry? entry)
          (let ((val ((lazy-entry-proc entry))))
            (vector-set! dir (- index 1) val)
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
       (parameterize ((validation-context
                       (cons <context> (validation-context))))
         <body> ...))))

  (define tl-magic-bytes (string->utf8 "GOBJ\nMETADATA\r\n\x1a;"))

  (define (make/validate-typelib tl namespace)
    (define (lose msg . irritants)
      (apply raise-validation-error msg irritants))
    (let ((tld (tl-data tl)))
      (let-attributes header-fetcher tld
                      (magic major-version minor-version n-entries)
        (and-let* ((magic (memcpy (make-bytevector 16) magic 16))
                   ((not (bytevector=? magic tl-magic-bytes))))
          (lose "invalid magic" magic))
        (or (and (= major-version 2) (= minor-version 0))
            (lose "version mismatch" major-version minor-version))
        (validate-blob-sizes tld)
        (let ((typelib (make-typelib tl
                                     namespace
                                     (make-hashtable string-hash string=?)
                                     (make-eqv-hashtable)
                                     (make-vector n-entries)
                                     (make-eqv-hashtable)
                                     #f)))
          (fill/validate-directory! typelib tld)
          (register-gtype-lookup! (typelib-gtype-lookup typelib))
          typelib))))

  (define (validate-blob-sizes tld)
    (for-each
     (lambda (blob-name/size)
       (let ((actual-size ((header-fetcher
                            (symbol-append (car blob-name/size) '- 'blob-size))
                           tld)))
         (or (= (cdr blob-name/size) actual-size)
             (raise-validation-error "blob size mismatch"
                                     blob-name/size
                                     actual-size))))
     '((entry . 12)
       (function . 20)
       (callback . 12)
       (signal . 16)
       (vfunc . 20)
       (arg . 16)
       (property . 16)
       (field . 16)
       (value . 12)
       (constant . 24)
       (error-domain . 16)
       (attribute . 12)
       (signature . 8)
       (enum . 24)
       (struct . 32)
       (object . 44)
       (interface . 40)
       (union . 40))))

  (define (fill/validate-directory! typelib tld)
    (let-attributes header-fetcher tld
                    (entry-blob-size n-entries n-local-entries directory)
      (do ((dir (typelib-directory typelib))
           (name-table (typelib-name-table typelib))
           (gtype-table (typelib-gtype-table typelib))
           (i 0 (+ i 1))
           (entry-ptr
            (validated-pointer+ tld directory
                                (* n-local-entries entry-blob-size))
            (pointer+ entry-ptr entry-blob-size)))
          ((>= i n-entries))
        (let-attributes dir-entry-fetcher entry-ptr
                        (name)
          (let* ((name (get/validate-string tld name))
                 (gtype (get/validate-dir-entry-gtype typelib tld
                                                      name entry-ptr)))
            (vector-set! dir i
                         (make-lazy-entry
                          (make-entry-loader typelib tld name entry-ptr gtype)))
            (when gtype
              (hashtable-set! gtype-table gtype (+ i 1)))
            (when (< i n-local-entries)
              (hashtable-set! name-table name (+ i 1))))))))

  (define (make-entry-loader typelib tld entry-name entry-ptr gtype)
    (with-validation-context entry-name
      (let-attributes dir-entry-fetcher entry-ptr
                      (name offset local blob-type)
        (let ((content-ptr (validated-pointer+ tld offset 1)))
          (proc/validation-context
           (decorated-loader
            typelib
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
                ((case blob-type
                   ((1) make-function-loader)
                   ((2) make-callback-loader)
                   ((3) make-record-loader)
                   ((5) make-enum-loader)
                   ((6) make-flags-loader)
                   ((7) make-class-loader)
                   ((8) make-interface-loader)
                   ((9) make-constant-loader)
                   ((11) make-union-loader)
                   (else
                    (lambda args
                      (lambda ()
                        (raise-validation-error
                         "unknown entry type encountered" blob-type)))))
                 typelib tld entry-ptr entry-name gtype))))))))

  (define (make-function-loader typelib tld entry-ptr name gtype)
    (let ((blob (validated-pointer+
                 tld
                 ((dir-entry-fetcher 'offset) entry-ptr)
                 ((header-fetcher 'function-blob-size) tld))))
      (lambda ()
        (make/validate-function typelib tld blob #f))))

  (define (get-signature typelib tld signature constructor? container)
    (or (hashtable-ref (typelib-signatures typelib) signature #f)
        (let ((result (make/validate-signature
                       typelib tld signature constructor? container)))
          (hashtable-set! (typelib-signatures typelib) signature result)
          result)))

  (define (make/validate-function typelib tld blob container)
    (let-attributes
            function-blob-fetcher blob
            (blob-type deprecated wraps-vfunc index name symbol signature
                       constructor getter setter)
      (let ((name (get/validate-string tld name)))
        (unless (= blob-type 1)
          (raise-validation-error
           "invalid blob type for function entry" blob-type))
        (when (and (bool constructor) (not container))
          (raise-validation-error "constructor without container"))
        (unless (or (= index 0) (bool setter) (bool getter) (bool wraps-vfunc))
          (raise-validation-error
           "indexed function blob must be setter getter or wrapper"))
        (let ((signature (get-signature typelib tld signature
                                        (bool constructor) container)))
          (cond
           ((= wraps-vfunc 1)
            (make/validate-vfunc-caller tld index (signature-callout signature)))
           ((= getter 1)
            (raise-validation-error "getters not yet supported"))
           ((= setter 1)
            (raise-validation-error "setters not yet supported"))
           (else
            (let* ((sym-name (get/validate-string tld symbol))
                   (proc ((signature-callout signature)
                          (or
                           (typelib-dlsym typelib sym-name)
                           (raise-validation-error "unable to lookup symbol"
                                                   (typelib-namespace typelib)
                                                   sym-name)))))
              (if (= deprecated 1)
                  (mark-deprecated typelib tld name proc)
                  proc))))))))

  (define (make-callback-loader typelib tld entry-ptr name gtype)
    (let ((blob (validated-pointer+
                 tld
                 ((dir-entry-fetcher 'offset) entry-ptr)
                 ((header-fetcher 'callback-blob-size) tld))))
      (lambda ()
        (let-attributes callback-blob-fetcher blob (deprecated signature)
          (make/validate-signature typelib tld signature #f #f)))))

  (define (make/validate-callout typelib tld signature-offset
                                 constructor? container)
    (let-attributes signature-blob-fetcher
        (validated-pointer+ tld signature-offset
                            ((header-fetcher 'signature-blob-size) tld))
        (return-type
         may-return-null
         caller-owns-return-value
         caller-owns-return-container
         n-arguments
         arguments)
      (let-values
          (((rti)
            (if constructor?
                (make-type-info container #t (bool may-return-null))
                (stblob-type-info typelib tld return-type
                                  (bool may-return-null) #f #f #f)))
           ((arg-types setup collect cleanup arg-flags)
            (arg-blobs-callout-values typelib
                                      tld
                                      arguments
                                      n-arguments
                                      constructor?
                                      container)))
        (when (and constructor? (not (gobject-class? (type-info-type rti))))
          (raise-validation-error "constructor does not return a class type"))
        (make-callout rti arg-types
                      setup collect cleanup
                      (calc-flags #f #f #f
                                  (bool caller-owns-return-value)
                                  (bool caller-owns-return-container))
                      arg-flags
                      (typelib-gtype-lookup typelib)))))

  (define (arg-blobs-type-infos typelib tld arg-blobs n-args arg-blob-size)
    (map (lambda (blob)
           (let-attributes arg-blob-fetcher blob (closure destroy scope)
             (stblob-type-info typelib
                               tld
                               (arg-type blob)
                               (bool (arg-null-ok blob))
                               (and (>= closure 0) closure)
                               (and (>= destroy 0) destroy)
                               (scope->symbol scope))))
         (reverse (make-array-pointers arg-blobs n-args arg-blob-size))))

  (define (type-infos-length-indices type-infos)
    (filter-map (lambda (ti)
                  (let ((type (type-info-type ti)))
                    (and (array-type? type)
                         (array-length-index type))))
                type-infos))

  (define (arg-blobs-callout-values typelib tld arg-blobs n-args
                                    constructor? container)
    (let* ((arg-blob-size ((header-fetcher 'arg-blob-size) tld))
           (type-infos (arg-blobs-type-infos typelib tld
                                             arg-blobs n-args arg-blob-size))
           (length-indices (type-infos-length-indices type-infos))
           (closure-indices (filter-map type-info-closure-index type-infos))
           (destroy-indices (filter-map type-info-destroy-index type-infos))
           (has-self-ptr? (and container (not constructor?)))
           (gtype-lookup (typelib-gtype-lookup typelib)))
      (let loop ((arg-types '())
                 (setup-steps '())
                 (collect-steps '())
                 (cleanup-steps '())
                 (flags '())
                 (tis type-infos)
                 (i (- n-args 1)))
        (if (< i 0)
            (if has-self-ptr?
                (receive (setup! collect cleanup)
                         (arg-callout-steps
                          has-self-ptr?
                          (make-type-info container #t #f)
                          0
                          (arg-flags in)
                          gtype-lookup)
                  (assert (not (or collect cleanup)))
                  (values (cons (make-type-info 'pointer #f #f) arg-types)
                          (cons setup! setup-steps)
                          collect-steps
                          cleanup-steps
                          (cons (arg-flags in) flags)))
                (values arg-types setup-steps collect-steps cleanup-steps flags))
            (let* ((arg-blob (pointer+ arg-blobs (* i arg-blob-size)))
                   (in? (bool (arg-in arg-blob)))
                   (out? (bool (arg-out arg-blob)))
                   (tf-os? (bool (arg-transfer-ownership arg-blob)))
                   (tf-c-os? (bool (arg-transfer-container-ownership arg-blob)))
                   (final-i  (if has-self-ptr? (+ i 1) i))
                   (length? (memv final-i length-indices))
                   (closure? (memv final-i closure-indices))
                   (destroy? (memv final-i destroy-indices))
                   (arg-flags (calc-flags i in? out? tf-os? tf-c-os?)))
              (receive (setup! collect cleanup)
                       (arg-callout-steps has-self-ptr?
                                          (car tis)
                                          final-i
                                          arg-flags
                                          gtype-lookup)
                (cond ((or length? closure? destroy?)
                       (loop (cons (car tis) arg-types)
                             (cons #f setup-steps)
                             collect-steps
                             cleanup-steps
                             (cons arg-flags flags)
                             (cdr tis)
                             (- i 1)))
                      (else
                       (loop (cons (car tis) arg-types)
                             (cons setup! setup-steps)
                             (if collect
                                 (cons collect collect-steps)
                                 collect-steps)
                             (if cleanup (cons cleanup cleanup-steps) cleanup-steps)
                             (cons arg-flags flags)
                             (cdr tis)
                             (- i 1))))))))))

  (define (calc-flags i in? out? transfer-ownership? transfer-container-ownership?)
    (list->arg-flags
     (append
      (cond ((and in? out?) '(in-out))
            (out? '(out))
            (in? '(in))
            ((eqv? i #f) '()) ;; return value
            (else
             (raise-validation-error "argument has no direction" i)))
      (if transfer-ownership? '(transfer-ownership) '())
      (if transfer-container-ownership? '(transfer-container-ownership) '()))))

  (define (arg-blobs-callback-values typelib tld arg-blobs n-args method? container)
    (let* ((arg-blob-size ((header-fetcher 'arg-blob-size) tld))
           (type-infos (arg-blobs-type-infos typelib tld
                                             arg-blobs n-args arg-blob-size))
           (length-indices (type-infos-length-indices type-infos))
           (closure-indices (filter-map type-info-closure-index type-infos))
           (has-self-ptr? (and method? container))
           (gtype-lookup (typelib-gtype-lookup typelib)))
      (let loop ((arg-types '())
                 (prepare-steps '())
                 (store-steps '())
                 (flags '())
                 (tis type-infos)
                 (i (- n-args 1)))
        (if (< i 0)
            (if has-self-ptr?
                (receive (prepare store)
                         (arg-callback-steps (make-type-info container #t #f)
                                             0
                                             gtype-lookup)
                  (values (cons (make-type-info 'pointer #f #f) arg-types)
                          (cons prepare prepare-steps)
                          store-steps
                          (cons (arg-flags in) flags)))
                (values arg-types prepare-steps store-steps flags))
            (let* ((arg-blob (pointer+ arg-blobs (* i arg-blob-size)))
                   (in? (bool (arg-in arg-blob)))
                   (out? (bool (arg-out arg-blob)))
                   (transfer-ownership? (bool (arg-transfer-ownership arg-blob)))
                   (transfer-container-ownership?
                    (bool (arg-transfer-container-ownership arg-blob)))
                   (ignore? (or (memv i length-indices)
                                (memv i closure-indices))))
              (define (arg-flags)
                (calc-flags i in? out?
                            transfer-ownership?
                            transfer-container-ownership?))
              (receive (prepare store)
                       (arg-callback-steps (car tis)
                                           (if has-self-ptr? (+ i 1) i)
                                           gtype-lookup)
                (cond (ignore?
                       (loop (cons (car tis) arg-types)
                             prepare-steps
                             store-steps
                             (cons (arg-flags) flags)
                             (cdr tis)
                             (- i 1)))
                      (else
                       (loop (cons (car tis) arg-types)
                             (cons prepare prepare-steps)
                             (if out?
                                 (cons store store-steps)
                                 store-steps)
                             (cons (arg-flags) flags)
                             (cdr tis)
                             (- i 1))))))))))

  (define (make/validate-callback typelib tld signature-offset method? container)
    (let-attributes signature-blob-fetcher
        (validated-pointer+ tld signature-offset
                            ((header-fetcher 'signature-blob-size) tld))
        (return-type n-arguments arguments may-return-null)
      (receive (arg-types prepare store flags)
               (arg-blobs-callback-values typelib
                                          tld
                                          arguments
                                          n-arguments
                                          method?
                                          container)
        (make-callback
         (stblob-type-info typelib tld return-type
                           (bool may-return-null) #f #f #f)
         arg-types
         prepare
         store
         flags
         (typelib-gtype-lookup typelib)))))

  (define (make-interface-loader typelib tld entry-ptr name gtype)
    (let-attributes header-fetcher tld (interface-blob-size field-blob-size)
      (let* ((offset ((dir-entry-fetcher 'offset) entry-ptr))
             (blob (validated-pointer+ tld offset interface-blob-size)))
        (lambda ()
          (make-gobject-class
           (typelib-namespace typelib)
           name
           gtype
           (lambda (class)
             (let-attributes interface-blob-fetcher blob
                             (blob-type
                              deprecated name
                              n-prerequisites n-properties n-methods n-signals
                              n-vfuncs n-constants)
               (unless (= blob-type 8)
                 (raise-validation-error
                  "invalid blob type for object entry" blob-type))
               (let ((prereq-size (c-type-align 'uint32 (* 2 n-prerequisites))))
                 (validated-pointer+ tld offset (+ interface-blob-size prereq-size))
                 (let* ((prereqs (pointer+ blob interface-blob-size)))
                   (make/validate-gobject-class-attributes
                    typelib tld class deprecated gtype #f
                    n-prerequisites prereqs
                    0 #f
                    (+ offset interface-blob-size prereq-size)
                    n-properties n-methods n-signals n-vfuncs n-constants))))))))))


  (define (make/validate-vfunc-caller tld index callout)
    (raise-sbank-error "vfunc calling not yet implemented"))

  (define (make-class-loader typelib tld entry-ptr name gtype)
    (let* ((offset ((dir-entry-fetcher 'offset) entry-ptr))
           (blob (validated-pointer+ tld offset
                                     ((header-fetcher 'object-blob-size) tld))))
      (lambda ()
        (make-gobject-class
         (typelib-namespace typelib)
         name
         gtype
         (lambda (class)
           (let-attributes header-fetcher tld
                           (object-blob-size field-blob-size property-blob-size
                                             function-blob-size signal-blob-size
                                             vfunc-blob-size constant-blob-size)
             (let-attributes object-blob-fetcher blob
                             (blob-type
                              deprecated name gtype-name gtype-init parent
                              n-interfaces n-fields n-properties n-methods n-signals
                              n-vfuncs n-constants)
               (unless (= blob-type 7)
                 (raise-validation-error
                  "invalid blob type for object entry" blob-type))
               (when (= deprecated 1)
                 ((typelib-deprecation-handler)
                  typelib
                  (get/validate-string tld name)))
               (let ((ifaces-size (c-type-align 'uint32 (* 2 n-interfaces)))
                     (fields-size (* field-blob-size n-fields)))
                 (validated-pointer+ tld offset
                                     (+ object-blob-size ifaces-size fields-size))
                 (let* ((ifaces (pointer+ blob object-blob-size))
                        (fields (pointer+ ifaces ifaces-size))
                        (parent (and (> parent 0)
                                     (typelib-get-entry/index typelib parent))))
                   (make/validate-gobject-class-attributes
                    typelib tld class deprecated gtype parent
                    n-interfaces ifaces
                    n-fields fields
                    (+ offset object-blob-size ifaces-size fields-size)
                    n-properties n-methods n-signals
                    n-vfuncs n-constants))))))))))


  (define (gtype-additional-constructors gtype)
    (if gtype
        (case (gtype->symbol gtype)
          ((object) (list (cons 'new*
                                (make-lazy-entry (make-gobject-new* type->gtype)))))
          (else '()))
        '()))

  (define (make/validate-gobject-class-attributes
           typelib tld class deprecated gtype parent
           n-interfaces ifaces
           n-fields fields
           prop-offset n-properties n-methods n-signals n-vfuncs n-constants)
    (define (interface-maker i)
      (typelib-get-entry/index typelib (pointer-uint16-ref ifaces (* 2 i))))
    (define (make-lazy context name proc)
      (with-validation-context context
        (cons name (make-lazy-entry (proc/validation-context proc)))))
    (define (make-lazy/offset name-offset proc)
      (let ((name (scheme-ified-symbol (get/validate-string tld name-offset))))
        (make-lazy name name proc)))
    (define (member-func-maker blob)
      (make-lazy/offset ((function-blob-fetcher 'name) blob)
                        (lambda (container)
                          (make/validate-function typelib tld blob container))))
    (define (signal-maker blob)
      (let-attributes signal-blob-fetcher blob
                        (name signature)
        (make-lazy/offset
         name
         (lambda (container)
           (make/validate-signature typelib tld signature #f container)))))
    (define (field-getter-maker blob)
      (let ((name (get/validate-string tld (field-name blob)))
            (struct-offset (field-struct-offset blob)))
        (and (= (field-readable blob) 1)
             (= (field-bits blob) 0) ;; FIXME: support bitfields
             (not (= struct-offset #x0ffff))
             (make-lazy name
                        (string->symbol
                         (string-append "get-" (scheme-ified-string name)))
                        (field-getter-method typelib
                                             tld
                                             (field-type blob)
                                             struct-offset)))))
    (define (field-setter-maker blob)
      (let ((name (get/validate-string tld (field-name blob)))
            (struct-offset (field-struct-offset blob)))
        (and (field-writable? blob)
             (make-lazy name
                        (string->symbol
                         (string-append "set-" (scheme-ified-string name)))
                        (field-setter-method typelib
                                             tld
                                             (field-type blob)
                                             struct-offset)))))
    (define (property-maker blob)
      (let-attributes property-blob-fetcher blob
                      (name
                       deprecated readable writable
                       construct construct-only type)
        (make-lazy/offset name
                          (lambda (class)
                            (make-property-info
                             (stblob-type-info typelib tld type #f #f #f #f)
                             (bool readable)
                             (bool writable)
                             (bool construct)
                             (bool construct-only))))))
    (let-attributes header-fetcher tld
                    (property-blob-size
                     function-blob-size signal-blob-size
                     field-blob-size vfunc-blob-size constant-blob-size)
      (when (= deprecated 1)
        ((typelib-deprecation-handler) typelib (gobject-class-name class)))
      (let ((properties-size (* property-blob-size n-properties))
            (methods-size (* function-blob-size n-methods))
            (vfuncs-size (* vfunc-blob-size n-vfuncs))
            (signals-size (* signal-blob-size n-signals))
            (constants-size (* constant-blob-size n-constants)))
        (validated-pointer+ tld prop-offset
                            (+ properties-size methods-size signals-size
                               vfuncs-size constants-size))
        (let* ((properties (pointer+ tld prop-offset))
               (methods (pointer+ properties properties-size))
               (signals (pointer+ methods methods-size))
               (vfuncs (pointer+ signals signals-size))
               (constants (pointer+ vfuncs vfuncs-size)))
          (receive (constructors methods)
                   (partition (lambda (m)
                                (let-attributes function-blob-fetcher m
                                                (constructor)
                                  (= constructor 1)))
                              (make-array-pointers methods n-methods
                                                   function-blob-size))
            (let ((field-blob-ptrs
                   (make-array-pointers fields n-fields field-blob-size)))
              (values parent
                      (map interface-maker (iota n-interfaces))
                      (append (gtype-additional-constructors gtype)
                              (map member-func-maker constructors))
                      (append (map member-func-maker methods)
                              (filter-map field-getter-maker field-blob-ptrs)
                              (filter-map field-setter-maker field-blob-ptrs))
                      (map signal-maker
                           (make-array-pointers signals n-signals signal-blob-size))
                      (map property-maker
                           (make-array-pointers properties
                                                n-properties
                                                property-blob-size)))))))))


  (define (field-getter-method typelib tld stblob struct-offset)
    (lambda (class)
      (let ((ti (stblob-type-info typelib tld stblob #t #f #f #f)))
        (receive (prim-type out-convert back-convert cleanup)
                 (type-info/prim-type+procs ti)
          (let ((getter (make-pointer-c-element-getter prim-type
                                                       struct-offset
                                                       #f
                                                       #f)))
            (lambda (obj)
              (let ((result (getter (ginstance-ptr obj))))
                (if back-convert
                    (back-convert result)
                    result))))))))

  (define (field-setter-method typelib tld stblob struct-offset)
    (lambda (class)
      (let ((ti (stblob-type-info typelib tld stblob #t #f #f #f)))
        (receive (prim-type out-convert back-convert cleanup)
                 (type-info/prim-type+procs ti)
          (let ((setter (make-pointer-c-element-setter prim-type
                                                       struct-offset
                                                       #f
                                                       #f)))
            (lambda (obj val)
              (setter (ginstance-ptr obj)
                      (if out-convert (out-convert val) val))))))))

  (define (make/validate-signature typelib tld signature-offset
                                   constructor? container)
    (let-attributes signature-blob-fetcher
        (validated-pointer+ tld signature-offset
                            ((header-fetcher 'signature-blob-size) tld))
        (return-type n-arguments arguments may-return-null)
      (let ((arg-blob-size ((header-fetcher 'arg-blob-size) tld)))
        (make-signature
         (proc/validation-context
          (lambda ()
            (stblob-type-info typelib tld return-type
                              (bool may-return-null) #f #f #f)))
         (proc/validation-context
          (lambda ()
            (arg-blobs-type-infos typelib tld arguments n-arguments arg-blob-size)))
         (proc/validation-context
          (lambda ()
            (make/validate-callout typelib tld signature-offset
                                   constructor? container)))
         (proc/validation-context
          (lambda ()
            (make/validate-callback typelib tld signature-offset
                                    (not constructor?) container)))))))

  (define (make/validate-enum/flags typelib tld entry-ptr entry-name
                                    gtype constructor)
    (let ((blob (validated-pointer+ tld
                                    ((dir-entry-fetcher 'offset) entry-ptr)
                                    ((header-fetcher 'enum-blob-size) tld))))
      (let-attributes enum-blob-fetcher blob
                      (blob-type gtype-init n-values values)
        (unless (or (= blob-type 5) (= blob-type 6)) ;; FIXME: need to treat flags
          (raise-validation-error "invalid blob type for enum entry" blob-type))
        (constructor
         (get/validate-gtype typelib tld gtype-init)
         (map (lambda (val-ptr)
                (let-attributes value-blob-fetcher val-ptr
                                (name value)
                  (cons (enum-symbol tld name) value)))
              (make-array-pointers values n-values
                                   ((header-fetcher 'value-blob-size) tld)))))))

  (define (make-enum-loader typelib tld entry-ptr entry-name gtype)
    (lambda ()
      (make/validate-enum/flags typelib tld entry-ptr entry-name
                                gtype make-genum)))

  (define (make-flags-loader typelib tld entry-ptr entry-name gtype)
    (lambda ()
      (make/validate-enum/flags typelib tld entry-ptr entry-name
                                gtype make-gflags)))

  (define (enum-symbol tld name-offset)
    (let ((name (get/validate-string tld name-offset)))
      (cond ((string-index name (char-set-complement char-set:digit))
             (scheme-ified-symbol name))
            (else ;; all digits
             (string->number name)))))

  (define (make-constant-loader typelib tld entry-ptr entry-name gtype)
    (lambda ()
      (let ((blob (validated-pointer+ tld
                                      ((dir-entry-fetcher 'offset) entry-ptr)
                                      ((header-fetcher 'constant-blob-size) tld))))
        (let-attributes constant-blob-fetcher blob
                        (blob-type deprecated name type size offset)
          (unless (= blob-type 9)
            (raise-validation-error
             "invalid blob type for constant entry" blob-type))
          (let* ((ti (stblob-type-info typelib tld type #f #f #f #f))
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

  (define (make-record-loader typelib tld entry-ptr entry-name gtype)
    (let-attributes header-fetcher tld (struct-blob-size field-blob-size)
      (let* ((offset ((dir-entry-fetcher 'offset) entry-ptr))
             (blob (validated-pointer+ tld offset struct-blob-size)))
        (lambda ()
          (make-gobject-record-class
           (typelib-namespace typelib)
           entry-name
           gtype
           (lambda (class)
             (let-attributes record-blob-fetcher blob
                             (blob-type deprecated unregistered name size
                                        gtype-name gtype-init
                                        n-fields n-methods)
               (let ((fields (pointer+ blob struct-blob-size))
                     (fields-size (* n-fields field-blob-size)))
                 (receive
                     (parent interfaces constructors methods signals properties)
                     (make/validate-gobject-class-attributes
                      typelib tld class deprecated gtype #f
                      0 #f
                      n-fields fields
                      (+ offset struct-blob-size fields-size)
                      0 n-methods 0 0 0)
                   (if (> size 0)
                       (values
                        (append
                         constructors
                         `((alloc . ,(make-lazy-entry (record-allocator size)))
                           (new/alist . ,(make-lazy-entry
                                          (record-alist-constructor typelib
                                                                    tld
                                                                    size
                                                                    n-fields
                                                                    fields)))))
                        (append methods `((free . ,record-free))))
                       (values constructors methods)))))))))))

  (define (record-allocator size)
    (lambda (class)
      (lambda ()
        (make-ginstance class (malloc size)))))

  (define (record-free instance)
    (free (ginstance-ptr instance)))

  (define (field-writable? field-blob)
    (and (= (field-writable field-blob) 1)
         (= (field-bits field-blob) 0)  ;; FIXME: support bitfields
         (not (= (field-struct-offset field-blob) #x0ffff))))

  (define (record-alist-constructor typelib tld size n-fields fields)
    (let-attributes header-fetcher tld (field-blob-size)
      (lambda (class)
        (define (field-name+setter blob)
          (let-attributes field-blob-fetcher blob
                          (name writable bits struct-offset type)
            (let ((field-name (get/validate-string tld name)))
              (if (field-writable? blob)
                  (values (scheme-ified-symbol field-name)
                          ;; TODO: don't instantiate the setter method twice
                          ((field-setter-method typelib tld type struct-offset) class))
                  (values #f #f)))))
        (let ((field-setters (filter-map
                              (lambda (field-blob)
                                (receive (name setter!) (field-name+setter field-blob)
                                  (and name (cons name setter!))))
                              (make-array-pointers fields n-fields field-blob-size))))
          (lambda (alist)
            (let* ((mem (malloc size))
                   (instance (make-ginstance class mem)))
              (memset mem 0 size)
              (for-each (lambda (entry)
                          (let ((setter (assq-ref field-setters (car entry))))
                            (unless setter
                              (free mem)
                              (error 'record-alist-constructor
                                     "no writable field with this name" (car entry)))
                            (setter instance (cdr entry))))
                        alist)
              instance))))))

  (define (make-union-loader typelib tld entry-ptr entry-name gtype)
    (lambda ()
      (make-gobject-union-class (typelib-namespace typelib)
                                entry-name
                                gtype
                                (lambda (class)
                                  (values '() '())))))

  (define (proc/validation-context proc)
    (let ((context (validation-context)))
      (lambda args
        (parameterize ((validation-context context))
          (apply proc args)))))

  (define (mark-deprecated typelib tld name proc)
    (let ((encountered? #f))
      (lambda args
        (cond (encountered? (apply proc args))
              (else
               (set! encountered? #t)
               ((typelib-deprecation-handler) typelib name)
               (apply proc args))))))

  (define (decorated-loader typelib name loader)
    (lambda ()
      (let ((decorator (lookup-typelib-decorator (typelib-namespace typelib)
                                                 name)))
        (if decorator (decorator typelib (loader)) (loader)))))


  (define (get/validate-dir-entry-gtype typelib tld name entry-ptr)
    (let-attributes dir-entry-fetcher entry-ptr
                    (blob-type local offset)
      (if (= local 0)
          #f
          (case blob-type
            ((7 8) ;; object, interface
             (let-attributes interface-blob-fetcher (pointer+ tld offset)
                             (gtype-init)
               (let ((init-name (get/validate-string tld gtype-init)))
                 (cond ((and (string=? "intern" init-name)
                             (string=? "GObject" (typelib-namespace typelib)))
                        (case (string->symbol name)
                          ((Object) (symbol->gtype 'object))
                          (else     #f)))
                       (else
                        (get/validate-gtype typelib tld init-name))))))
            ((3 5 11) ;; struct, enum, union
             (let-attributes registered-type-blob-fetcher (pointer+ tld offset)
                             (unregistered gtype-init)
               (get/validate-gtype typelib tld gtype-init)))
            (else
             #f)))))

  (define get/validate-gtype
    (let ((callout (make-c-callout gtype-ctype '())))
      (lambda (typelib tld name)
        (let ((name-str (if (string? name)
                            name
                            (get/validate-string tld name))))
          (and-let* ((func-ptr (typelib-dlsym typelib name-str)))
            ((callout func-ptr)))))))

  ;;
  ;; Helpers
  ;;
  (define (stblob-type-info typelib tld st-blob null-ok?
                            closure-index destroy-index scope)
    (let-attributes simple-type-blob-fetcher st-blob
                    (offset
                     flags.reserved
                     flags.reserved2
                     flags.pointer
                     flags.tag)
      (cond ((and (= flags.reserved 0) (= flags.reserved2 0))
             (when (>= flags.tag type-tag-array)
               (raise-validation-error "wrong tag in simple type" flags.tag))
             (when (and (>= flags.tag type-tag-utf8) (= flags.pointer 0))
               (raise-validation-error "pointer type expected for tag" flags.tag))
             (let ((tag-symbol (type-tag->symbol flags.tag)))
               (cond ((and (= flags.pointer 1) (eq? tag-symbol 'void))
                      (when destroy-index
                        (raise-validation-error
                         "no destroy notification expected for pointer"))
                      (make-type-info 'pointer #f null-ok? closure-index #f #f))
                     (else
                      (when (or closure-index destroy-index)
                        (raise-validation-error
                         "no closure or destroy notification expected for type"
                         tag-symbol))
                      (make-type-info tag-symbol (bool flags.pointer) null-ok?)))))
            (else
             (make/validate-type-info typelib tld offset null-ok?
                                      closure-index destroy-index scope)))))

  (define (make/validate-type-info typelib tld offset null-ok?
                                   closure-index destroy-index scope)
    (let-attributes interface-type-blob-fetcher (validated-pointer+ tld offset 4)
                    (tag)
      (let ((tag-symbol (type-tag->symbol tag)))
        (define (assert-no-closure)
          (when (or closure-index destroy-index)
            (raise-validation-error
             "no closure or destroy notification expected for tag" tag-symbol)))
        (case tag-symbol
          ((array)
           (assert-no-closure)
           (let-attributes array-type-blob-fetcher (validated-pointer+ tld offset 4)
                           (pointer
                            tag zero-terminated
                            has-length has-size
                            dimensions.length dimensions.size
                            type)
             (when (and (= has-length 1) (= has-size 1))
               (raise-validation-error "array type has both length and size"))
             (make-type-info
              (make-array-type (stblob-type-info typelib tld type #f #f #f #f)
                               (bool zero-terminated)
                               (and (= has-size 1) dimensions.size)
                               (and (= has-length 1) dimensions.length))
              #f
              null-ok?)))
          ((interface)
           (let-attributes interface-type-blob-fetcher
               (validated-pointer+ tld offset 4)
               (interface)
             (let* ((entry (typelib-get-entry/index typelib interface))
                    (pointer? (not (genum? entry))))
               (unless (signature? entry)
                 (assert-no-closure))
               (make-type-info entry pointer? null-ok?
                               closure-index destroy-index scope))))
          ((error)
           (assert-no-closure)
           (let-attributes error-type-blob-fetcher (validated-pointer+ tld offset 4)
                           (tag n-domains domains)
             (make-type-info (make-gerror-type) #t #f)))
          ((glist gslist ghash)
           (assert-no-closure)
           (let-attributes parameter-type-blob-fetcher
               (validated-pointer+ tld offset 4)
               (pointer tag n-types type)
             (make-type-info tag-symbol
                             #t
                             null-ok?
                             (map (lambda (blob)
                                    (stblob-type-info typelib tld blob
                                                      #f #f #f #f))
                                  (make-array-pointers type n-types 4)))))
          (else
           (raise-validation-error
            "non-simple type of this kind not yet implemented"
            (type-tag->symbol tag)))))))

  (define type-tag-utf8 (symbol->type-tag 'utf8))
  (define type-tag-array (symbol->type-tag 'array))

  (define (get/validate-string tld offset)
    ;; FIXME: This should actually check we don't go beyond the data
    ;; when assembling the string
    (utf8z-ptr->string (validated-pointer+ tld offset 1)))

  (define (validated-pointer+ tld offset size)
    (when (> (+ offset size) (header-size tld))
      (raise-validation-error
       "offset/size out of bounds" offset size (header-size tld)))
    (pointer+ tld offset))

  (define (make-array-pointers base n size)
    (do ((i (- n 1) (- i 1))
         (result '() (cons (pointer+ base (* i size)) result)))
        ((< i 0) result)))

  (define (bool i)
    (not (= 0 i)))

  (define (warning msg . args)
    (apply format (current-error-port) msg args)
    (newline (current-error-port)))

  (define-c-callouts libgir
    (g-ir-require% 'pointer "g_irepository_require"
                   '(pointer pointer pointer uint pointer)))

  ;; Initialize the GObject type system
  (g-type-init))


;; Local Variables:
;; scheme-indent-styles: ((let-attributes 2))
;; End
