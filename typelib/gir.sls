#!r6rs
(library (sbank typelib gir)
  (export gir-xml->stypes)

  (import (rnrs base)
          (rnrs control)
          (rnrs lists)
          (rnrs hashtables)
          (rnrs io ports)
          (rnrs io simple)
          (rnrs records syntactic)
          (spells receive)
          (spells alist)
          (spells format)
          (spells tracing)
          (only (spells strings) string-skip-right)
          (sxml ssax)
          (sxml transform)
          (xitomatl sxml-tools sxpath)
          (xitomatl sxml-tools sxpathlib)
          (xitomatl sxml-tools sxpath-ext)
          (xitomatl srfi and-let*)
          (sbank config ctype-sizes))

  (define (gir-xml->stypes port)
    (let ((types primitive-types))

      (define (compound-maker tag)
        (lambda sxml
          (let ((name-attrs ((sxpath '(^ name)) sxml)))
            (receive (components size alignment)
                (calculate-sizes tag name-attrs ((sxpath '((*OR* field record union))) sxml))
              (when (eqv? size #f)
                (warning "unable to calculate size of compound ~s~%" name-attrs))
              (let ((compound (cons tag (append name-attrs
                                                components
                                                (or (and-let* ((size size))
                                                      `((size ,size)
                                                        (alignment ,alignment)))
                                                    '())))))
                (set! types (append types (list compound)))
                compound)))))
      
      (define (select-type name)
        ((node-join (select-kids
                     (lambda (node)
                       (let ((names ((select-kids (ntype?? 'name)) node)))
                         (equal? names `((name ,name)))))))
         types))

      (pre-post-order
       (ssax:xml->sxml port '((core . "http://www.gtk.org/introspection/core/1.0")
                              (c . "http://www.gtk.org/introspection/c/1.0")))
       `((*TOP* *MACRO* . ,(lambda top
                             ((sxpath '(// core:namespace (*OR* core:record core:union))) top)))
         (core:record . ,(compound-maker 'record))
         (core:union . ,(compound-maker 'union))
         (core:field . ,(lambda field field
                          (cons* 'field
                                 (sxpath-ref field '(^ name))
                                 (sxpath-ref field '(type))
                                 (or
                                  (and-let* ((bits (sxpath-attr field '(^ bits))))
                                    (list (list 'bits (string->number bits))))
                                  '()))))
         (core:array . ,(lambda array 
                          `(type (array (element-type
                                         ,(sxpath-ref array '(type)))
                                        (size ,(ctype-sizeof 'pointer))))))
         (core:type *PREORDER* . ,(type-resolver select-type))
         (^ *PREORDER* . ,list)))))


  (define (type-resolver select-type)
    (lambda type
      (let* ((name (sxpath-attr type '(^ name)))
             (ctype (sxpath-attr type '(^ c:type)))
             (pointer-depth (cond ((not ctype) 0)
                                  ((string-skip-right ctype #\*)
                                   => (lambda (i)
                                        (- (string-length ctype) (+ i 1))))
                                  (else 0)))
             (base-type 
              (cond ((and-let* ((types (select-type name))
                                ((pair? types)))
                       (car types)) => values)
                    ((> pointer-depth 0) #f)
                    (else
                     (error 'gir-xml->stypes "type not found" name)))))
        (if (> pointer-depth 0)
            `(type (pointer (base-type ,base-type)
                            (depth ,pointer-depth)
                            (size ,(ctype-sizeof 'pointer))
                            (alignment ,(ctype-alignof 'pointer))))
            `(type ,base-type)))))
  
  (define primitive-types
    (cons
     'types
     (map (trace-lambda info (info)
            (let ((size (cadr info)))
              `(primitive (name ,(symbol->string (car info)))
                          (size ,(cond ((eqv? size #f)
                                  (ctype-sizeof (car info)))
                                 ((symbol? size)
                                  (ctype-sizeof size))
                                 (else
                                  size)))
                          (alignment ,(ctype-alignof
                                       (if (symbol? size) size (car info)))))))
          `((char 1)    (uchar 1)
            (int8 1)    (uint8 1)
            (short #f)  (ushort #f)
            (int16 2)   (uint16 2)
            (int #f)    (uint #f)
            (int32 4)   (uint32 4)
            (long #f)   (ulong #f)
            (int64 8)   (uint64 8)
            (llong #f)  (ullong #f)

            (size_t #f)

            (boolean int)))))

  ;; This is currently coded towards x86-64, but others are likely
  ;; similiar
  (trace-define (calculate-sizes tag name components)

    (trace-define (component-size c)
      (and (not (sxpath-attr c '(bits)))
           (or (sxpath-attr c '(size))
               (sxpath-attr c '(type * size)))))
    
    (trace-define (component-alignment c)
      (or (sxpath-attr c '(alignment))
          (sxpath-attr c '(type * alignment))))
    
    (define (extend sxml . attrs)
      (cons (car sxml) (append (cdr sxml) attrs)))

    (case tag
      ((union) (values components #f #f))
      ((record)
       (let loop ((result '())
                  (size 0) (max-align 0) (bwcomps '()) (bwlist '())
                  (components components))
         (trace-define (result-with-bitfields)
           (let loop ((result result) (bit-offset 0) (comps bwcomps) (bws bwlist))
             (if (null? comps)
                 result
                 (loop (cons (extend (car comps) `(bit-offset ,bit-offset))
                             result)
                       (+ bit-offset (car bws))
                       (cdr comps)
                       (cdr bws)))))
         (write (list 'loop size max-align bwcomps bwlist (length components))) (newline)
         (if (null? components)
             (let ((max-align (and max-align
                                   (max max-align (if (null? bwlist)
                                                      0
                                                      (component-alignment (car bwcomps)))))))
               (values (reverse (result-with-bitfields))
                       (and size max-align (align size max-align))
                       max-align))
             (let ((comp-size (component-size (car components)))
                   (comp-align (component-alignment (car components)))
                   (comp-bits (sxpath-attr (car components) '(bits))))
               (define (alignment)
                 (let ((bitfields-align (if (null? bwcomps)
                                            0
                                            (sxpath-attr (car bwcomps) '(type * alignment)))))
                   (max max-align comp-align bitfields-align)))
               (trace-define (offset)
                 (if (and size comp-align)
                     `((offset ,(align size comp-align)))
                     '()))
               (write (list 'comp comp-size comp-align comp-bits)) (newline)
               (cond ((and size comp-size comp-align)
                      (loop (cons (apply extend (car components) (offset))
                                  (result-with-bitfields))
                            (+ (align size comp-align) comp-size)
                            (alignment)
                            '() '()
                            (cdr components)))
                     ((and comp-bits
                           (or (null? bwlist)
                               (and-let* ((bwtype (sxpath-attr (car bwcomps) '(type)))
                                          ((eq? bwtype (sxpath-attr (car components) '(type)))))
                                 (bitfields-fit-inside? bwlist (sxpath-attr bwtype '(size))))))
                      (loop result size max-align
                            (cons (apply extend
                                         (car components)
                                         (offset))
                                  bwcomps)
                            (cons comp-bits bwlist)
                            (cdr components)))
                     (else
                      (unless (and comp-size comp-align)
                        (warning "size or alignment of component ~a in ~a not known~%"
                                 (sxpath-attr (car components) '(name))
                                 (list tag name)))
                      (loop (cons (car components)
                                  (result-with-bitfields))
                            #f #f '() '()
                            (cdr components))))))))
      (else
       (error 'calculate-size "invalid type tag" tag))))
  
  (trace-define (align n alignment)
    (+ n (mod (- alignment (mod n alignment)) alignment)))
  
  (trace-define (bitfields-fit-inside? bwlist size)
    (>= (* size 8)
        (apply + bwlist)))

  (define (sxpath-ref sxml path)
    (and-let* ((result ((sxpath path) sxml))
               ((pair? result)))
      (car result)))
  
  (define (sxpath-attr sxml path)
    (and-let* ((result ((sxpath path) sxml))
               ((pair? result)))
      (cadar result)))

  (define (warning msg . args)
    (apply format (current-error-port) msg args))
)