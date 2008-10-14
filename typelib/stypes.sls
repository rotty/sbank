#!r6rs
(library (sbank typelib stypes)
  (export primitive-stypes
          stypes-adjoin
          stypes-ref
          stype-attribute
          stype-fetcher)
  (import (rnrs base)
          (rnrs control)
          (rnrs arithmetic bitwise)
          (rnrs io simple)
          (xitomatl srfi and-let*)
          (xitomatl sxml-tools sxpathlib)
          (xitomatl sxml-tools sxpath)
          (spells alist)
          (spells tracing)
          (spells foreign)
          (spells receive)
          (spells format)
          (sbank sxpath-utils))

  (define primitive-stypes
    (cons
     'types
     (map (lambda (info)
            (let ((size (cadr info)))
              `(primitive (name ,(symbol->string (car info)))
                          (size ,(cond ((eqv? size #f)
                                  (c-type-sizeof (car info)))
                                 ((symbol? size)
                                  (c-type-sizeof size))
                                 (else
                                  size)))
                          (alignment ,(c-type-alignof
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

  (define (stypes-adjoin stypes . new-types)
    (cons (car stypes) (append (cdr stypes)
                               (map (lambda (new-type)
                                          (resolve-types new-type stypes))
                                    new-types))))

  (define (stype-attribute stype name)
    (sxpath-attr stype (list name)))
  
  (define (resolve-types type types)
    (if (pair? type)
        (case (car type)
          ((type)
           (list 'type (stypes-ref types (cadr type))))
          ((record union)
           (let ((name-attrs ((sxpath '(name)) type))
                 (name (sxpath-ref type '(name))))
             (receive (components size alignment)
                 (calculate-sizes (car type)
                                  name
                                  ((sxpath '((*OR* field record union)))
                                   (cons (car type)
                                         (map (lambda (stype)
                                                (resolve-types stype types))
                                              (cdr type)))))
               (when (eqv? size #f)
                 (warning "unable to calculate size of compound ~s" name))
               (cons (car type)
                     (append name-attrs components (or (and-let* ((size size))
                                                         `((size ,size)
                                                           (alignment ,alignment)))
                                                       '()))))))
          (else
           (cons (car type) (map (lambda (t)
                                   (resolve-types t types))
                                 (cdr type)))))
        type))

  
  (define (stypes-ref stypes name)
    (and-let* ((components ((select-component name) stypes))
               ((pair? components)))
      (car components)))
  
  (define (stype-fetcher stype path)
    (define (lose msg . irritants)
      (apply error 'stype-fetcher msg irritants))
    (cond ((string? path)
           (construct-stype-fetcher (stype-ref stype path) lose))
          ((pair? path)
           (let ((fetchers (fetcher-chain stype path)))
             (lambda (pointer)
               (let loop ((pointer pointer) (fetchers fetchers))
                 (if (null? fetchers)
                     pointer
                     (loop ((car fetchers) pointer) (cdr fetchers)))))))
          (else
           (lose "invalid path" path))))

  
  (define (make-primitive-fetcher offset type-sym bit-offset bits lose)
    (let ((ptr-ref (make-pointer-ref type-sym)))
      (cond ((and bits bit-offset)
             (let ((end-offset (+ bit-offset bits)))
               (lambda (pointer)
                 (let ((val (ptr-ref pointer offset)))
                   (bitwise-bit-field val bit-offset end-offset)))))
            ((not (or bits bit-offset))
             (lambda (pointer) (ptr-ref pointer offset)))
            (else
             (lose "either both or none of 'bits' and 'bit-offset' must be specified")))))
  
  (define (construct-stype-fetcher component lose)
    (let ((offset (sxpath-attr component '(offset)))
          (bits (sxpath-attr component '(bits)))
          (bit-offset (sxpath-attr component '(bit-offset)))
          (type (sxpath-attr component '(type))))
      (case (car type)
        ((primitive)
         (make-primitive-fetcher offset
                                 (string->symbol (stype-attribute type 'name))
                                 bit-offset
                                 bits
                                 lose))
        ((record union)
         (unless (not (or bits bit-offset))
           (lose "'bits' or 'bit-offset' specified for composite member" component))
         (lambda (pointer)
           (integer->pointer (+ (pointer->integer pointer) offset)))))))
  
  (define (fetcher-chain stype path)
    (let loop ((fetchers '()) (stype stype) (path path))
      (if (null? path)
          (reverse fetchers)
          (let ((component ((select-component (car path)) stype)))
            (unless component
              (error 'fetcher-chain "cannot find component in stype" stype path))
            (loop (cons (construct-stype-fetcher component) fetchers)
                  component
                  (cdr path))))))

  (define (stype-ref stype name)
    (let ((components ((select-component name) stype))
          (anonymous ((select-kids (lambda (node)
                                     (and
                                      (null? ((select-kids (ntype?? 'name)) node))
                                      (pair? ((select-kids (ntype?? 'size)) node)))))
                      stype)))
      (if (null? components)
          (and (pair? anonymous)
               (stype-ref (car anonymous) name))
          (car components))))
  
  (define (select-component name)
    (select-kids (lambda (node)
                   (let ((names ((select-kids (ntype?? 'name)) node)))
                     (equal? names `((name ,name)))))))

  (define (warning msg . args)
    (apply format (current-error-port) msg args)
    (newline (current-error-port)))

  ;; This is currently coded towards x86-64, but others are likely
  ;; similiar
  (define (calculate-sizes tag name components)

    (case tag
      ((union) (calculate tag name components
                          (lambda (size comp-align)
                            `((offset 0)))
                          (lambda (size comp-size comp-align)
                            (and size comp-size (max size comp-size)))))
      ((record) (calculate tag name components
                           (lambda (size comp-align)
                            (if (and size comp-align)
                                `((offset ,(align size comp-align)))
                                '()))
                           (lambda (size comp-size comp-align)
                             (+ (align size comp-align) comp-size))))
      (else
       (error 'calculate-size "invalid type tag" tag))))

  (define (calculate tag name components offset updated-size)

    (define (component-size c)
      (and (not (sxpath-attr c '(bits)))
           (or (sxpath-attr c '(size))
               (sxpath-attr c '(type * size)))))
    
    (define (component-alignment c)
      (or (sxpath-attr c '(alignment))
          (sxpath-attr c '(type * alignment))))
    
    (define (extend sxml . attrs)
      (cons (car sxml) (append (cdr sxml) attrs)))

    (let loop ((result '())
               (size 0) (max-align 0) (bwcomps '()) (bwlist '())
               (components components))
      (define (result-with-bitfields)
        (let loop ((result result) (bit-offset 0) (comps (reverse bwcomps)) (bws (reverse bwlist)))
          (if (null? comps)
              result
              (loop (cons (extend (car comps) `(bit-offset ,bit-offset))
                          result)
                    (+ bit-offset (car bws))
                    (cdr comps)
                    (cdr bws)))))
      (if (null? components)
          (let ((max-align (and max-align
                                (max max-align (if (null? bwlist)
                                                   0
                                                   (component-alignment (car bwcomps))))))
                (size (and size
                           (+ size (if (null? bwlist)
                                       0
                                       (sxpath-attr (car bwcomps) '(type * size)))))))
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
            
            (cond ((and size comp-size comp-align)
                   (loop (cons (apply extend (car components) (offset size comp-align))
                               (result-with-bitfields))
                         (updated-size size comp-size comp-align)
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
                                      (offset size comp-align))
                               bwcomps)
                         (cons comp-bits bwlist)
                         (cdr components)))
                  (else
                   (unless (and comp-size comp-align)
                     (warning "size or alignment of component ~a in ~a not known"
                              (sxpath-attr (car components) '(name))
                              (list tag name)))
                   (loop (cons (car components)
                               (result-with-bitfields))
                         #f #f '() '()
                         (cdr components))))))))
  
  (define (align n alignment)
    (+ n (mod (- alignment (mod n alignment)) alignment)))
  
  (define (bitfields-fit-inside? bwlist size)
    (>= (* size 8)
        (apply + bwlist))))