#!r6rs
(library (sbank typelib gir)
  (export gir-xml->stype-list)

  (import (rnrs base)
          (rnrs control)
          (rnrs lists)
          (rnrs hashtables)
          (rnrs io ports)
          (rnrs records syntactic)
          (spells receive)
          (spells alist)
          (spells tracing)
          (spells foreign)
          (only (spells strings) string-skip-right)
          (sxml ssax)
          (sxml transform)
          (xitomatl sxml-tools sxpath)
          (xitomatl sxml-tools sxpathlib)
          (xitomatl sxml-tools sxpath-ext)
          (xitomatl srfi and-let*)
          (sbank sxpath-utils))

  (define (gir-xml->stype-list port)

    (define (compound-maker tag)
      (trace-lambda compound sxml
        (cons tag (append ((sxpath '(^ name)) sxml)
                          ((sxpath '((*OR* field record union))) sxml)))))
      
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
                                       ,((sxpath '(type)) array))))))
       (core:type *PREORDER* . ,type-pointifier)
       (^ *PREORDER* . ,list))))


  (define (type-pointifier . type)
    (let* ((name (sxpath-attr type '(^ name)))
           (ctype (sxpath-attr type '(^ c:type)))
           (pointer-depth (cond ((not ctype) 0)
                                ((string-skip-right ctype #\*)
                                 => (lambda (i)
                                      (- (string-length ctype) (+ i 1))))
                                (else 0))))
      (if (> pointer-depth 0)
          `(type (pointer (base-type (type ,name))
                          (depth ,pointer-depth)
                          (size ,(c-type-sizeof 'pointer))
                          (alignment ,(c-type-alignof 'pointer))))
          `(type ,name)))))