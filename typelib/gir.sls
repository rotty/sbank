#!r6rs
(library (sbank typelib gir)
  (export gir-xml->stypes)

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
          (sbank typelib stypes)
          (sbank sxpath-utils))

  (define (gir-xml->stypes port)
    (let ((types primitive-stypes))

      (define (compound-maker tag)
        (lambda sxml
          (let ((name-attrs ((sxpath '(^ name)) sxml)))
            )))
      
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
                                        (size ,(c-type-sizeof 'pointer))
                                        (alignment ,(c-type-alignof 'pointer))))))
         (core:type *PREORDER* . ,(type-resolver types))
         (^ *PREORDER* . ,list)))
      types))


  (define (type-resolver types)
    (lambda type
      (let* ((name (sxpath-attr type '(^ name)))
             (ctype (sxpath-attr type '(^ c:type)))
             (pointer-depth (cond ((not ctype) 0)
                                  ((string-skip-right ctype #\*)
                                   => (lambda (i)
                                        (- (string-length ctype) (+ i 1))))
                                  (else 0)))
             (base-type 
              (cond ((stypes-ref types name) => values)
                    ((> pointer-depth 0) #f)
                    (else
                     (error 'gir-xml->stypes "type not found" name)))))
        (if (> pointer-depth 0)
            `(type (pointer (base-type ,base-type)
                            (depth ,pointer-depth)
                            (size ,(c-type-sizeof 'pointer))
                            (alignment ,(c-type-alignof 'pointer))))
            `(type ,base-type))))))