;;; stypes.sls --- SXML-compatible representation of C types.

;; Copyright (C) 2008 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library provides functions to create and access a data
;; structure called "stypes", which is SXML-compatible and contains
;; information about C types, such as size and alignment.

;; Using `stypes-adjoin', one can add new compound types (records, aka
;; structs) and unions, which will have their field offset, size, and
;; alignment information calculated when added.

;; Via the `stype-fetcher` procedure and the `stype-accessor-definer`
;; and `stype-fetcher-factory-definer' syntax-expander procedures, one
;; can access the fields of compound types given a pointer to an
;; instance of the compound type.

;;; Code:
#!r6rs

(library (sbank stypes)
  (export primitive-stypes
          stypes-adjoin
          stypes-ref
          stype-attribute
          stype-fetcher
          stype-attribute-definer
          stype-accessor-definer
          stype-fetcher-factory-definer)
  (import (rnrs base)
          (rnrs control)
          (rnrs lists)
          (rnrs syntax-case)
          (rnrs io simple)
          (xitomatl srfi and-let*)
          (xitomatl sxml-tools sxpathlib)
          (xitomatl sxml-tools sxpath)
          (only (spells lists) append-map)
          (only (spells strings) string-map)
          (spells alist)
          (spells tracing)
          (spells foreign)
          (spells receive)
          (spells format)
          (for (spells define-values) run expand)
          (sbank utils)
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
            (float #f)  (double #f)

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
           (cond ((string? (cadr type))
                  (list 'type (stypes-ref types (cadr type))))
                 ((pair? (cadr type))
                  (case (caadr type)
                    ((pointer)
                     (list 'type (append (resolve-types (cadr type) types) pointer-attrs)))
                    ((array)
                     (array-resolver type types))
                    (else
                     type)))
                 (error 'resolve-types "cannot resolve" type)))
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
          ((alias)
           (let ((name (cadr type))
                 (aliased-type (stypes-ref types (caddr type))))
             (cons (car aliased-type)
                   (append `((name ,name)) (filter (lambda (comp)
                                                     (not (eq? (car comp) 'name)))
                                                   (cdr aliased-type))))))
          (else
           (cons (car type) (map (lambda (t)
                                   (resolve-types t types))
                                 (cdr type)))))
        type))



  (define (array-resolver type types)
    (let* ((resolved (resolve-types (cadr type) types))
           (size (and-let* ((element-count (sxpath-attr resolved '(element-count)))
                            (element-size (sxpath-attr resolved
                                                       '(element-type type * size))))
                   `((size ,(* element-size element-count)))))
           (alignment
            (and-let* ((alignment
                        (sxpath-attr resolved '(element-type type * alignment))))
              `((alignment ,alignment))))) 

      (if (and size alignment)
          (list 'type (append resolved size alignment))
          (list 'type (append resolved pointer-attrs)))))
                       
  (define (stypes-ref stypes name)
    (and-let* ((components ((select-component name) stypes))
               ((pair? components)))
      (car components)))
  
  (define (stype-fetcher stype path)
    (define (lose msg . irritants)
      (apply error 'stype-fetcher msg irritants))
    (cond ((string? path)
           (construct-stype-fetcher (stype-ref stype path)))
          ((pair? path)
           (let ((fetchers (fetcher-chain stype path)))
             (lambda (pointer)
               (let loop ((pointer pointer) (fetchers fetchers))
                 (if (null? fetchers)
                     pointer
                     (loop ((car fetchers) pointer) (cdr fetchers)))))))
          (else
           (lose "invalid path" path))))

  (define pointer-attrs `((size ,(c-type-sizeof 'pointer))
                          (alignment ,(c-type-alignof 'pointer))))
  
  (define (construct-stype-fetcher component)
    (call-with-values (lambda () (stype-compound-element-fetcher-values component))
      c-compound-element-fetcher))
  
  (define (stype-compound-element-fetcher-values component)
    (let ((offset (sxpath-attr component '(offset)))
          (bits (sxpath-attr component '(bits)))
          (bit-offset (sxpath-attr component '(bit-offset)))
          (type (sxpath-attr component '(type))))
      (case (car type)
        ((primitive)
         (values (string->symbol (stype-attribute type 'name)) offset bit-offset bits))
        ((record union)
         (values (car type) offset #f #f))
        ((array)
         (let ((element-count (sxpath-attr type '(element-count))))
           (if (not element-count)
               (values 'pointer offset #f #f)
               (values 'array offset #f #f))))
        ((pointer)
         (values 'pointer offset #f #f))
        (else
         (error 'stype-compound-element-fetcher-values "invalid component type" component)))))

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
      (define (bitfield-size)
         (if (null? bwlist) 0 (sxpath-attr (car bwcomps) '(type * size))))
      (define (bitfield-align)
        (if (null? bwcomps) 0 (sxpath-attr (car bwcomps) '(type * alignment))))
      (define (updated-size/bitfield size)
        (if (= (bitfield-size) 0)
            size
            (updated-size size (bitfield-size) (bitfield-align))))
      (if (null? components)
          (let ((max-align (and max-align
                                (max max-align (if (null? bwlist)
                                                   0
                                                   (component-alignment (car bwcomps))))))
                (size (and size (updated-size/bitfield size))))
            (values (reverse (result-with-bitfields))
                    (and size max-align (align size max-align))
                    max-align))
          (let ((comp-size (component-size (car components)))
                (comp-align (component-alignment (car components)))
                (comp-bits (sxpath-attr (car components) '(bits))))
            (define (alignment)
              (max max-align comp-align (bitfield-align)))
            (cond ((and size comp-size comp-align)
                   (let ((cur-offset (updated-size/bitfield size)))
                     (loop (cons (apply extend (car components) (offset cur-offset comp-align))
                                 (result-with-bitfields))
                           (updated-size cur-offset comp-size comp-align)
                           (alignment)
                           '() '()
                           (cdr components))))
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
        (apply + bwlist)))

  (define (stype-accessor-definer types)
    (lambda (stx)
      (syntax-case stx ()
        ((k <type-name> (<fetcher-name> <field-name>) ...)
         (let ((stype (stypes-ref types (syntax->datum #'<type-name>))))
           (with-syntax (((args ...)
                          (map (lambda (field-name)
                                 (datum->syntax
                                  #'k
                                  (call-with-values
                                      (lambda ()
                                        (stype-compound-element-fetcher-values
                                         (stype-ref stype field-name)))
                                    list)))
                               (map syntax->datum #'(<field-name> ...)))))
             #`(define-values (<fetcher-name> ...)
                 (values (apply c-compound-element-fetcher 'args) ...))))))))

  (define (stype-fetcher-factory-definer types)
    (lambda (stx)
      (syntax-case stx ()
        ((k <name> <type-name>)
         (with-syntax (((field ...)
                        (append-map (lambda (comp)
                                      (component-fetcher-alist comp 0))
                                    (cdr (stypes-ref types (syntax->datum #'<type-name>))))))
           #'(define <name>
               (let ((fields '(field ...)))
                 (lambda (sym)
                   (apply c-compound-element-fetcher
                          (cond ((assq sym fields) => cdr)
                                (else (error '<name>
                                             "no such field" sym fields))))))))))))

  (define (stype-attribute-definer types)
    (lambda (stx)
      (syntax-case stx ()
        ((k <type-name> (<name> <attribute-name>) ...)
         (let ((stype (stypes-ref types (syntax->datum #'<type-name>))))
           (with-syntax (((attribute-value ...)
                          (map (lambda (attr-name)
                                 (datum->syntax
                                  #'k
                                  (stype-attribute stype attr-name)))
                               (map syntax->datum #'(<attribute-name> ...)))))
             #`(define-values (<name> ...)
                 (values attribute-value ...))))))))
  
  (define (component-fetcher-alist comp base-offset)
    (case (car comp)
      ((field record array union)
       (let ((name (sxpath-attr comp '(name)))
             (offset (sxpath-attr comp '(offset))))
         (cond (name
                (list
                 (datum->syntax
                  #'k
                  (cons (scheme-ified-symbol name)
                        (receive (type offset bit-offset bit-size)
                            (stype-compound-element-fetcher-values comp)
                          (list type (+ base-offset offset) bit-offset bit-size))))))
               (else (append-map (lambda (nested-comp)
                                   (component-fetcher-alist nested-comp offset))
                                 (cdr comp))))))
      (else '()))))

