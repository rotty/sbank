;;; stypes.sls --- SXML-compatible representation of C types.

;; Copyright (C) 2008-2010 Andreas Rottmann <a.rottmann@gmx.at>

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

(library (sbank support stypes)
  (export primitive-stypes
          stypes-adjoin
          stypes-ref
          stype-attribute
          stype-fetcher
          stype-setter
          stype-attribute-definer
          stype-accessor-definer
          stype-fetcher-factory-definer)
  (import (for (rnrs base) run expand (meta -1))
          (rnrs control)
          (for (rnrs lists) run expand (meta -1))
          (rnrs syntax-case)
          (rnrs io simple)
          (only (srfi :1 lists) append-map filter-map split-at)
          (srfi :2 and-let*)
          (only (srfi :13 strings) string-map)
          (wak sxml-tools sxpathlib)
          (wak sxml-tools sxpath)
          (spells alist)
          (spells tracing)
          (for (spells foreign) run expand (meta -1))
          (srfi :8 receive)
          (spells misc)
          (for (spells define-values) run expand (meta -1))
          (for (sbank support utils) run expand (meta -1))
          (sbank support sxpath-utils))

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
            
            (pointer #f)
            (size_t #f)

            (boolean int)))))

  ;; Returns an adjecency list for the types in @1, disregarding any
  ;; references to types not found in @1.
  (define (stypes-graph stypes)
    (let ((type-names (map cadr ((sxpath '((*OR* record union alias) name))
                                 stypes))))
      (map (lambda (stype)
             (cons (stype-attribute stype 'name)
                   (filter-map (lambda (type)
                                 (let ((name (cadr type)))
                                   (and (member name type-names)
                                        name)))
                               ((sxpath '(// type)) stype))))
           (cdr stypes))))


  (define (stypes-topo-sort stypes)
    (map (lambda (name)
           (stypes-ref stypes name #f))
         (reverse (topological-sort (stypes-graph stypes) string=?))))

  (define (stypes-adjoin stypes . new-types)
    (let loop ((result stypes) (types (stypes-topo-sort (cons 'types new-types))))
      (if (null? types)
          result
          (loop (cons (car result)
                      (append (cdr result)
                              (list (resolve-types (car types) result))))
                (cdr types)))))

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
                     (list 'type
                           (append (resolve-types (cadr type) types)
                                   pointer-attrs)))
                    ((array)
                     (list 'type (array-resolver (cadr type) types)))
                    ((record union)
                     (list 'type (compound-resolver (cadr type) types)))
                    (else
                     type)))
                 (error 'resolve-types "cannot resolve" type)))
          ((record union)
           (compound-resolver type types))
          ((alias)
           type)
          (else
           (cons (car type) (map (lambda (t)
                                   (resolve-types t types))
                                 (cdr type)))))
        type))



  (define (type-attr type name)
    (sxpath-attr type `(type * ,name)))

  (define (array-resolver type types)
    (let* ((element-type (sxpath-ref type '(element-type)))
           (et-resolved `(element-type ,(resolve-types (cadr element-type) types)))
           (resolved (cons 'array (map (lambda (x)
                                         (cond ((and (pair? x)
                                                     (eq? (car x) 'element-type))
                                                et-resolved)
                                               (else x)))
                                        (cdr type))))
           (size (and-let* ((element-count (sxpath-attr type '(element-count)))
                            (element-size (type-attr et-resolved 'size)))
                   `((size ,(* element-size element-count)))))
           (alignment
            (and-let* ((alignment (type-attr et-resolved 'alignment)))
              `((alignment ,alignment)))))

      (cond
       ((and size alignment)
        (append resolved size alignment))
       (else
        (warning "array with unknown element size/alignment encountered: ~s" type)
        resolved))))


  (define (compound-resolver type types)
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

  (define stypes-ref
    (case-lambda
      ((stypes name resolve-aliases?)
       (and-let* ((types ((select-component name) stypes))
                  ((pair? types))
                  (type (car types)))
         (if (and resolve-aliases? (eq? (car type) 'alias))
             (stypes-ref stypes (sxpath-attr type '(target)) resolve-aliases?)
             type)))
      ((stypes name)
       (stypes-ref stypes name #t))))

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

  (define (stype-setter stype path)
    (define (lose msg . irritants)
      (apply error 'stype-fetcher msg irritants))
    (cond ((string? path)
           (construct-stype-setter (stype-ref stype path)))
          ((pair? path)
           (receive (path last) (split-at path (- (length path) 1))
             (let ((fetchers (fetcher-chain stype path))
                   (setter (construct-stype-setter (stype-ref (car last)))))
               (lambda (pointer val)
                 (let loop ((pointer pointer) (fetchers fetchers))
                   (if (null? fetchers)
                       (setter pointer val)
                       (loop ((car fetchers) pointer) (cdr fetchers))))))))
          (else
           (lose "invalid path" path))))

  (define pointer-attrs `((size ,(c-type-sizeof 'pointer))
                          (alignment ,(c-type-alignof 'pointer))))

  (define (construct-stype-fetcher component)
    (call-with-values (lambda () (stype-compound-element-fetcher-values component))
      make-pointer-c-element-getter))

  (define (construct-stype-setter component)
    (call-with-values (lambda () (stype-compound-element-fetcher-values component))
      make-pointer-c-element-setter))

  (define (stype-compound-element-fetcher-values component)
    (let ((offset (sxpath-attr component '(offset)))
          (bits (sxpath-attr component '(bits)))
          (bit-offset (sxpath-attr component '(bit-offset)))
          (type (sxpath-attr component '(type))))
      (if type
          (case (car type)
            ((primitive)
             (values (string->symbol (stype-attribute type 'name))
                     offset
                     bit-offset
                     bits))
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
             (error 'stype-compound-element-fetcher-values
                    "invalid component type" component)))
          ;; the component itself is a type (record or union
          (values (car component) offset #f #f))))

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
          (anonymous ((select-kids
                       (lambda (node)
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

  (define (warning . args)
    (for-each (lambda (thing)
                (display thing (current-error-port)))
              (cons "WARNING (sbank support stypes): " args))
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
        (let loop ((result result)
                   (bit-offset 0)
                   (comps (reverse bwcomps))
                   (bws (reverse bwlist)))
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
          (let ((max-align
                 (and max-align
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
                     (loop (cons (apply extend
                                        (car components)
                                        (offset cur-offset comp-align))
                                 (result-with-bitfields))
                           (updated-size cur-offset comp-size comp-align)
                           (alignment)
                           '() '()
                           (cdr components))))
                  ((and comp-bits
                        (or (null? bwlist)
                            (and-let*
                                ((bwtype (sxpath-attr (car bwcomps) '(type)))
                                 ((eq? bwtype
                                       (sxpath-attr (car components) '(type)))))
                              (bitfields-fit-inside?
                               bwlist (sxpath-attr bwtype '(size))))))
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
    (if (= n 0)
        0
        (+ n (mod (- alignment (mod n alignment)) alignment))))

  (define (bitfields-fit-inside? bwlist size)
    (>= (* size 8)
        (apply + bwlist)))

  (define (stype-accessor-definer types)
    (lambda (stx)
      (syntax-case stx ()
        ((k <type-name> <field-spec> ...)
         (let* ((stype (stypes-ref types (syntax->datum #'<type-name>)))
                (specs (map syntax->datum #'(<field-spec> ...)))
                (arg-vals (map (lambda (spec)
                                 (call-with-values
                                     (lambda ()
                                       (stype-compound-element-fetcher-values
                                        (stype-ref stype (if (= (length spec) 3)
                                                             (caddr spec)
                                                             (cadr spec)))))
                                   list))
                               specs)))
           (with-syntax (((all-args ...)
                          (map (lambda (vals) (datum->syntax #'k vals)) arg-vals))
                         ((setter-args ...)
                          (filter-map (lambda (vals spec)
                                        (and (= (length spec) 3)
                                             (datum->syntax #'k vals)))
                                      arg-vals specs))
                         ((fetcher-name ...)
                          (map (lambda (spec)
                                 (datum->syntax #'k (car spec)))
                               specs))
                         ((setter-name ...)
                          (filter-map (lambda (spec)
                                        (and (= (length spec) 3)
                                             (datum->syntax #'k (cadr spec))))
                                      specs)))
             #'(define-values (fetcher-name ... setter-name ...)
                 (apply
                  values
                  (append
                   (map-apply make-pointer-c-element-getter
                              '(all-args ...))
                   (map-apply make-pointer-c-element-setter
                              '(setter-args ...)))))))))))

  (define (stype-fetcher-factory-definer types)
    (lambda (stx)
      (syntax-case stx ()
        ((k <name> <type-name>)
         (with-syntax (((field ...)
                        (datum->syntax
                         #'k
                         (append-map
                          (lambda (comp)
                            (component-fetcher-alist comp 0))
                          (cdr (stypes-ref types (syntax->datum #'<type-name>)))))))
           #'(define <name>
               (let ((fields '(field ...)))
                 (lambda (sym)
                   (apply make-pointer-c-element-getter
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
    (define (nested-comps-alist comp offset)
      (append-map (lambda (nested-comp)
                    (component-fetcher-alist nested-comp offset))
                  (cdr comp)))
    (case (car comp)
      ((field record array union)
       (let ((name (sxpath-attr comp '(name)))
             (offset (sxpath-attr comp '(offset))))
         (cond
          (name
           (let ((comp-name (scheme-ified-symbol name)))
             (receive (type offset bit-offset bit-size)
                      (stype-compound-element-fetcher-values comp)
               (cons*
                (list comp-name type (+ base-offset offset) bit-offset bit-size)
                (map (lambda (elt)
                       (cons (symbol-append comp-name "." (car elt))
                             (cdr elt)))
                     (nested-comps-alist comp offset))))))
          (else
           (nested-comps-alist comp offset)))))
      (else '()))))
