;;; gir.sls --- Parse gobject-introspection XML .gir files.

;; Copyright (C) 2008-2010 Andreas Rottmann <a.rottmann@gmx.at>

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

;;; Code:
#!r6rs

(library (sbank typelib gir)
  (export gir-xml->stype-list)

  (import (rnrs base)
          (rnrs control)
          (rnrs lists)
          (rnrs hashtables)
          (rnrs io ports)
          (rnrs records syntactic)
          (srfi :8 receive)
          (spells alist)
          (spells tracing)
          (spells foreign)
          (only (srfi :13 strings) string-skip-right)
          (wak ssax parsing)
          (wak ssax tree-trans)
          (wak sxml-tools sxpath)
          (wak sxml-tools sxpathlib)
          (wak sxml-tools sxpath-ext)
          (srfi :2 and-let*)
          (sbank support sxpath-utils))

  (define (gir-xml->stype-list port)

    (define (compound-maker tag)
      (lambda sxml
        (cons tag (append ((sxpath '(^ name)) sxml)
                          ((sxpath '((*OR* field record union))) sxml)))))
      
    (pre-post-order
     (ssax:xml->sxml port '((core . "http://www.gtk.org/introspection/core/1.0")
                            (c . "http://www.gtk.org/introspection/c/1.0")))
     `((*TOP* *MACRO*
              . ,(lambda top
                   ((sxpath '(// core:namespace (*OR* core:record
                                                      core:union
                                                      core:callback
                                                      core:alias
                                                      core:bitfield)))
                    top)))
       (core:record ,field-rules . ,(compound-maker 'record))
       (core:union ,field-rules . ,(compound-maker 'union))
       (core:callback *PREORDER* . ,(lambda sxml
                                      `(alias ,(sxpath-ref sxml '(^ name))
                                              (target "pointer"))))
       (core:bitfield *PREORDER* . ,(lambda sxml
                                      `(alias ,(sxpath-ref sxml '(^ name))
                                              (target "uint"))))
       (core:alias *PREORDER* . ,(lambda sxml
                                   `(alias ,(sxpath-ref sxml '(^ name ))
                                           ,(sxpath-ref sxml '(^ target)))))
       (^ *PREORDER* . ,list))))

  (define (ignore . args)
    #f)
  
  (define type-rules
    `((core:array
        . ,(lambda array
             (let ((element-count (cond ((sxpath-attr array '(^ fixed-size))
                                         => (lambda (size)
                                              (string->number size)))
                                        (else 0))))
               `(type (array (element-type ,(sxpath-ref array '(type)))
                             (element-count ,element-count))))))
      (core:type . ,(lambda sxml (type-pointifier sxml)))))
  
  (define field-rules
    `((core:field
       ,type-rules
        . ,(lambda field
             (cons* 'field
                    (sxpath-ref field '(^ name))
                    (sxpath-ref field '(type))
                    (or
                     (and-let* ((bits (sxpath-attr field '(^ bits))))
                       (list (list 'bits (string->number bits))))
                     '()))))
      (core:constructor *PREORDER* . ,ignore)
      (core:callback *PREORDER*
                     . ,(lambda sxml
                          `(field ,(sxpath-ref sxml '(^ name)) (type "pointer"))))))

  (define (type-pointifier type)
    (define (construct-type base-type depth)
      (case depth
        ((0)
         `(type ,base-type))
        ((1)
         `(type (pointer (base-type (type ,base-type)))))
        (else
         `(type (pointer (base-type (type ,base-type))
                         (depth ,depth))))))
    
    (let ((name (sxpath-attr type '(^ name)))
          (ctype (sxpath-attr type '(^ c:type))))
      (cond ((string=? name "any")
             (construct-type "void" 1))
            ((string=? name "utf8")
             (construct-type "char" 1))
            ((string=? name "GType")
             (construct-type "gtype" 0))
            ((not ctype)
             `(type ,name))
            ((string-skip-right ctype #\*)
             => (lambda (i)
                  (construct-type name (- (string-length ctype) (+ i 1)))))
            (else
             `(type ,name))))))
