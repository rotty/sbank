;;; gir.sls --- Parse gobject-introspection XML .gir files.

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
          (xitomatl srfi receive)
          (spells alist)
          (spells tracing)
          (spells foreign)
          (only (xitomatl srfi strings) string-skip-right)
          (sxml ssax)
          (sxml transform)
          (xitomatl sxml-tools sxpath)
          (xitomatl sxml-tools sxpathlib)
          (xitomatl sxml-tools sxpath-ext)
          (xitomatl srfi and-let*)
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
                   ((sxpath '(// core:namespace (*OR* core:record core:union)))
                    top)))
       (core:record . ,(compound-maker 'record))
       (core:union . ,(compound-maker 'union))
       (core:field
        . ,(lambda field
             (cons* 'field
                    (sxpath-ref field '(^ name))
                    (sxpath-ref field '(type))
                    (or
                     (and-let* ((bits (sxpath-attr field '(^ bits))))
                       (list (list 'bits (string->number bits))))
                     '()))))
       (core:array
        . ,(lambda array
             (let ((element-count (cond ((sxpath-attr array '(^ fixed-size))
                                         => (lambda (size)
                                              (string->number size)))
                                        (else 0))))
               `(type (array (element-type ,(sxpath-ref array '(type)))
                             (element-count ,element-count))))))
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
                          (depth ,pointer-depth)))
          `(type ,name)))))
