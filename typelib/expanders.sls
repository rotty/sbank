;;; expanders.sls --- Syntax-case expanders

;; Copyright (C) 2008, 2009, 2011 Andreas Rottmann <a.rottmann@gmx.at>

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

;; This library provides the `typelib-import-expander' procedure,
;; which returns a procedure that can be used on the right-hand-side
;; of `define-syntax'.

;;; Code:
#!r6rs

(library (sbank typelib expanders)
  (export typelib-import-expander
          typelib-exported-names)
  (import (for (rnrs base) run expand (meta -1))
          (rnrs control)
          (rnrs syntax-case)
          (only (rnrs lists) memq)
          (only (srfi :1 lists)
                partition concatenate append-map filter)
          (srfi :8 receive)
          (for (spells define-values) expand (meta -1))
          (spells match)
          (spells tracing)
          (sbank support utils)
          (for (sbank typelib base) run expand (meta -1)))


  (define (typelib-import-expander who)
    (lambda (stx)
      (syntax-case stx ()
        ((k <import-spec> ...)
         (receive (setup-clauses import-specs)
                  (partition (lambda (import-spec)
                               (and (pair? import-spec)
                                    (eq? 'setup (car import-spec))))
                             (map syntax->datum #'(<import-spec> ...)))
           (with-syntax (((typelib-name ...)
                          (generate-temporaries import-specs)))
             (with-syntax
                 (((form ...)
                   (cons
                    #`(define-values ()
                        #,@(append-map (lambda (setup-clause)
                                         (map (lambda (item)
                                                (datum->syntax #'k `(,item)))
                                              (cdr setup-clause)))
                                       setup-clauses))
                    (concatenate
                     (map (lambda (import-spec name)
                            (expand-import who #'k name import-spec))
                          import-specs
                          #'(typelib-name ...))))))
               #'(begin form ...))))))))

  (define (expand-import who k typelib import-spec)
    (receive (namespace version bindings)
             (eval-import-spec who import-spec #f)
      (with-syntax (((get-entry) (generate-temporaries '(get-entry))))
        (append
         (list
          #`(define #,typelib (require-typelib #,(datum->syntax k namespace)
                                               #,(datum->syntax k version)
                                               0))
          #`(define (get-entry binding name)
              (or (typelib-get-entry #,typelib name)
                  (error '#,(datum->syntax #'k who)
                         "unable to import binding from namespace"
                         '(#,(datum->syntax k namespace)
                           #,(datum->syntax k version)
                           name)
                         binding))))
         (map
          (lambda (binding)
            #`(define #,(datum->syntax k (car binding))
                (get-entry '#,(datum->syntax k (car binding))
                           #,(datum->syntax k (cdr binding)))))
          bindings)))))

  (define typelib-exported-names
    (case-lambda
      ((import-spec pred)
        (receive (namespace version bindings)
                 (eval-import-spec 'typelib-exported-names import-spec pred)
          (map car bindings)))
      ((import-spec)
       (typelib-exported-names import-spec #f))))

  (define (eval-import-spec who spec pred)
    (match spec
     (('only base . names)
      (receive (namespace version base-bindings)
               (eval-import-spec who base pred)
        (values namespace
                version
                (filter (lambda (x)
                          (memq (car x) names))
                        base-bindings))))
     (('prefix base prefix)
      (receive (namespace version base-bindings)
               (eval-import-spec who base pred)
        (values namespace
                version
                (map (lambda (x)
                       (cons (name-symbol/prefix (car x) prefix)
                             (cdr x)))
                     base-bindings))))
     (((? string? namespace) version)
      (values namespace
              version
              (get-bindings who namespace version pred)))
     (_
      (error who "bad import spec" spec))))

  (define (get-bindings who namespace version pred)
    (let* ((tl (require-typelib namespace version 0))
           (names (typelib-get-entry-names tl)))
      (map (lambda (name)
             (cons (scheme-ified-symbol name) name))
           (if pred (filter pred names) names)))))
