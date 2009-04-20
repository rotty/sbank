;;; expanders.sls --- Syntax-case expanders

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

;; This library provides the `typelib-import-expander' procedure,
;; which returns a procedure that can be used on the right-hand-side
;; of `define-syntax'.

;;; Code:
#!r6rs

(library (sbank typelib expanders)
  (export typelib-import-expander)
  (import (for (rnrs base) run expand (meta -1))
          (rnrs control)
          (rnrs syntax-case)
          (only (rnrs lists) memq)
          (only (srfi :1 lists)
                partition concatenate append-map filter)
          (srfi :8 receive)
          (for (spells define-values) expand (meta -1))
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
    (receive (namespace version prefix bindings)
             (destructure-import-spec who import-spec)
      (let ((bindings (or bindings (get-bindings who namespace version))))
        (with-syntax (((get-entry) (generate-temporaries '(get-entry))))
          (append
           (list
            #`(define #,typelib (require-typelib #,(datum->syntax k namespace)
                                                 #,(datum->syntax k version)
                                                 0))
            #`(define (get-entry name binding)
                (or (typelib-get-entry #,typelib name)
                    (error '#,(datum->syntax #'k who)
                           "unable to import binding from namespace"
                           '(#,(datum->syntax k namespace)
                             #,(datum->syntax k version))
                           binding))))
           (map
            (lambda (name)
              #`(define #,(datum->syntax
                           k
                           (if prefix (name-symbol/prefix name prefix) name))
                  (get-entry #,(datum->syntax k (c-ified-string name))
                             ',#(datum->syntax k name))))
            bindings))))))

  (define (destructure-import-spec who import-spec)
    (define (lose msg . irritants)
      (apply syntax-violation
             who
             (string-append "invalid import spec - " msg)
             import-spec
             irritants))
    (cond ((not (pair? import-spec))
           (lose "must be a pair"))
          ((string? (car import-spec)) ; (<namespace> <version>)
           (values (car import-spec) (cadr import-spec) #f #f))
          (else
           (case (car import-spec)
             ((prefix)
              (unless (and (pair? (cdr import-spec))
                           (pair? (cddr import-spec))
                           (null? (cdddr import-spec)))
                (lose "prefix must have exactly two items"))
              (receive (namespace version prefix bindings)
                  (destructure-import-spec who (cadr import-spec))
                (when prefix
                  (lose "double 'prefix'"))
                (values namespace version (caddr import-spec) bindings)))
             ((only)
              (unless (pair? (cdr import-spec))
                (lose "only must be followed by an import-spec"))
              (receive (namespace version prefix bindings)
                  (destructure-import-spec who (cadr import-spec))
                (when bindings
                  (lose "double 'only'"))
                (values namespace version prefix (cddr import-spec))))
             (else
              (lose "bad keyword" (car import-spec)))))))

  (define (get-bindings who namespace version)
    (let* ((tl (require-typelib namespace version 0))
           (names (typelib-get-entry-names tl)))
      (map scheme-ified-symbol names))))

