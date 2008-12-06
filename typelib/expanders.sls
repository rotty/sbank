;;; expanders.sls --- Syntax-case expanders

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

;; This library provides the `typelib-import-expander' procedure,
;; which returns a procedure that can be used on the right-hand-side
;; of `define-syntax'.

;;; Code:


(library (sbank typelib expanders)
  (export typelib-import-expander)
  (import (rnrs base)
          (rnrs control)
          (rnrs syntax-case)
          (spells receive)
          (spells tracing)
          (sbank support utils)
          (sbank typelib base))


  (define (typelib-import-expander who)
    (lambda (stx)
      (syntax-case stx ()
        ((k <import-spec> ...)
         (with-syntax ((typelib-names (generate-temporaries #'(<import-spec> ...))))
           (with-syntax (((form ...)
                          (apply append
                                 (map (lambda (import-spec name)
                                        (expand-import who #'k name (syntax->datum import-spec)))
                                      #'(<import-spec> ...)
                                      #'typelib-names))))
             #'(begin form ...)))))))

  (define (expand-import who k typelib import-spec)
    (receive (namespace version prefix bindings) (destructure-import-spec who import-spec)
      (let ((bindings (or bindings (get-bindings who namespace version))))
        (cons #`(define #,typelib (require-typelib #,(datum->syntax k namespace)
                                                   #,(datum->syntax k version)
                                                   0))
              (map (lambda (name)
                     #`(define #,(datum->syntax k (if prefix (name-symbol/prefix name prefix) name))
                         (or (typelib-get-entry #,typelib #,(datum->syntax k (c-ified-string name)))
                             (error '#,(datum->syntax #'k who) "unable to import binding"
                                    '#,(datum->syntax #'k name)))))
                   bindings)))))

  (define (destructure-import-spec who import-spec)
    (define (lose msg . irritants)
      (apply syntax-violation who (string-append "invalid import spec - " msg) import-spec irritants))
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

