;;; conjure.sls --- sbank build system support

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

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

(library (sbank support conjure)
  (export typelib-fetcher)
  (import (rnrs base)
          (rnrs sorting)
          (rnrs exceptions)
          (except (srfi :1 lists) for-each map)
          (spells match)
          (spells misc)
          (spells logging)
          (xitomatl irregex)
          (fmt)
          (conjure utils)
          (only (sbank gobject internals) gerror?)
          (sbank typelib expanders)
          (sbank typelib base))

(define (typelib-fetcher)
  (lambda (project)
    (lambda (missing datum)
      (match datum
        (('typelib-exports spec . opts)
         (append-map fetch-exports missing))
        (('typelib-availability-filter . names)
         (let ((result (filter typelib-available? names)))
           (log/sbank 'info (cat "available typelibs: "
                                 (fmt-join dsp names " ")))
           (list (cons datum (fmt #f (wrt result))))))
        (_
         #f)))))

(define (opts-ref opts key default)
  (cond ((assq key opts) => cadr)
        (else default)))

(define (opts-ref* opts key default)
  (cond ((assq key opts) => cdr)
        (else default)))

(define (includes+excludes->pred includes excludes)
  (let ((include-irxs (and includes (map irregex includes)))
        (exclude-irxs (map irregex excludes)))
    (lambda (name)
      (and (or (not includes)
               (or-map (lambda (irx)
                         (irregex-match irx name))
                       include-irxs))
           (not (or-map (lambda (irx)
                          (irregex-match irx name))
                        exclude-irxs))))))

(define (fetch-exports item)
  (guard (c ((gerror? c)
             '()))
    (match item
           (('typelib-exports (? list? spec) . opts)
            (let* ((pred (includes+excludes->pred (opts-ref* opts 'include #f)
                                                  (opts-ref* opts 'exclude '())))
                   (name (opts-ref opts 'name #f))
                   (names-string
                    (fmt #f (fmt-join dsp
                                      (sort-list (typelib-exported-names spec pred)
                                                 (lambda (n1 n2)
                                                   (string<? (symbol->string n1)
                                                             (symbol->string n2))))
                                      " "))))
              (cons* (cons item names-string)
                     (if name
                         (list (cons `(typelib-exports ,name) names-string))
                         '()))))
           (_
            '()))))

(define log/sbank (make-fmt-log '(conjure sbank)))

)
