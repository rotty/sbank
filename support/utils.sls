;;; utils.sls --- Miscellaneous utilities for sbank.

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

;;; Code:
#!r6rs

(library (sbank support utils)
  (export define-enum
          map-apply
          vector-index
          ->vector
          make-lazy-entry lazy-entry? lazy-entry-proc
          symbol-append
          scheme-ified-symbol
          scheme-ified-string
          c-ified-string
          name-symbol/prefix
          define-setup-procedure)
  (import (rnrs base)
          (rnrs control)
          (rnrs unicode)
          (rnrs records syntactic)
          (spells define-values)
          (srfi :14 char-sets)
          (only (srfi :13 strings) string-map string-tokenize))

  (define-syntax define-enum
    (syntax-rules ()
      ((define-enum (val->symbol symbol->val) (symbol ...))
       (define-values (val->symbol symbol->val)
         (let ((sym-vec '#(symbol ...)))
           (values (lambda (val) (vector-ref sym-vec val))
                   (lambda (sym) (vector-index eq? sym-vec sym))))))))

  (define (vector-index equal vec val)
    (let loop ((i 0))
      (cond ((>= i (vector-length vec)) #f)
            ((equal (vector-ref vec i) val) i)
            (else (loop (+ i 1))))))
  
  (define (map-apply proc lst)
    (map (lambda (elt)
           (apply proc elt))
         lst))

  (define-record-type lazy-entry
    (fields
     (immutable proc lazy-entry-proc)))  

  (define (->vector val)
    (cond ((vector? val) val)
          ((list? val) (list->vector val))
          (else (error '->vector "cannot convert argument to vector" val))))

  (define (symbol-append . syms)
    (string->symbol (apply string-append (map symbol->string syms))))

  (define (name-symbol/prefix sym prefix)
    (let ((s (symbol->string sym)))
      (cond ((or (enclosed-by? s #\* #\*)
                 (enclosed-by? s #\< #\>))
             (string->symbol
              (string-append (string (string-ref s 0))
                             (symbol->string prefix)
                             (strip-enclosers s)
                             (string (string-ref s (- (string-length s) 1))))))
            (else
             (symbol-append prefix sym)))))
  
  (define (scheme-ified-symbol s)
    (string->symbol (scheme-ified-string s)))

  (define scheme-ified-string
    (let ((upper-id-cs (char-set-union char-set:upper-case (char-set #\_ #\-))))
      (lambda (s)
        (cond ((char-set<= (string->char-set s) upper-id-cs)
               (string-append "*" (string-map downcase/dash  s) "*"))
              ((camel-cased? s)
               (string-append "<" (un-camel-case s) ">"))
              (else
               (string-map dash s))))))

  (define (enclosed-by? s start-c end-c)
    (and (char=? (string-ref s 0) start-c)
         (char=? (string-ref s (- (string-length s) 1)) end-c)))

  (define (strip-enclosers s)
    (substring s 1 (- (string-length s) 1)))
  
  (define (c-ified-string sym)
    (let ((s (symbol->string sym)))
      (cond ((enclosed-by? s #\* #\*)
             (string-map upcase/uscore (strip-enclosers s)))
            ((enclosed-by? s #\< #\>)
             (camel-case (strip-enclosers s)))
            (else
             (string-map uscore s)))))

  (define (dash c) (case c ((#\_) #\-) (else c)))
  (define (downcase/dash c) (case c ((#\_) #\-) (else (char-downcase c))))
  (define (uscore c) (case c ((#\-) #\_) (else c)))
  (define (upcase/uscore c) (case c ((#\-) #\_) (else (char-upcase c))))

  (define (camel-cased? s)
    (and (>= (string-length s) 2)
         (char-upper-case? (string-ref s 0))
         (char-lower-case? (string-ref s 1))))
  
  (define (un-camel-case s)
    (let loop ((result-chars '()) (i 0) (in-word? #f))
      (if (>= i (string-length s))
          (list->string (reverse result-chars))
          (let ((c (string-ref s i)))
            (cond ((and in-word? (char-upper-case? c))
                   (loop (append (list (char-downcase c) #\-) result-chars)
                         (+ i 1)
                         #t))
                  ((or (char-alphabetic? c) (char-numeric? c))
                   (loop (cons (char-downcase c) result-chars) (+ i 1) #t))
                  (else
                   (loop (cons c result-chars) (+ i 1) #f)))))))

  (define (camel-case s)
    (let loop ((result-chars '()) (i 0) (had-dash? #t))
      (if (>= i (string-length s))
          (list->string (reverse result-chars))
          (let ((c (string-ref s i)))
            (cond ((and had-dash? (char-lower-case? c))
                   (loop (cons (char-upcase c) result-chars) (+ i 1) #f))
                  ((char=? c #\-)
                   (loop result-chars (+ i 1) #t))
                  (else
                   (loop (cons c result-chars) (+ i 1) #f)))))))

  (define-syntax define-setup-procedure
    (syntax-rules ()
      ((_ (name arg ...) body ...)
       (define name
         (let ((ran? #f))
           (lambda (arg ...)
             (unless ran?
               body ...
               (set! ran? #t))))))))
  )
