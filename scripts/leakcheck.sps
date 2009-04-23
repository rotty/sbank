;;; leakcheck.sps --- Do things that might trigger leaks.

;; Copyright (C) 2008, 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

#!r6rs

(import (rnrs)
        (only (srfi :1 lists) append-map unfold reduce)
        (only (srfi :27 random-bits) random-integer)
        (spells define-values)
        (spells string-utils)
        (spells alist)
        (only (spells gc) collect)
        (sbank support utils)
        (sbank gobject)
        (sbank gobject gvalue)
        (sbank typelib))

(typelib-import ("Everything" #f)
                (setup gobject-setup!))

(define-syntax repeat
  (syntax-rules ()
    ((repeat n body ...)
     (do ((i 0 (+ i 1)))
         ((>= i n))
       body ...))))

(define *checks* '())

(define-syntax define-check
  (syntax-rules ()
    ((_ name body ...)
     (define-values (name)
       (let ((check (lambda () body ...)))
         (set! *checks* (append *checks* (list (cons 'name check))))
         check)))))

(define (random-integers n k)
  (unfold (lambda (i) (> i n))
          (lambda (i) (random-integer k))
          (lambda (i) (+ i 1))
          0))

(define N 10000)

(define-check method-call
  (repeat N (test-boolean #t))
  (repeat N (test-boolean #f)))

(define-check array-transfer
  (let ((v (random-integers 5 10)))
    (repeat N (test-array-int-in-take v))))

(define-check array-no-transfer
  (let ((v (random-integers 5 10)))
    (repeat N (test-array-int-in v))))

(define-check obj-alloc
  (repeat N (send <test-obj> (new/props))))

(define-check obj-alloc-nested
  (repeat N (send <test-obj>
              (new/props 'bare (send <test-obj> (new/props))))))

(define-check callback
  (let ((cb (lambda () 42)))
    (repeat N (test-callback cb))))

(define (make-test-cb n)
  (lambda () n))

(define-check callback-freshproc
  (repeat N
    (test-callback (make-test-cb (random-integer N)))))

(define-check callback-notified
  (repeat N
    (let ((nums (random-integers 5 10)))
      (assert (equal?
               (map test-callback-destroy-notify (map make-test-cb nums))
               nums))
      (assert (= (test-callback-thaw-notifications) (reduce + 0 nums))))))

(define-check signal-callback
  (let ((obj (send <test-obj> (new/props))))
    (repeat N
      (let ((sig (send obj (connect 'test (lambda (obj) #f)))))
        (send obj (emit 'test))
        (send obj (disconnect sig))))))

(define (println fmt . args)
  (string-substitute #t fmt args 'braces)
  (newline))

(define (main argv)
  (let ((tests (append-map (lambda (arg)
                             (let ((sym (string->symbol arg)))
                               (case sym
                                 ((all) (map car *checks*))
                                 (else  (list sym)))))
                           (cdr argv))))
    (for-each (lambda (test)
                (println "running: {0}" test)
                (cond ((assq-ref *checks* test) => (lambda (proc) (proc)))
                      (else (println "No code found for {0}, skipping" test)))
                (collect)
                (collect-gobjects))
              tests)
    (println "done, press Ctrl+D to quit")
    (read)))

(main (command-line))
