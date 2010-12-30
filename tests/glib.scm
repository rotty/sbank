#!r6rs
;;; glib.scm --- Unit test for the GLib bindings

;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(import (rnrs)
        (wak trc-testing)
        (sbank glib))

(define-test-suite glib-tests
  "GLib bindings")

(define (main-loop-counter main-loop n proc)
  (let ((c n))
    (lambda ()
      (cond ((> c 0)
             (proc c)
             (set! c (- c 1))
             #t)
            (else
             (send main-loop (quit))
             #f)))))

(define-test-case glib-tests mainloop ()
  (let ((main-loop (send <g-main-loop> (new #f #f)))
        (results '()))
    (define (add-square! x)
      (set! results (cons (* x x) results)))
    (g-idle-add
     (main-loop-counter main-loop 5 add-square!))
    (send main-loop (run))
    (test-equal '(1 4 9 16 25) results)
    
    (set! results '())
    (g-timeout-add
     50
     (main-loop-counter main-loop 4 add-square!))
    (send main-loop (run))
    (test-equal '(1 4 9 16) results)))

(run-test-suite glib-tests)
