;;; ptr-table.scm --- Unit tests for (sbank support ptr-table)

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

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

(define-test-suite pt-tests
  "Pointer tables")

(define-test-case pt-tests basic ()
  (let* ((pt (make-ptr-table))
         (p1 (ptr-table-add! pt "Value1"))
         (p2 (ptr-table-add! pt 'another-value)))
    (test-equal #t (pointer? p1))
    (test-equal #t (pointer? p2))
    (test-equal "Value1" (ptr-table-ref pt p1 #f))
    (test-equal 'another-value (ptr-table-ref pt p2 #f))
    (ptr-table-remove! pt p1)
    (test-equal #f (ptr-table-ref pt p1 #f))
    (test-equal 'another-value (ptr-table-ref pt p2 #f))))

(run-test-suite pt-tests)
