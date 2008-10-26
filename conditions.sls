;;; conditions.sls --- Condition types for sbank.

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


(library (sbank conditions)
  (export &sbank-error
          make-sbank-error
          sbank-error?
          
          &sbank-callout-error
          make-sbank-callout-error
          sbank-callout-error?

          &sbank-callback-error
          make-sbank-callback-error
          sbank-callback-error?)

  (import (rnrs base)
          (rnrs conditions))

  (define-condition-type &sbank-error &error
    make-sbank-error sbank-error?)

  (define-condition-type &sbank-callout-error &sbank-error
    make-sbank-callout-error sbank-callout-error?)

  (define-condition-type &sbank-callback-error &sbank-error
    make-sbank-callback-error sbank-callback-error?))
