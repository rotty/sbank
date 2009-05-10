;;; gdk.sls --- Glossing for the Gdk namespace

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

(library (sbank gdk)
  (export send gdk-setup!)
  (import (rnrs base)
          (rnrs control)
          (srfi :8 receive)
          (sbank support utils)
          (sbank typelib decorators)
          (sbank gobject))

  (define (color-parse-decorator func)
    (lambda (str)
      (receive (success? color) (func str)
        (and success? color))))

  (define-setup-procedure (gdk-setup!)
    (gobject-setup!)
    (register-typelib-decorator "Gdk" "color_parse" color-parse-decorator))
  
  )
