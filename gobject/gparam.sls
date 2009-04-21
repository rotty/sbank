;;; gparam.sls --- GParamater low-level access

;; Copyright (C) 2008, 2009 Andreas Rottmann <a.rottmann@gmx.at>

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
#!r6rs

(library (sbank gobject gparam)
  (export g-param-alloc
          g-param-name g-param-name-set!
          g-param-value
          g-param-size)
  (import (rnrs base)
          (spells foreign)
          (sbank support shlibs)
          (for (sbank support stypes) expand)
          (for (sbank typelib stypes) expand))

  (define-syntax define-accessors (stype-accessor-definer (typelib-stypes)))
  (define-syntax define-attributes (stype-attribute-definer (typelib-stypes)))

  (define (g-param-alloc n)
    (let ((n-bytes (* n g-param-size)))
      (memset (g-malloc n-bytes) 0 n-bytes)))
  
  (define-accessors "GParameter"
    (g-param-name g-param-name-set! "name")
    (g-param-value "value"))

  (define-attributes "GParameter"
    (g-param-size size)))
