;;; tests.scm --- List of unit-test files for running with testeez.

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


((systems sbank)
 (files
  ("utils.scm" (sbank utils))

  ("gobject.scm"
   (sbank gobject gtype)
   (sbank gobject gvalue)
   (sbank gobject internals)
   (spells foreign))
  
  ("stypes.scm"
   (sbank stypes) (spells foreign)
   (only (spells lists) make-list)
   (rnrs control)
   (rnrs arithmetic bitwise)
   (rnrs bytevectors)
   (rnrs lists))))
