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
  ("utils.scm" (testeez) (sbank support utils))

  ("stypes.scm"
   (testeez)
   (sbank support stypes)
   (spells foreign)
   (only (srfi :1 lists) make-list iota)
   (rnrs control)
   (rnrs arithmetic bitwise)
   (rnrs bytevectors)
   (rnrs lists))

  ("gobject.scm"
   (testeez)
   (sbank gobject gtype)
   (sbank gobject gvalue)
   (sbank gobject internals)
   (spells foreign))

  ((code (gobject-setup!))
   "everything.scm"
   (testeez)
   (sbank typelib)
   (only (sbank ctypes basic) null-ok-always-on?)
   (spells parameter)
   (srfi :1 lists)
   (sbank gobject))

  ((code (gtk-setup!))
   "gtk.scm"
   (testeez)
   (sbank gtk)
   (sbank typelib))))
