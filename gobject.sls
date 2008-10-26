;;; gobject.sls --- Public interface to the GObject mapping.

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

;; This library provides the interface to the GObject to Scheme mapping.

;;; Code:


(library (sbank gobject)
  (export gobject-class? ginstance?
          genum? genum-lookup
          send
          install-gobject-decorators)
  (import (rnrs base)
          (only (spells assert) cout)
          (sbank typelib decorators)
          (sbank gobject signals)
          (sbank gobject internals))

  (define (gobject-decorator class)
    ;; IMPLEMENTME
    class)

  (define (install-gobject-decorators)
    (register-typelib-decorator "GObject" "Object" gobject-decorator)))
