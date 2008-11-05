;;; boxed-values.sls --- Scheme values as GObject boxed type.

;; Copyright (C) 2008 Andreas Rottmann <a.rottmann@gmx.at>

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

(library (sbank gobject boxed-values)
  (export g-boxed-value-type)
  (import (rnrs base)
          (rnrs control)
          (spells foreign)
          (sbank shlibs)
          (sbank ctypes simple)
          (sbank gobject gtype))

  (define g-boxed-value-type
    (let-callouts libgobject ((register% gtype-ctype "g_pointer_type_register_static" '(pointer)))
      (let ((type #f))
        (lambda ()
          (unless type
            (let ((name-ptr (string->utf8z-ptr "boxed-scm")))
              (set! type (register% name-ptr))
              (free name-ptr)))
          type)))))
