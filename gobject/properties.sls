;;; properties.sls --- GObject property support.

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


(library (sbank gobject properties)
  (export g-object-get-property
          g-object-set-property)
  (import (rnrs base)
          (spells receive)
          (spells foreign)
          (spells tracing)
          (sbank shlibs)
          (sbank ctypes)
          (sbank type-data)
          (sbank gobject gvalue)
          (sbank gobject internals))

  (define (property-lookup who obj property)
    (or (gobject-class-get-property-info (ginstance-class obj) property)
                         (error who
                                "no such property in class of instance"
                                obj property)))
  
  (define g-object-get-property
    (let-callouts libgobject
        ((get-property% 'void "g_object_get_property" '(pointer pointer pointer)))
      (trace-lambda prop-get (obj property)
        (define who 'g-object-get-property)
        (let ((pinfo (property-lookup who obj property)))
          (let ((gvalue (g-value-new (property-info-type pinfo)))
                (name-ptr (string->utf8z-ptr (symbol->string property))))
            (get-property% (ginstance-ptr obj) name-ptr gvalue)
            (free name-ptr)
            (let ((result (g-value-ref gvalue (property-info-type pinfo))))
              (g-value-free gvalue)
              result))))))
  
  (define g-object-set-property
    (let-callouts libgobject
        ((set-property% 'void "g_object_set_property" '(pointer pointer pointer)))
      (lambda (obj property value)
        (define who 'g-object-set-property)
        (let ((pinfo (property-lookup who obj property)))
          (let ((gvalue (->g-value value (property-info-type pinfo)))
                (name-ptr (string->utf8z-ptr (symbol->string property))))
            (set-property% (ginstance-ptr obj) name-ptr gvalue)
            (free name-ptr)
            (g-value-free gvalue)))))))
