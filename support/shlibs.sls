;;; shlibs.sls --- Shared libraries needed in sbank.

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
#!r6rs

(library (sbank support shlibs)
  (export libgir libgobject libglib
          let-callouts
          define-callouts)
  (import (rnrs base)
          (spells foreign))

  ;; TODO: callout "compression"
  (define-syntax let-callouts
    (syntax-rules ()
      ((let-callouts shlib ((name ret-type c-name arg-types) ...) body ...)
       (let ((name ((make-c-callout ret-type arg-types)
                    (dlsym shlib c-name)))
             ...)
         body ...))))

  (define-syntax define-callouts
    (syntax-rules ()
      ((define-callouts shlib (name ret-type c-name arg-types) ...)
       (begin
         (define name ((make-c-callout ret-type arg-types) (dlsym shlib c-name)))
         ...))))

  (define (checked-dlopen name)
    (or (dlopen name)
        (error 'checked-dlopen "unable to open shared library" name (dlerror))))
  
  (define libgir (checked-dlopen "libgirepository-1.0.so.0"))
  (define libgobject (checked-dlopen "libgobject-2.0.so.0"))
  (define libglib (checked-dlopen "libglib-2.0.so.0")))
