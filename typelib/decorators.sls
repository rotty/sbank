;;; decorators.sls --- Decorator registry for typelib.

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


(library (sbank typelib decorators)
  (export register-typelib-decorator
          lookup-typelib-decorator)
  (import (rnrs base)
          (spells table)
          (spells parameter))

  (define *typelib-decorators* (make-table 'equal))
  
  (define (lookup-typelib-decorator namespace name)
    (table-ref *typelib-decorators* (cons namespace name)))

  (define (register-typelib-decorator namespace name decorator)
    (let* ((key (cons namespace name))
           (old-decorator (table-ref *typelib-decorators* key)))
      (table-set! *typelib-decorators*
                  key
                  (if old-decorator
                      (lambda (obj)
                        (decorator (old-decorator obj)))
                      decorator)))))
