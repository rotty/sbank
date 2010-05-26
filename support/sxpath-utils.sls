;;; sxpath-utils.sls --- Utilities based on sxpath.

;; Copyright (C) 2008, 2010 Andreas Rottmann <a.rottmann@gmx.at>

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

(library (sbank support sxpath-utils)
  (export sxpath-ref sxpath-attr)
  (import (rnrs base)
          (srfi :2 and-let*)
          (wak sxml-tools sxpath))

  (define (sxpath-ref sxml path)
    (and-let* ((result ((sxpath path) sxml))
               ((pair? result)))
      (car result)))
  
  (define (sxpath-attr sxml path)
    (and-let* ((result ((sxpath path) sxml))
               ((pair? result)))
      (cadar result))))
