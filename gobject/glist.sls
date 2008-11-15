;;; glist.sls --- GList primitives.

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

(library (sbank gobject glist)
  (export g-list-append

          g-slist-prepend
          g-slist-reverse
          g-slist-next
          g-slist-data
          g-slist-free)
  (import (rnrs base)
          (sbank shlibs)
          (sbank typelib stypes)
          (sbank stypes))

  (define-syntax define-accessors (stype-accessor-definer (typelib-stypes)))
  
  (define-callouts libglib
    (g-list-append 'pointer "g_list_append" '(pointer pointer))
    (g-slist-prepend 'pointer "g_slist_prepend" '(pointer pointer))
    (g-slist-reverse 'pointer "g_slist_reverse" '(pointer))
    (g-slist-free 'void "g_slist_free" '(pointer)))

  (define-accessors "GSList"
    (g-slist-next "next")
    (g-slist-data "data")))
