;;; sys-def.scm --- System definition for sbank

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

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

;; This file contains some metainformation and conjure build
;; instructions for sbank.

;;; Code:

(define-system sbank
  (dependencies spells)
  
  (conjure
   (import (rnrs)
           (spells pathname)
           (conjure dsl))

   (define (typelib-fender namespace)
     (lambda ()
       (let ((available? (procedure-from-environment/lazy typelib-available?
                                                          (sbank typelib base))))
         (available? namespace))))

   (task (configure
          (produce '((("sbank") "config.sls") <= "config.sls.in")
                   '((("sbank") "glib.sls") <= "glib.sls.in")
                   `((("sbank") "gtk.sls") <= "gtk.sls.in"
                     (? ,(typelib-fender "Gtk")))
                   `((("sbank") "soup.sls") <= "soup.sls.in"
                     (? ,(typelib-fender "Soup"))))
          (fetchers (procedure-from-environment/lazy
                     (typelib-fetcher)
                     (sbank support conjure)))))))
