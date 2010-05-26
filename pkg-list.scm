;;; sys-def.scm --- System definition for sbank

;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

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

(package (sbank (0))
  (depends (srfi)
           (wak-foof-loop)
           (spells)
           (spells-foreign)
           (wak-ssax)
           (wak-sxml-tools))
  
  (libraries
   ("data" -> ("sbank" "data"))
   (sls -> "sbank")
   (("examples" . sls) -> ("sbank" "examples")))

  (documentation
   "AUTHORS" "COPYING" "README" "TODO"
   "examples")

  (conjure
   (import (rnrs)
           (sbank support conjure))
   (sbank-build-tasks))
  
  (installation-hook ((needs-source? . #t))
    (import (rnrs)
            (spells pathname)
            (conjure dsl)
            (conjure dorodango)
            (only (sbank support conjure)
                  typelib-fender
                  typelib-fetcher
                  install-products))

    (make-conjure-hook
     (lambda (agent)
       (sbank-build-tasks)
       
       (task install
         (ordinary
          (depends 'configure)
          (proc (lambda (self)
                  (install-products
                   agent
                   ((self 'project) 'product-dir)
                   (map (lambda (filename)
                          (make-pathname #f '("sbank") filename))
                        '("config.sls"
                          "glib.sls"
                          "gtk.sls"
                          "soup.sls")))))))))))

;; Local Variables:
;; scheme-indent-styles: (pkg-list conjure-dsl)
;; End:
