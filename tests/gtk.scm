;; Ported to sbank from guile-gnome
;; Copyright (C) 2003,2004,2008,2009 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org


(define-test-suite gtk-tests
  "GTK+")

(define-test-case gtk-tests list-store ()
  (let ((store (send <gtk-list-store> (newv (list 'boxed 'utf8))))
        (tview (send <gtk-tree-view> (new))))
    (for-each
     (lambda (item)
       (send store
         (set-values (send store (append))
                     0 (cadr item)
                     1 (car item))))
     `(("Multiplication" ,*)
       ("Addition" ,+)))
    (send tview (set-model store))
    (let ((iter
           (send (send tview (get-model))
             (append))))
      (send (send tview (get-model))
        (set-values iter 0 / 1 "Division"))
      (test-eq /
        (send store (get-value iter 0))))))

(run-test-suite gtk-tests)

;; Local Variables:
;; scheme-indent-styles: (trc-testing)
;; End:
