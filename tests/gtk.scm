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


(typelib-import
 (prefix (only ("Gtk" #f) <list-store> <tree-view>) gtk-)
 (setup gtk-setup!))

(testeez "ListStore"
  (test-define "creating" store (send <gtk-list-store> (newv (list 'boxed 'utf8))))
  (test-eval "populating"
             (for-each
              (lambda (item)
                (send store
                  (set-values (send store (append))
                              0 (cadr item)
                              1 (car item))))
              `(("Multiplication" ,*)
                ("Addition" ,+))))
  (test-define "creating view" tview (send <gtk-tree-view> (new)))
  (test-eval "setting model" (send tview (set-model store)))
  (test-define "getting iter (using upcast)" iter
    (send (send tview (get-model)) (append)))
  (test-eval "setting values (using upcast)"
    (send (send tview (get-model)) (set-values iter 0 / 1 "Division")))
  (test/eq "getting values (no upcast)"
    (send store (get-value iter 0)) /))
