;; Copyright (C) 2008 Andreas Rottmann
;; Copyright (C) 2004 Patrick Bernaud
;;
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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

#!r6rs

(library (sbank examples gtk demo list-store)
  (export main name description)
  (import (rnrs)
          (sbank gobject)
          (sbank gtk)
          (sbank typelib))

(define data
  '((#f 60482 "Normal"      "scrollable notebooks and hidden tabs")
    (#f 60620 "Critical"
        "gdk_window_clear_area (gdkwindow-win32.c) is not thread-safe")
    (#f 50214 "Major"       "Xft support does not clean up correctly")
    (#t 52877 "Major"       "GtkFileSelection needs a refresh method. ")
    (#f 56070 "Normal"      "Can't click button after setting in sensitive")
    (#t 56355 "Normal"      "GtkLabel - Not all changes propagate correctly")
    (#f 50055 "Normal"      "Rework width/height computations for TreeView")
    (#f 58278 "Normal"      "gtk_dialog_set_response_sensitive () doesn't work")
    (#f 55767 "Normal"      "Getters for all setters")
    (#f 56925 "Normal"      "Gtkcalender size")
    (#f 56221 "Normal"      "Selectable label needs right-click copy menu")
    (#t 50939 "Normal"      "Add shift clicking to GtkTextView")
    (#f 6112  "Enhancement" "netscape-like collapsable toolbars")
    (#f 1     "Normal"      "First bug :=)")))


(define (populate-model store)
  ;; add data to the list store
  (for-each 
   (lambda (b)
     (let ((iter (send store (append))))
       (for-each
	(lambda (col data)
	  (send store (set-value iter col data)))
	'(0 1 2 3) b)))
   data))
		
(define (fixed-toggled treemodel pathstr)
  (let* (
	 ;; get toggled iter
	 (iter  (send treemodel (get-iter pathstr)))
	 ;; get current value and invert
	 (fixed (not (send treemodel (get-value iter 0)))))
    ;; set the new value
    (send treemodel (set-value iter 0 fixed))))

(define (add-columns treeview)
  (let* ((model    (send treeview (get-model)))
	 ;; column for fixed toggles
	 (renderer1 (send <gtk-cell-renderer-toggle> (new)))
	 (column1   (send <gtk-tree-view-column> (new)))
	 ;; column for bug numbers
	 (renderer2 (send <gtk-cell-renderer-text> (new)))
	 (column2   (send <gtk-tree-view-column> (new)))
	 ;; column for severities
	 (renderer3 (send <gtk-cell-renderer-text> (new)))
	 (column3   (send <gtk-tree-view-column> (new)))
	 ;; column for description
	 (renderer4 (send <gtk-cell-renderer-text> (new)))
	 (column4   (send <gtk-tree-view-column> (new)))
	 )
    (send renderer1
      (connect 'toggled (lambda (w p)
                          (fixed-toggled model p))))

    (send column1
      (set 'title "Fixed?" 
           ;; set this column to a fixed sizing (of 50 pixels)
           'sizing 'fixed
           'fixed-width 50)
      (pack-start renderer1 #f)
      (add-attribute renderer1 "active" 0))

    (send column2
      (set 'title "Bug number")
      (pack-start renderer2 #f)
      (add-attribute renderer2 "text" 1)
      (set-sort-column-id 1))

    (send column3
      (set 'title "Severity")
      (pack-start renderer3 #f)
      (add-attribute renderer3 "text" 2)
      (set-sort-column-id 2))
    
    (send column4
      (set 'title "Description")
      (pack-start renderer4 #f)
      (add-attribute renderer4 "text" 3)
      (set-sort-column-id 4))
    
    (send treeview
      (append-column column1)
      (append-column column2)
      (append-column column3)
      (append-column column4))))

(define (main)
  (let* (
	 ;; create window, etc
	 (window   (send <gtk-window> (new 'toplevel)))
	 (vbox     (send <gtk-v-box> (new #f 8)))
	 (label    (send <gtk-label>
                     (new (string-append 
                           "This is the bug list (note: not based on real "
                           "data, it would be nice to have a nice ODBC "
                           "interface to bugzilla or so, though)."))))
	 (sw       (send <gtk-scrolled-window> (new #f #f)))
	 ;; create list store
	 (model    (send <gtk-list-store> (newv (list 'boolean
                                                      'uint
                                                      'utf8
                                                      'utf8))))
	 ;; create tree view
	 (treeview (send <gtk-tree-view> (new))))
    
    (populate-model model)

    (send window (add vbox))

    (send vbox
      (pack-start label #f #f 0)
      (pack-start sw #t #t 0))

    (send sw
      (set 'hscrollbar-policy 'never
           'vscrollbar-policy 'automatic
           'shadow-type 'etched-in)
      (add treeview))

    (send treeview
      (set-model model)
      (set 'rules-hint #t 'search-column 3))
    ;; add columns to the tree view
    (add-columns treeview)

    (send window
      (set 'title "GtkListStore demo"
           'default-width 280 'default-height 250 'border-width 8)
      (show-all))))

(define name "Tree View/List Store")
(define description
  (string-append
   "The GtkListStore is used to store data in list form, to be used "
   "later on by a GtkTreeView to display it. This demo builds a "
   "simple GtkListStore and displays it. See the Stock Browser "
   "demo for a more advanced example."))
)
