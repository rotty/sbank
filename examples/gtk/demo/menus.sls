;; Copyright (C) 2008 Andreas Rottmann
;; Copyright (C) 2004 Patrick Bernaud
;; GNU General Public License version 2 or later. No warrantee.
#!r6rs

(library (sbank examples gtk demo menus)
  (export main name description)
  (import (rnrs base)
          (spells format)
          (sbank gobject)
          (sbank gtk)
          (sbank typelib))

  (typelib-import (prefix (only ("Gtk" #f)
                                <accel-group>
                                <button>
                                <menu> <menu-bar> <menu-item>
                                <radio-menu-item> <tearoff-menu-item>
                                <window>
                                <v-box>)
                          gtk-))

  (define (main)

    (define (create-menu depth tearoff)
      (and (>= depth 1)
           (let ((menu  (send <gtk-menu> (new))))
             (if tearoff (send menu (append (send <gtk-tearoff-menu-item> (new)))))
             (let loop ((i     0)
                        (group #f))
               (let ((menuitem
                      (send <gtk-radio-menu-item>
                        (new-with-label group
                                        (format #f "item ~A - ~A" depth (+ 1 i))))))
                 (send menu (append menuitem))
                 (send menuitem (set-sensitive (not (eq? i 3))))
                 (if (> depth 1)
                     (send menuitem (set-submenu 
                                     (create-menu (- depth 1) tearoff))))
                 (if (< i 4)
                     (loop (+ i 1) (send menuitem (get-group))))))
             menu)))
    
    (let ((window     (send <gtk-window> (new 'toplevel)))
          (accelgroup (send <gtk-accel-group> (new)))
          (box1       (send <gtk-v-box> (new #f 0)))
          (menubar    (send <gtk-menu-bar> (new)))
          (box2       (send <gtk-v-box> (new #f 10)))
          (button     (send <gtk-button> (new-with-label "close"))))

      (send window
        (set 'title "menus"
             'border-width 0)
        (connect 'delete-event (lambda (w e) #t))
        (add-accel-group accelgroup)
        (add box1))

      (send box1 (pack-start menubar #f #t 0))
      
      (for-each 
       (lambda (m)
         (let ((menu       (create-menu (cdr m) #t))
               (menuitem   (send <gtk-menu-item> (new-with-label (car m)))))
           (send menuitem
             (set-submenu menu)
             (set-right-justified (string=? (car m) "bar")))
           (send menubar (append menuitem))))
       '(("test\nline2" . 2) 
         ("foo" . 3) 
         ("bar" . 4)))

      (send box1 (pack-start box2 #f #t 0))
      (send button (connect 'clicked (lambda (w)
                                       (send window (destroy)))))
      (send box2
        (set 'border-width 10)
        (pack-start button #t #t 0))
      
      ;;    GTK_WIDGET_SET_FLAGS (button, GTK_CAN_DEFAULT);
      ;;    (grab-default button)
      (send window (show-all))))


  (define name "Menus")
  (define description
    (string-append
     "There are several widgets involved in displaying menus. The"
     "GtkMenuBar widget is a horizontal menu bar, which normally appears"
     "at the top of an application. The GtkMenu widget is the actual menu"
     "that pops up. Both GtkMenuBar and GtkMenu are subclasses of"
     "GtkMenuShell; a GtkMenuShell contains menu items"
     "(GtkMenuItem). Each menu item contains text and/or images and can"
     "be selected by the user."
     "\n"
     "There are several kinds of menu item, including plain GtkMenuItem,"
     "GtkCheckMenuItem which can be checked/unchecked, GtkRadioMenuItem"
     "which is a check menu item that's in a mutually exclusive group,"
     "GtkSeparatorMenuItem which is a separator bar, GtkTearoffMenuItem"
     "which allows a GtkMenu to be torn off, and GtkImageMenuItem which"
     "can place a GtkImage or other widget next to the menu text."
     "\n"
     "A GtkMenuItem can have a submenu, which is simply a GtkMenu to pop"
     "up when the menu item is selected. Typically, all menu items in a menu bar"
     "have submenus."
     "\n"
     "GtkUIManager provides a higher-level interface for creating menu bars"
     "and menus; while you can construct menus manually, most people don't"
     "do that. There's a separate demo for GtkUIManager."))
  )
