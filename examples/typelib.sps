#!r6rs
(import (rnrs base)
        (rnrs control)
;;         (rnrs lists)
        (rnrs io simple)
        (only (ikarus) collect nanosleep)
;;         (spells pretty-print)
        (spells time-it)
        (spells receive)
;;         (sbank typelib base)
        (sbank typelib)
        (sbank typelib gobject)
        (spells foreign))

(typelib-import (prefix (only ("Gtk" #f) <window> main init) gtk-))

#|
(let* ((libgtk (dlopen "libgtk-x11-2.0.so"))
       (gtk-init ((make-c-callout 'void '(pointer pointer)) (dlsym libgtk "gtk_init"))))
  (gtk-init (integer->pointer 0) (integer->pointer 0)))
|#

(gtk-init #f #f)

(let ((w (send <gtk-window> (new 'toplevel))))
  (send w (show)))

(gtk-main)

(define (println . args)
  (for-each display args)
  (newline))

