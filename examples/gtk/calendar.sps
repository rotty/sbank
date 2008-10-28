(import (rnrs)
        (sbank typelib)
        (sbank gobject))

(install-gobject-decorators)
(typelib-import (prefix (only ("Gtk" #f)
                              <window> <calendar>
                              init main main-quit) gtk-))

(gtk-init (command-line))

(let* ((window (send <gtk-window> (new 'toplevel)))
       (calendar (send <gtk-calendar> (new))))
  (send window
        (connect 'delete-event (lambda (w e) (gtk-main-quit) #f))
        (add calendar)
        (show-all)))

(gtk-main)


