;; Copyright (C) 2008 Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software. It comes without any warranty, to the
;; extent permitted by applicable law. You can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want To Public
;; License, Version 2, as published by Sam Hocevar. See
;; http://sam.zoy.org/wtfpl/COPYING for more details.

#!r6rs

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


