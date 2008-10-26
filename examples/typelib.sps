;;; typelib.sps --- Example for using typelib

;; Copyright (C) 2008 Andreas Rottmann <a.rottmann@gmx.at>

;; This file is placed in the public domain.

#!r6rs
(import (rnrs base)
        (rnrs control)
        (rnrs io simple)
        (rnrs programs)
        (only (ikarus) collect nanosleep)
        (spells time-it)
        (spells receive)
        (sbank typelib)
        (sbank gobject)
        (spells foreign))

(install-gobject-decorators)
(typelib-import (prefix (only ("Gtk" #f) <window> main init-check) gtk-))

(receive (ok? args) (gtk-init-check (list->vector (command-line)))
  (println "gtk-init: ok: " ok? ", remaining args: " args)
  (or ok? (exit #f)))

(let ((w (send <gtk-window> (new 'toplevel))))
  (send w (show)))

(gtk-main)

(define (println . args)
  (for-each display args)
  (newline))

