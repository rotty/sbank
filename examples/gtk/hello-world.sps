;;; typelib.sps --- Example for using typelib

;; Copyright (C) 2008 Andreas Rottmann <a.rottmann@gmx.at>

;; This file is placed in the public domain.

#!r6rs
(import (rnrs)
        (sbank typelib)
        (sbank gobject))

(install-gobject-decorators)
(typelib-import (prefix (only ("Gtk" #f)
                              <window> <button>
                              init main main-quit) gtk-))

(gtk-init (command-line))

(let ((window (send <gtk-window> (new 'toplevel)))
      (button (send <gtk-button> (new-with-label "Hello World"))))
  (send button
        (connect 'clicked (lambda (widget)
                            (println "Hello World")))
        (connect 'clicked (lambda (widget)
                            (send window (destroy))))
        (show))
  (send window
        (connect 'delete-event (lambda (widget event)
                                 (println "delete-event: " widget " " event)
                                 #t))
        (connect 'destroy (lambda (widget) (gtk-main-quit)))
        (set-border-width 10)
        (add button)
        (show)))

(gtk-main)

(define (println . args)
  (for-each display args)
  (newline))

