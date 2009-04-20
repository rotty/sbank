;;; hello-world.sps --- Example translated from the GTK+ tutorial

;; Copyright (C) 2008, 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software. It comes without any warranty, to the
;; extent permitted by applicable law. You can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want To Public
;; License, Version 2, as published by Sam Hocevar. See
;; http://sam.zoy.org/wtfpl/COPYING for more details.

#!r6rs

(import (rnrs)
        (sbank gtk))

(define (println . args)
  (for-each display args)
  (newline))

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
                                 ;; returning #t to prevent further
                                 ;; propogation of this signal...
                                 #t))
        (connect 'destroy (lambda (widget) (gtk-main-quit)))
        (set-border-width 10)
        (add button)
        (show)))

(gtk-main)
