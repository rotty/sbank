;;; hello-world2.sps --- Example translated from the GTK+ tutorial

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

(define (signal-handler w message)
  (println "Hello again -- " message " was pressed"))

(gtk-init (command-line))

(let ((window (send <gtk-window> (new 'toplevel)))
      (box1 (send <gtk-h-box> (new #f 0)))
      (button1 (send <gtk-button> (new-with-label "Button 1")))
      (button2 (send <gtk-button> (new-with-label "Button 2"))))

  (send box1
        (pack-start button1 #t #t 0)
        (pack-start button2 #t #t 0))
  
  (send button1 (connect 'clicked (lambda (b) (signal-handler b "button 1"))))
  (send button2 (connect 'clicked (lambda (b) (signal-handler b "button 2"))))
  
  (send window
        (connect 'delete-event (lambda (widget event)
                                 (gtk-main-quit)
                                 #f))
        (set-border-width 10)
        (add box1)
        (show-all)))

(gtk-main)

