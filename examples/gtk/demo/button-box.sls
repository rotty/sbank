;; Copyright (C) 2008 Andreas Rottmann
;; Copyright (C) 2004 Patrick Bernaud
;; GNU General Public License version 2 or later. No warrantee.
#!r6rs

(library (sbank examples gtk demo button-box)
  (export name main description)
  (import (rnrs)
          (sbank typelib)
          (sbank gtk))

  (typelib-import (prefix (only ("Gtk" #f)
                                <window> <frame> <button>
                                <v-box> <h-box>
                                <h-button-box> <v-button-box>)
                          gtk-))

  (define (create-bbox horizontal title spacing layout)
    (let ((frame (send <gtk-frame> (new title)))
          (bbox  (send (if horizontal
                           <gtk-h-button-box>
                           <gtk-v-button-box>)
                   (new))))
      (send bbox
            (set-layout layout)
            (set-spacing spacing)
            (set-border-width 5))
    
      (send frame (add bbox))
    
      (for-each
       (lambda (s)
         (send bbox (add (send <gtk-button> (new-from-stock (gtk-stock-id s))))))
       '(ok cancel help))
    
      frame))

  (define (main)
    (let ((window    (send <gtk-window> (new 'toplevel)))
          (mainvbox  (send <gtk-v-box> (new #f 0)))
          (framehorz (send <gtk-frame> (new "Horizontal Button Boxes")))
          (vbox      (send <gtk-v-box> (new #f 0)))
          (framevert (send <gtk-frame> (new "Vertical Button Boxes")))
          (hbox      (send <gtk-h-box> (new #f 0))))

      (send window
        (set-title "Button Boxes")
        (set-border-width 10)
        (add mainvbox))

      (send mainvbox (pack-start framehorz #t #t 10))
      (send framehorz (add vbox))


      (send vbox
        (set-border-width 10)
        (pack-start (create-bbox #t "Spread" 40 'spread) #t #t 0)
        (pack-start (create-bbox #t "Edge" 40 'edge)     #t #t 5)
        (pack-start (create-bbox #t "Start" 40 'start)   #t #t 5)
        (pack-start (create-bbox #t "End" 40 'end)       #t #t 5))

      (send mainvbox (pack-start framevert #t #t 10))
      (send framevert (add hbox))

      (send hbox
        (set-border-width 10)
        (pack-start (create-bbox #f "Spread" 30 'spread)  #t #t 0)
        (pack-start (create-bbox #f "Edge" 30 'edge)      #t #t 5)
        (pack-start (create-bbox #f "Start" 30 'start)    #t #t 5)
        (pack-start (create-bbox #f "End" 30 'end)        #t #t 5))

      (send window (show-all))))


  (define name "Button Boxes")
  (define description
    (string-append
     "The Button Box widgets are used to arrange buttons with padding.")))
