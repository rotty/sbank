;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software. It comes without any warranty, to the
;; extent permitted by applicable law. You can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want To Public
;; License, Version 2, as published by Sam Hocevar. See
;; http://sam.zoy.org/wtfpl/COPYING for more details.

#!r6rs

(import (rnrs)
        (sbank typelib)
        (sbank glib)
        (sbank glib-daemon))

(typelib-import (prefix (only ("GLib" #f)
                              main-loop-new
                              main-loop-quit
                              main-loop-run
                              timeout-add)
                        g-)
                (setup glib-setup!))

(let ((main-loop (g-main-loop-new #f #f)))
  (g-install-signal-handler
   '(int)
   (lambda (sig)
     (for-each display
               (list "Received signal " sig ", terminating...\n"))
     (g-main-loop-quit main-loop)
     #f))
  (display "Launching timeout every 500ms. Press Ctrl+C to quit.\n")
  (g-timeout-add 500
                 (let ((count 1))
                   (lambda ()
                     (for-each display
                               (list "Timeout handler called: " count "\n"))
                     (set! count (+ count 1))
                     #t)))
  (g-main-loop-run main-loop))
