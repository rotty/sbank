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
