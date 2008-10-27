(import (rnrs)
        (sbank gobject)
        (sbank typelib))

(typelib-import (only ("GdkPixbuf" #f) <pixbuf>))

(define (main argv)
  (when (not (= (length argv) 2))
    (println "Usage: " (car argv) " <image-file>")
    (exit #f))
  (let ((p (send <pixbuf> (new-from-file (cadr argv)))))
    (println "Size of " (cadr argv) " is " (send p (get-width)) "x" (send p (get-height)))))

(define (println . args)
  (for-each display args)
  (newline))

(main (command-line))