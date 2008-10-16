#!r6rs
(import (rnrs base)
        (rnrs lists)
        (rnrs io simple)
        (spells pretty-print)
        (spells time-it)
        (sbank typelib))

(time-it (let ((glib (open-typelib "GLib" #f 0)))
           (for-each (lambda (name)
                        (typelib-get-entry glib name)) ; (println name ": ")
                     (typelib-get-entry-names glib))))

(define (println . args)
  (for-each display args)
  (newline))

#|
(let ((types '(types
               (primitive (name "char") (size 1))
               (primitive (name "uint8") (size 1)))))
  (pretty-print ((node-join (select-kids
                             (lambda (node)
                               (let ((names ((select-kids (ntype?? 'name)) node)))
                                 (equal? names '((name "uint8")))))))
                 types)))

|#

