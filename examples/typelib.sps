#!r6rs
(import (rnrs base)
        (rnrs lists)
        (rnrs io simple)
        (spells pretty-print)
        (spells time-it)
        (spells receive)
        (sbank typelib))

(let* ((tl (time-it (open-typelib "Gtk" #f 0)))
       (names (typelib-get-entry-names tl)))
  (time-it (for-each (lambda (name)
                       (typelib-get-entry tl name))
                     names))
  (println "number of entries: " (length names))
  (receive (deprecated? setter? getter? constructor? wraps-vfunc? index name)
      (typelib-get-entry tl "main")
    (println "Gtk.main name=" name " index=" index)))

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

