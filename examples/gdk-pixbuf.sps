;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software. It comes without any warranty, to the
;; extent permitted by applicable law. You can redistribute it and/or
;; modify it under the terms of the Do What The Fuck You Want To Public
;; License, Version 2, as published by Sam Hocevar. See
;; http://sam.zoy.org/wtfpl/COPYING for more details.

#!r6rs

(import (rnrs)
        (sbank gobject)
        (sbank typelib))

(typelib-import (only ("GdkPixbuf" #f) <pixbuf>))

(define (main argv)
  (unless (= (length argv) 2)
    (println "Usage: " (car argv) " <image-file>")
    (exit #f))
  (let ((p (send <pixbuf> (new-from-file (cadr argv)))))
    (println "Size of " (cadr argv) " is "
             (send p (get-width)) "x" (send p (get-height)))))

(define (println . args)
  (for-each display args)
  (newline))

(main (command-line))
