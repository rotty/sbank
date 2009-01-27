#!r6rs

(library (sbank gdk)
  (export send gdk-setup!)
  (import (rnrs base)
          (rnrs control)
          (srfi :8 receive)
          (sbank typelib)
          (sbank typelib base)
          (sbank typelib decorators)
          (sbank gobject))

  (define (color-parse-decorator func)
    (lambda (str)
      (receive (sucess? color) (func str)
        (and sucess? color))))

  (define gdk-setup!
    (let ((installed? #f))
      (lambda ()
        (unless installed?
          (gobject-setup!)
          (register-typelib-decorator "Gdk" "color_parse" color-parse-decorator)
          (set! installed? #t)))))
  
  )
