#!r6rs

(library (sbank gdk)
  (export send gdk-setup!)
  (import (rnrs base)
          (rnrs control)
          (srfi :8 receive)
          (sbank support utils)
          (sbank typelib decorators)
          (sbank gobject))

  (define (color-parse-decorator func)
    (lambda (str)
      (receive (success? color) (func str)
        (and success? color))))

  (define-setup-procedure (gdk-setup!)
    (gobject-setup!)
    (register-typelib-decorator "Gdk" "color_parse" color-parse-decorator))
  
  )
