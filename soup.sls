(library (sbank soup)
  (export send soup-setup! soup-status)
  (import (rnrs base)
          (rnrs control)
          (sbank gobject)
          (sbank typelib))

  (typelib-import (only ("Soup" #f) <known-status-code>))
  
  (define (soup-status code)
    (if (symbol? code)
        (genum-lookup <known-status-code> code)
        code))
  
  (define soup-setup!
    (let ((installed? #f))
      (lambda ()
        (unless installed?
          (gobject-setup!)
          (set! installed? #t))))))
