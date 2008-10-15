(library (sbank typelib stypes)
  (export typelib-stypes)
  (import (rnrs base)
          (rnrs control)
          (rnrs lists)
          (rnrs io simple)
          (sbank stypes)
          (sbank typelib gir))
  
  (define typelib-stypes
    (let ((stypes #f))
      (lambda ()
        (unless stypes
          (set! stypes (fold-left stypes-adjoin
                                  primitive-stypes
                                  (append
                                   '((record (name "GError")
                                             (field (name "domain") (type "uint32"))
                                             (field (name "code") (type "int"))
                                             (field (name "message")
                                                    (type (array (element-type (type "char")))))))
                                   (call-with-input-file "../../systems/sbank/typelib.xml"
                                     gir-xml->stype-list)))))
        stypes))))