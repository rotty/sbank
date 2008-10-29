(library (sbank gtk)
  (export send gtk-stock-id)
  (import (rnrs base)
          (sbank gobject))
  
  (define (gtk-stock-id nick)
    (string-append "gtk-" (symbol->string nick))))
