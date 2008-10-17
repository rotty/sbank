(library (sbank typelib)
  (export typelib-import)
  (import (rnrs base)
          (for (sbank typelib expanders) expand))
         
  (define-syntax typelib-import (typelib-import-expander 'typelib-import)))
