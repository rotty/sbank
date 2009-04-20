(define-system sbank
  (dependencies spells)
  
  (r6rs-libs ".")

  (conjure
   (import (rnrs)
           (conjure dsl)
           (sbank support conjure))

   (task (configure
          (produce '((("sbank") "glib.sls") <= "glib.sls.in")
                   '((("sbank") "gtk.sls") <= "gtk.sls.in")
                   '((("sbank") "soup.sls") <= "soup.sls.in"))
          (fetchers (typelib-fetcher))))))
