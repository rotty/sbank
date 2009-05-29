(define-system sbank
  (dependencies spells)
  
  (r6rs-libs ".")

  (conjure
   (import (rnrs)
           (only (spells filesys)
                 create-symbolic-link)
           (spells pathname)
           (conjure dsl))

   (task (configure
          (depends 'data-symlink)
          (produce '((("sbank") "glib.sls") <= "glib.sls.in")
                   '((("sbank") "gtk.sls") <= "gtk.sls.in")
                   '((("sbank") "soup.sls") <= "soup.sls.in"))
          (fetchers (procedure-from-environment/lazy
                     (typelib-fetcher)
                     (sbank support conjure)))))

   (task data-symlink
         (file
          (product '(("sbank") "data"))
          (proc (lambda (step)
                  (create-symbolic-link
                   (pathname-join '((back) () #f)
                                  ((step 'project) 'source-dir) "data")
                   '(("sbank") "data"))))))))
