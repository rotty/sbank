(define-system sbank
  (dependencies spells)
  
  (conjure
   (import (rnrs)
           (only (spells filesys)
                 create-symbolic-link)
           (spells pathname)
           (conjure dsl))

   (define (typelib-fender namespace)
     (lambda ()
       (let ((available? (procedure-from-environment/lazy typelib-available?
                                                          (sbank typelib base))))
         (available? namespace))))

   (task (configure
          (depends 'data-symlink)
          (produce '((("sbank") "config.sls") <= "config.sls.in")
                   '((("sbank") "glib.sls") <= "glib.sls.in")
                   `((("sbank") "gtk.sls") <= "gtk.sls.in"
                     (? ,(typelib-fender "Gtk")))
                   `((("sbank") "soup.sls") <= "soup.sls.in"
                     (? ,(typelib-fender "Soup"))))
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
