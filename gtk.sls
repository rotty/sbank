(library (sbank gtk)
  (export send gtk-stock-id gtk-setup!)
  (import (rnrs base)
          (rnrs control)
          (spells tracing)
          (spells receive)
          (sbank typelib)
          (sbank typelib decorators)
          (sbank gobject))

  (typelib-import (only ("Gtk" #f) <tree-iter>))
  
  (define (text-buffer-decorator class)
    (gobject-class-decorate class
                            values
                            (gobject-method-overrider `((create-tag . ,text-buffer-create-tag)))
                            values))

  (define (tree-model-decorator class)
    (gobject-class-decorate class
                            values
                            (gobject-method-overrider `((get-iter . ,tree-model-get-iter)))
                            values))

  (define (tree-selection-decorator class)
    (gobject-class-decorate class
                            values
                            (gobject-method-overrider
                             `((get-selected . ,tree-selection-get-selected)))
                            values))

  (define (list-store-decorator class)
    (gobject-class-decorate class
                            values
                            (gobject-method-overrider `((append . ,list-store-append)
                                                        (set-values . ,list-store-set-values)))
                            values))

  (define (list-store-append next-method)
    (case-lambda
      ((store) (let ((iter (send <tree-iter> (alloc))))
                 (next-method store iter)
                 iter))
      ((store iter) (next-method store iter))))

  (define (list-store-set-values next-method)
    (lambda (store iter . cols/vals)
      (let loop ((cols '()) (vals '()) (cols/vals cols/vals))
        (cond ((null? cols/vals)
                (send store (set-valuesv iter (reverse cols) (reverse vals))))
              ((pair? (cdr cols/vals))
               (loop (cons (car cols/vals) cols) (cons (cadr cols/vals) vals) (cddr cols/vals)))
              (else
               (error 'list-store-set-values "uneven number of colum/value arguments" cols/vals))))))
  
  (define (text-buffer-create-tag next-method)
    (lambda (text-buffer tag-name . properties)
      (typelib-import (only ("Gtk" #f) <text-tag>))
      (let ((tag (send <text-tag> (new tag-name))))
        (apply send-message tag 'set properties)
        (send (send text-buffer (get 'tag-table)) (add tag))
        tag)))

  (define (tree-model-get-iter next-method)
    (lambda (tree-model path)
      (typelib-import (only ("Gtk" #f) <tree-iter>))
      (let ((iter (send <tree-iter> (alloc))))
        (cond ((next-method tree-model iter path) iter)
              (else
               (send iter (free))
               #f)))))

  (define (tree-selection-get-selected next-method)
    (lambda (tree-selection)
      (let ((iter (send <tree-iter> (alloc))))
        (receive (selected? model) (next-method tree-selection iter)
          (cond (selected? (values model iter))
                (else
                 (send iter (free))
                 (values model #f)))))))
  
  (define gtk-setup!
    (let ((installed? #f))
      (lambda ()
        (unless installed?
          (gobject-setup!)
          (register-typelib-decorator "Gtk" "ListStore" list-store-decorator)
          (register-typelib-decorator "Gtk" "TextBuffer" text-buffer-decorator)
          (register-typelib-decorator "Gtk" "TreeModel" tree-model-decorator)
          (register-typelib-decorator "Gtk" "TreeSelection" tree-selection-decorator)
          (set! installed? #t)))))
  
  (define (gtk-stock-id nick)
    (string-append "gtk-" (symbol->string nick))))
