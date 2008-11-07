(library (sbank gtk)
  (export send gtk-stock-id gtk-setup!)
  (import (rnrs base)
          (rnrs control)
          (spells tracing)
          (spells receive)
          (spells foreign)
          (sbank ctypes simple)
          (sbank typelib)
          (sbank typelib decorators)
          (sbank gobject gvalue)
          (sbank gobject))

  (typelib-import (only ("Gtk" #f) <tree-iter> <text-iter>))

  (define (text-buffer-decorator class)
    (gobject-class-decorate
     class
     values
     (gobject-method-overrider `((create-tag . ,text-buffer-create-tag)
                                 (get-bounds . ,text-buffer-get-bounds)
                                 (get-iter-at-offset . ,text-buffer-get-iter-at-offset)))
     values))

  (define (tree-model-decorator class)
    (gobject-class-decorate class
                            values
                            (gobject-method-overrider `((get-iter . ,tree-model-get-iter)
                                                        (get-value . ,tree-model-get-value)))
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
                                                        (set-values . ,list-store-set-values)
                                                        (set-value . ,list-store-set-values)))
                            values))

  (define (list-store-append next-method)
    (case-lambda
      ((store) (let ((iter (send <tree-iter> (alloc))))
                 (next-method store iter)
                 iter))
      ((store iter) (next-method store iter))))

  (define (list-store-set-values next-method)
    (lambda (store iter . cols/vals)
      (let ((n (length cols/vals)))
        (unless (even? n)
          (error 'list-store-set-values "uneven number of colum/value arguments" cols/vals))
        (let ((gvalues (g-value-alloc (/ n 2))))
          (let loop ((cols '())  (cols/vals cols/vals) (i 0))
            (cond ((null? cols/vals)
                   (send store (set-valuesv iter (reverse cols) gvalues))
                   (free gvalues))
                  (else
                   (let ((col (car cols/vals)))
                     (g-value-set! (pointer+ gvalues (* i g-value-size))
                                   (cadr cols/vals)
                                   (send store (get-column-type col)))
                     (loop (cons col cols) (cddr cols/vals) (+ i 1))))))))))

  (define (text-buffer-create-tag next-method)
    (lambda (text-buffer tag-name . properties)
      (typelib-import (only ("Gtk" #f) <text-tag>))
      (let ((tag (send <text-tag> (new tag-name))))
        (apply send-message tag 'set properties)
        (send (send text-buffer (get 'tag-table)) (add tag))
        tag)))

  (define (text-buffer-get-bounds next-method)
    (lambda (text-buffer)
      (let ((start (send <text-iter> (alloc)))
            (end (send <text-iter> (alloc))))
        (next-method text-buffer start end)
        (values start end))))

  (define (text-buffer-get-iter-at-offset next-method)
    (lambda (text-buffer offset)
      (let ((iter (send <text-iter> (alloc))))
        (next-method text-buffer iter offset)
        iter)))

  (define (tree-model-get-iter next-method)
    (lambda (tree-model path)
      (let ((iter (send <tree-iter> (alloc))))
        (cond ((next-method tree-model iter path) iter)
              (else
               (send iter (free))
               #f)))))

  (define (tree-model-get-value next-method)
    (lambda (tree-model iter column)
      (let ((gvalue (g-value-alloc 1)))
        (next-method tree-model iter column gvalue)
        (g-value-ref gvalue #f))))

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
