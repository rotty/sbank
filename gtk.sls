#!r6rs

(library (sbank gtk)
  (export send gtk-stock-id gtk-setup!)
  (import (rnrs base)
          (rnrs control)
          (spells tracing)
          (only (spells assert) cout)
          (srfi :8 receive)
          (spells foreign)
          (sbank support utils)
          (sbank ctypes basic)
          (sbank typelib)
          (sbank typelib base)
          (sbank typelib decorators)
          (sbank gobject gvalue)
          (sbank gobject)
          (sbank gdk))

  (typelib-import (only ("Gtk" #f) <tree-iter> <text-iter> <tree-path>))

  (define (text-buffer-decorator typelib class)
    (gobject-class-decorate
     class
     values
     (gobject-method-overrider
      typelib
      `((create-tag . ,text-buffer-create-tag)
        (get-bounds . ,text-buffer-get-bounds)
        (get-iter-at-offset . ,text-buffer-get-iter-at-offset)))
     values))

  (define (tree-model-decorator typelib class)
    (gobject-class-decorate
     class
     values
     (gobject-method-overrider typelib
                               `((get-iter . ,tree-model-get-iter)
                                 (get-value . ,tree-model-get-value)))
     values))

  (define (tree-selection-decorator typelib class)
    (gobject-class-decorate class
                            values
                            (gobject-method-overrider
                             typelib
                             `((get-selected . ,tree-selection-get-selected)))
                            values))

  (define (list-store-decorator typelib class)
    (gobject-class-decorate
     class
     values
     (gobject-method-overrider typelib
                               `((append . ,list-store-append)
                                 (set-values . ,list-store-set-values)
                                 (set-value . ,list-store-set-values)))
     values))

  (define (list-store-append typelib next-method)
    (case-lambda
      ((store) (let ((iter (send <tree-iter> (alloc))))
                 (next-method store iter)
                 iter))
      ((store iter) (next-method store iter))))

  (define (list-store-set-values typelib next-method)
    (lambda (store iter . cols/vals)
      (let ((n (length cols/vals)))
        (when (odd? n)
          (error 'list-store-set-values
                 "odd number of colum/value arguments" cols/vals))
        ;; note that this will be free'd by the array arg cleanup code
        (let ((gvalues (g-value-alloc (/ n 2))))
          (let loop ((cols '())  (cols/vals cols/vals) (i 0))
            (cond ((null? cols/vals)
                   (send store (set-valuesv iter (reverse cols) gvalues)))
                  (else
                   (let* ((col (car cols/vals))
                          (gv (pointer+ gvalues (* i g-value-size)))
                          (gtype (send store (get-column-type col))))
                     (g-value-init! gv gtype)
                     (g-value-set! gv (cadr cols/vals))
                     (loop (cons col cols) (cddr cols/vals) (+ i 1))))))))))

  (define (text-buffer-create-tag typelib next-method)
    (lambda (text-buffer tag-name . properties)
      (typelib-import (only ("Gtk" #f) <text-tag>))
      (let ((tag (send <text-tag> (new tag-name))))
        (apply send-message tag 'set properties)
        (send (send text-buffer (get 'tag-table)) (add tag))
        tag)))

  (define (text-buffer-get-bounds typelib next-method)
    (lambda (text-buffer)
      (let ((start (send <text-iter> (alloc)))
            (end (send <text-iter> (alloc))))
        (next-method text-buffer start end)
        (values start end))))

  (define (text-buffer-get-iter-at-offset typelib next-method)
    (lambda (text-buffer offset)
      (let ((iter (send <text-iter> (alloc))))
        (next-method text-buffer iter offset)
        iter)))

  (define (tree-model-get-iter typelib next-method)
    (lambda (tree-model path)
      (let ((iter (send <tree-iter> (alloc)))
            (path (cond ((string? path)
                         (send <tree-path> (new-from-string path)))
                        (else
                         path))))
        (cond ((next-method tree-model iter path) iter)
              (else
               (send iter (free))
               #f)))))

  (define (tree-model-get-value typelib next-method)
    (lambda (tree-model iter column)
      (let ((gvalue (g-value-alloc 1)))
        (next-method tree-model iter column gvalue)
        (g-value-ref gvalue))))

  (define (tree-selection-get-selected typelib next-method)
    (lambda (tree-selection)
      (let ((iter (send <tree-iter> (alloc))))
        (receive (selected? model) (next-method tree-selection iter)
          (cond (selected? (values model iter))
                (else
                 (send iter (free))
                 (values model #f)))))))

  (define-setup-procedure (gtk-setup!)
    (gdk-setup!)
    (register-typelib-decorator "Gtk" "ListStore" list-store-decorator)
    (register-typelib-decorator "Gtk" "TextBuffer" text-buffer-decorator)
    (register-typelib-decorator "Gtk" "TreeModel" tree-model-decorator)
    (register-typelib-decorator "Gtk" "TreeSelection"
                                tree-selection-decorator))

  (define (gtk-stock-id nick)
    (string-append "gtk-" (symbol->string nick))))
