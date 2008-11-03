(library (sbank gtk)
  (export send gtk-stock-id install-gtk-decorators)
  (import (rnrs base)
          (rnrs control)
          (spells tracing)
          (spells receive)
          (sbank typelib)
          (sbank typelib decorators)
          (sbank gobject))

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
      (typelib-import (only ("Gtk" #f) <tree-iter>))
      (let ((iter (send <tree-iter> (alloc))))
        (receive (selected? model) (next-method tree-selection iter)
          (cond (selected? (values model iter))
                (else
                 (send iter (free))
                 (values model #f)))))))
  
  (define install-gtk-decorators
    (let ((installed? #f))
      (lambda ()
        (unless installed?
          (install-gobject-decorators)
          (register-typelib-decorator "Gtk" "TextBuffer" text-buffer-decorator)
          (register-typelib-decorator "Gtk" "TreeModel" tree-model-decorator)
          (register-typelib-decorator "Gtk" "TreeSelection" tree-selection-decorator)
          (set! installed? #t)))))
  
  (define (gtk-stock-id nick)
    (string-append "gtk-" (symbol->string nick))))
