(library (sbank gtk)
  (export send gtk-stock-id install-gtk-decorators)
  (import (rnrs base)
          (rnrs control)
          (spells tracing)
          (sbank typelib)
          (sbank typelib decorators)
          (sbank gobject))

  (define (text-buffer-decorator class)
    (gobject-class-decorate class
                            values
                            (gobject-method-overrider `((create-tag . ,text-buffer-create-tag)))
                            values))

  (trace-define (text-buffer-create-tag text-buffer tag-name . properties)
    (typelib-import (only ("Gtk" #f) <text-tag>))
    (let ((tag (send <text-tag> (new tag-name))))
      (apply send-message tag 'set properties)
      (send (send text-buffer (get 'tag-table)) (add tag))
      tag))
  
  (define install-gtk-decorators
    (let ((installed? #f))
      (lambda ()
        (unless installed?
          (install-gobject-decorators)
          (register-typelib-decorator "Gtk" "TextBuffer" text-buffer-decorator)
          (set! installed? #t)))))
  
  (define (gtk-stock-id nick)
    (string-append "gtk-" (symbol->string nick))))
