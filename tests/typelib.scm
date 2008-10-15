(testeez "Header"
  (test-define "GLib typelib" glib (open-typelib "GLib" #f 0))
  (test/equal "magic"
    (memcpy (make-bytevector 16) (typelib-magic glib) 16)
    (string->utf8 "GOBJ\nMETADATA\r\n\x1a;")))
