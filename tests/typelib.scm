(testeez "Header"
  (test-define "GLib typelib" glib (open-typelib "GLib" #f 0))
  (test/equal "magic"
    (typelib-magic glib)
    (string->utf8 "GOBJ\nMETADATA\r\n\x1a;")))
