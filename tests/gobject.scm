(testeez "basic"
  (test-define "widget" <gtk-widget>
               (make-gobject-class "Gtk" "Widget" #f
                                   '()
                                   `((show . ,(lambda (inst)
                                                (list 'show: inst))))))
  (test-define "window" <gtk-window>
               (make-gobject-class "Gtk" "Window" <gtk-widget>
                                   `((new . ,(lambda () 'new-widget)))
                                   `()))
  (test/equal "construct/show"(send (send <gtk-window> (new)) (show)) (list 'show: 'new-widget)))
