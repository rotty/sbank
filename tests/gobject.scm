;;; gobject.scm --- Unit tests for (sbank gobject)

;; Copyright (C) 2008 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This library is free software; you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public
;; License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

;; This library is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public
;; License along with this library. If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(testeez "object system"
  (test-define "widget" <gtk-widget>
               (make-gobject-class "Gtk" "Widget" #f
                                   (lambda (class)
                                     (values
                                      #f
                                      '()
                                      '()
                                      `((show . ,(lambda (inst)
                                                   'show-result)))
                                      '()
                                      '()))))
  (test-define "window" <gtk-window>
    (make-gobject-class
     "Gtk" "Window" #f
     (lambda (class)
       (values
        <gtk-widget>
        '()
        `((new . ,(lambda () (make-ginstance class 'new-widget))))
        '()
        '()
        '()))))
  (test/equal "construct/show"
    (send (send <gtk-window> (new)) (show))
    'show-result))

(g-type-init)
(testeez "GValue"
  (test-define "creating (int)" int-gv (g-value-new 'int))
  (test-eval "setting (int)" (g-value-set! int-gv 42))
  (test/equal "getting (int)" (g-value-ref int-gv) 42)

  (test-define "creating/setting (bool)" bool-gv (->g-value #f 'boolean))
  (test/equal "getting (bool)" (g-value-ref bool-gv) #f)

  (test-define "creating/setting (boxed)"
      boxed-gv (->g-value (list 'foo 42) 'boxed))
  (test/equal "getting (boxed)" (g-value-ref boxed-gv) (list 'foo 42))

  (test-define "creating/setting (string)" string-gv (->g-value "FooBar" 'string))
  (test/equal "getting (string)" (g-value-ref string-gv) "FooBar"))
