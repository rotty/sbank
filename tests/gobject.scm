;;; gobject.scm --- Unit tests for (sbank gobject)

;; Copyright (C) 2008, 2009 Andreas Rottmann <a.rottmann@gmx.at>

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
        `((new . ,(lambda () (make-ginstance class 'new-window))))
        '()
        '()
        '()))))

  (test-define "button" <gtk-button>
    (make-gobject-class
     "Gtk" "Button" #f
     (lambda (class)
       (values
        <gtk-widget>
        '()
        `((new . ,(lambda () (make-ginstance class 'new-button))))
        '()
        '()
        '()))))

  (test/equal "construct/show"
    (send (send <gtk-window> (new)) (show))
    'show-result)

  (test-define "button instance" button (send <gtk-button> (new)))
  (test-true "is-a? base case" (ginstance-is-a? button <gtk-button>))
  (test-define "window instance" window (send <gtk-window> (new)))
  (test-true "window is a widget" (ginstance-is-a? window <gtk-widget>))
  (test-false "button is no window" (ginstance-is-a? button <gtk-window>)))

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

(testeez "Enums & Flags"
  (test-define "enum1" enum1 (make-genum #f '((foo . 1) (bar . 2) (baz . 3))))
  (test/equal "lookup by name"
    (map (lambda (name) (genumerated-lookup enum1 name)) '(bar foo baz))
    '(2 1 3))
  (test/equal "lookup by value"
    (map (lambda (i) (genumerated-lookup enum1 i)) '(3 2 1))
    '(baz bar foo))
  (test-define "flags1" flags1 (make-gflags #f '((good . 1) (bad . 2) (ugly . 4))))
  (test/equal "gflags->integer"
    (map (lambda (flags) (gflags->integer flags1 flags))
         '((good) (good ugly) (bad ugly) ()))
    '(1 5 6 0))
  (test/equal "integer->gflags"
    (map (lambda (i) (integer->gflags flags1 i)) '(7 6 2 1 0))
    '((good bad ugly)
      (bad ugly)
      (bad)
      (good)
      ())))
