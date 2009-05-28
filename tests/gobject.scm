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

(define-test-suite gobject-tests
  "GObject")

(define-test-case gobject-tests oo ()
  (let* ((<gtk-widget>
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
         (<gtk-window>
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
         (<gtk-button>
          (make-gobject-class
           "Gtk" "Button" #f
           (lambda (class)
             (values
              <gtk-widget>
              '()
              `((new . ,(lambda () (make-ginstance class 'new-button))))
              '()
              '()
              '())))))

    (test-equal 'show-result
      (send (send <gtk-window> (new)) (show)))

    (let ((button (send <gtk-button> (new)))
          (window (send <gtk-window> (new))))
      (test-eqv #t (ginstance-is-a? button <gtk-button>))
      (test-eqv #t (ginstance-is-a? window <gtk-widget>))
      (test-eqv #f (ginstance-is-a? button <gtk-window>)))))

(define-test-suite (gobject-tests.gvalue gobject-tests)
  "GValue")

(define-test-case gobject-tests.gvalue int ((setup (g-type-init)))
  (let ((int-gv (g-value-new 'int)))
    (g-value-set! int-gv 42)
    (test-equal 42 (g-value-ref int-gv))))

(define-test-case gobject-tests.gvalue bool ((setup (g-type-init)))
  (let ((bool-gv (->g-value #f 'boolean)))
    (test-equal #f (g-value-ref bool-gv))))

(define-test-case gobject-tests.gvalue boxed ((setup (g-type-init)))
  (let ((boxed-gv (->g-value (list 'foo 42) 'boxed)))
    (test-equal (list 'foo 42) (g-value-ref boxed-gv))))

(define-test-case gobject-tests.gvalue boxed ((setup (g-type-init)))
  (let ((string-gv (->g-value "FooBar" 'string)))
    (test-equal "FooBar" (g-value-ref string-gv))))

(define-test-suite (gobject-tests.enums gobject-tests)
  "Enums")

(define-test-case gobject-tests.enums lookup ()
  (let ((enum1 (make-genum #f '((foo . 1) (bar . 2) (baz . 3)))))
    (test-equal '(2 1 3)
      (map (lambda (name) (genumerated-lookup enum1 name)) '(bar foo baz)))
    (test-equal '(baz bar foo)
      (map (lambda (i) (genumerated-lookup enum1 i)) '(3 2 1)))))

(define-test-suite (gobject-tests.flags gobject-tests)
  "Flags")

(define test-gflags
  (make-gflags #f '((good . 1) (bad . 2) (ugly . 4))))

(define-test-case gobject-tests.flags gflags->integer ()
  (test-equal '(1 5 6 0)
    (map (lambda (flags) (gflags->integer test-gflags flags))
         '((good) (good ugly) (bad ugly) ()))))

(define-test-case gobject-tests.flags integer->gflags ()
  (test-equal '((good bad ugly)
                (bad ugly)
                (bad)
                (good)
                ())
    (map (lambda (i) (integer->gflags test-gflags i)) '(7 6 2 1 0))))

(run-test-suite gobject-tests)

;; Local Variables:
;; scheme-indent-styles: (trc-testing)
;; End:
