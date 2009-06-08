;;; everything.scm --- Tests for the "Everything" namespace bindings

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


(typelib-import ("Everything" #f)
                (setup gobject-setup!))

(define-test-suite everything-tests
  "Everything test typelib")

(define-test-case everything-tests basic-types ()
  (test-equal (list #t #f)
    (list (test-boolean #t) (test-boolean #f))))

(define-test-suite (everything-tests.arrays everything-tests)
  "Arrays")

(define-test-case everything-tests.arrays int ()
  (test-equal
    (+ 41 42 43)
    (test-array-int-in '#(41 42 43)))
  (test-equal
    (+ 44 45 46)
    (test-array-int-in-take '#(44 45 46))))

(define-test-case everything-tests.arrays strv ()
  (test-equal (list #t #f)
    (list (test-strv-in '#("1" "2" "3"))
          (test-strv-in '#("0" "1" "2"))))
  (test-equal '#("1" "2" "3")
    (test-strv-out))
  (test-equal '#("1" "2" "3")
    (test-strv-outarg)))

(define-test-suite (everything-tests.structs everything-tests)
  "Structs")

(define-test-case everything-tests.structs get&set ()
  (let ((a (send <test-struct-a> (alloc))))
    (send a
      (set-some-int 12345)
      (set-some-int8 42)
      (set-some-double 0.3141)
      (set-some-enum 'value2))
    
    (test-equal 12345 (send a (get-some-int)))
    (test-equal 42 (send a (get-some-int8)))
    (test-equal 0.3141 (send a (get-some-double)))
    (test-equal 'value2 (send a (get-some-enum)))

    ;; Disabled until http://bugzilla.gnome.org/show_bug.cgi?id=573314
    ;; is resolved
    #;
    (let ((a-cloned (send a (clone))))
      (test-eqv #t (ginstance? a-cloned))
      (test-equal (list 12345 42 0.3141 'value2)
        (list (send a-cloned (get-some-int))
              (send a-cloned (get-some-int8))
              (send a-cloned (get-some-double))
              (send a-cloned (get-some-enum)))))))

(define-test-case everything-tests.structs obj-member ()
  (let ((obj (send <test-obj> (new*)))
        (c (send <test-struct-c> (alloc))))
    (send c
      (set-another-int 666)
      (set-obj obj))
    (test-equal 666 (send c (get-another-int)))
    (test-compare ginstance=? obj
      (send c (get-obj)))))

(define-test-case everything-tests glist ()
  (test-equal '("1" "2" "3")
    (test-glist-nothing-return)))

(define-test-case everything-tests gslist ()
  (test-gslist-container-in '("1" "2" "3"))
  (test-gslist-everything-in '("1" "2" "3")))

(define-test-suite (everything-tests.gobject everything-tests)
  "GObject features")

(define-test-case everything-tests.gobject bare-member ()
  (let ((obj (send <test-obj> (new*)))
        (other (send <test-obj> (new*))))
    (test-equal #f (send obj (get-bare)))
    (send obj (set-bare other))
    (test-compare ginstance=? other
      (send obj (get-bare)))
    (send obj (set 'bare #f))
    (test-equal #f
      (send obj (get 'bare)))
    (let ((obj2 (send <test-obj> (new* 'bare other))))
      (test-compare ginstance=? other
        (send obj2 (get-bare))))))

(define-test-case everything-tests.gobject boxed-member ()
  (let ((b (send <test-boxed> (new))))
    #; ; This should be made to work
    (send b
      (set-some-int8 123)
      (set-nested-a '((some-int . 123456)
                      (some-int8 . 123)
                      (some-double . 4.5)
                      (some-enum . value1))))
    (let ((o (send <test-obj> (new* 'boxed b))))
      (test-eqv #t
        (send b (equals (send o (get 'boxed))))))))

(define-test-case everything-tests callbacks ()
  (test-equal '(43 666 1234)
    (map test-callback (map (lambda (n)
                              (lambda () n))
                            '(43 666 1234))))
  (test-equal (iota 10)
    (map test-callback-user-data (map (lambda (n)
                                        (lambda () n))
                                      (iota 10)))))

(define-test-case everything-tests signals ()
  (let ((signal-args #f)
        (obj (send <test-obj> (new*))))
    (send obj (connect 'test (lambda args
                               (set! signal-args args))))
    (send obj (emit 'test))
    (test-compare (lambda (l1 l2)
                   (and (= (length l1) (length l2))
                        (every (lambda (x y)
                                 (ginstance=? x y))
                               l1 l2)))
        (list obj)
      signal-args)))

(run-test-suite everything-tests)

;; Local Variables:
;; scheme-indent-styles: (trc-testing sbank)
;; End:
