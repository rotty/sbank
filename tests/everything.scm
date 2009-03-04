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


(typelib-import ("Everything" #f))

(testeez "basic types"
  (test/equal "boolean"
    (list (test-boolean #t) (test-boolean #f))
    (list #t #f)))

(testeez "structs"
  (test-define "A" a (send <test-struct-a> (alloc)))
  (test-define "obj" obj (send <test-obj> (new/props)))
  (test-eval "setting fields"
    (send a
      (set-some-int 12345)
      (set-some-int8 42)
      (set-some-double 0.3141)
      (set-some-enum 'value2)))
  (test/equal "get int" (send a (get-some-int)) 12345)
  (test/equal "get int8" (send a (get-some-int8)) 42)
  (test/equal "get double" (send a (get-some-double)) 0.3141)
  (test/equal "get enum" (send a (get-some-enum)) 'value2)

  (test-define "A-cloned" a-cloned (send a (clone)))
  (test-true "clone is an instance" (ginstance? a-cloned))
  (test/equal "fields correct"
    (list (send a-cloned (get-some-int))
          (send a-cloned (get-some-int8))
          (send a-cloned (get-some-double))
          (send a-cloned (get-some-enum)))
    (list 12345 42 0.3141 'value2))

  (test-define "C" c (send <test-struct-c> (alloc)))
  (test-eval "setting fields"
    (send c
      (set-another-int 666)
      (set-obj obj)))
  (test/equal "get int" (send c (get-another-int)) 666)
  (test/equiv "get obj"
    (send c (get-obj))
    obj
    (ginstance=?)))

(parameterize ((null-ok-always-on? #t))
  (testeez "objects"
    (test-define "obj" obj (send <test-obj> (new/props)))
    (test/equal "get" (send obj (get-bare)) #f)
    (test-define "other" other (send <test-obj> (new/props)))
    (test-eval "set" (send obj (set-bare other)))
    (test/equiv "check value"
      (send obj (get-bare))
      other
      (ginstance=?))
    (test-eval "set property" (send obj (set 'bare #f)))
    (test/equal "check value via property"
      (send obj (get 'bare))
      #f)
    (test-define "obj2" obj2 (send <test-obj> (new/props 'bare other)))
    (test/equiv "check value"
      (send obj2 (get-bare))
      other
      (ginstance=?))))

(testeez "callbacks"
  (test/equal "simple"
    (map test-callback (map (lambda (n)
                              (lambda () n))
                            '(43 666 1234)))
    '(43 666 1234)))

(let ((signal-args #f))
  (testeez "signals"
    (test-define "obj" obj (send <test-obj> (new/props)))
    (test-eval "connect" (send obj (connect 'test (lambda args
                                                    (set! signal-args args)))))
    (test-eval "emit" (send obj (emit 'test)))
    (test/equiv "check" signal-args (list obj)
                ((lambda (l1 l2)
                   (and (= (length l1) (length l2))
                        (every (lambda (x y)
                                 (ginstance=? x y))
                               l1 l2)))))))
