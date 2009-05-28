;;; stypes.scm --- Unit tests for (sbank support stypes)

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


(define (bool x) (if x #t #f))

(define-test-suite stypes-tests
  "C types represented as sexps")

(define-test-case stypes-tests basic-ops ()
  (test-equal "foo"
    (stype-attribute '(primitive (name "foo")) 'name)))

(define-test-case stypes-tests stypes-ref ()
  (test-equal #f (stypes-ref '(types) "foo"))
  (test-equal '(record (name "Foo") body)
    (stypes-ref '(types (record (name "Foo") body)) "Foo"))
  (test-equal '(union (name "Bar") bar-data)
    (stypes-ref '(types (primitive (name "qux") qux-data)
                        (record (name "Foo") foo-data)
                        (union (name "Bar") bar-data))
                "Bar")))

(define-test-case stypes-tests primitives ()
  (let* ((primlist '("char" "uchar" "int8" "uint8"
                     "short" "ushort" "int16" "uint16"
                     "int" "uint" "int32" "uint32" "long" "ulong"
                     "llong" "ullong" "int64" "uint64"))
         (sprims (map (lambda (name) (stypes-ref primitive-stypes name))
                      primlist)))
    (test-equal (make-list (length primlist) '(name #t size #t alignment #t))
      (map (lambda (x name)
             (and (pair? x)
                  (list 'name (equal? (stype-attribute x 'name) name)
                        'size (bool (stype-attribute x 'size))
                        'alignment (bool (stype-attribute x 'alignment)))))
           sprims
           primlist))))

(define-test-case stypes-tests size-calculation ()
  (let* ((ref (lambda (name)
                (stypes-ref test-stypes name)))
         (data-offset (c-type-align 'uint
                                    (+ (c-type-align 'double (c-type-sizeof 'uint))
                                       (* 3 (c-type-sizeof 'double)))))
         (tag-offset (c-type-align 'uint8 (+ data-offset (c-type-sizeof 'pointer))))
         (u-offset (c-type-align 'double (+ tag-offset 1)))
         (last-offset (c-type-align 'int (+ u-offset (c-type-sizeof 'double)))))
    
    (test-equal
        `(record (name "Foo")
                 (field (name "frobotz")
                        (type ,(ref "uint"))
                        (offset 0))
                 (field (name "val")
                        (type
                         (array
                          (element-type (type ,(ref "double")))
                          (element-count 3)
                          (size ,(* 3 (c-type-sizeof 'double)))
                          (alignment ,(c-type-alignof 'double))))
                        (offset ,(c-type-align 'double (c-type-sizeof 'uint))))
                 (field (name "data")
                        (type (pointer (type ,(ref "uint8"))
                                       (size ,(c-type-sizeof 'pointer))
                                       (alignment ,(c-type-alignof 'pointer))))
                        (offset ,data-offset))
                 (field (name "tag")
                        (type ,(ref "uint8"))
                        (bits 5)
                        (offset ,(c-type-align
                                  'uint8
                                  (+ data-offset (c-type-sizeof 'pointer))))
                        (bit-offset 0))
                 (union (field (name "u1")
                               (type ,(ref "double")) (offset 0))
                        (field (name "u2")
                               (type ,(ref "uint16")) (offset 0))
                        (size ,(c-type-sizeof 'double))
                        (alignment ,(c-type-alignof 'double))
                        (offset ,u-offset))
                 (field (name "last")
                        (type ,(ref "boolean"))
                        (offset ,last-offset))
                 (size ,(c-type-align 'double (+ last-offset (c-type-sizeof 'int))))
                 (alignment ,(max (c-type-alignof 'uint) (c-type-alignof 'pointer)
                                  (c-type-alignof 'double))))
      (ref "Foo")))

  (test-compare >=  (+ (c-type-sizeof 'size_t)
                       (* 2 (c-type-sizeof 'double)))
    (stype-attribute (stypes-ref test-stypes "GValue") 'size)))


(define-test-suite (stypes-tests.fetch/set stypes-tests)
  "Value fetching")

(define-syntax define-accessors
  (stype-accessor-definer test-stypes))

(define-test-case stypes-tests.fetch/set basics ()
  (let ((stblob-mem (let ((bv (make-bytevector 4)))
                      (bytevector-u32-native-set! bv 0 #x1200cd85)
                      (memcpy (malloc 4) bv 4))))
    
    (define-accessors "SimpleTypeBlob"
      (tag-fetcher "tag")
      (pointer-fetcher "pointer")
      (offset-fetcher offset-setter "offset"))
    
    (test-equal 5 (tag-fetcher stblob-mem))
    (test-equal 1 (pointer-fetcher stblob-mem))
    (test-equal #x1200cd85 (offset-fetcher stblob-mem))

    (begin
      (offset-setter stblob-mem 0)
      (test-equal 0 (tag-fetcher stblob-mem)))))

(define-test-case stypes-tests.fetch/set array ()
  (let ((record-mem
         (let* ((ptr-offset (c-type-align 'pointer (* 17 (c-type-sizeof 'uint))))
                (byte-size (+ ptr-offset (c-type-sizeof 'pointer)))
                (bv (make-bytevector byte-size)))
           (do ((i 0 (+ i 1)))
               ((>= i 17))
             (bytevector-uint-set! bv
                                   (* i (c-type-sizeof 'uint))
                                   (bitwise-arithmetic-shift 1 i)
                                   (native-endianness)
                                   (c-type-sizeof 'uint)))
           (bytevector-uint-set! bv ptr-offset 0 (native-endianness)
                                 (c-type-sizeof 'pointer))
           (memcpy (malloc byte-size) bv byte-size))))
    
    (define-accessors "RecordWithArrays"
      (data-fetcher "data")
      (ptr-fetcher "ptr"))

    (test-compare pointer=? (pointer+ record-mem (c-type-sizeof 'uint))
      (data-fetcher record-mem))
    
    ;; working when the 'array' is really a pointer?
    (test-eqv #t (null-pointer? (ptr-fetcher record-mem)))))

(run-test-suite stypes-tests)

;; Local Variables:
;; scheme-indent-styles: (trc-testing)
;; End:
