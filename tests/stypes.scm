;;; stypes.scm --- Unit tests for (sbank stypes)

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

(testeez "basic stype operations"
  (test/equal "attribute ref existing"
    (stype-attribute '(primitive (name "foo")) 'name)
    "foo"))

(testeez "basic stype collection operations"
  (test/equal "ref nonexisting" (stypes-ref '(types) "foo") #f)
  (test/equal "ref existing"
    (stypes-ref '(types (record (name "Foo") body)) "Foo")
    '(record (name "Foo") body))
  (test/equal "ref existing, # of stypes > 1"
    (stypes-ref '(types (primitive (name "qux") qux-data)
                        (record (name "Foo") foo-data)
                        (union (name "Bar") bar-data))
                "Bar")
    '(union (name "Bar") bar-data)))

(let* ((primlist '("char" "uchar" "int8" "uint8"
                   "short" "ushort" "int16" "uint16"
                   "int" "uint" "int32" "uint32" "long" "ulong"
                   "llong" "ullong" "int64" "uint64"))
       (sprims (map (lambda (name) (stypes-ref primitive-stypes name))
                    primlist)))
  (testeez "primitive types"
    (test/equal "all types and their attributes present"
      (map (lambda (x name)
             (and (pair? x)
                  (list 'name (equal? (stype-attribute x 'name) name)
                        'size (bool (stype-attribute x 'size))
                        'alignment (bool (stype-attribute x 'alignment)))))
           sprims
           primlist)
      (make-list (length primlist) '(name #t size #t alignment #t)))))

(let* ((stypes (stypes-adjoin primitive-stypes
                              '(record
                                (name "Foo")
                                (field (name "frobotz") (type "uint"))
                                (field (name "val")
                                       (type (array (element-type (type "double"))
                                                    (element-count 3))))
                                (field (name "data") (type (pointer (type "uint8"))))
                                (field (name "tag") (type "uint8") (bits 5))
                                (union
                                 (field (name "u1") (type "double"))
                                 (field (name "u2") (type "uint16")))
                                (field (name "last") (type "boolean")))))
       (data-offset (c-type-align 'uint (+ (c-type-align 'double (c-type-sizeof 'uint))
                                           (* 3 (c-type-sizeof 'double)))))
       (tag-offset (c-type-align 'uint8 (+ data-offset (c-type-sizeof 'pointer))))
       (u-offset (c-type-align 'double (+ tag-offset 1)))
       (last-offset (c-type-align 'int (+ u-offset (c-type-sizeof 'double)))))
  
  (testeez "adjoining"
    (test/equal "resolution and annotations correctness"
      (stypes-ref stypes "Foo")
      `(record (name "Foo")
               (field (name "frobotz")
                      (type ,(stypes-ref stypes "uint"))
                      (offset 0))
               (field (name "val")
                      (type (array (element-type (type ,(stypes-ref stypes "double")))
                                   (element-count 3)
                                   (size ,(* 3 (c-type-sizeof 'double)))
                                   (alignment ,(c-type-alignof 'double))))
                      (offset ,(c-type-align 'double (c-type-sizeof 'uint))))
               (field (name "data")
                      (type (pointer (type ,(stypes-ref stypes "uint8"))
                                     (size ,(c-type-sizeof 'pointer))
                                     (alignment ,(c-type-alignof 'pointer))))
                      (offset ,data-offset))
               (field (name "tag")
                      (type ,(stypes-ref stypes "uint8"))
                      (bits 5)
                      (offset ,(c-type-align 'uint8 (+ data-offset (c-type-sizeof 'pointer))))
                      (bit-offset 0))
               (union (field (name "u1") (type ,(stypes-ref stypes "double")) (offset 0))
                      (field (name "u2") (type ,(stypes-ref stypes "uint16")) (offset 0))
                      (size ,(c-type-sizeof 'double))
                      (alignment ,(c-type-alignof 'double))
                      (offset ,u-offset))
               (field (name "last")
                      (type ,(stypes-ref stypes "boolean"))
                      (offset ,last-offset))
               (size ,(c-type-align 'double (+ last-offset (c-type-sizeof 'int))))
               (alignment ,(max (c-type-alignof 'uint) (c-type-alignof 'pointer)
                                (c-type-alignof 'double)))))
    (test-eval "u1" (procedure? (stype-fetcher (stypes-ref stypes "Foo") "u1")))
    (test-eval "u2" (procedure? (stype-fetcher (stypes-ref stypes "Foo") "u2")))))


(let* ((stypes
        (stypes-adjoin
         primitive-stypes
         '(alias (name "gtype") (target "size_t"))
         '(record (name "GValue")
                  (field (name "g_type") (type "gtype"))
                  (field
                   (name "data")
                   (type (array (element-type
                                 (type
                                  (union (field (name "v_int") (type "int"))
                                         (field (name "v_double") (type "double")))))
                                (element-count 2)))))))
       (gv-stype (stypes-ref stypes "GValue")))
  (testeez "Size calculation"
    (test-true "GValue size sane" (>= (stype-attribute gv-stype 'size)
                                      (+ (c-type-sizeof 'size_t)
                                         (* 2 (c-type-sizeof 'double)))))))

(let ((stypes
       (stypes-adjoin
        primitive-stypes
        '(union (name "SimpleTypeBlob")
                (record
                 (field (name "tag") (type "uint") (bits 5))
                 (field (name "reserved3") (type "uint") (bits 2))
                 (field (name "pointer") (type "uint") (bits 1)))
                (field (name "offset") (type "uint32")))
        '(record (name "RecordWithArrays")
                 (field (name "type") (type "uint"))
                 (field (name "data") (type (array (element-type (type "uint"))
                                                   (element-count 16))))
                 (field (name "ptr") (type (pointer (type "uchar")))))))
      
      (stblob-mem (let ((bv (make-bytevector 4)))
                    (bytevector-u32-native-set! bv 0 #x1200cd85)
                    (memcpy (malloc 4) bv 4)))

      (record-mem
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
         (bytevector-uint-set! bv ptr-offset 0 (native-endianness) (c-type-sizeof 'pointer))
         (memcpy (malloc byte-size) bv byte-size))))

  (let* ((stblob (stypes-ref stypes "SimpleTypeBlob"))
         (tag-fetcher (stype-fetcher stblob "tag"))
         (pointer-fetcher (stype-fetcher stblob "pointer"))
         (offset-fetcher (stype-fetcher stblob "offset")))
    (testeez "basic fetching"
      (test/equal "tag" (tag-fetcher stblob-mem) 5)
      (test/equal "pointer" (pointer-fetcher stblob-mem) 1)
      (test/equal "offset" (offset-fetcher stblob-mem) #x1200cd85)))

  (let* ((record (stypes-ref stypes "RecordWithArrays"))
         (data-fetcher (stype-fetcher record "data"))
         (ptr-fetcher (stype-fetcher record "ptr")))
    (testeez "array member fetching"
      (test/equal "pointer correct"
        (pointer->integer (data-fetcher record-mem))
        (+ (pointer->integer record-mem) (c-type-sizeof 'uint)))
      (test/equal "correctness when the 'array' is really a pointer"
        (pointer->integer (ptr-fetcher record-mem))
        0))))
