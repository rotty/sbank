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
                                (field (name "data")
                                       (type (array (element-type (type "uint8")))))
                                (field (name "tag") (type "uint8") (bits 5))
                                (field (name "last") (type "boolean")))))
       (data-offset (c-type-align 'uint (+ (c-type-align 'double (c-type-sizeof 'uint))
                                           (* 3 (c-type-sizeof 'double)))))
       (tag-offset (c-type-align 'uint8 (+ data-offset (c-type-sizeof 'pointer))))
       (last-offset (c-type-align 'int (+ tag-offset 1))))
  
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
                      (type (array (element-type (type ,(stypes-ref stypes "uint8")))
                                   (size ,(c-type-sizeof 'pointer))
                                   (alignment ,(c-type-alignof 'pointer))))
                      (offset ,data-offset))
               (field (name "tag")
                      (type ,(stypes-ref stypes "uint8"))
                      (bits 5)
                      (offset ,(c-type-align 'uint8 (+ data-offset (c-type-sizeof 'pointer))))
                      (bit-offset 0))
               (field (name "last")
                      (type ,(stypes-ref stypes "boolean"))
                      (offset ,last-offset))
               (size ,(c-type-align 'double (+ last-offset (c-type-sizeof 'int))))
               (alignment ,(max (c-type-alignof 'uint) (c-type-alignof 'pointer)
                                (c-type-alignof 'double)))))))

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
                 (field (name "ptr") (type (array (element-type (type "uchar"))))))))
      
      (stblob-mem (let ((bv (make-bytevector 4)))
                    (bytevector-u32-native-set! bv 0 #x1200cd85)
                    (memcpy (malloc 4) bv 4)))

      (record-mem
       (let* ((byte-size (+ (* 17 (c-type-sizeof 'uint)) (c-type-sizeof 'pointer)))
              (bv (make-bytevector byte-size)))
         (do ((i 0 (+ i 1)))
             ((>= i 17))
           (bytevector-uint-set! bv
                                 (* i (c-type-sizeof 'uint))
                                 (bitwise-arithmetic-shift 1 i)
                                 (native-endianness)
                                 (c-type-sizeof 'uint)))
         (bytevector-uint-set! bv (* 17 (c-type-sizeof 'uint)) 0
                               (native-endianness) (c-type-sizeof 'pointer))
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
