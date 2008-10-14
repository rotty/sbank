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

(let ((stypes (stypes-adjoin primitive-stypes
                             '(record
                               (name "Foo")
                               (field (name "frobotz") (type "uint"))
                               (field (name "data")
                                      (type (array (element-type (type "uint8")))))))))
  (testeez "adjoining"
    (test/equal "resolution and annotations correctness"
      (stypes-ref stypes "Foo")
      `(record (name "Foo")
               (field (name "frobotz")
                      (type ,(stypes-ref stypes "uint"))
                      (offset 0))
               (field (name "data")
                      (type (array (element-type (type ,(stypes-ref stypes "uint8")))
                                   (size ,(c-type-sizeof 'pointer))
                                   (alignment ,(c-type-alignof 'pointer))))
                      (offset ,(c-type-align 'pointer (c-type-sizeof 'uint))))
               (size ,(* 2 (max (c-type-sizeof 'uint) (c-type-sizeof 'pointer))))
               (alignment ,(max (c-type-alignof 'uint) (c-type-alignof 'pointer)))))))

(let ((stypes
       (stypes-adjoin
        primitive-stypes
        '(union (name "SimpleTypeBlob")
                (record
                 (field (name "tag") (type "uint") (bits 5))
                 (field (name "reserved3") (type "uint") (bits 2))
                 (field (name "pointer") (type "uint") (bits 1)))
                (field (name "offset") (type "uint32")))))

      (stblob-mem (let ((bv (make-bytevector 4)))
                    (bytevector-u32-native-set! bv 0 #x1200cd85)
                    (memcpy (malloc 4) bv 4))))

  (let* ((stblob (stypes-ref stypes "SimpleTypeBlob"))
         (tag-fetcher (stype-fetcher stblob "tag"))
         (pointer-fetcher (stype-fetcher stblob "pointer"))
         (offset-fetcher (stype-fetcher stblob "offset")))
    (testeez "basic fetching"
      (test/equal "tag" (tag-fetcher stblob-mem) 5)
      (test/equal "pointer" (pointer-fetcher stblob-mem) 1)
      (test/equal "offset" (offset-fetcher stblob-mem) #x1200cd85))))
