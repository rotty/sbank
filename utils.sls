(library (sbank utils)
  (export symbol-append
          scheme-ified-symbol
          c-ified-string
          string-split)
  (import (rnrs base)
          (rnrs unicode)
          (spells char-set)
          (only (spells strings) string-map string-tokenize))

  (define (symbol-append . syms)
    (string->symbol (apply string-append (map symbol->string syms))))

  (define scheme-ified-symbol
    (let ((upper-id-cs (char-set-union char-set:upper-case (char-set #\_ #\-))))
      (lambda (s)
        (let ((cs (string->char-set s)))
          (string->symbol
           (if (char-set<= cs upper-id-cs)
               (string-append "*" (string-map downcase/dash  s) "*")
               (string-map dash s)))))))

  (define (c-ified-string sym)
    (let ((s (symbol->string sym)))
      (if (and (char=? (string-ref s 0) #\*)
               (char=? (string-ref s (- (string-length s) 1)) #\*))
          (string-map upcase/uscore s)
          (string-map uscore s))))

  (define (dash c) (case c ((#\_) #\-) (else c)))
  (define (downcase/dash c) (case c ((#\_) #\-) (else (char-downcase c))))
  (define (uscore c) (case c ((#\-) #\_) (else c)))
  (define (upcase/uscore c) (case c ((#\-) #\_) (else (char-upcase c))))

  (define (string-split s c)
    (string-tokenize s (char-set-complement (char-set c)))))
