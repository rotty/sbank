(library (sbank utils)
  (export make-lazy-entry lazy-entry? lazy-entry-proc
          symbol-append
          scheme-ified-symbol
          c-ified-string
          string-split
          name-symbol/prefix)
  (import (rnrs base)
          (rnrs unicode)
          (rnrs records syntactic)
          (spells char-set)
          (only (spells strings) string-map string-tokenize))

  (define-record-type lazy-entry
    (fields
     (immutable proc lazy-entry-proc)))  

  (define (symbol-append . syms)
    (string->symbol (apply string-append (map symbol->string syms))))

  (define (name-symbol/prefix sym prefix)
    (let ((s (symbol->string sym)))
      (cond ((or (enclosed-by? s #\* #\*)
                 (enclosed-by? s #\< #\>))
             (string->symbol
              (string-append (string (string-ref s 0))
                             (symbol->string prefix)
                             (strip-enclosers s)
                             (string (string-ref s (- (string-length s) 1))))))
            (else
             (symbol-append prefix sym)))))
  
  (define scheme-ified-symbol
    (let ((upper-id-cs (char-set-union char-set:upper-case (char-set #\_ #\-))))
      (lambda (s)
        (string->symbol
         (cond ((char-set<= (string->char-set s) upper-id-cs)
                (string-append "*" (string-map downcase/dash  s) "*"))
               ((camel-cased? s)
                (string-append "<" (un-camel-case s) ">"))
               (else
                (string-map dash s)))))))

  (define (enclosed-by? s start-c end-c)
    (and (char=? (string-ref s 0) start-c)
         (char=? (string-ref s (- (string-length s) 1)) end-c)))

  (define (strip-enclosers s)
    (substring s 1 (- (string-length s) 1)))
  
  (define (c-ified-string sym)
    (let ((s (symbol->string sym)))
      (cond ((enclosed-by? s #\* #\*)
             (string-map upcase/uscore (strip-enclosers s)))
            ((enclosed-by? s #\< #\>)
             (camel-case (strip-enclosers s)))
            (else
             (string-map uscore s)))))

  (define (dash c) (case c ((#\_) #\-) (else c)))
  (define (downcase/dash c) (case c ((#\_) #\-) (else (char-downcase c))))
  (define (uscore c) (case c ((#\-) #\_) (else c)))
  (define (upcase/uscore c) (case c ((#\-) #\_) (else (char-upcase c))))

  (define (camel-cased? s)
    (and (>= (string-length s) 2)
         (char-upper-case? (string-ref s 0))
         (char-lower-case? (string-ref s 1))))
  
  (define (un-camel-case s)
    (let loop ((result-chars '()) (i 0) (in-word? #f))
      (if (>= i (string-length s))
          (list->string (reverse result-chars))
          (let ((c (string-ref s i)))
            (cond ((and in-word? (char-upper-case? c))
                   (loop (append (list (char-downcase c) #\-) result-chars) (+ i 1) #t))
                  ((or (char-alphabetic? c) (char-numeric? c))
                   (loop (cons (char-downcase c) result-chars) (+ i 1) #t))
                  (else
                   (loop (cons c result-chars) (+ i 1) #f)))))))

  (define (camel-case s)
    (let loop ((result-chars '()) (i 0) (had-dash? #t))
      (if (>= i (string-length s))
          (list->string (reverse result-chars))
          (let ((c (string-ref s i)))
            (cond ((and had-dash? (char-lower-case? c))
                   (loop (cons (char-upcase c) result-chars) (+ i 1) #f))
                  ((char=? c #\-)
                   (loop result-chars (+ i 1) #t))
                  (else
                   (loop (cons c result-chars) (+ i 1) #f)))))))
  
  (define (string-split s c)
    (string-tokenize s (char-set-complement (char-set c)))))
