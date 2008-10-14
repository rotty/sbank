(library (sbank sxpath-utils)
  (export sxpath-ref sxpath-attr)
  (import (rnrs base)
          (xitomatl srfi and-let*)
          (xitomatl sxml-tools sxpath))

  (define (sxpath-ref sxml path)
    (and-let* ((result ((sxpath path) sxml))
               ((pair? result)))
      (car result)))
  
  (define (sxpath-attr sxml path)
    (and-let* ((result ((sxpath path) sxml))
               ((pair? result)))
      (cadar result))))


