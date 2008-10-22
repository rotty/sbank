(library (sbank conditions)
  (export &sbank-error
          make-sbank-error
          sbank-error?
          
          &sbank-callout-error
          make-sbank-callout-error
          sbank-callout-error?)

  (import (rnrs base)
          (rnrs conditions))

  (define-condition-type &sbank-error &error
    make-sbank-error sbank-error?)

  (define-condition-type &sbank-callout-error &sbank-error
    make-sbank-callout-error sbank-callout-error?))
