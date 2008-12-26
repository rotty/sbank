#!r6rs

(library (sbank soup)
  (export send
          soup-setup!
          soup-status
          make-soup-output-port+flusher)
  (import (rnrs base)
          (rnrs control)
          (rnrs lists)
          (rnrs bytevectors)
          (rnrs io ports)
          (spells cells)
          (spells tracing)
          (sbank ctypes basic)
          (sbank gobject)
          (sbank typelib))

  (typelib-import (only ("Soup" #f) <known-status-code>))

  ;;@ Returns two values: a binary output port, and a flusher
  ;; procedure, which is a thunk that appends all data written to the
  ;; output port since the creation of the port, or the last
  ;; invokation of the flusher procedure, to message-body. Note that
  ;; closing the port does @emph{not} imply a call to the flusher
  ;; procedure, so that the port may be used and closed by code that
  ;; isn't aware of the underlying message-body object, which might
  ;; become invalid while the code using the port is running.
  (define (make-soup-output-port+flusher message-body)
    (let* ((chunks (make-cell '())))
      (values
       (make-custom-binary-output-port "soup"
                                       (soup-output-port-write! message-body chunks)
                                       #f ; get-position
                                       #f ; set-position!
                                       (soup-output-port-close message-body chunks))
       (lambda () (sync-chunks! chunks message-body)))))

  (define (sync-chunks! chunks message-body)
    (let ((size (fold-left (lambda (size chunk)
                             (+ size (bytevector-length chunk)))
                           0
                           (cell-ref chunks))))
      (when (> size 0)
        (let ((buffer (make-bytevector size)))
          (let loop ((i 0) (chunks (reverse (cell-ref chunks))))
            (when (not (null? chunks))
              (let ((size (bytevector-length (car chunks))))
                (bytevector-copy! (car chunks) 0 buffer i size)
                (loop (+ i size) (cdr chunks)))))
          (send message-body (append 'copy buffer))))
      (cell-set! chunks '())))

  (define (soup-output-port-write! message-body chunks)
    (lambda (bytevector start count)
      (when (not (= count 0))
        (cell-set! chunks (cons (let ((part (make-bytevector count)))
                                  (bytevector-copy! bytevector start part 0 count)
                                  part)
                                (cell-ref chunks))))
      count))

  (define (soup-output-port-close message-body chunks)
    (lambda ()
      (send message-body (complete))))

  ;;@ Resolve @1 to an HTTP status code. If @1 is a symbol, it is
  ;; looked up among the Soup known status codes enumeration (that
  ;; lookup returns @code{#f} if it is not in the enumeration). If @1
  ;; is not a symbol, it is returned with any action done.
  (define (soup-status code)
    (if (symbol? code)
        (genumerated-lookup <known-status-code> code)
        code))

  ;;@ Do the setup necessary for using Soup. You should call this
  ;; before doing a @code{typelib-import} of the "Soup"
  ;; namespace. Note that this does not call @code{g-thread-init},
  ;; which must be called before actually using any Soup
  ;; functionality.
  (define soup-setup!
    (let ((installed? #f))
      (lambda ()
        (unless installed?
          (gobject-setup!)
          (set! installed? #t))))))
