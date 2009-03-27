;;; soup.sls --- Glossing for the Soup namespace

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

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
          (spells foreign)
          (spells cells)
          (spells tracing)
          (sbank support utils)
          (sbank ctypes basic)
          (sbank gobject internals)
          (sbank gobject)
          (sbank typelib base)
          (sbank typelib decorators)
          (sbank typelib))

  (typelib-import (only ("Soup" #f)
                        <known-status-code>))

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
          (send message-body (append buffer))))
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
  ;; is not a symbol, it is simply returned.
  (define (soup-status code)
    (if (symbol? code)
        (genumerated-lookup <known-status-code> code)
        code))

  (define (message-decorator typelib class)
    (gobject-class-decorate
     class
     values
     (gobject-method-overrider typelib `((set-response . ,message-set-response)))
     values))

  (define (message-set-response typelib next-method)
    (lambda (message content-type data)
      (let ((resp-body (send message (get-response-body)))
            (hdrs (send message (get-response-headers))))
        (cond (content-type
               (send hdrs (replace "Content-Type" content-type))
               (send resp-body (append data)))
              (else
               (send hdrs (remove "Content-Type"))
               (send resp-body (truncate)))))))

  (define (buffer-decorator typelib class)
    (gobject-simple-class-decorate
     class
     (gobject-method-overrider typelib `((new . ,buffer-new)))
     values))

  (define buffer-new-callout
    (make-c-callout 'pointer '(int pointer size_t)))

  (define (get-memory-take typelib)
    (genumerated-lookup (typelib-get-entry typelib "MemoryUse") 'take))

  (define (make-buffer-new% typelib)
    (let ((buffer-new%
           (buffer-new-callout
            (typelib-dlsym typelib "soup_buffer_new")))
          (memory-take% (get-memory-take typelib)))
      (lambda (data)
        (let* ((size (bytevector-length data))
               (data-ptr (memcpy (g-malloc size) data size)))
          (buffer-new% memory-take% data-ptr size)))))

  (define (buffer-new typelib next-method)
    (let ((buffer-new% (make-buffer-new% typelib))
          (<buffer> (typelib-get-entry typelib "Buffer")))
      (lambda (data)
        (make-ginstance <buffer> (buffer-new% data)))))

  (define (message-body-decorator typelib class)
    (gobject-simple-class-decorate
     class
     values
     (gobject-method-overrider typelib `((append . ,message-body-append)))))

  (define (message-body-append typelib next-method)
    (let ((<buffer> (typelib-get-entry typelib "Buffer")))
      (lambda (body data)
        (let ((buffer (send <buffer> (new data))))
          (next-method body buffer)
          (send buffer (free))))))

  ;;@ Do the setup necessary for using Soup. You should call this
  ;; before doing a @code{typelib-import} of the "Soup"
  ;; namespace. Note that this does not call @code{g-thread-init},
  ;; which must be called before actually using any Soup
  ;; functionality.
  (define-setup-procedure (soup-setup!)
    (gobject-setup!)
    (register-typelib-decorator "Soup" "Buffer" buffer-decorator)
    (register-typelib-decorator "Soup" "Message" message-decorator)
    (register-typelib-decorator "Soup" "MessageBody" message-body-decorator))

  )
