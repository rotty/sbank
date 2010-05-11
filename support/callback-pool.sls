;;; callback-pool.sls --- Reuse for callbacks

;; Copyright (C) 2010 Andreas Rottmann <a.rottmann@gmx.at>

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

;; Here we work the "pool magic", by wrapping the actual callback
;; procedures inside a "reusable" wrapper. When the callback is done
;; with, we return the wrapper to the pool.

;;; Code:
#!r6rs

(library (sbank support callback-pool)
  (export callback-pool?
          make-callback-pool
          callback-pool-getter

          callback-destroy-notify)
  (import (rnrs)
          (srfi :8 receive)
          (spells foreign))

  (define-record-type callback-pool
    (fields (immutable callback)
            (mutable list))
    (protocol (lambda (p)
                (lambda (callback)
                  (p callback '())))))

  (define (callback-pool-pop! pool)
    (let* ((callbacks (callback-pool-list pool))
           (pooled-cb (car callbacks)))
      (callback-pool-list-set! pool (cdr callbacks))
      pooled-cb))

  (define (callback-pool-getter pool)
    (lambda (proc)
      (let ((prepare (callback-pool-callback pool)))
        (let ((pooled-cb
               (if (null? (callback-pool-list pool))
                   (make-pooled-cb prepare proc)
                   (let ((pcb (callback-pool-pop! pool)))
                     (pooled-cb-proc-set! pcb proc)
                     pcb))))
          (values (pooled-cb-ptr pooled-cb)
                  (pooled-cb-reclaimer pool pooled-cb))))))
  
  (define-record-type pooled-cb
    (fields (immutable ptr)
            (mutable proc))
    (protocol (lambda (p)
                (lambda (prepare proc)
                  (letrec* ((outer (lambda args (apply (pooled-cb-proc self) args)))
                            (self (p (prepare outer) proc)))
                    self)))))

  (define (pooled-cb-reclaimer pool pooled-cb)
    (lambda ()
      (pooled-cb-proc-set! pooled-cb #f)
      (callback-pool-list-set! pool (cons pooled-cb
                                          (callback-pool-list pool)))))
  
  ;; We kindof cheat here to be more general, and don't specify any
  ;; argument types, but this is not a problem because of the C
  ;; calling convention (i.e. caller-pops-args).
  (define destroy-notify-cb-pool
    (make-callback-pool (make-c-callback 'void '())))
  
  (define get-destroy-notify-callback
    (callback-pool-getter destroy-notify-cb-pool))
  
  (define (callback-destroy-notify reclaim-callback)
    (let ((reclaim-destroy-notify #f))
      (receive (destroy-notify reclaim)
               (get-destroy-notify-callback 
                (lambda ()
                  (reclaim-callback)
                  (reclaim-destroy-notify)))
        (set! reclaim-destroy-notify reclaim)
        destroy-notify)))

  )
