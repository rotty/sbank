;;; glib.sls --- Glossing for the GLib namespace.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:
#!r6rs

(library (sbank glib)
  (export glib-setup!)
  (import (rnrs base)
          (only (srfi :1 lists) split-at)
          (srfi :8 receive)
          (spells tracing)
          (sbank support utils)
          (sbank typelib decorators))


  (define default-prio 0)
  
  (define (priority-func-decorator n-args prio-pos)
    (lambda (proc) 
      (lambda args
        (let ((arg-count (length args)))
          (cond ((= arg-count n-args)
                 (apply proc args))
                ((= arg-count (- n-args 1))
                 (receive (front back) (split-at args prio-pos)
                   (apply proc (append front (list default-prio) back))))
                (else
                 (error "glib (function with priority)"
                        "invalid number of arguments" args)))))))


  (define (register-priority-func name n-args prio-pos)
    (register-typelib-decorator "GLib"
                                name
                                (priority-func-decorator n-args prio-pos)))
  
  (define-setup-procedure (glib-setup!)
    (register-priority-func "io_add_watch" 4 1)
    (register-priority-func "timeout_add" 3 0)
    (register-priority-func "timeout_add_seconds" 3 0)
    (register-priority-func "idle_add" 2 0))
  
  )
