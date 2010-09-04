;;; glib-daemon.sls --- Conviniency library for using GLib with libdaemon

;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

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

(library (sbank glib-daemon)
  (export g-install-signal-handler)
  (import (rnrs base)
          (rnrs control)
          (sbank typelib)
          (sbank glib)
          (sbank libdaemon))

  ;;@ Install @2 as signal handler for the signals specified by the
  ;; list @1. Note that you can only call @0 only once; to install
  ;; more than a single conceptual signal handler, you will have to
  ;; call @0 with all signals you want to handle and then add your own
  ;; dispatching logic.

  ;; The signal handler @2 is expected to be a single-argument
  ;; procedure receiving the number of the received signal. When the
  ;; handler returns a true value, it will remain active, receiving
  ;; any future signals. If it returns @code{#f}, no more signals will
  ;; be handled; this is the expected return value after calling
  ;; @code{g-main-loop-quit} from the handler.
  ;;
  ;; The signals specified as elements of @1 may be either signal
  ;; numbers, or from the set of symbols listed in the documentation
  ;; for @ref{sbank.libdaemon daemon-signal-init}.
  (define (g-install-signal-handler sigs proc)
    (apply daemon-signal-init sigs)
    (let ((io (send <g-i-o-channel> (unix-new (daemon-signal-fd)))))
      (g-io-add-watch io
                      '(in err hup)
                      (lambda (source condition)
                        (cond ((equal? '(in) condition)
                               (let loop ((sig (daemon-signal-next)))
                                 (cond ((= sig 0)
                                        #t)
                                       ((proc sig)
                                        (loop (daemon-signal-next)))
                                       (else
                                        #f))))
                              (else
                               (error 'g-install-signal-handler
                                      "unexpected condition on signal fd: {0}"
                                      condition))))))))
