;;; libdaemon.sls --- Bindings for libdaemon.

;; Copyright (C) 2008, 2009 Andreas Rottmann <a.rottmann@gmx.at>

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

;; While currently distributed with sbank, this library is usable
;; (and useful) stand-alone, only depending on (spells foreign).

;;; Code:
#!r6rs

(library (sbank libdaemon)
  (export daemon-signal-init
          daemon-signal-fd
          daemon-signal-next)

  (import (rnrs base)
          (rnrs lists)
          (spells foreign))

  (define (checked-dlopen name)
    (or (dlopen name)
        (error 'checked-dlopen "unable to open shared library" name (dlerror))))

  (define known-signals
    '((alrm . 14)
      (hup  .  1)
      (int  .  2)
      (kill .  9)
      (pipe . 13)
      (term . 15)
      (abrt .  6)
      (fpe  .  8)
      (ill  .  4)
      (quit .  3)
      (segv . 11)
      (trap .  5)))

  ;;@ Arrange for the signals specified by @1 to be handled by the
  ;; library. Each element of @1 may be either a signal number, or one
  ;; of the following symbols:
  ;;
  ;; @itemize
  ;; @item @code{alrm}
  ;; @item @code{hup}
  ;; @item @code{int}
  ;; @item @code{kill}
  ;; @item @code{pipe}
  ;; @item @code{term}
  ;; @item @code{abrt}
  ;; @item @code{fpe}
  ;; @item @code{ill}
  ;; @item @code{quit}
  ;; @item @code{segv}
  ;; @item @code{trap}
  ;; @end itemize
  ;;
  (define (daemon-signal-init . sigs)
    (for-each (lambda (sig)
                (let ((sig-num (cond ((and (symbol? sig)
                                           (assq sig known-signals))
                                      => cdr)
                                     (else
                                      sig))))
                  (or (= 0 (signal-install% sig-num))
                      (error 'daemon-signal-init "unable to install signal" sig-num))))
              sigs))

  ;;@ Return the UNIX file descriptor (an integer) which the library
  ;; announces signal reception on.
  (define (daemon-signal-fd)
    (signal-fd%))

  ;;@ Return the number of the next signal that has been received. If no
  ;; more signals are outstanding,
  (define (daemon-signal-next)
    (signal-next%))

  (define libdaemon (checked-dlopen "libdaemon.so.0"))

  (define-c-callouts libdaemon
    (signal-install% 'int "daemon_signal_install" '(int))
    (signal-fd%      'int "daemon_signal_fd" '())
    (signal-next%    'int "daemon_signal_next" '()))

  )
