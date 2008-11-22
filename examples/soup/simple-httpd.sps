;;; simple-httpd.sps -- Example ported from libsoup test/simple-httpd.c

;; Copyright (C) 2008 Andreas Rottmann <a.rottmann@gmx.at>

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
(import (rnrs)
        (spells parameter)
        (spells string-substitute)
        (sbank soup)
        (sbank typelib)
        (sbank ctypes basic))

(soup-setup!)
(typelib-import
 (prefix (only ("GLib" #f)
               thread-init
               main-loop-new main-loop-run)
         g-)
 (prefix (only ("Soup" #f) <server>)
         soup-))

(let ((port  8001))
  (g-thread-init #f)
  (parameterize ((null-ok-always-on? #t)) ;; Needed for field access, will go away
    (let ((server (send <soup-server> (new/props 'port port
                                                 'server-header "simple-httpd"))))
      (unless server
        (bail-out "Unable to bind to server port {0}\n" port))
      (send server
        (add-handler #f server-callback)
        (run-async))
      (println "Waiting for requests...")
      (g-main-loop-run (g-main-loop-new #f #t)))))

;; Note that `user-data' will go away when I get around to implement hiding it
(define (server-callback server msg path query client user-data)
  (println "{0} {1} HTTP/1.{2}" (send msg (get 'method)) path (send msg (get 'http-version)))
  (send (send msg (get-request-headers))
    (foreach (lambda (name value user-data)
               (println "{0}: {1}" name value))))
  (let ((body (send msg (get-request-body))))
    (when (> (send body (get-length)) 0)
      (println (send body (get-data)))))
  (send msg (set-status (soup-status 'not-implemented)))
  (println " -> {0} {1}" (send msg (get 'status-code)) (send msg (get 'reason-phrase))))

(define (println fmt . args)
  (string-substitute #t fmt args)
  (newline))

(define (bail-out msg . args)
  (string-substitute (current-error-port) msg args)
  (newline (current-error-port)))