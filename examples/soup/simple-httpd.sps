;;; simple-httpd.sps -- Example ported from libsoup test/simple-httpd.c

;; Copyright (C) 2008, 2009 Andreas Rottmann <a.rottmann@gmx.at>

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
(import (except (rnrs) file-exists? delete-file)
        (spells parameter)
        (only (srfi :13 strings) string-join)
        (spells string-utils)
        (spells pathname)
        (spells filesys)
        (spells tracing)
        (sbank soup)
        (sbank typelib)
        (sbank ctypes basic))

(typelib-import
 (prefix (only ("GLib" #f)
               thread-init
               main-loop-new main-loop-run
               markup-escape-text)
         g-)
 (prefix (only ("Soup" #f) <server>)
         soup-)
 (setup soup-setup!))

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

(define (server-callback server msg path query client)
  (let ((method (send msg (get 'method)))
        (file-path (string-append "." path)))
    (println "{0} {1} HTTP/1.{2}" method  path (send msg (get 'http-version)))
    (send (send msg (get-request-headers))
      (foreach (lambda (name value)
                 (println "{0}: {1}" name value))))
    (let ((body (send msg (get-request-body))))
      (when (> (send body (get-length)) 0)
        (println (send body (get-data)))))
    (cond ((member method '("GET" "HEAD"))
           (do-get server msg file-path))
          (else
           (send msg (set-status (soup-status 'not-implemented)))))
    (println " -> {0} {1}"
             (send msg (get 'status-code))
             (send msg (get 'reason-phrase)))))

(define (do-get server msg path)
  (let ((pathname (x->pathname path)))
    (cond
     ((file-readable? pathname)
      (cond
       ((file-directory? pathname)
        (if (pathname-file pathname)
            (let ((uri (send (send msg (get-uri)) (to-string))))
              (send (send msg (get-response-headers))
                (append "Location" (string-append uri "/"))))
            (let ((index-path (pathname-with-file pathname "index.html")))
              (if (file-exists? index-path)
                  (do-get server msg index-path)
                  (send msg
                    (set-response "text/html"
                                  'copy (get-directory-listing pathname)))))))
       ((string=? "GET" (send msg (get-method)))
        (call-with-port (open-file-input-port (x->namestring pathname))
          (lambda (port)
            (send msg (set-response "application/octet-stream"
                                    'copy (get-bytevector-all port))))))
       (else ;; "HEAD" method
        (send (send msg (get-response-headers))
          (append "Content-Length"
                  (number->string (file-size-in-bytes pathname))))))
      (send msg (set-status (soup-status 'ok))))
     (else
      (send msg
        (set-status (soup-status
                     (if (file-exists? pathname) 'forbidden 'not-found))))))))

(define (get-directory-listing path)
  (string-join
   (append
    (list "<html>")
    (let ((escaped (g-markup-escape-text (file-namestring path) -1)))
      (list (ssubst "<head><title>Index of {0}</title></head>" escaped)
            "<body>"
            (ssubst "<h1>Index of {0}</h1>" escaped)
            "<p>"))
    (directory-fold
     path
     (lambda (entry rest)
       (let ((escaped (g-markup-escape-text (file-namestring entry) -1)))
         (cons (ssubst "<a href=\"{0}\">{1}</a><br>" escaped escaped)
               rest)))
     '())
    (list "</body>"
          "</html>"))
   "\n" 'suffix))

(define (println fmt . args)
  (string-substitute #t fmt args 'braces)
  (newline))

(define (ssubst fmt . args)
  (string-substitute #f fmt args 'braces))

(define (bail-out msg . args)
  (string-substitute (current-error-port) msg args)
  (newline (current-error-port)))
