;;; hello-world.sps --- Example translated from the GTK+ tutorial

;; Copyright (C) 2008 Andreas Rottmann <a.rottmann@gmx.at>

;; This file is placed in the public domain.

#!r6rs
(import (rnrs)
        (spells string-substitute)
        (sbank soup)
        (sbank typelib))

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
  (let ((server (send <soup-server> (new/props 'port port
                                               'server-header "simple-httpd"))))
    (unless server
      (bail-out "Unable to bind to server port {0}\n" port))
    (send server
      (add-handler #f server-callback)
      (run-async))
    (println "Waiting for requests...")
    (g-main-loop-run (g-main-loop-new #f #t))))

;; Note that `user-data' will go away when I get around to implement hiding it
(define (server-callback server msg path query client user-data)
  (println "{0} {1} HTTP/1.{2}" (send msg (get 'method)) path (send msg (get 'http-version)))
  (send msg (set-status (soup-status 'not-implemented)))
  (println " -> {0} {1}" (send msg (get 'status-code)) (send msg (get 'reason-phrase))))

(define (println fmt . args)
  (string-substitute #t fmt args)
  (newline))

(define (bail-out msg . args)
  (string-substitute (current-error-port) msg args)
  (newline (current-error-port)))