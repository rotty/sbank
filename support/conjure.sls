;;; conjure.sls --- sbank build system support

;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>

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

(library (sbank support conjure)
  (export typelib-fetcher
          typelib-fender

          sbank-build-tasks
          install-products)
  (import (except (rnrs) delete-file file-exists?)
          (only (srfi :1) append-map)
          (srfi :8 receive)
          (only (srfi :13) string-trim-right)
          (srfi :45 lazy)
          (wak irregex)
          (wak fmt)
          (wak prometheus)
          (spells match)
          (spells string-utils)
          (spells misc)
          (spells logging)
          (spells filesys)
          (spells pathname)
          (spells process)
          (spells sysutils)
          (spells tracing)
          (spells test-runner)
          (conjure utils)
          (conjure run)
          (conjure base)
          (conjure dsl)
          (only (conjure base) logger:conjure))

(import-procedures/lazy
 (only (sbank typelib base)
       require-typelib
       typelib-available?)
 (only (sbank typelib expanders)
       typelib-exported-names)
 (only (sbank ctypes basic)
       gerror?))

(define (typelib-fender namespace)
  (lambda ()
    (guard (c ((gerror? c)
               (log/sbank 'warning
                          "error loading typelib '" namespace "': "
                          (condition-message c))
               #f))
      (require-typelib namespace #f 0)
      #t)))

(define (typelib-fetcher)
  (lambda (project)
    (lambda (missing datum)
      (match datum
        (('typelib-exports spec . opts)
         (append-map fetch-exports missing))
        (('typelib-availability-filter . names)
         (let ((result (filter typelib-available? names)))
           (log/sbank 'info (cat "available typelibs: "
                                 (fmt-join dsp result ", ")
                                 " (of " (fmt-join dsp names ", ") ")"))
           (list (cons datum (fmt #f (wrt result))))))
        (_
         #f)))))

(define (opts-ref opts key default)
  (cond ((assq key opts) => cadr)
        (else default)))

(define (opts-ref* opts key default)
  (cond ((assq key opts) => cdr)
        (else default)))

(define (includes+excludes->pred includes excludes)
  (let ((include-irxs (and includes (map irregex includes)))
        (exclude-irxs (map irregex excludes)))
    (lambda (name)
      (and (or (not includes)
               (or-map (lambda (irx)
                         (irregex-match irx name))
                       include-irxs))
           (not (or-map (lambda (irx)
                          (irregex-match irx name))
                        exclude-irxs))))))

(define (fetch-exports item)
  (match item
    (('typelib-exports (? list? spec) . opts)
     (guard (c ((gerror? c) '()))
       (let* ((pred (includes+excludes->pred (opts-ref* opts 'include #f)
                                             (opts-ref* opts 'exclude '())))
              (name (opts-ref opts 'name #f))
              (names-string
               (fmt #f (fmt-join dsp
                                 (sort-list (typelib-exported-names spec pred)
                                            (lambda (n1 n2)
                                              (string<? (symbol->string n1)
                                                        (symbol->string n2))))
                                 " "))))
         (cons* (cons item names-string)
                (if name
                    (list (cons `(typelib-exports ,name) names-string))
                    '())))))
    (_
     '())))

(define (install-products agent product-dir pathnames)
  (for-each (lambda (pathname)
              (let ((product (pathname-join product-dir pathname)))
                (when (file-exists? product)
                  (agent 'install-file
                         'libraries
                         (->namestring pathname)
                         (->namestring product)))))
            pathnames))

(define (sbank-build-tasks)
  (task configure
    (configure
     (produce '((("sbank") "config.sls") <= "config.sls.in")
              '((("sbank") "glib.sls") <= "glib.sls.in")
              `((("sbank") "gtk.sls") <= "gtk.sls.in"
                (? ,(typelib-fender "Gtk")))
              `((("sbank") "soup.sls") <= "soup.sls.in"
                (? ,(typelib-fender "Soup"))))
     (fetchers (typelib-fetcher))))
  (task test
    (ordinary
     (depends 'build-regress 'configure)
     (proc (lambda (step)
             (run-tests-in-directory
              (merge-pathnames '(("tests"))
                               ((step 'project) 'source-dir))
              #f)))))
  (task build-regress
    (g-ir-compiler (file '(() ("Regress-1.0" "gir")))))
  (task
    (g-ir-scanner
     (files (gi-datadir '(("tests") "regress.h"))
            (gi-datadir '(("tests") "regress.c")))
     (gir-namespace "Regress")
     (gir-version "1.0")
     (gir-libraries "libregress.la")
     (gir-includes "cairo-1.0" "Gio-2.0")))
  (task cc (cc-conf))
  (task (lt-link (product "libregress.la")
                 (objects "regress.lo")
                 (lt-flags "-export-dynamic" "-rpath" "/usr/local/lib")
                 (ld-flags : (regress-ld-flags))
                 (compiler 'cc)))
  (task (lt-compile (product "regress.lo")
                    (sources (gi-datadir '(("tests") "regress.c")))
                    (compiler 'cc)
                    (c-flags : (regress-c-flags)))))

(define (regress-c-flags)
  (append
   '("-Wall")
   (string-split (pkg-config "gobject-2.0" "gio-2.0" "cairo" "--cflags") '(#\space))))

(define (regress-ld-flags)
  (append
   '("-avoid-version")
   (string-split (pkg-config "gobject-2.0" "gio-2.0" "cairo" "--libs") '(#\space))))


;;; pkg-config

(define %gi-datadir
  (delay (pkg-config "gobject-introspection-1.0" "--variable=gidatadir")))

(define %pkg-config-path
  (delay (find-exec-path "pkg-config")))

(define (gi-datadir pathname)
  (merge-pathnames pathname (pathname-as-directory (force %gi-datadir))))

(define (pkg-config . args)
  (receive (status signal result)
           (apply run-process/string
                  #f
                  (or (force %pkg-config-path)
                      (build-failure
                       "cannot find `pkg-config' in PATH"))
                  args)
    (unless (eqv? 0 status)
      (build-failure "`pkg-config' returned non-zero exit status" status))
    (string-trim-right result)))


;;; g-ir-scanner

(define-object <g-ir-scanner> (<program>)
  (program "g-ir-scanner")
  (runner (<runner> 'clone))
  (gir-namespace #f)
  (gir-version #f)
  (gir-packages '())
  (gir-libraries '())
  (gir-includes '())
  (gir-export-packages '())
  (gir-scanner-flags '())
  (gir-c-flags '())
  (gir-ld-flags '())
  
  ((scan-files self resend dest srcs)
   ((self 'runner) 'run (cons (self 'program-path)
                              (g-ir-scanner-args self dest srcs)))))

(define-object <g-ir-scanner-step> (<file-step>)
  ((build self resend)
   (let ((scanner (<g-ir-scanner> 'clone)))
     (for-each (lambda (property)
                 (scanner 'add-value-slot! property (self 'prop property)))
               '(gir-namespace
                 gir-version
                 gir-packages
                 gir-includes
                 gir-export-packages
                 gir-scanner-flags
                 gir-c-flags
                 gir-ld-flags))
     (scanner 'add-value-slot!
              'gir-libraries
              (self 'resolve-files (self 'prop 'gir-libraries)))
     (scanner 'scan-files
              (self 'prop 'product)
              (self 'resolve-files (self 'prop 'files))))))

(define (g-ir-scanner-deduce-product task)
  (make-pathname
   #f
   '()
   (make-file (string-append (task 'prop 'gir-namespace)
                             "-"
                             (task 'prop 'gir-version))
              "gir")))

(define-object <g-ir-scanner-task> (<file-task>)
  (arguments '())
  (properties
   `((gir-namespace (singleton string))
     (gir-version (singleton string))
     (gir-packages (list-of string) ())
     (gir-libraries (list-of pathname) ())
     (gir-includes (list-of pathname) ())
     (gir-export-packages (list-of string) ())
     (gir-scanner-flags (list-of string) ())
     (gir-c-flags (list-of string) ())
     (gir-ld-flags (list-of string) ())
     (product (singleton pathname) ,g-ir-scanner-deduce-product)
     (files (list-of pathname))))
  (step-prototype <g-ir-scanner-step>)
  
  ((new self resend name args props)
   (let ((task (resend #f 'new name args props)))
     (task 'add-value-slot! 'products (list (task 'prop 'product)))
     (task 'add-value-slot! 'sources (append (task 'prop 'files)
                                             (task 'prop 'gir-libraries)))
     task)))

(define (g-ir-scanner-args scanner dest srcs)
  (define (option-arglist option message)
    (cond ((scanner message)
           => (lambda (value)
                (list option value)))
          (else
           '())))
  (define (multi-arglist option message)
    (append-map (lambda (value)
                  (list option value))
                (scanner message)))
  (append (option-arglist "--namespace" 'gir-namespace)
          (option-arglist "--nsversion" 'gir-version)
          (multi-arglist "--pkg" 'gir-packages)
          (multi-arglist "--include" 'gir-includes)
          (multi-arglist "--pkg-export" 'gir-export-packages)
          (multi-arglist "--library" 'gir-libraries)
          (scanner 'gir-scanner-flags)
          (scanner 'gir-c-flags)
          (scanner 'gir-ld-flags)
          srcs
          (list "--output" dest)))

;;; g-ir-compiler

(define-object <g-ir-compiler> (<program>)
  (program "g-ir-compiler")
  (runner (<runner> 'clone))
  
  ((compile-file self resend dest src)
   ((self 'runner) 'run (list (self 'program-path) "-o" dest src))))

(define-object <g-ir-compiler-step> (<file-step>)
  ((build self resend)
   (<g-ir-compiler> 'compile-file
                    (self 'prop 'product)
                    (self 'resolve-file (self 'prop 'file)))))

(define (g-ir-compiler-deduce-product task)
  (pathname-replace-type (task 'prop 'file) "typelib"))

(define-object <g-ir-compiler-task> (<file-task>)
  (arguments '())
  (properties
   `((product (singleton pathname) ,g-ir-compiler-deduce-product)
     (file (singleton pathname))))
  
  (step-prototype <g-ir-compiler-step>)
  
  ((new self resend name args props)
   (let ((task (resend #f 'new name args props)))
     (task 'add-value-slot! 'products (list (task 'prop 'product)))
     (task 'add-value-slot! 'sources (list (task 'prop 'file)))
     task)))

;;; C compiler

(define-object <cc-task> (<file-task>)
  (arguments '())
  (properties
   '((compiler (singleton symbol))
     (c-flags (list-of string) ())
     (ld-flags (list-of string) ())))
  
  ((construct-step self resend project)
   (let ((step (resend #f 'construct-step project))
         (compiler-step (project 'get-step (self 'prop 'compiler))))
     (step 'add-value-slot! 'compiler (object ((compiler-step 'build))
                                        (c-flags (self 'prop 'c-flags))
                                        (ld-flags (self 'prop 'ld-flags))))
     step)))

(define-object <cc-link-step> (<file-step>)
  ((build self resend)
   (let ((compiler (self 'compiler)))
     (compiler 'compile-program (self 'prop 'product) ((self 'task) 'sources)))))

(define-object <cc-link-task> (<cc-task>)
  (arguments '())
  (properties
   `((product (singleton pathname))
     (objects (list-of pathname))
     (libraries (list-of pathname) ())
     ,@(filter-props '(compiler c-flags ld-flags) (<cc-task> 'properties))))
   
  (step-prototype <cc-link-step>)

  ((new self resend name args props)
   (let ((task (resend #f 'new name args props)))
     (task 'add-value-slot! 'products (list (task 'prop 'product)))
     (task 'add-value-slot! 'sources (append (task 'prop 'objects)
                                             (task 'prop 'libraries)))
     task)))

(define-object <cc-compile-step> (<file-step>)
  ((build self resend)
   (let ((compiler (self 'compiler)))
     (compiler 'compile-object (self 'prop 'product) ((self 'task) 'sources)))))

(define-object <cc-compile-task> (<cc-task>)
  (arguments '())
  (properties
   `((product (singleton pathname))
     ,@(filter-props '(sources) (<file-task> 'properties))
     ,@(filter-props '(compiler c-flags ld-flags) (<cc-task> 'properties))))

  (step-prototype <cc-compile-step>)

  ((new self resend name args props)
   (let ((task (resend #f 'new name args props)))
     (task 'add-value-slot! 'products (list (task 'prop 'product)))
     task)))

;;; libtool

(define (make-lt-cc-task task)
  (object (task)
    (properties
     (append '((lt-flags (list-of string) ())) (task 'properties)))
    ((construct-step self resend project)
     (let* ((step (resend #f 'construct-step project))
            (original-compiler (step 'compiler)))
       (step 'add-value-slot! 'compiler (make-lt-cc-wrapper original-compiler
                                                            self))
       step))))

(define (make-lt-cc-wrapper cc task)
  (object (cc)
    ((program-path self resend)
     (<lt-program> 'program-path))
    
    ((compile-object-args self resend dest srcs)
     (append (lt-cc-wrapper-args "compile")
             (list (resend cc 'program-path))
             (task 'prop 'lt-flags)
             (resend #f 'compile-object-args dest srcs)))
    ((compile-program-args self resend dest srcs)
     (append (lt-cc-wrapper-args "link")
             (list (cc 'program-path))
             (task 'prop 'lt-flags)
             (resend #f 'compile-program-args dest srcs)))))

(define (lt-cc-wrapper-args mode)
  (list "--quiet" "--tag=CC" (string-append "--mode=" mode)))

(define <lt-link-task> (make-lt-cc-task <cc-link-task>))

(define-object <lt-compile-task> ((make-lt-cc-task <cc-compile-task>))
  ((construct-step self resend project)
   (let ((step (resend #f 'construct-step project)))
     (step 'add-value-slot!
           'products
           (append (step 'products)
                   (list (pathname-replace-type (self 'prop 'product) "o"))))
     step)))

(define (run-libtool argv)
  (<lt-program> 'run argv))

(define-object <lt-program> (<program>)
  (program "libtool")
  (runner (<runner> 'clone))
  ((run self resend argv)
   ((self 'runner) 'run (cons* (self 'program-path) "--quiet" argv))))



(define logger:conjure.libtool (make-logger logger:conjure 'libtool))
(define log/libtool (make-fmt-log logger:conjure.libtool))

(define logger:conjure.sbank (make-logger logger:conjure 'sbank))
(define log/sbank (make-fmt-log logger:conjure.sbank))

(register-task-prototype 'lt-compile <lt-compile-task>)
(register-task-prototype 'lt-link <lt-link-task>)
(register-task-prototype 'g-ir-scanner <g-ir-scanner-task>)
(register-task-prototype 'g-ir-compiler <g-ir-compiler-task>)

)

;; Local Variables:
;; scheme-indent-styles: (as-match conjure-dsl)
;; End:
