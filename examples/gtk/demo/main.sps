;; Ported to sbank from guile-gnome
;; Copyright (C) 2003,2004,2008 Free Software Foundation, Inc.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org
#!r6rs

(import (except (rnrs) delete-file file-exists?)
        (rnrs eval)
        (spells receive)
        (spells pathname)
        (spells filesys)
        (spells find-file)
        (spells tracing)
        (only (spells strings) string-unfold)
        (only (spells assert) cout)
        (sbank gtk)
        (sbank typelib)
        (sbank typelib decorators))

(gtk-setup!)
(typelib-import
 (prefix (only ("Gtk" #f)
               <window> <scrolled-window>
               <notebook>
               <tree-view> <tree-view-column>
               <list-store>
               <cell-renderer-text>
               <text-view>
               <label> <h-box>
               init main main-quit)
         gtk-))

(define (port->string p)
  (string-unfold eof-object?
                 values
                 (lambda (seed)
                   (read-char p))
                 (read-char p)))

(define demo-dir '((sbank examples gtk demo)))

;; Detecting and loading up the demos
(define (get-demos)
  (let ((dir (find-file demo-dir (library-search-paths))))
    (define (cons-demo pathname demos)
      (let ((file (pathname-file pathname)))
        (if (and (file-regular? pathname)
                 (string=? (file-type file) "sls"))
            (cons
             (cons (environment
                    `(sbank examples gtk demo ,(string->symbol (file-name file))))
                   (call-with-input-file (x->namestring pathname)
                     port->string))
             demos)
            demos)))
    (or dir
        (error 'get-demos "unable to locate demo dir"
               (x->namestring demo-dir) (library-search-paths)))
    (directory-fold dir cons-demo '())))

(define (demo-environment demo)
  (car demo))
(define (demo-source demo)
  (cdr demo))
(define (demo-name demo)
  (eval 'name (demo-environment demo)))
(define (demo-main demo)
  (eval 'main (demo-environment demo)))
(define (demo-description demo)
  (eval 'description (demo-environment demo)))

(define (make-scrolled)
  (let ((s-w (send <gtk-scrolled-window> (new #f #f))))
    (send s-w (set-policy 'automatic 'automatic))
    s-w))

(define (make-tree-view)
  (let* ((store (send <gtk-list-store> (newv (list 'boxed 'utf8))))
         (tree-view (send <gtk-tree-view> (new)))
         (cellrenderer (send <gtk-cell-renderer-text> (new)))
         (column (send <gtk-tree-view-column> (new)))
         (selection (send tree-view (get-selection))))

    (for-each
     (lambda (demo)
       (send store
         (set-values (send store (append))
                     0 demo
                     1 (demo-name demo))))
     (get-demos))

    (send selection (set-mode 'single))
    (send column
      (set 'title "Widget (double-click to show)")
      (pack-start cellrenderer #t)
      (add-attribute cellrenderer "text" 1))
    (send tree-view
      (set-model store)
      (append-column column)
      (set-size-request 200 -1))
    tree-view))

(define (make-text-view notebook wrap-mode pix name)
  (let* ((scrolled (make-scrolled))
         (text-view (send <gtk-text-view> (new)))
         (text-buffer (send text-view (get-buffer)))
         (label (send <gtk-label> (new name))))
    (send text-view
      (set 'editable #f
           'cursor-visible #f
           'wrap-mode wrap-mode
           'pixels-above-lines pix
           'pixels-below-lines pix))
    (send scrolled
      (set 'shadow-type 'in)
      (add text-view))
    (send label
      (set-use-underline #t))
    (send notebook
      (append-page scrolled label))
    text-buffer))

(define (clear-buffer buf)
  (receive (start end) (send buf (get-bounds))
    (send buf (delete start end))))

(define (main argv)
  (gtk-init argv)

  (let* ((main-window (send <gtk-window> (new 'toplevel)))
         (notebook (send <gtk-notebook> (new)))
         (info-buffer (make-text-view notebook 'word 2 "_Info"))
         (source-buffer (make-text-view notebook 'none 0 "_Source"))
         (tree-view (make-tree-view)))

    (define (set-demo demo)
      (clear-buffer info-buffer)
      (let ((name (demo-name demo))
            (desc (demo-description demo))
            (iter (send info-buffer (get-iter-at-offset 0)))
            (start #f))
        (send info-buffer (insert iter name -1))
        (set! start (send info-buffer (get-iter-at-offset 0)))
        (send info-buffer
          (apply-tag-by-name "title" start iter)
          (insert iter "\n" -1)
          (insert iter desc -1)))
      (clear-buffer source-buffer)
      (let ((source (demo-source demo))
            (iter (send source-buffer (get-iter-at-offset 0)))
            (start #f))
        (send source-buffer (insert iter source -1))
        (set! start (send source-buffer (get-iter-at-offset 0)))
        (send source-buffer (apply-tag-by-name "source" start iter))))

    (send info-buffer (create-tag "title" 'font "Sans 18"))
    (send source-buffer (create-tag "source" 'font "Courier 12"
                                    'pixels-above-lines 0
                                    'pixels-below-lines 0))
    (send main-window
      (set-title "sbank GTK+ Demo")
      (set-default-size 600 400))


    ;; Pack the widgetssi
    (let ((hbox (send <gtk-h-box> (new #f 5))))
      (let ((scrolled (make-scrolled)))
        (send scrolled (add tree-view))
        (send hbox
          (pack-start scrolled #f #f 0)
          (pack-start notebook #t #t 0)))
      (send main-window (add hbox)))

    ;; Signals...
    (send main-window (connect 'delete-event (lambda (w e) (gtk-main-quit) #f)))
    (send tree-view
      (connect 'row-activated
               (lambda (tview path col)
                 (let* ((model (send tview (get-model)))
                        (iter (send model (get-iter path)))
                        (demo (send model (get-value iter 0)))
                        (main (demo-main demo)))
                   (main)))))
    (send (send tree-view (get-selection))
      (connect 'changed
               (lambda (selection)
                 (receive (model iter) (send selection (get-selected))
                   (if iter
                       (set-demo (send model (get-value iter 0)))))))
      (emit 'changed))

    (send main-window (show-all))

    (gtk-main)))

(main (command-line))
