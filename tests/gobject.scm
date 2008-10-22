;;; gobject.scm --- Unit tests for (sbank gobject)

;; Copyright (C) 2008 Andreas Rottmann <a.rottmann@gmx.at>

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


(testeez "basic"
  (test-define "widget" <gtk-widget>
               (make-gobject-class "Gtk" "Widget" #f
                                   '()
                                   `((show . ,(lambda (inst)
                                                (list 'show: inst))))))
  (test-define "window" <gtk-window>
               (make-gobject-class "Gtk" "Window" <gtk-widget>
                                   `((new . ,(lambda () 'new-widget)))
                                   `()))
  (test/equal "construct/show"(send (send <gtk-window> (new)) (show)) (list 'show: 'new-widget)))
