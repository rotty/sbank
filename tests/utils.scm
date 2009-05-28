;;; utils.scm --- Unit tests for (sbank support utils)

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

;;; Code:

(define-test-suite utils-tests
  "Utilities")

(define-test-case utils-tests scheme-ified-symbol ()
  (test-equal 'foobar (scheme-ified-symbol "foobar"))
  (test-equal 'foo-bar-1 (scheme-ified-symbol "foo_bar_1"))
  (test-equal '*this-is-a-constant*
    (scheme-ified-symbol "THIS_IS_A_CONSTANT"))
  (test-equal '<window> (scheme-ified-symbol "Window"))
  (test-equal '<foo-dialog-bar>
    (scheme-ified-symbol "FooDialogBar"))
  (test-equal '<v-box>
    (scheme-ified-symbol "VBox")))

(define-test-case utils-tests c-ified-string ()
  (test-equal "foobar" (c-ified-string 'foobar))
  (test-equal "foo_bar_1" (c-ified-string 'foo-bar-1))
  (test-equal "THIS_IS_A_CONSTANT" (c-ified-string '*this-is-a-constant*))
  (test-equal "Window" (c-ified-string '<window>))
  (test-equal "FooDialogBar"
    (c-ified-string '<foo-dialog-bar>)))

(define-test-case utils-tests name-symbol/prefix ()
  (test-equal 'gtk-main (name-symbol/prefix 'main 'gtk-))
  (test-equal '*gtk-a-constant* (name-symbol/prefix '*a-constant* 'gtk-))
  (test-equal '<gtk-window> (name-symbol/prefix '<window> 'gtk-)))

(run-test-suite utils-tests)

;; Local Variables:
;; scheme-indent-styles: (trc-testing)
;; End:
