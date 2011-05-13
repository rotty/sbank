;;; utils.scm --- Unit tests for (sbank support utils)

;; Copyright (C) 2008, 2009, 2011 Andreas Rottmann <a.rottmann@gmx.at>

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

(import (rnrs)
        (wak trc-testing)
        (sbank support utils))

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
    (scheme-ified-symbol "VBox"))
  (test-equal '<http-version>
    (scheme-ified-symbol "HTTPVersion"))
  (test-equal '<foo-io-channel>
    (scheme-ified-symbol "FooIOChannel"))
  (test-equal '<io-win32-channel>
    (scheme-ified-symbol "IOWin32Channel"))
  (test-equal '<foo_bar>
    (scheme-ified-symbol "Foo_Bar")))

(define-test-case utils-tests name-symbol/prefix ()
  (test-equal 'gtk-main (name-symbol/prefix 'main 'gtk-))
  (test-equal '*gtk-a-constant* (name-symbol/prefix '*a-constant* 'gtk-))
  (test-equal '<gtk-window> (name-symbol/prefix '<window> 'gtk-)))

(run-test-suite utils-tests)

;; Local Variables:
;; scheme-indent-styles: (trc-testing)
;; End:
