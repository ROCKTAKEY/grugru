;;; grugru-utils-test.el --- Test for grugru-utils

;; Copyright (C) 2021  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'undercover)
(undercover "*.el"
            (:report-format 'codecov)
            (:report-file "coverage-final.json")
            (:send-report nil))

(require 'grugru-utils)

(require 'ert)

(ert-deftest grugru-utils-lisp-exchange-args--same-symbol ()
  (should
   (string=
    (grugru-utils-lisp-exchange-args "(abc 1 2 3 4)" '(1 3 2 4))
    "(abc 1 3 2 4)"))
  (should
   (string=
    (grugru-utils-lisp-exchange-args "(abc 1 2 3 4 )" '(1 3 2 4))
    "(abc 1 3 2 4)"))
  (should
   (string=
    (grugru-utils-lisp-exchange-args "(abc 1 2 3\n 4)" '(1 3 2 4))
    "(abc 1 3 2\n 4)"))
  (should
   (string=
    (grugru-utils-lisp-exchange-args "(abc 1 2\n 3 4)" '(1 3 2 4))
    "(abc 1\n 3 2 4)"))
  (should
   (string=
    (grugru-utils-lisp-exchange-args "(abc\n 1 2 3 4)" '(1 3 2 4))
    "(abc\n 1 3 2 4)")))

(ert-deftest grugru-utils-lisp-exchange-args-same-list ()
  (should
   (string=
    (grugru-utils-lisp-exchange-args "(abc 1 2 (lambda (_arg) 3) 4)" '(1 3 2 4))
    "(abc 1 (lambda (_arg) 3) 2 4)"))
  (should
   (string=
    (grugru-utils-lisp-exchange-args "(abc 1 2 (lambda (_arg) 3) 4 )" '(1 3 2 4))
    "(abc 1 (lambda (_arg) 3) 2 4)"))
  (should
   (string=
    (grugru-utils-lisp-exchange-args "(abc 1 2 (lambda (_arg) 3)\n 4)" '(1 3 2 4))
    "(abc 1 (lambda (_arg) 3) 2\n 4)"))
  (should
   (string=
    (grugru-utils-lisp-exchange-args "(abc 1 2\n (lambda (_arg) 3) 4)" '(1 3 2 4))
    "(abc 1\n (lambda (_arg) 3) 2 4)"))
  (should
   (string=
    (grugru-utils-lisp-exchange-args "(abc\n 1 2 (lambda (_arg) 3) 4)" '(1 3 2 4))
    "(abc\n 1 (lambda (_arg) 3) 2 4)"))
  (should
   (string=
    (grugru-utils-lisp-exchange-args "(abc 1 2\n (lambda (_arg)\n 3) 4)" '(1 3 2 4))
    "(abc 1\n (lambda (_arg)\n 3) 2 4)"))
  (should
   (string=
    (grugru-utils-lisp-exchange-args "(abc\n 1 2 (lambda (_arg)\n 3) 4)" '(1 3 2 4))
    "(abc\n 1 (lambda (_arg)\n 3) 2 4)")))

(ert-deftest grugru-utils-lisp-exchange-args--less-symbol ()
  (should
   (string=
    (grugru-utils-lisp-exchange-args "(abc 1 2 3 4)" '(1 3 2))
    "(abc 1 3 2 4)"))
  (should
   (string=
    (grugru-utils-lisp-exchange-args "(abc 1 2 3 4 )" '(1 3 2))
    "(abc 1 3 2 4)"))
  (should
   (string=
    (grugru-utils-lisp-exchange-args "(abc 1 2 3\n 4)" '(1 3 2))
    "(abc 1 3 2\n 4)"))
  (should
   (string=
    (grugru-utils-lisp-exchange-args "(abc 1 2\n 3 4)" '(1 3 2))
    "(abc 1\n 3 2 4)"))
  (should
   (string=
    (grugru-utils-lisp-exchange-args "(abc\n 1 2 3 4)" '(1 3 2))
    "(abc\n 1 3 2 4)")))

(ert-deftest grugru-utils-lisp-exchange-args-less-list ()
  (should
   (string=
    (grugru-utils-lisp-exchange-args "(abc 1 2 (lambda (_arg) 3) 4)" '(1 3 2))
    "(abc 1 (lambda (_arg) 3) 2 4)"))
  (should
   (string=
    (grugru-utils-lisp-exchange-args "(abc 1 2 (lambda (_arg) 3) 4 )" '(1 3 2))
    "(abc 1 (lambda (_arg) 3) 2 4)"))
  (should
   (string=
    (grugru-utils-lisp-exchange-args "(abc 1 2 (lambda (_arg) 3)\n 4)" '(1 3 2))
    "(abc 1 (lambda (_arg) 3) 2\n 4)"))
  (should
   (string=
    (grugru-utils-lisp-exchange-args "(abc 1 2\n (lambda (_arg) 3) 4)" '(1 3 2))
    "(abc 1\n (lambda (_arg) 3) 2 4)"))
  (should
   (string=
    (grugru-utils-lisp-exchange-args "(abc\n 1 2 (lambda (_arg) 3) 4)" '(1 3 2))
    "(abc\n 1 (lambda (_arg) 3) 2 4)"))
  (should
   (string=
    (grugru-utils-lisp-exchange-args "(abc 1 2\n (lambda (_arg)\n 3) 4)" '(1 3 2))
    "(abc 1\n (lambda (_arg)\n 3) 2 4)"))
  (should
   (string=
    (grugru-utils-lisp-exchange-args "(abc\n 1 2 (lambda (_arg)\n 3) 4)" '(1 3 2))
    "(abc\n 1 (lambda (_arg)\n 3) 2 4)")))


(provide 'grugru-utils-test)
;;; grugru-utils-test.el ends here
