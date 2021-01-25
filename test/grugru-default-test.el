;;; grugru-default-test.el --- test for grugru-default

;; Copyright (C) 2021  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords:

;; Version: 0.0.0
;; Package-Requires:
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

;;; Commentary:

;;

;;; Code:
(require 'undercover)
(undercover "*.el"
            (:report-file "coverage-final.json")
            (:send-report nil))

(require 'grugru)
(require 'grugru-default)

(require 'ert)
(require 'cursor-test)

(ert-deftest grugru-default@emacs-lisp ()
  (should
   (equal
    (grugru-default@emacs-lisp+nth!aref "(nth 1 '(foo bar))")
    (cons '(1 . 4) "(aref '(foo bar) 1)")))
  (should
   (equal
    (grugru-default@emacs-lisp+nth!aref "(aref '(foo bar) 1)")
    (cons '(1 . 5) "(nth 1 '(foo bar))"))))

(ert-deftest grugru-default-setup ()
  (let (grugru--global-grugru-alist
        grugru-major-modes-grugru-alist)
   (grugru-default-setup)))

(provide 'grugru-default-test)
;;; grugru-default-test.el ends here
