;;; grugru-test.el --- test for grugru

;; Copyright (C) 2019-2020  ROCKTAKEY

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
(undercover "*.el")

(require 'grugru)
(require 'ert)
(require 'cursor-test)

;; Global

(ert-deftest grugru-define-global-2-symbol-end-same-length ()
  (let (grugru-buffer-global-grugru-alist)
    (cursor-test/equal*
     :init "foo| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "bar"))
       (call-interactively #'grugru))
     :expect "bar| hoge")
    (cursor-test/equal*
     :init "bar| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "bar"))
       (call-interactively #'grugru))
     :expect "foo| hoge")))

(ert-deftest grugru-define-global-3-symbol-end-same-length ()
  (let (grugru-buffer-global-grugru-alist)
    (cursor-test/equal*
     :init "foo| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "bar" "baz"))
       (call-interactively #'grugru))
     :expect "bar| hoge")
    (cursor-test/equal*
     :init "bar| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "bar" "baz"))
       (call-interactively #'grugru))
     :expect "baz| hoge")

    (cursor-test/equal*
     :init "baz| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "bar" "baz"))
       (call-interactively #'grugru))
     :expect "foo| hoge")))

(ert-deftest grugru-define-global-2-symbol-end-different-length ()
  (let (grugru-buffer-global-grugru-alist)
    (cursor-test/equal*
     :init "foo| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar"))
       (call-interactively #'grugru))
     :expect "baa|r hoge")
    (cursor-test/equal*
     :init "baar| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar"))
       (call-interactively #'grugru))
     :expect "foo| hoge")

    (cursor-test/equal*
     :init "foo| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar"))
       (call-interactively #'grugru)
       (call-interactively #'grugru))
     :expect "foo| hoge")))

(ert-deftest grugru-define-global-3-symbol-end-different-length ()
  (let (grugru-buffer-global-grugru-alist)
    (cursor-test/equal*
     :init "foo| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar" "baaaz"))
       (call-interactively #'grugru))
     :expect "baa|r hoge")
    (cursor-test/equal*
     :init "baar| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar" "baaaz"))
       (call-interactively #'grugru))
     :expect "baaa|z hoge")
    (cursor-test/equal*
     :init "baaaz| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar" "baaaz"))
       (call-interactively #'grugru))
     :expect "foo| hoge")

    (cursor-test/equal*
     :init "foo| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar" "baaaz"))
       (call-interactively #'grugru)
       (call-interactively #'grugru)
       (call-interactively #'grugru))
     :expect "foo| hoge")
    (cursor-test/equal*
     :init "baar| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar" "baaaz"))
       (call-interactively #'grugru)
       (call-interactively #'grugru))
     :expect "foo| hoge")))

(ert-deftest grugru-define-global-2-symbol-beginning-same-length ()
  (let (grugru-buffer-global-grugru-alist)
    (cursor-test/equal*
     :init "hoge |foo"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "bar"))
       (call-interactively #'grugru))
     :expect "hoge |bar")
    (cursor-test/equal*
     :init "hoge |bar"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "bar"))
       (call-interactively #'grugru))
     :expect "hoge |foo")))

(ert-deftest grugru-define-global-3-symbol-beginning-same-length ()
  (let (grugru-buffer-global-grugru-alist)
    (cursor-test/equal*
     :init "hoge |foo"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "bar" "baz"))
       (call-interactively #'grugru))
     :expect "hoge |bar")
    (cursor-test/equal*
     :init "hoge |bar"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "bar" "baz"))
       (call-interactively #'grugru))
     :expect "hoge |baz")

    (cursor-test/equal*
     :init "hoge |baz"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "bar" "baz"))
       (call-interactively #'grugru))
     :expect "hoge |foo")))

(ert-deftest grugru-define-global-2-symbol-beginning-different-length ()
  (let (grugru-buffer-global-grugru-alist)
    (cursor-test/equal*
     :init "hoge |foo"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar"))
       (call-interactively #'grugru))
     :expect "hoge |baar")
    (cursor-test/equal*
     :init "hoge |baar"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar"))
       (call-interactively #'grugru))
     :expect "hoge |foo")

    (cursor-test/equal*
     :init "hoge |foo"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar"))
       (call-interactively #'grugru)
       (call-interactively #'grugru))
     :expect "hoge |foo")))

(ert-deftest grugru-define-global-3-symbol-beginning-different-length ()
  (let (grugru-buffer-global-grugru-alist)
    (cursor-test/equal*
     :init "hoge |foo"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar" "baaaz"))
       (call-interactively #'grugru))
     :expect "hoge |baar")
    (cursor-test/equal*
     :init "hoge |baar"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar" "baaaz"))
       (call-interactively #'grugru))
     :expect "hoge |baaaz")
    (cursor-test/equal*
     :init "hoge |baaaz"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar" "baaaz"))
       (call-interactively #'grugru))
     :expect "hoge |foo")

    (cursor-test/equal*
     :init "hoge |foo"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar" "baaaz"))
       (call-interactively #'grugru)
       (call-interactively #'grugru)
       (call-interactively #'grugru))
     :expect "hoge |foo")
    (cursor-test/equal*
     :init "hoge |baar"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar" "baaaz"))
       (call-interactively #'grugru)
       (call-interactively #'grugru))
     :expect "hoge |foo")))



(ert-deftest grugru-define-global-2-word-end-same-length ()
  (let (grugru-buffer-global-grugru-alist)
    (cursor-test/equal*
     :init "aaa-foo| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'word '("foo" "bar"))
       (call-interactively #'grugru))
     :expect "aaa-bar| hoge")
    (cursor-test/equal*
     :init "aaa-bar| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'word '("foo" "bar"))
       (call-interactively #'grugru))
     :expect "aaa-foo| hoge")))

(ert-deftest grugru-define-global-3-word-end-same-length ()
  (let (grugru-buffer-global-grugru-alist)
    (cursor-test/equal*
     :init "aaa-foo| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'word '("foo" "bar" "baz"))
       (call-interactively #'grugru))
     :expect "aaa-bar| hoge")
    (cursor-test/equal*
     :init "aaa-bar| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'word '("foo" "bar" "baz"))
       (call-interactively #'grugru))
     :expect "aaa-baz| hoge")

    (cursor-test/equal*
     :init "aaa-baz| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'word '("foo" "bar" "baz"))
       (call-interactively #'grugru))
     :expect "aaa-foo| hoge")))

(ert-deftest grugru-define-global-2-word-end-different-length ()
  (let (grugru-buffer-global-grugru-alist)
    (cursor-test/equal*
     :init "aaa-foo| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'word '("foo" "baar"))
       (call-interactively #'grugru))
     :expect "aaa-baa|r hoge")
    (cursor-test/equal*
     :init "aaa-baar| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'word '("foo" "baar"))
       (call-interactively #'grugru))
     :expect "aaa-foo| hoge")

    (cursor-test/equal*
     :init "aaa-foo| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'word '("foo" "baar"))
       (call-interactively #'grugru)
       (call-interactively #'grugru))
     :expect "aaa-foo| hoge")))

(ert-deftest grugru-define-global-3-word-end-different-length ()
  (let (grugru-buffer-global-grugru-alist)
    (cursor-test/equal*
     :init "aaa-foo| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'word '("foo" "baar" "baaaz"))
       (call-interactively #'grugru))
     :expect "aaa-baa|r hoge")
    (cursor-test/equal*
     :init "aaa-baar| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'word '("foo" "baar" "baaaz"))
       (call-interactively #'grugru))
     :expect "aaa-baaa|z hoge")
    (cursor-test/equal*
     :init "aaa-baaaz| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'word '("foo" "baar" "baaaz"))
       (call-interactively #'grugru))
     :expect "aaa-foo| hoge")

    (cursor-test/equal*
     :init "aaa-foo| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'word '("foo" "baar" "baaaz"))
       (call-interactively #'grugru)
       (call-interactively #'grugru)
       (call-interactively #'grugru))
     :expect "aaa-foo| hoge")
    (cursor-test/equal*
     :init "aaa-baar| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'word '("foo" "baar" "baaaz"))
       (call-interactively #'grugru)
       (call-interactively #'grugru))
     :expect "aaa-foo| hoge")))

(ert-deftest grugru-define-global-2-word-beginning-same-length ()
  (let (grugru-buffer-global-grugru-alist)
    (cursor-test/equal*
     :init "hoge |foo-aaa"
     :exercise
     (lambda ()
       (grugru-define-global 'word '("foo" "bar"))
       (call-interactively #'grugru))
     :expect "hoge |bar")
    (cursor-test/equal*
     :init "hoge |aaa-bar"
     :exercise
     (lambda ()
       (grugru-define-global 'word '("foo" "bar"))
       (call-interactively #'grugru))
     :expect "hoge |foo-aaa")))

(ert-deftest grugru-define-global-3-word-beginning-same-length ()
  (let (grugru-buffer-global-grugru-alist)
    (cursor-test/equal*
     :init "hoge |foo-aaa"
     :exercise
     (lambda ()
       (grugru-define-global 'word '("foo" "bar" "baz"))
       (call-interactively #'grugru))
     :expect "hoge |bar-aaa")
    (cursor-test/equal*
     :init "hoge |bar-aaa"
     :exercise
     (lambda ()
       (grugru-define-global 'word '("foo" "bar" "baz"))
       (call-interactively #'grugru))
     :expect "hoge |baz-aaa")

    (cursor-test/equal*
     :init "hoge |baz-aaa"
     :exercise
     (lambda ()
       (grugru-define-global 'word '("foo" "bar" "baz"))
       (call-interactively #'grugru))
     :expect "hoge |foo-aaa")))

(ert-deftest grugru-define-global-2-word-beginning-different-length ()
  (let (grugru-buffer-global-grugru-alist)
    (cursor-test/equal*
     :init "hoge |foo-aaa"
     :exercise
     (lambda ()
       (grugru-define-global 'word '("foo" "baar"))
       (call-interactively #'grugru))
     :expect "hoge |baar-aaa")
    (cursor-test/equal*
     :init "hoge |baar -aaa"
     :exercise
     (lambda ()
       (grugru-define-global 'word '("foo" "baar"))
       (call-interactively #'grugru))
     :expect "hoge |foo-aaa")

    (cursor-test/equal*
     :init "hoge |foo-aaa"
     :exercise
     (lambda ()
       (grugru-define-global 'word '("foo" "baar"))
       (call-interactively #'grugru)
       (call-interactively #'grugru))
     :expect "hoge |foo-aaa")))

(ert-deftest grugru-define-global-3-word-beginning-different-length ()
  (let (grugru-buffer-global-grugru-alist)
    (cursor-test/equal*
     :init "hoge |foo-aaa"
     :exercise
     (lambda ()
       (grugru-define-global 'word '("foo" "baar" "baaaz"))
       (call-interactively #'grugru))
     :expect "hoge |baar-aaa")
    (cursor-test/equal*
     :init "hoge |baar-aaa"
     :exercise
     (lambda ()
       (grugru-define-global 'word '("foo" "baar" "baaaz"))
       (call-interactively #'grugru))
     :expect "hoge |baaaz-aaa")
    (cursor-test/equal*
     :init "hoge |baaaz-aaa"
     :exercise
     (lambda ()
       (grugru-define-global 'word '("foo" "baar" "baaaz"))
       (call-interactively #'grugru))
     :expect "hoge |foo-aaa")

    (cursor-test/equal*
     :init "hoge |foo-aaa"
     :exercise
     (lambda ()
       (grugru-define-global 'word '("foo" "baar" "baaaz"))
       (call-interactively #'grugru)
       (call-interactively #'grugru)
       (call-interactively #'grugru))
     :expect "hoge |foo-aaa")
    (cursor-test/equal*
     :init "hoge |baar-aaa"
     :exercise
     (lambda ()
       (grugru-define-global 'word '("foo" "baar" "baaaz"))
       (call-interactively #'grugru)
       (call-interactively #'grugru))
     :expect "hoge |foo-aaa")))


;; Local

(ert-deftest grugru-define-local-3-symbol-end-same-length ()
  (cursor-test/equal*
   :init "foo| hoge"
   :exercise
   (lambda ()
     (grugru-define-local 'symbol '("foo" "bar" "baz"))
     (call-interactively #'grugru))
   :expect "bar| hoge")
  (cursor-test/equal*
   :init "bar| hoge"
   :exercise
   (lambda ()
     (grugru-define-local 'symbol '("foo" "bar" "baz"))
     (call-interactively #'grugru))
   :expect "baz| hoge")

  (cursor-test/equal*
   :init "baz| hoge"
   :exercise
   (lambda ()
     (grugru-define-local 'symbol '("foo" "bar" "baz"))
     (call-interactively #'grugru))
   :expect "foo| hoge"))

(ert-deftest grugru-define-local-2-symbol-end-different-length ()
  (cursor-test/equal*
   :init "foo| hoge"
   :exercise
   (lambda ()
     (grugru-define-local 'symbol '("foo" "baar"))
     (call-interactively #'grugru))
   :expect "baa|r hoge")
  (cursor-test/equal*
   :init "baar| hoge"
   :exercise
   (lambda ()
     (grugru-define-local 'symbol '("foo" "baar"))
     (call-interactively #'grugru))
   :expect "foo| hoge")

  (cursor-test/equal*
   :init "foo| hoge"
   :exercise
   (lambda ()
     (grugru-define-local 'symbol '("foo" "baar"))
     (call-interactively #'grugru)
     (call-interactively #'grugru))
   :expect "foo| hoge"))

(ert-deftest grugru-define-local-3-symbol-end-different-length ()
  (cursor-test/equal*
   :init "foo| hoge"
   :exercise
   (lambda ()
     (grugru-define-local 'symbol '("foo" "baar" "baaaz"))
     (call-interactively #'grugru))
   :expect "baa|r hoge")
  (cursor-test/equal*
   :init "baar| hoge"
   :exercise
   (lambda ()
     (grugru-define-local 'symbol '("foo" "baar" "baaaz"))
     (call-interactively #'grugru))
   :expect "baaa|z hoge")
  (cursor-test/equal*
   :init "baaaz| hoge"
   :exercise
   (lambda ()
     (grugru-define-local 'symbol '("foo" "baar" "baaaz"))
     (call-interactively #'grugru))
   :expect "foo| hoge")

  (cursor-test/equal*
   :init "foo| hoge"
   :exercise
   (lambda ()
     (grugru-define-local 'symbol '("foo" "baar" "baaaz"))
     (call-interactively #'grugru)
     (call-interactively #'grugru)
     (call-interactively #'grugru))
   :expect "foo| hoge")
  (cursor-test/equal*
   :init "baar| hoge"
   :exercise
   (lambda ()
     (grugru-define-local 'symbol '("foo" "baar" "baaaz"))
     (call-interactively #'grugru)
     (call-interactively #'grugru))
   :expect "foo| hoge"))

(ert-deftest grugru-define-local-2-symbol-beginning-same-length ()
  (cursor-test/equal*
   :init "hoge |foo"
   :exercise
   (lambda ()
     (grugru-define-local 'symbol '("foo" "bar"))
     (call-interactively #'grugru))
   :expect "hoge |bar")
  (cursor-test/equal*
   :init "hoge |bar"
   :exercise
   (lambda ()
     (grugru-define-local 'symbol '("foo" "bar"))
     (call-interactively #'grugru))
   :expect "hoge |foo"))

(ert-deftest grugru-define-local-3-symbol-beginning-same-length ()
  (cursor-test/equal*
   :init "hoge |foo"
   :exercise
   (lambda ()
     (grugru-define-local 'symbol '("foo" "bar" "baz"))
     (call-interactively #'grugru))
   :expect "hoge |bar")
  (cursor-test/equal*
   :init "hoge |bar"
   :exercise
   (lambda ()
     (grugru-define-local 'symbol '("foo" "bar" "baz"))
     (call-interactively #'grugru))
   :expect "hoge |baz")

  (cursor-test/equal*
   :init "hoge |baz"
   :exercise
   (lambda ()
     (grugru-define-local 'symbol '("foo" "bar" "baz"))
     (call-interactively #'grugru))
   :expect "hoge |foo"))

(ert-deftest grugru-define-local-2-symbol-beginning-different-length ()
  (cursor-test/equal*
   :init "hoge |foo"
   :exercise
   (lambda ()
     (grugru-define-local 'symbol '("foo" "baar"))
     (call-interactively #'grugru))
   :expect "hoge |baar")
  (cursor-test/equal*
   :init "hoge |baar"
   :exercise
   (lambda ()
     (grugru-define-local 'symbol '("foo" "baar"))
     (call-interactively #'grugru))
   :expect "hoge |foo")

  (cursor-test/equal*
   :init "hoge |foo"
   :exercise
   (lambda ()
     (grugru-define-local 'symbol '("foo" "baar"))
     (call-interactively #'grugru)
     (call-interactively #'grugru))
   :expect "hoge |foo"))

(ert-deftest grugru-define-local-3-symbol-beginning-different-length ()
  (cursor-test/equal*
   :init "hoge |foo"
   :exercise
   (lambda ()
     (grugru-define-local 'symbol '("foo" "baar" "baaaz"))
     (call-interactively #'grugru))
   :expect "hoge |baar")
  (cursor-test/equal*
   :init "hoge |baar"
   :exercise
   (lambda ()
     (grugru-define-local 'symbol '("foo" "baar" "baaaz"))
     (call-interactively #'grugru))
   :expect "hoge |baaaz")
  (cursor-test/equal*
   :init "hoge |baaaz"
   :exercise
   (lambda ()
     (grugru-define-local 'symbol '("foo" "baar" "baaaz"))
     (call-interactively #'grugru))
   :expect "hoge |foo")

  (cursor-test/equal*
   :init "hoge |foo"
   :exercise
   (lambda ()
     (grugru-define-local 'symbol '("foo" "baar" "baaaz"))
     (call-interactively #'grugru)
     (call-interactively #'grugru)
     (call-interactively #'grugru))
   :expect "hoge |foo")
  (cursor-test/equal*
   :init "hoge |baar"
   :exercise
   (lambda ()
     (grugru-define-local 'symbol '("foo" "baar" "baaaz"))
     (call-interactively #'grugru)
     (call-interactively #'grugru))
   :expect "hoge |foo"))



(ert-deftest grugru-define-local-2-word-end-same-length ()
  (cursor-test/equal*
   :init "aaa-foo| hoge"
   :exercise
   (lambda ()
     (grugru-define-local 'word '("foo" "bar"))
     (call-interactively #'grugru))
   :expect "aaa-bar| hoge")
  (cursor-test/equal*
   :init "aaa-bar| hoge"
   :exercise
   (lambda ()
     (grugru-define-local 'word '("foo" "bar"))
     (call-interactively #'grugru))
   :expect "aaa-foo| hoge"))

(ert-deftest grugru-define-local-3-word-end-same-length ()
  (cursor-test/equal*
   :init "aaa-foo| hoge"
   :exercise
   (lambda ()
     (grugru-define-local 'word '("foo" "bar" "baz"))
     (call-interactively #'grugru))
   :expect "aaa-bar| hoge")
  (cursor-test/equal*
   :init "aaa-bar| hoge"
   :exercise
   (lambda ()
     (grugru-define-local 'word '("foo" "bar" "baz"))
     (call-interactively #'grugru))
   :expect "aaa-baz| hoge")

  (cursor-test/equal*
   :init "aaa-baz| hoge"
   :exercise
   (lambda ()
     (grugru-define-local 'word '("foo" "bar" "baz"))
     (call-interactively #'grugru))
   :expect "aaa-foo| hoge"))

(ert-deftest grugru-define-local-2-word-end-different-length ()
  (cursor-test/equal*
   :init "aaa-foo| hoge"
   :exercise
   (lambda ()
     (grugru-define-local 'word '("foo" "baar"))
     (call-interactively #'grugru))
   :expect "aaa-baa|r hoge")
  (cursor-test/equal*
   :init "aaa-baar| hoge"
   :exercise
   (lambda ()
     (grugru-define-local 'word '("foo" "baar"))
     (call-interactively #'grugru))
   :expect "aaa-foo| hoge")

  (cursor-test/equal*
   :init "aaa-foo| hoge"
   :exercise
   (lambda ()
     (grugru-define-local 'word '("foo" "baar"))
     (call-interactively #'grugru)
     (call-interactively #'grugru))
   :expect "aaa-foo| hoge"))

(ert-deftest grugru-define-local-3-word-end-different-length ()
  (cursor-test/equal*
   :init "aaa-foo| hoge"
   :exercise
   (lambda ()
     (grugru-define-local 'word '("foo" "baar" "baaaz"))
     (call-interactively #'grugru))
   :expect "aaa-baa|r hoge")
  (cursor-test/equal*
   :init "aaa-baar| hoge"
   :exercise
   (lambda ()
     (grugru-define-local 'word '("foo" "baar" "baaaz"))
     (call-interactively #'grugru))
   :expect "aaa-baaa|z hoge")
  (cursor-test/equal*
   :init "aaa-baaaz| hoge"
   :exercise
   (lambda ()
     (grugru-define-local 'word '("foo" "baar" "baaaz"))
     (call-interactively #'grugru))
   :expect "aaa-foo| hoge")

  (cursor-test/equal*
   :init "aaa-foo| hoge"
   :exercise
   (lambda ()
     (grugru-define-local 'word '("foo" "baar" "baaaz"))
     (call-interactively #'grugru)
     (call-interactively #'grugru)
     (call-interactively #'grugru))
   :expect "aaa-foo| hoge")
  (cursor-test/equal*
   :init "aaa-baar| hoge"
   :exercise
   (lambda ()
     (grugru-define-local 'word '("foo" "baar" "baaaz"))
     (call-interactively #'grugru)
     (call-interactively #'grugru))
   :expect "aaa-foo| hoge"))

(ert-deftest grugru-define-local-2-word-beginning-same-length ()
  (cursor-test/equal*
   :init "hoge |foo-aaa"
   :exercise
   (lambda ()
     (grugru-define-local 'word '("foo" "bar"))
     (call-interactively #'grugru))
   :expect "hoge |bar")
  (cursor-test/equal*
   :init "hoge |aaa-bar"
   :exercise
   (lambda ()
     (grugru-define-local 'word '("foo" "bar"))
     (call-interactively #'grugru))
   :expect "hoge |foo-aaa"))

(ert-deftest grugru-define-local-3-word-beginning-same-length ()
  (cursor-test/equal*
   :init "hoge |foo-aaa"
   :exercise
   (lambda ()
     (grugru-define-local 'word '("foo" "bar" "baz"))
     (call-interactively #'grugru))
   :expect "hoge |bar-aaa")
  (cursor-test/equal*
   :init "hoge |bar-aaa"
   :exercise
   (lambda ()
     (grugru-define-local 'word '("foo" "bar" "baz"))
     (call-interactively #'grugru))
   :expect "hoge |baz-aaa")

  (cursor-test/equal*
   :init "hoge |baz-aaa"
   :exercise
   (lambda ()
     (grugru-define-local 'word '("foo" "bar" "baz"))
     (call-interactively #'grugru))
   :expect "hoge |foo-aaa"))

(ert-deftest grugru-define-local-2-word-beginning-different-length ()
  (cursor-test/equal*
   :init "hoge |foo-aaa"
   :exercise
   (lambda ()
     (grugru-define-local 'word '("foo" "baar"))
     (call-interactively #'grugru))
   :expect "hoge |baar-aaa")
  (cursor-test/equal*
   :init "hoge |baar -aaa"
   :exercise
   (lambda ()
     (grugru-define-local 'word '("foo" "baar"))
     (call-interactively #'grugru))
   :expect "hoge |foo-aaa")

  (cursor-test/equal*
   :init "hoge |foo-aaa"
   :exercise
   (lambda ()
     (grugru-define-local 'word '("foo" "baar"))
     (call-interactively #'grugru)
     (call-interactively #'grugru))
   :expect "hoge |foo-aaa"))

(ert-deftest grugru-define-local-3-word-beginning-different-length ()
  (cursor-test/equal*
   :init "hoge |foo-aaa"
   :exercise
   (lambda ()
     (grugru-define-local 'word '("foo" "baar" "baaaz"))
     (call-interactively #'grugru))
   :expect "hoge |baar-aaa")
  (cursor-test/equal*
   :init "hoge |baar-aaa"
   :exercise
   (lambda ()
     (grugru-define-local 'word '("foo" "baar" "baaaz"))
     (call-interactively #'grugru))
   :expect "hoge |baaaz-aaa")
  (cursor-test/equal*
   :init "hoge |baaaz-aaa"
   :exercise
   (lambda ()
     (grugru-define-local 'word '("foo" "baar" "baaaz"))
     (call-interactively #'grugru))
   :expect "hoge |foo-aaa")

  (cursor-test/equal*
   :init "hoge |foo-aaa"
   :exercise
   (lambda ()
     (grugru-define-local 'word '("foo" "baar" "baaaz"))
     (call-interactively #'grugru)
     (call-interactively #'grugru)
     (call-interactively #'grugru))
   :expect "hoge |foo-aaa")
  (cursor-test/equal*
   :init "hoge |baar-aaa"
   :exercise
   (lambda ()
     (grugru-define-local 'word '("foo" "baar" "baaaz"))
     (call-interactively #'grugru)
     (call-interactively #'grugru))
   :expect "hoge |foo-aaa"))


;; Major-mode-local

(ert-deftest grugru-define-major-mode-local-2-symbol-end-same-length ()
  (let (grugru-major-modes-grugru-alist)
    (cursor-test/equal*
     :init "foo| hoge"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'symbol '("foo" "bar"))
         (call-interactively #'grugru))
     :expect "bar| hoge")
    (cursor-test/equal*
     :init "bar| hoge"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'symbol '("foo" "bar"))
         (call-interactively #'grugru))
     :expect "foo| hoge")))

(ert-deftest grugru-define-major-mode-local-3-symbol-end-same-length ()
  (let (grugru-major-modes-grugru-alist)
    (cursor-test/equal*
     :init "foo| hoge"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'symbol '("foo" "bar" "baz"))
         (call-interactively #'grugru))
     :expect "bar| hoge")
    (cursor-test/equal*
     :init "bar| hoge"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'symbol '("foo" "bar" "baz"))
         (call-interactively #'grugru))
     :expect "baz| hoge")

    (cursor-test/equal*
     :init "baz| hoge"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'symbol '("foo" "bar" "baz"))
         (call-interactively #'grugru))
     :expect "foo| hoge")))

(ert-deftest grugru-define-major-mode-local-2-symbol-end-different-length ()
  (let (grugru-major-modes-grugru-alist)
    (cursor-test/equal*
     :init "foo| hoge"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'symbol '("foo" "baar"))
         (call-interactively #'grugru))
     :expect "baa|r hoge")
    (cursor-test/equal*
     :init "baar| hoge"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'symbol '("foo" "baar"))
         (call-interactively #'grugru))
     :expect "foo| hoge")

    (cursor-test/equal*
     :init "foo| hoge"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'symbol '("foo" "baar"))
         (call-interactively #'grugru)
         (call-interactively #'grugru))
     :expect "foo| hoge")))

(ert-deftest grugru-define-major-mode-local-3-symbol-end-different-length ()
  (let (grugru-major-modes-grugru-alist)
    (cursor-test/equal*
     :init "foo| hoge"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'symbol '("foo" "baar" "baaaz"))
         (call-interactively #'grugru))
     :expect "baa|r hoge")
    (cursor-test/equal*
     :init "baar| hoge"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'symbol '("foo" "baar" "baaaz"))
         (call-interactively #'grugru))
     :expect "baaa|z hoge")
    (cursor-test/equal*
     :init "baaaz| hoge"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'symbol '("foo" "baar" "baaaz"))
         (call-interactively #'grugru))
     :expect "foo| hoge")

    (cursor-test/equal*
     :init "foo| hoge"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'symbol '("foo" "baar" "baaaz"))
         (call-interactively #'grugru)
         (call-interactively #'grugru)
         (call-interactively #'grugru))
     :expect "foo| hoge")
    (cursor-test/equal*
     :init "baar| hoge"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'symbol '("foo" "baar" "baaaz"))
         (call-interactively #'grugru)
         (call-interactively #'grugru))
     :expect "foo| hoge")))

(ert-deftest grugru-define-major-mode-local-2-symbol-beginning-same-length ()
  (let (grugru-major-modes-grugru-alist)
    (cursor-test/equal*
     :init "hoge |foo"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'symbol '("foo" "bar"))
         (call-interactively #'grugru))
     :expect "hoge |bar")
    (cursor-test/equal*
     :init "hoge |bar"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'symbol '("foo" "bar"))
         (call-interactively #'grugru))
     :expect "hoge |foo")))

(ert-deftest grugru-define-major-mode-local-3-symbol-beginning-same-length ()
  (let (grugru-major-modes-grugru-alist)
    (cursor-test/equal*
     :init "hoge |foo"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'symbol '("foo" "bar" "baz"))
         (call-interactively #'grugru))
     :expect "hoge |bar")
    (cursor-test/equal*
     :init "hoge |bar"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'symbol '("foo" "bar" "baz"))
         (call-interactively #'grugru))
     :expect "hoge |baz")

    (cursor-test/equal*
     :init "hoge |baz"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'symbol '("foo" "bar" "baz"))
         (call-interactively #'grugru))
     :expect "hoge |foo")))

(ert-deftest grugru-define-major-mode-local-2-symbol-beginning-different-length ()
  (let (grugru-major-modes-grugru-alist)
    (cursor-test/equal*
     :init "hoge |foo"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'symbol '("foo" "baar"))
         (call-interactively #'grugru))
     :expect "hoge |baar")
    (cursor-test/equal*
     :init "hoge |baar"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'symbol '("foo" "baar"))
         (call-interactively #'grugru))
     :expect "hoge |foo")

    (cursor-test/equal*
     :init "hoge |foo"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'symbol '("foo" "baar"))
         (call-interactively #'grugru)
         (call-interactively #'grugru))
     :expect "hoge |foo")))

(ert-deftest grugru-define-major-mode-local-3-symbol-beginning-different-length ()
  (let (grugru-major-modes-grugru-alist)
    (cursor-test/equal*
     :init "hoge |foo"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'symbol '("foo" "baar" "baaaz"))
         (call-interactively #'grugru))
     :expect "hoge |baar")
    (cursor-test/equal*
     :init "hoge |baar"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'symbol '("foo" "baar" "baaaz"))
         (call-interactively #'grugru))
     :expect "hoge |baaaz")
    (cursor-test/equal*
     :init "hoge |baaaz"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'symbol '("foo" "baar" "baaaz"))
         (call-interactively #'grugru))
     :expect "hoge |foo")

    (cursor-test/equal*
     :init "hoge |foo"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'symbol '("foo" "baar" "baaaz"))
         (call-interactively #'grugru)
         (call-interactively #'grugru)
         (call-interactively #'grugru))
     :expect "hoge |foo")
    (cursor-test/equal*
     :init "hoge |baar"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'symbol '("foo" "baar" "baaaz"))
         (call-interactively #'grugru)
         (call-interactively #'grugru))
     :expect "hoge |foo")))



(ert-deftest grugru-define-major-mode-local-2-word-end-same-length ()
  (let (grugru-major-modes-grugru-alist)
    (cursor-test/equal*
     :init "aaa-foo| hoge"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'word '("foo" "bar"))
         (call-interactively #'grugru))
     :expect "aaa-bar| hoge")
    (cursor-test/equal*
     :init "aaa-bar| hoge"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'word '("foo" "bar"))
         (call-interactively #'grugru))
     :expect "aaa-foo| hoge")))

(ert-deftest grugru-define-major-mode-local-3-word-end-same-length ()
  (let (grugru-major-modes-grugru-alist)
    (cursor-test/equal*
     :init "aaa-foo| hoge"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'word '("foo" "bar" "baz"))
         (call-interactively #'grugru))
     :expect "aaa-bar| hoge")
    (cursor-test/equal*
     :init "aaa-bar| hoge"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'word '("foo" "bar" "baz"))
         (call-interactively #'grugru))
     :expect "aaa-baz| hoge")

    (cursor-test/equal*
     :init "aaa-baz| hoge"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'word '("foo" "bar" "baz"))
         (call-interactively #'grugru))
     :expect "aaa-foo| hoge")))

(ert-deftest grugru-define-major-mode-local-2-word-end-different-length ()
  (let (grugru-major-modes-grugru-alist)
    (cursor-test/equal*
     :init "aaa-foo| hoge"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'word '("foo" "baar"))
         (call-interactively #'grugru))
     :expect "aaa-baa|r hoge")
    (cursor-test/equal*
     :init "aaa-baar| hoge"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'word '("foo" "baar"))
         (call-interactively #'grugru))
     :expect "aaa-foo| hoge")

    (cursor-test/equal*
     :init "aaa-foo| hoge"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'word '("foo" "baar"))
         (call-interactively #'grugru)
         (call-interactively #'grugru))
     :expect "aaa-foo| hoge")))

(ert-deftest grugru-define-major-mode-local-3-word-end-different-length ()
  (let (grugru-major-modes-grugru-alist)
    (cursor-test/equal*
     :init "aaa-foo| hoge"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'word '("foo" "baar" "baaaz"))
         (call-interactively #'grugru))
     :expect "aaa-baa|r hoge")
    (cursor-test/equal*
     :init "aaa-baar| hoge"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'word '("foo" "baar" "baaaz"))
         (call-interactively #'grugru))
     :expect "aaa-baaa|z hoge")
    (cursor-test/equal*
     :init "aaa-baaaz| hoge"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'word '("foo" "baar" "baaaz"))
         (call-interactively #'grugru))
     :expect "aaa-foo| hoge")

    (cursor-test/equal*
     :init "aaa-foo| hoge"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'word '("foo" "baar" "baaaz"))
         (call-interactively #'grugru)
         (call-interactively #'grugru)
         (call-interactively #'grugru))
     :expect "aaa-foo| hoge")
    (cursor-test/equal*
     :init "aaa-baar| hoge"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'word '("foo" "baar" "baaaz"))
         (call-interactively #'grugru)
         (call-interactively #'grugru))
     :expect "aaa-foo| hoge")))

(ert-deftest grugru-define-major-mode-local-2-word-beginning-same-length ()
  (let (grugru-major-modes-grugru-alist)
    (cursor-test/equal*
     :init "hoge |foo-aaa"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'word '("foo" "bar"))
         (call-interactively #'grugru))
     :expect "hoge |bar")
    (cursor-test/equal*
     :init "hoge |aaa-bar"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'word '("foo" "bar"))
         (call-interactively #'grugru))
     :expect "hoge |foo-aaa")))

(ert-deftest grugru-define-major-mode-local-3-word-beginning-same-length ()
  (let (grugru-major-modes-grugru-alist)
    (cursor-test/equal*
     :init "hoge |foo-aaa"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'word '("foo" "bar" "baz"))
         (call-interactively #'grugru))
     :expect "hoge |bar-aaa")
    (cursor-test/equal*
     :init "hoge |bar-aaa"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'word '("foo" "bar" "baz"))
         (call-interactively #'grugru))
     :expect "hoge |baz-aaa")

    (cursor-test/equal*
     :init "hoge |baz-aaa"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'word '("foo" "bar" "baz"))
         (call-interactively #'grugru))
     :expect "hoge |foo-aaa")))

(ert-deftest grugru-define-major-mode-local-2-word-beginning-different-length ()
  (let (grugru-major-modes-grugru-alist)
    (cursor-test/equal*
     :init "hoge |foo-aaa"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'word '("foo" "baar"))
         (call-interactively #'grugru))
     :expect "hoge |baar-aaa")
    (cursor-test/equal*
     :init "hoge |baar -aaa"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'word '("foo" "baar"))
         (call-interactively #'grugru))
     :expect "hoge |foo-aaa")

    (cursor-test/equal*
     :init "hoge |foo-aaa"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'word '("foo" "baar"))
         (call-interactively #'grugru)
         (call-interactively #'grugru))
     :expect "hoge |foo-aaa")))

(ert-deftest grugru-define-major-mode-local-3-word-beginning-different-length ()
  (let (grugru-major-modes-grugru-alist)
    (cursor-test/equal*
     :init "hoge |foo-aaa"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'word '("foo" "baar" "baaaz"))
         (call-interactively #'grugru))
     :expect "hoge |baar-aaa")
    (cursor-test/equal*
     :init "hoge |baar-aaa"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'word '("foo" "baar" "baaaz"))
         (call-interactively #'grugru))
     :expect "hoge |baaaz-aaa")
    (cursor-test/equal*
     :init "hoge |baaaz-aaa"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'word '("foo" "baar" "baaaz"))
         (call-interactively #'grugru))
     :expect "hoge |foo-aaa")

    (cursor-test/equal*
     :init "hoge |foo-aaa"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'word '("foo" "baar" "baaaz"))
         (call-interactively #'grugru)
         (call-interactively #'grugru)
         (call-interactively #'grugru))
     :expect "hoge |foo-aaa")
    (cursor-test/equal*
     :init "hoge |baar-aaa"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'word '("foo" "baar" "baaaz"))
         (call-interactively #'grugru)
         (call-interactively #'grugru))
     :expect "hoge |foo-aaa")))


(ert-deftest grugru-define-function ()
  (grugru-define-function grugru-test-1 ()
    "Document"
    (symbol "foo" "bar" "baz")
    (word "qwe" "rty" "uio"))
  (grugru-define-function grugru-test-2 ()
    (symbol "foo" "bar" "baz")
    (word "qwe" "rty" "uiop"))
  (cursor-test/equal*
   :init
   "foo|"
   :exercise
   (lambda ()
     (call-interactively #'grugru-test-1))
   :expect
   "bar|")
  (cursor-test/equal*
   :init
   "bar|"
   :exercise
   (lambda ()
     (call-interactively #'grugru-test-1))
   :expect
   "baz|")
  (cursor-test/equal*
   :init
   "baz|"
   :exercise
   (lambda ()
     (call-interactively #'grugru-test-1))
   :expect
   "foo|")
  (cursor-test/equal*
   :init
   "aaa-qwe|"
   :exercise
   (lambda ()
     (call-interactively #'grugru-test-1))
   :expect
   "aaa-rty|")
  (cursor-test/equal*
   :init
   "aaa-rty|"
   :exercise
   (lambda ()
     (call-interactively #'grugru-test-1))
   :expect
   "aaa-uio|")
  (cursor-test/equal*
   :init
   "aaa-uio|"
   :exercise
   (lambda ()
     (call-interactively #'grugru-test-1))
   :expect
   "aaa-qwe|")
  (cursor-test/equal*
   :init
   "aaa-qwe|"
   :exercise
   (lambda ()
     (call-interactively #'grugru-test-2))
   :expect
   "aaa-rty|")
  (cursor-test/equal*
   :init
   "aaa-rty|"
   :exercise
   (lambda ()
     (call-interactively #'grugru-test-2))
   :expect
   "aaa-uio|p")
  (cursor-test/equal*
   :init
   "aaa-uiop|"
   :exercise
   (lambda ()
     (call-interactively #'grugru-test-2))
   :expect
   "aaa-qwe|")
  (cursor-test/equal*
   :init
   "aaa-rty|"
   :exercise
   (lambda ()
     (call-interactively #'grugru-test-2)
     (call-interactively #'grugru-test-2)
     (call-interactively #'grugru-test-2))
   :expect
   "aaa-rty|"))

(provide 'grugru-test)
;;; grugru-test.el ends here
