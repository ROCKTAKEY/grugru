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
(require 'grugru-default)

(require 'ert)
(require 'cursor-test)

(unless (functionp 'macroexpand-1)
 (defun macroexpand-1 (form &optional environment)
  "Perform (at most) one step of macroexpansion."
  (cond
   ((consp form)
    (let* ((head (car form))
           (env-expander (assq head environment)))
      (if env-expander
          (if (cdr env-expander)
              (apply (cdr env-expander) (cdr form))
            form)
        (if (not (and (symbolp head) (fboundp head)))
            form
          (let ((def (autoload-do-load (symbol-function head) head 'macro)))
            (cond
             ;; Follow alias, but only for macros, otherwise we may end up
             ;; skipping an important compiler-macro (e.g. cl--block-wrapper).
             ((and (symbolp def) (macrop def)) (cons def (cdr form)))
             ((not (consp def)) form)
             (t
              (if (eq 'macro (car def))
                  (apply (cdr def) (cdr form))
                form))))))))
   (t form))))



;; Global

(ert-deftest grugru-define-global-2-symbol-end-same-length ()
  (let (grugru--global-grugru-alist)
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
  (let (grugru--global-grugru-alist)
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
  (let (grugru--global-grugru-alist)
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
  (let (grugru--global-grugru-alist)
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
  (let (grugru--global-grugru-alist)
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
  (let (grugru--global-grugru-alist)
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
  (let (grugru--global-grugru-alist)
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
  (let (grugru--global-grugru-alist)
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
  (let (grugru--global-grugru-alist)
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
  (let (grugru--global-grugru-alist)
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
  (let (grugru--global-grugru-alist)
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
  (let (grugru--global-grugru-alist)
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
  (let (grugru--global-grugru-alist)
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
  (let (grugru--global-grugru-alist)
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
  (let (grugru--global-grugru-alist)
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
  (let (grugru--global-grugru-alist)
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
  (let (grugru--major-modes-grugru-alist)
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
  (let (grugru--major-modes-grugru-alist)
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
  (let (grugru--major-modes-grugru-alist)
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
  (let (grugru--major-modes-grugru-alist)
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
  (let (grugru--major-modes-grugru-alist)
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
  (let (grugru--major-modes-grugru-alist)
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
  (let (grugru--major-modes-grugru-alist)
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
  (let (grugru--major-modes-grugru-alist)
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
  (let (grugru--major-modes-grugru-alist)
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
  (let (grugru--major-modes-grugru-alist)
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
  (let (grugru--major-modes-grugru-alist)
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
  (let (grugru--major-modes-grugru-alist)
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
  (let (grugru--major-modes-grugru-alist)
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
  (let (grugru--major-modes-grugru-alist)
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
  (let (grugru--major-modes-grugru-alist)
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
  (let (grugru--major-modes-grugru-alist)
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


;; Remove
(ert-deftest grugru-remove-on-major-mode ()
  (let (grugru--major-modes-grugru-alist)
    (grugru-define-on-major-mode 'fundamental-mode 'word
                                 '("foo" "bar" "baz"))
    (grugru-define-on-major-mode 'fundamental-mode 'word
                                 '("aaa" "bbb" "ccc"))
    (grugru-remove-on-major-mode 'fundamental-mode 'word
                                 '("foo" "bar" "baz"))
    (cursor-test/equal*
     :init "hoge |foo-aaa"
     :exercise
     #'(lambda ()
         (call-interactively #'grugru))
     :expect "hoge |foo-aaa")
    (cursor-test/equal*
     :init "hoge foo-|aaa"
     :exercise
     #'(lambda ()
         (call-interactively #'grugru))
     :expect "hoge foo-|bbb"))
  (let (grugru--major-modes-grugru-alist)
    (grugru-define-on-major-mode '(lisp-interaction-mode fundamental-mode) 'word
                                 '("foo" "bar" "baz"))
    (grugru-define-on-major-mode '(lisp-interaction-mode fundamental-mode) 'word
                                 '("aaa" "bbb" "ccc"))
    (grugru-remove-on-major-mode '(lisp-interaction-mode fundamental-mode) 'word
                                 '("foo" "bar" "baz"))
    (cursor-test/equal*
     :init "hoge |foo-aaa"
     :exercise
     #'(lambda ()
         (call-interactively #'grugru))
     :expect "hoge |foo-aaa")
    (cursor-test/equal*
     :init "hoge foo-|aaa"
     :exercise
     #'(lambda ()
         (call-interactively #'grugru))
     :expect "hoge foo-|bbb")))

(ert-deftest grugru-remove-on-local-major-mode ()
  (let (grugru--major-modes-grugru-alist)
    (cursor-test/equal*
     :init "hoge |foo-aaa"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'word
                                            '("foo" "bar" "baz"))
         (grugru-define-on-local-major-mode 'word
                                            '("aaa" "bbb" "ccc"))
         (grugru-remove-on-local-major-mode 'word
                                            '("foo" "bar" "baz"))
         (call-interactively #'grugru))
     :expect "hoge |foo-aaa")
    (cursor-test/equal*
     :init "hoge foo-|aaa"
     :exercise
     #'(lambda ()
         (grugru-define-on-local-major-mode 'word
                                            '("foo" "bar" "baz"))
         (grugru-define-on-local-major-mode 'word
                                            '("aaa" "bbb" "ccc"))
         (grugru-remove-on-local-major-mode 'word
                                            '("foo" "bar" "baz"))
         (call-interactively #'grugru))
     :expect "hoge foo-|bbb")))

(ert-deftest grugru-remove-local ()
  (let (grugru--major-modes-grugru-alist)
    (cursor-test/equal*
     :init "hoge |foo-aaa"
     :exercise
     #'(lambda ()
         (grugru-define-local 'word
                                 '("foo" "bar" "baz"))
         (grugru-define-local 'word
                                 '("aaa" "bbb" "ccc"))
         (grugru-remove-local 'word
                                 '("foo" "bar" "baz"))
         (call-interactively #'grugru))
     :expect "hoge |foo-aaa")
    (cursor-test/equal*
     :init "hoge foo-|aaa"
     :exercise
     #'(lambda ()
         (grugru-define-local 'word
                                 '("foo" "bar" "baz"))
         (grugru-define-local 'word
                                 '("aaa" "bbb" "ccc"))
         (grugru-remove-local 'word
                                 '("foo" "bar" "baz"))
         (call-interactively #'grugru))
     :expect "hoge foo-|bbb")))

(ert-deftest grugru-remove-global ()
  (let (grugru--major-modes-grugru-alist)
    (grugru-define-global 'word
                          '("foo" "bar" "baz"))
    (grugru-define-global 'word
                          '("aaa" "bbb" "ccc"))
    (grugru-remove-global 'word
                          '("foo" "bar" "baz"))
    (cursor-test/equal*
     :init "hoge |foo-aaa"
     :exercise
     #'(lambda ()
         (call-interactively #'grugru))
     :expect "hoge |foo-aaa")
    (cursor-test/equal*
     :init "hoge foo-|aaa"
     :exercise
     #'(lambda ()
         (call-interactively #'grugru))
     :expect "hoge foo-|bbb")))


(ert-deftest grugru-redefine-on-major-mode ()
  (let (grugru--major-modes-grugru-alist)
    (grugru-define-on-major-mode 'fundamental-mode 'word
                                 '("foo" "bar" "baz"))
    (grugru-define-on-major-mode 'fundamental-mode 'word
                                 '("aaa" "bbb" "ccc"))
    (grugru-redefine-on-major-mode 'fundamental-mode 'word
                                   '("foo" "bar" "baz")
                                   '("foo" "baq" "baqq"))
    (should-error
     (grugru-redefine-on-major-mode 'fundamental-mode 'symbol
                                    '("aaa" "bbb" "ccc")
                                    '("foo" "baq" "baqq")))
    (should-error
     (grugru-redefine-on-major-mode 'fundamental-mode 'word
                                    '("aaa" "bbb" "cccc")
                                    '("foo" "baq" "baqq")))
    (cursor-test/equal*
     :init "hoge |foo-aaa"
     :exercise
     #'(lambda ()
         (call-interactively #'grugru))
     :expect "hoge |baq-aaa")
    (cursor-test/equal*
     :init "hoge foo-|aaa"
     :exercise
     #'(lambda ()
         (call-interactively #'grugru))
     :expect "hoge foo-|bbb"))
  (let (grugru--major-modes-grugru-alist)
    (grugru-define-on-major-mode '(lisp-interaction-mode fundamental-mode) 'word
                                 '("foo" "bar" "baz"))
    (grugru-define-on-major-mode '(lisp-interaction-mode fundamental-mode) 'word
                                 '("aaa" "bbb" "ccc"))
    (grugru-redefine-on-major-mode '(lisp-interaction-mode fundamental-mode) 'word
                                   '("foo" "bar" "baz")
                                 '("foo" "baq" "baqq"))
    (should-error
     (grugru-redefine-on-major-mode '(lisp-interaction-mode fundamental-mode) 'symbol
                                    '("aaa" "bbb" "ccc")
                                    '("foo" "baq" "baqq")))
    (should-error
     (grugru-redefine-on-major-mode '(lisp-interaction-mode fundamental-mode) 'word
                                    '("aaa" "bbb" "cccc")
                                    '("foo" "baq" "baqq")))
    (cursor-test/equal*
     :init "hoge |foo-aaa"
     :exercise
     #'(lambda ()
         (call-interactively #'grugru))
     :expect "hoge |baq-aaa")
    (cursor-test/equal*
     :init "hoge foo-|aaa"
     :exercise
     #'(lambda ()
         (call-interactively #'grugru))
     :expect "hoge foo-|bbb")))

(ert-deftest grugru-redefine-local ()
  (let (grugru--major-modes-grugru-alist)
    (cursor-test/equal*
     :init "hoge |foo-aaa"
     :exercise
     #'(lambda ()
         (grugru-define-local 'word
                                 '("foo" "bar" "baz"))
         (grugru-define-local 'word
                                 '("aaa" "bbb" "ccc"))
         (grugru-redefine-local 'word
                                '("foo" "bar" "baz")
                                 '("foo" "baq" "baqq"))
         (call-interactively #'grugru))
     :expect "hoge |baq-aaa")
    (cursor-test/equal*
     :init "hoge foo-|aaa"
     :exercise
     #'(lambda ()
         (grugru-define-local 'word
                                 '("foo" "bar" "baz"))
         (grugru-define-local 'word
                                 '("aaa" "bbb" "ccc"))
         (grugru-redefine-local 'word
                                '("foo" "bar" "baz")
                                 '("foo" "baq" "baqq"))
         (call-interactively #'grugru))
     :expect "hoge foo-|bbb")))

(ert-deftest grugru-redefine-global ()
  (let (grugru--major-modes-grugru-alist)
    (grugru-define-global 'word
                          '("foo" "bar" "baz"))
    (grugru-define-global 'word
                          '("aaa" "bbb" "ccc"))
    (grugru-redefine-global 'word
                            '("foo" "bar" "baz")
                          '("foo" "baq" "baqq"))
    (cursor-test/equal*
     :init "hoge |foo-aaa"
     :exercise
     #'(lambda ()
         (call-interactively #'grugru))
     :expect "hoge |baq-aaa")
    (cursor-test/equal*
     :init "hoge foo-|aaa"
     :exercise
     #'(lambda ()
         (call-interactively #'grugru))
     :expect "hoge foo-|bbb")))


(ert-deftest grugru-define-multiple-global ()
  (should
   (equal
    (macroexpand-1
     '(grugru-define-multiple
       (word . ("aaa" "bbb" "ccc"))
       (symbol . ("xxx" "yyy" "zzz"))
       (word . ("abc" "def" "ghi"))))
    '(progn
       (grugru-define-global 'word '("aaa" "bbb" "ccc"))
       (grugru-define-global 'symbol '("xxx" "yyy" "zzz"))
       (grugru-define-global 'word '("abc" "def" "ghi")))))
  (should
   (equal
    (macroexpand-1
     '(grugru-define-multiple
       (word "aaa" "bbb" "ccc")
       (symbol "xxx" "yyy" "zzz")
       (word "abc" "def" "ghi")))
    '(progn
       (grugru-define-global 'word '("aaa" "bbb" "ccc"))
       (grugru-define-global 'symbol '("xxx" "yyy" "zzz"))
       (grugru-define-global 'word '("abc" "def" "ghi"))))))

(ert-deftest grugru-define-multiple-major-mode ()
  (should
   (equal
    (macroexpand-1
     '(grugru-define-multiple
       (fundamental-mode
        . ((word . ("aaa" "bbb" "ccc"))
           (symbol . ("xxx" "yyy" "zzz"))
           (word . ("abc" "def" "ghi"))))))
    '(progn
       (progn
         (grugru-define-on-major-mode 'fundamental-mode 'word '("aaa" "bbb" "ccc"))
         (grugru-define-on-major-mode 'fundamental-mode 'symbol '("xxx" "yyy" "zzz"))
         (grugru-define-on-major-mode 'fundamental-mode 'word '("abc" "def" "ghi"))))))
  (should
   (equal
    (macroexpand-1
     '(grugru-define-multiple
       (fundamental-mode
        . ((word "aaa" "bbb" "ccc")
           (symbol "xxx" "yyy" "zzz")
           (word "abc" "def" "ghi")))))
    '(progn
       (progn
         (grugru-define-on-major-mode 'fundamental-mode 'word '("aaa" "bbb" "ccc"))
         (grugru-define-on-major-mode 'fundamental-mode 'symbol '("xxx" "yyy" "zzz"))
         (grugru-define-on-major-mode 'fundamental-mode 'word '("abc" "def" "ghi")))))))

(ert-deftest grugru-define-multiple-major-mode-multi ()
  (should
   (equal
    (macroexpand-1
     '(grugru-define-multiple
       ((fundamental-mode lisp-interaction-mode)
        . ((word . ("aaa" "bbb" "ccc"))
           (symbol . ("xxx" "yyy" "zzz"))
           (word . ("abc" "def" "ghi"))))))
    '(progn
       (progn
         (grugru-define-on-major-mode '(fundamental-mode lisp-interaction-mode)
                                      'word '("aaa" "bbb" "ccc"))
         (grugru-define-on-major-mode '(fundamental-mode lisp-interaction-mode)
                                      'symbol '("xxx" "yyy" "zzz"))
         (grugru-define-on-major-mode '(fundamental-mode lisp-interaction-mode)
                                      'word '("abc" "def" "ghi"))))))
  (should
   (equal
    (macroexpand-1
     '(grugru-define-multiple
       ((fundamental-mode lisp-interaction-mode)
        (word "aaa" "bbb" "ccc")
        (symbol "xxx" "yyy" "zzz")
        (word "abc" "def" "ghi"))))
    '(progn
       (progn
         (grugru-define-on-major-mode '(fundamental-mode lisp-interaction-mode)
                                      'word '("aaa" "bbb" "ccc"))
         (grugru-define-on-major-mode '(fundamental-mode lisp-interaction-mode)
                                      'symbol '("xxx" "yyy" "zzz"))
         (grugru-define-on-major-mode '(fundamental-mode lisp-interaction-mode)
                                      'word '("abc" "def" "ghi")))))))

(ert-deftest grugru-define-multiple-nest ()
  (should
   (equal
    (macroexpand-1
     '(grugru-define-multiple
       ((word . ("aaa" "bbb" "ccc"))
        (symbol . ("xxx" "yyy" "zzz")))
       (word . ("abc" "def" "ghi"))))
    '(progn
       (grugru-define-multiple
        (word . ("aaa" "bbb" "ccc"))
        (symbol . ("xxx" "yyy" "zzz")))
       (grugru-define-global 'word '("abc" "def" "ghi")))))
  (should
   (equal
    (macroexpand-1
     '(grugru-define-multiple
       ((word "aaa" "bbb" "ccc")
        (symbol "xxx" "yyy" "zzz"))
       (word "abc" "def" "ghi")))
    '(progn
       (grugru-define-multiple
        (word . ("aaa" "bbb" "ccc"))
        (symbol . ("xxx" "yyy" "zzz")))
       (grugru-define-global 'word '("abc" "def" "ghi"))))))

(ert-deftest grugru-define-multiple-complex ()
  (should
   (equal
    (macroexpand-1
     '(grugru-define-multiple
       (fundamental-mode
        . ((word . ("aaa" "bbb" "ccc"))
           (symbol . ("xxx" "yyy" "zzz"))
           (word . ("abc" "def" "ghi"))))
       (word . ("aaaa" "bbbb" "cccc"))
       (symbol . ("xxxx" "yyyyy" "zzzzz"))
       (word . ("abcd" "defd" "ghid"))))
    '(progn
       (progn
         (grugru-define-on-major-mode 'fundamental-mode 'word '("aaa" "bbb" "ccc"))
         (grugru-define-on-major-mode 'fundamental-mode 'symbol '("xxx" "yyy" "zzz"))
         (grugru-define-on-major-mode 'fundamental-mode 'word '("abc" "def" "ghi")))
       (grugru-define-global 'word '("aaaa" "bbbb" "cccc"))
       (grugru-define-global 'symbol '("xxxx" "yyyyy" "zzzzz"))
       (grugru-define-global 'word '("abcd" "defd" "ghid")))))
  (should
   (equal
    (macroexpand-1
     '(grugru-define-multiple
       (fundamental-mode
        (word "aaa" "bbb" "ccc")
        (symbol "xxx" "yyy" "zzz")
        (word  "abc" "def" "ghi"))
       (word  "aaaa" "bbbb" "cccc")
       (symbol "xxxx" "yyyyy" "zzzzz")
       (word "abcd" "defd" "ghid")))
    '(progn
       (progn
         (grugru-define-on-major-mode 'fundamental-mode 'word '("aaa" "bbb" "ccc"))
         (grugru-define-on-major-mode 'fundamental-mode 'symbol '("xxx" "yyy" "zzz"))
         (grugru-define-on-major-mode 'fundamental-mode 'word '("abc" "def" "ghi")))
       (grugru-define-global 'word '("aaaa" "bbbb" "cccc"))
       (grugru-define-global 'symbol '("xxxx" "yyyyy" "zzzzz"))
       (grugru-define-global 'word '("abcd" "defd" "ghid"))))))


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


(ert-deftest grugru-default-setup ()
  (let (grugru--global-grugru-alist
        grugru-major-modes-grugru-alist)
   (grugru-default-setup)))

(provide 'grugru-test)
;;; grugru-test.el ends here
