;;; grugru-test.el --- test for grugru

;; Copyright (C) 2019-2021  ROCKTAKEY

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
            (:report-format 'codecov)
            (:report-file "coverage-final.json")
            (:send-report nil))

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

(ert-deftest grugru--get-non-alphabet ()
  (with-temp-buffer
    (insert "abc || efg")
    (goto-char 5)
    (should (equal (grugru--get-non-alphabet)
                   (cons 5 7)))
    (should
     (string= (let ((bounds (grugru--get-non-alphabet)))
                (buffer-substring-no-properties (car bounds) (cdr bounds)))
              "||")))
  (with-temp-buffer
    (insert "abc || efg")
    (goto-char 6)
    (should (equal (grugru--get-non-alphabet)
                   (cons 5 7)))
    (string= (let ((bounds (grugru--get-non-alphabet)))
               (buffer-substring-no-properties (car bounds) (cdr bounds)))
             "||"))
  (with-temp-buffer
    (insert "abc||efg")
    (goto-char 4)
    (should (equal (grugru--get-non-alphabet)
                   (cons 4 6)))
    (string= (let ((bounds (grugru--get-non-alphabet)))
               (buffer-substring-no-properties (car bounds) (cdr bounds)))
             "||"))
  (with-temp-buffer
    (insert "abc||efg")
    (goto-char 5)
    (should (equal (grugru--get-non-alphabet)
                   (cons 4 6)))
    (string= (let ((bounds (grugru--get-non-alphabet)))
               (buffer-substring-no-properties (car bounds) (cdr bounds)))
             "||")))

(ert-deftest grugru--get-tex-command ()
  (with-temp-buffer
    (insert "Take $\\alpha$.")
    (goto-char 7)
    (should (equal (grugru--get-tex-command)
                   (cons 7 13)))
    (should
     (string= (let ((bounds (grugru--get-tex-command)))
                (buffer-substring-no-properties (car bounds) (cdr bounds)))
              "\\alpha")))
  (with-temp-buffer
    (insert "Take $\\alpha$.")
    (goto-char 8)
    (should (equal (grugru--get-tex-command)
                   (cons 7 13)))
    (should
     (string= (let ((bounds (grugru--get-tex-command)))
                (buffer-substring-no-properties (car bounds) (cdr bounds)))
              "\\alpha")))
  (with-temp-buffer
    (insert "Take $\\alpha$.")
    (goto-char 13)
    (should (equal (grugru--get-tex-command)
                   (cons 7 13)))
    (should
     (string= (let ((bounds (grugru--get-tex-command)))
                (buffer-substring-no-properties (car bounds) (cdr bounds)))
              "\\alpha")))
  (with-temp-buffer
    (insert "Take $\\alpha$.")
    (goto-char 14)
    (should (equal (grugru--get-tex-command) nil)))

  (with-temp-buffer
    (insert "Hello \\textit{world}.")
    (goto-char 7)
    (should (equal (grugru--get-tex-command)
                   (cons 7 14)))
    (string= (let ((bounds (grugru--get-tex-command)))
               (buffer-substring-no-properties (car bounds) (cdr bounds)))
             "\\textit"))
  (with-temp-buffer
    (insert "Hello \\textit{world}.")
    (goto-char 8)
    (should (equal (grugru--get-tex-command)
                   (cons 7 14)))
    (string= (let ((bounds (grugru--get-tex-command)))
               (buffer-substring-no-properties (car bounds) (cdr bounds)))
             "\\textit"))
  (with-temp-buffer
    (insert "Hello \\textit{world}.")
    (goto-char 14)
    (should (equal (grugru--get-tex-command)
                   (cons 7 14)))
    (string= (let ((bounds (grugru--get-tex-command)))
               (buffer-substring-no-properties (car bounds) (cdr bounds)))
             "\\textit"))
  (with-temp-buffer
    (insert "Hello \\textit{world}.")
    (goto-char 15)
    (should (equal (grugru--get-tex-command) nil))))

(ert-deftest grugru--get-with-integer ()
  (with-temp-buffer
    (insert "123456789")
    (goto-char 5)
    (should (equal (grugru--get-with-integer 1)
                   (cons 5 6))))
  (with-temp-buffer
    (insert "123456789")
    (goto-char 5)
    (should (equal (grugru--get-with-integer -1)
                   (cons 4 5))))
  (with-temp-buffer
    (insert "123456789")
    (goto-char 5)
    (should (equal (grugru--get-with-integer 2)
                   (cons 5 7))))
  (with-temp-buffer
    (insert "123456789")
    (goto-char 5)
    (should (equal (grugru--get-with-integer -2)
                   (cons 3 5))))
  (with-temp-buffer
    (insert "123456789")
    (goto-char 1)
    (should (equal (grugru--get-with-integer -1)
                   nil)))
  (with-temp-buffer
    (insert "123456789")
    (goto-char 10)
    (should (equal (grugru--get-with-integer 1)
                   nil))))

(ert-deftest grugru--get-valid-bound ()
  (should (grugru--get-valid-bound 1 '((1 . 3) (6 . 8))))
  (should (grugru--get-valid-bound 2 '((1 . 3) (6 . 8))))
  (should (grugru--get-valid-bound 3 '((1 . 3) (6 . 8))))
  (should-not (grugru--get-valid-bound 4 '((1 . 3) (6 . 8))))
  (should-not (grugru--get-valid-bound 5 '((1 . 3) (6 . 8))))
  (should (grugru--get-valid-bound 6 '((1 . 3) (6 . 8))))
  (should (grugru--get-valid-bound 7 '((1 . 3) (6 . 8))))
  (should (grugru--get-valid-bound 8 '((1 . 3) (6 . 8))))
  (should-not (grugru--get-valid-bound 9 '((1 . 3) (6 . 8))))

  (should (grugru--get-valid-bound 9 '((1 . 3) (9 . 9))))
  (should (grugru--get-valid-bound 9 '((9 . 9))))

  (should (grugru--get-valid-bound 1 '((1 . 3))))
  (should (grugru--get-valid-bound 2 '((1 . 3))))
  (should (grugru--get-valid-bound 3 '((1 . 3))))
  (should-not (grugru--get-valid-bound 4 '((1 . 3))))
  (should-not (grugru--get-valid-bound 5 '((1 . 3)))))

(ert-deftest grugru--get-next-string-strings ()
  (should
   (string=
    "bar"
    (grugru--get-next-string "foo" '("foo" "bar" "baz") 1)))
  (should
   (string=
    "foo"
    (grugru--get-next-string "baz" '("foo" "bar" "baz") 5))))

(ert-deftest grugru--get-next-string-function ()
  (should
   (string=
    "bar"
    (grugru--get-next-string
     "foo"
     (lambda (arg)
       (pcase arg
         ("foo" "bar")
         ("bar" "baz")
         ("baz" "foo")))
     1)))
  (should-not
   (grugru--get-next-string
    "fo"
    (lambda (arg)
      (pcase arg
        ("foo" "bar")
        ("bar" "baz")
        ("baz" "foo")))
    5)))

(ert-deftest grugru--get-next-string-function-valid-bound ()
  (should
   (string=
    "bar"
    (cdr
     (grugru--get-next-string
      "foo"
      (lambda (arg)
        (cons
         (cons 0 1)
         (pcase arg
           ("foo" "bar")
           ("bar" "baz")
           ("baz" "foo"))))
      1))))
  (should-not
   (grugru--get-next-string
    "foo"
    (lambda (arg)
      (let ((cons (cons
                   (cons 0 1)
                   (pcase arg
                     ("foo" "bar")
                     ("bar" "baz")
                     ("baz" "foo")))))
        (when (cdr cons) cons)))
    2)))

(ert-deftest grugru--get-previous-string-strings ()
  (should
   (string=
    "bar"
    (grugru--get-previous-string "foo" (nreverse '("foo" "bar" "baz")) 1)))
  (should
   (string=
    "foo"
    (grugru--get-previous-string "baz" (nreverse '("foo" "bar" "baz")) 5))))

(ert-deftest grugru--get-previous-string-function ()
  (should
   (string=
    "baz"
    (grugru--get-previous-string
     "foo"
     (lambda (arg &optional rev)
       (if rev
           (pcase arg
             ("foo" "baz")
             ("bar" "foo")
             ("baz" "bar"))
         (pcase arg
           ("foo" "bar")
           ("bar" "baz")
           ("baz" "foo"))))
     1)))
  (should-not
   (grugru--get-previous-string
    "fo"
    (lambda (arg &optional rev)
      (if rev
          (pcase arg
            ("foo" "baz")
            ("bar" "foo")
            ("baz" "bar"))
        (pcase arg
          ("foo" "bar")
          ("bar" "baz")
          ("baz" "foo"))))
    5)))

(ert-deftest grugru--get-previous-string-function-valid-bound ()
  (should
   (string=
    "baz"
    (cdr
     (grugru--get-previous-string
      "foo"
      (lambda (arg &optional rev)
        (cons
         (cons 0 1)
         (if rev
             (pcase arg
               ("foo" "baz")
               ("bar" "foo")
               ("baz" "bar"))
           (pcase arg
             ("foo" "bar")
             ("bar" "baz")
             ("baz" "foo")))))
      1))))
  (should-not
   (grugru--get-previous-string
    "foo"
    (lambda (arg &optional rev)
      (let ((cons (cons
                   (cons 0 1)
                   (if rev
                       (pcase arg
                         ("foo" "baz")
                         ("bar" "foo")
                         ("baz" "bar"))
                     (pcase arg
                       ("foo" "bar")
                       ("bar" "baz")
                       ("baz" "foo"))))))
        (when (cdr cons) cons)))
    2)))

(ert-deftest grugru--get-getter-function ()
  (let ((grugru-getter-alist
         '((a . ignore)
           (b . (lambda () t)))))
    (should (eq (grugru--get-getter-function 'a) 'ignore))
    (should (equal (grugru--get-getter-function 'b) (lambda () t)))
    (should (equal (grugru--get-getter-function
                    '(message "Hello"))
                   '(lambda () (message "Hello"))))))

(ert-deftest grugru--get-plist-normal ()
  (let ((grugru--global
         '(((lambda () (grugru--get-word)) . ("foo" "bar" "baz"))
           ((grugru--get-word) . ("aaa" "bbb" "ccc"))))
        (grugru--local
         '(((lambda () (grugru--get-word)) . ("fool" "bar" "bazl"))
           ((grugru--get-word) . ("aaal" "bbb" "cccl")))))
    (with-temp-buffer
      (insert "bar hoge")
      (goto-char 2)
      (let* ((plists
              (grugru--get-plist
               '((global . grugru--global)
                 (local . grugru--local))))
             (plist1 (nth 0 plists))
             (plist2 (nth 1 plists)))
        (should (equal (plist-get plist1 :symbol) 'global))
        (should (equal (plist-get plist1 :bound) '(1 . 4)))
        (should (equal (plist-get plist1 :next) "baz"))
        (should (equal (plist-get plist1 :getter) '(lambda () (grugru--get-word))))
        (should (equal (plist-get plist1 :strs-or-func) '("foo" "bar" "baz")))

        (should (equal (plist-get plist2 :symbol) 'local))
        (should (equal (plist-get plist2 :bound) '(1 . 4)))
        (should (equal (plist-get plist2 :next) "bazl"))
        (should (equal (plist-get plist2 :getter) '(lambda () (grugru--get-word))))
        (should (equal (plist-get plist2 :strs-or-func) '("fool" "bar" "bazl")))))
    (with-temp-buffer
      (insert "bbb hoge")
      (goto-char 2)
      (let* ((plists
              (grugru--get-plist
               '((global . grugru--global)
                 (local . grugru--local))))
             (plist1 (nth 0 plists))
             (plist2 (nth 1 plists)))
        (should (equal (plist-get plist1 :symbol) 'global))
        (should (equal (plist-get plist1 :bound) '(1 . 4)))
        (should (equal (plist-get plist1 :next) "ccc"))
        (should (equal (plist-get plist1 :getter) '(grugru--get-word)))
        (should (equal (plist-get plist1 :strs-or-func) '("aaa" "bbb" "ccc")))

        (should (equal (plist-get plist2 :symbol) 'local))
        (should (equal (plist-get plist2 :bound) '(1 . 4)))
        (should (equal (plist-get plist2 :next) "cccl"))
        (should (equal (plist-get plist2 :getter) '(grugru--get-word)))
        (should (equal (plist-get plist2 :strs-or-func) '("aaal" "bbb" "cccl")))))))

(ert-deftest grugru--get-plist-only-one ()
  (let ((grugru--global
         '(((lambda () (grugru--get-word)) . ("foo" "bar" "baz"))
           ((grugru--get-word) . ("aaa" "bbb" "ccc"))))
        (grugru--local
         '(((lambda () (grugru--get-word)) . ("fool" "bar" "bazl"))
           ((grugru--get-word) . ("aaal" "bbb" "cccl")))))
    (with-temp-buffer
      (insert "bar hoge")
      (goto-char 2)
      (let* ((plist
              (grugru--get-plist
               '((global . grugru--global)
                 (local . grugru--local))
               t)))
        (should (equal (plist-get plist :symbol) 'global))
        (should (equal (plist-get plist :bound) '(1 . 4)))
        (should (equal (plist-get plist :next) "baz"))
        (should (equal (plist-get plist :getter) '(lambda () (grugru--get-word))))
        (should (equal (plist-get plist :strs-or-func) '("foo" "bar" "baz")))))
    (with-temp-buffer
      (insert "bbb hoge")
      (goto-char 2)
      (let* ((plist
              (grugru--get-plist
               '((global . grugru--global)
                 (local . grugru--local))
                t)))
        (should (equal (plist-get plist :symbol) 'global))
        (should (equal (plist-get plist :bound) '(1 . 4)))
        (should (equal (plist-get plist :next) "ccc"))
        (should (equal (plist-get plist :getter) '(grugru--get-word)))
        (should (equal (plist-get plist :strs-or-func) '("aaa" "bbb" "ccc")))))))

(ert-deftest grugru--insert-sexp-append-to-file ()
  (let ((file "test1"))
    (unwind-protect
        (progn
          (when (file-exists-p file)
            (delete-file file))
          (let((buffer-file-coding-system))
            (grugru--insert-sexp-append-to-file '(aaa bbb) file)
            (grugru--insert-sexp-append-to-file '((ccc) ddd) file)
            (should
             (string=
              (with-temp-buffer
                (let ((coding-system-for-write 'utf-8))
                  (insert-file-contents "test1")
                  (encode-coding-string (buffer-string) 'utf-8)))
              "(aaa bbb)\n((ccc) ddd)\n"))))
      (when (file-exists-p file)
        (delete-file file)))))

(ert-deftest grugru--make-expression-global-new ()
  (should
   (equal
    (grugru--make-expression
     'global 'word '("aaa" "bbb" "ccc") '("ddd" "eee" "fff"))
    '(grugru-redefine-global 'word '("aaa" "bbb" "ccc") '("ddd" "eee" "fff")))))

(ert-deftest grugru--make-expression-global-remove ()
  (should
   (equal
    (grugru--make-expression 'global 'word '("aaa" "bbb" "ccc") nil)
    '(grugru-remove-global 'word '("aaa" "bbb" "ccc")))))

(ert-deftest grugru--make-expression-major-mode-new ()
  (should
   (equal
    (grugru--make-expression
     'fundamental-mode 'word '("aaa" "bbb" "ccc") '("ddd" "eee" "fff"))
    '(grugru-redefine-on-major-mode
      'fundamental-mode 'word '("aaa" "bbb" "ccc") '("ddd" "eee" "fff")))))

(ert-deftest grugru--make-expression-major-mode-remove ()
  (should
   (equal
    (grugru--make-expression 'fundamental-mode 'word '("aaa" "bbb" "ccc") nil)
    '(grugru-remove-on-major-mode 'fundamental-mode 'word '("aaa" "bbb" "ccc")))))

(ert-deftest grugru--strings-or-function-p ()
  (should (grugru--strings-or-function-p '("aaa" "bbb")))
  (should (grugru--strings-or-function-p #'grugru--strings-or-function-p))
  (should-not (grugru--strings-or-function-p '(xyz)))
  (should-not (grugru--strings-or-function-p "aaa"))
  (should-not (grugru--strings-or-function-p nil)))

(ert-deftest gruru--replace ()
  (cursor-test/equal*
   :init "12345678|9"
   :exercise
   (lambda ()
     (grugru--replace 2 4 "bcd"))
   :expect "1bcd45678|9"))

(ert-deftest grugru--load-and-cache-position ()
  (cursor-test/equal*
   :init "12345678|9"
   :exercise
   (lambda ()
     (let ((this-command #'grugru)
           (last-command #'grugru)
           (grugru--point-cache 7))
       (grugru--load-and-cache-position 2 2 4)))
   :expect
   "123|456789")
  (cursor-test/equal*
   :init "12345678|9"
   :exercise
   (lambda ()
     (let ((this-command #'grugru)
           (last-command #'grugru)
           (grugru--point-cache 2))
       (grugru--load-and-cache-position 2 5 4)))
   :expect
   "123|456789")
  (cursor-test/equal*
   :init "12345678|9"
   :exercise
   (lambda ()
     (let ((this-command #'grugru)
           (last-command #'self-insert-command)
           (grugru--point-cache 7))
       (grugru--load-and-cache-position 2 2 5)))
   :expect
   "123|456789")
  (cursor-test/equal*
   :init "12345678|9"
   :exercise
   (lambda ()
     (let ((this-command #'grugru)
           (last-command #'self-insert-command)
           (grugru--point-cache 2))
       (grugru--load-and-cache-position 2 5 3)))
   :expect
   "1234|56789"))

(ert-deftest grugru--select-generate-strings ()
  (should
   (equal
    (grugru--select-generate-strings
     "add"
     (lambda (str)
       (pcase str
         ("add" "remove")
         ("remove" "res")
         ("res" "for")))
     10)
    '("add" "remove" "res" "for")))
  (should
   (equal
    (grugru--select-generate-strings
     "add"
     (lambda (str)
       (pcase str
         ("add" "remove")
         ("remove" "res")
         ("res" "for")
         ("for" "add")))
     10)
    '("add" "remove" "res" "for")))
  (should
   (equal
    (grugru--select-generate-strings
     "add"
     (lambda (str)
       (pcase str
         ("add" "remove")
         ("remove" "res")
         ("res" "for")))
     2)
    '("add" "remove")))
  (should
   (equal
    (grugru--select-generate-strings
     "add"
     (lambda (str)
       (pcase str
         ("add" "remove")
         ("remove" "res")
         ("res" "for")
         ("for" "add")))
     2)
    '("add" "remove"))))


(ert-deftest grugru-edit ()
  (let ((grugru-edit-save-file
         (expand-file-name ".grugru" "."))
        grugru--global-grugru-alist)
    (unwind-protect
        (progn
          (grugru-define-global 'word '("aaa" "bbb"))
          (grugru-edit 'global 'word '("aaa" "bbb") '("aaa" "bbb" "ccc"))
          (cursor-test/equal*
           :init "bbb| hoge"
           :exercise
           (lambda ()
             (call-interactively #'grugru))
           :expect "ccc| hoge")
          (should
           (equal
            (read
             (with-temp-buffer
               (let ((coding-system-for-write 'utf-8))
                 (insert-file-contents grugru-edit-save-file)
                 (encode-coding-string (buffer-string) 'utf-8))))
            '(grugru-redefine-global 'word '("aaa" "bbb") '("aaa" "bbb" "ccc")))))
      (when (file-exists-p grugru-edit-save-file)
            (delete-file grugru-edit-save-file)))))


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



(ert-deftest grugru-define-global-2-integer-negative ()
  (let (grugru--global-grugru-alist)
    (cursor-test/equal*
     :init "foo| hoge"
     :exercise
     (lambda ()
       (grugru-define-global -3 '("foo" "bar"))
       (call-interactively #'grugru))
     :expect "bar| hoge")
    (cursor-test/equal*
     :init "bar| hoge"
     :exercise
     (lambda ()
       (grugru-define-global '-3 '("foo" "bar"))
       (call-interactively #'grugru))
     :expect "foo| hoge")))

(ert-deftest grugru-define-global-2-integer-positive ()
  (let (grugru--global-grugru-alist)
    (cursor-test/equal*
     :init "hoge |foo"
     :exercise
     (lambda ()
       (grugru-define-global 3 '("foo" "bar"))
       (call-interactively #'grugru))
     :expect "hoge |bar")
    (cursor-test/equal*
     :init "hoge |bar"
     :exercise
     (lambda ()
       (grugru-define-global 3 '("foo" "bar"))
       (call-interactively #'grugru))
     :expect "hoge |foo")))


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


(ert-deftest grugru-indent-after-rotate ()
  (let (grugru--buffer-local-grugru-alist
        (grugru-indent-after-rotate t))
    (should
     (cursor-test/equal*
      :init "hoge |foo"
      :exercise
      (lambda ()
        (grugru-define-local 'symbol '("foo" "(baar\nfuga)"))
        (call-interactively #'grugru))
      :expect "hoge |(baar\n fuga)")))
  (let (grugru--buffer-local-grugru-alist
        (grugru-indent-after-rotate nil))
    (should
     (cursor-test/equal*
      :init "hoge |foo"
      :exercise
      (lambda ()
        (grugru-define-local 'symbol '("foo" "(baar\nfuga)"))
        (call-interactively #'grugru))
      :expect "hoge |(baar\nfuga)"))))


(ert-deftest grugru-define-global-point-after-rotate-as-is ()
  (let (grugru--global-grugru-alist
        (grugru-point-after-rotate 'as-is))
    (cursor-test/equal*
     :init "hoge |foo"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar"))
       (call-interactively #'grugru))
     :expect "hoge |baar")
    (cursor-test/equal*
     :init "hoge f|oo"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar"))
       (call-interactively #'grugru))
     :expect "hoge b|aar")
    (cursor-test/equal*
     :init "hoge baar|"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar"))
       (call-interactively #'grugru))
     :expect "hoge foo|")))

(ert-deftest grugru-define-global-point-after-rotate-beginning ()
  (let (grugru--global-grugru-alist
        (grugru-point-after-rotate 'beginning))
    (cursor-test/equal*
     :init "hoge |foo"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar"))
       (call-interactively #'grugru))
     :expect "hoge |baar")
    (cursor-test/equal*
     :init "hoge f|oo"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar"))
       (call-interactively #'grugru))
     :expect "hoge |baar")
    (cursor-test/equal*
     :init "hoge baar|"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar"))
       (call-interactively #'grugru))
     :expect "hoge |foo")))

(ert-deftest grugru-define-global-point-after-rotate-end ()
  (let (grugru--global-grugru-alist
        (grugru-point-after-rotate 'end))
    (cursor-test/equal*
     :init "hoge |foo"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar"))
       (call-interactively #'grugru))
     :expect "hoge baar|")
    (cursor-test/equal*
     :init "hoge f|oo"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar"))
       (call-interactively #'grugru))
     :expect "hoge baar|")
    (cursor-test/equal*
     :init "hoge baar|"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar"))
       (call-interactively #'grugru))
     :expect "hoge foo|")))


;; Forward
(ert-deftest grugru-forward-grugru-define-global-2-symbol-end-same-length ()
  (let (grugru--global-grugru-alist)
    (cursor-test/equal*
     :init "foo| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "bar"))
       (call-interactively #'grugru-forward))
     :expect "bar| hoge")
    (cursor-test/equal*
     :init "bar| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "bar"))
       (call-interactively #'grugru-forward))
     :expect "foo| hoge")))

(ert-deftest grugru-forward-grugru-define-global-3-symbol-end-same-length ()
  (let (grugru--global-grugru-alist)
    (cursor-test/equal*
     :init "foo| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "bar" "baz"))
       (call-interactively #'grugru-forward))
     :expect "bar| hoge")
    (cursor-test/equal*
     :init "bar| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "bar" "baz"))
       (call-interactively #'grugru-forward))
     :expect "baz| hoge")

    (cursor-test/equal*
     :init "baz| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "bar" "baz"))
       (call-interactively #'grugru-forward))
     :expect "foo| hoge")))

(ert-deftest grugru-forward-grugru-define-global-2-symbol-end-different-length ()
  (let (grugru--global-grugru-alist)
    (cursor-test/equal*
     :init "foo| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar"))
       (call-interactively #'grugru-forward))
     :expect "baa|r hoge")
    (cursor-test/equal*
     :init "baar| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar"))
       (call-interactively #'grugru-forward))
     :expect "foo| hoge")

    (cursor-test/equal*
     :init "foo| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar"))
       (call-interactively #'grugru-forward)
       (call-interactively #'grugru-forward))
     :expect "foo| hoge")))

(ert-deftest grugru-forward-grugru-define-global-3-symbol-end-different-length ()
  (let (grugru--global-grugru-alist)
    (cursor-test/equal*
     :init "foo| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar" "baaaz"))
       (call-interactively #'grugru-forward))
     :expect "baa|r hoge")
    (cursor-test/equal*
     :init "baar| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar" "baaaz"))
       (call-interactively #'grugru-forward))
     :expect "baaa|z hoge")
    (cursor-test/equal*
     :init "baaaz| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar" "baaaz"))
       (call-interactively #'grugru-forward))
     :expect "foo| hoge")

    (cursor-test/equal*
     :init "foo| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar" "baaaz"))
       (call-interactively #'grugru-forward)
       (call-interactively #'grugru-forward)
       (call-interactively #'grugru-forward))
     :expect "foo| hoge")
    (cursor-test/equal*
     :init "baar| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar" "baaaz"))
       (call-interactively #'grugru-forward)
       (call-interactively #'grugru-forward))
     :expect "foo| hoge")))

(ert-deftest grugru-forward-grugru-define-global-2-symbol-beginning-same-length ()
  (let (grugru--global-grugru-alist)
    (cursor-test/equal*
     :init "hoge |foo"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "bar"))
       (call-interactively #'grugru-forward))
     :expect "hoge |bar")
    (cursor-test/equal*
     :init "hoge |bar"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "bar"))
       (call-interactively #'grugru-forward))
     :expect "hoge |foo")))

(ert-deftest grugru-forward-grugru-define-global-3-symbol-beginning-same-length ()
  (let (grugru--global-grugru-alist)
    (cursor-test/equal*
     :init "hoge |foo"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "bar" "baz"))
       (call-interactively #'grugru-forward))
     :expect "hoge |bar")
    (cursor-test/equal*
     :init "hoge |bar"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "bar" "baz"))
       (call-interactively #'grugru-forward))
     :expect "hoge |baz")

    (cursor-test/equal*
     :init "hoge |baz"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "bar" "baz"))
       (call-interactively #'grugru-forward))
     :expect "hoge |foo")))

(ert-deftest grugru-forward-grugru-define-global-2-symbol-beginning-different-length ()
  (let (grugru--global-grugru-alist)
    (cursor-test/equal*
     :init "hoge |foo"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar"))
       (call-interactively #'grugru-forward))
     :expect "hoge |baar")
    (cursor-test/equal*
     :init "hoge |baar"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar"))
       (call-interactively #'grugru-forward))
     :expect "hoge |foo")

    (cursor-test/equal*
     :init "hoge |foo"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar"))
       (call-interactively #'grugru-forward)
       (call-interactively #'grugru-forward))
     :expect "hoge |foo")))

(ert-deftest grugru-forward-grugru-define-global-3-symbol-beginning-different-length ()
  (let (grugru--global-grugru-alist)
    (cursor-test/equal*
     :init "hoge |foo"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar" "baaaz"))
       (call-interactively #'grugru-forward))
     :expect "hoge |baar")
    (cursor-test/equal*
     :init "hoge |baar"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar" "baaaz"))
       (call-interactively #'grugru-forward))
     :expect "hoge |baaaz")
    (cursor-test/equal*
     :init "hoge |baaaz"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar" "baaaz"))
       (call-interactively #'grugru-forward))
     :expect "hoge |foo")

    (cursor-test/equal*
     :init "hoge |foo"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar" "baaaz"))
       (call-interactively #'grugru-forward)
       (call-interactively #'grugru-forward)
       (call-interactively #'grugru-forward))
     :expect "hoge |foo")
    (cursor-test/equal*
     :init "hoge |baar"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "baar" "baaaz"))
       (call-interactively #'grugru-forward)
       (call-interactively #'grugru-forward))
     :expect "hoge |foo")))


;; Backward
(ert-deftest grugru-backward-grugru-define-global-2-symbol-end-same-length ()
  (let (grugru--global-grugru-alist)
    (cursor-test/equal*
     :init "foo| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "bar"))
       (call-interactively #'grugru-backward))
     :expect "bar| hoge")
    (cursor-test/equal*
     :init "bar| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol '("foo" "bar"))
       (call-interactively #'grugru-backward))
     :expect "foo| hoge")))

(ert-deftest grugru-backward-grugru-define-global-3-symbol-end-same-length ()
  (let (grugru--global-grugru-alist)
    (cursor-test/equal*
     :init "foo| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol (reverse '("foo" "bar" "baz")))
       (call-interactively #'grugru-backward))
     :expect "bar| hoge")
    (cursor-test/equal*
     :init "bar| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol (reverse '("foo" "bar" "baz")))
       (call-interactively #'grugru-backward))
     :expect "baz| hoge")

    (cursor-test/equal*
     :init "baz| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol (reverse '("foo" "bar" "baz")))
       (call-interactively #'grugru-backward))
     :expect "foo| hoge")))

(ert-deftest grugru-backward-grugru-define-global-2-symbol-end-different-length ()
  (let (grugru--global-grugru-alist)
    (cursor-test/equal*
     :init "foo| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol (reverse '("foo" "baar")))
       (call-interactively #'grugru-backward))
     :expect "baa|r hoge")
    (cursor-test/equal*
     :init "baar| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol (reverse '("foo" "baar")))
       (call-interactively #'grugru-backward))
     :expect "foo| hoge")

    (cursor-test/equal*
     :init "foo| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol (reverse '("foo" "baar")))
       (call-interactively #'grugru-backward)
       (call-interactively #'grugru-backward))
     :expect "foo| hoge")))

(ert-deftest grugru-backward-grugru-define-global-3-symbol-end-different-length ()
  (let (grugru--global-grugru-alist)
    (cursor-test/equal*
     :init "foo| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol (reverse '("foo" "baar" "baaaz")))
       (call-interactively #'grugru-backward))
     :expect "baa|r hoge")
    (cursor-test/equal*
     :init "baar| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol (reverse '("foo" "baar" "baaaz")))
       (call-interactively #'grugru-backward))
     :expect "baaa|z hoge")
    (cursor-test/equal*
     :init "baaaz| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol (reverse '("foo" "baar" "baaaz")))
       (call-interactively #'grugru-backward))
     :expect "foo| hoge")

    (cursor-test/equal*
     :init "foo| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol (reverse '("foo" "baar" "baaaz")))
       (call-interactively #'grugru-backward)
       (call-interactively #'grugru-backward)
       (call-interactively #'grugru-backward))
     :expect "foo| hoge")
    (cursor-test/equal*
     :init "baar| hoge"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol (reverse '("foo" "baar" "baaaz")))
       (call-interactively #'grugru-backward)
       (call-interactively #'grugru-backward))
     :expect "foo| hoge")))

(ert-deftest grugru-backward-grugru-define-global-2-symbol-beginning-same-length ()
  (let (grugru--global-grugru-alist)
    (cursor-test/equal*
     :init "hoge |foo"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol (reverse '("foo" "bar")))
       (call-interactively #'grugru-backward))
     :expect "hoge |bar")
    (cursor-test/equal*
     :init "hoge |bar"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol (reverse '("foo" "bar")))
       (call-interactively #'grugru-backward))
     :expect "hoge |foo")))

(ert-deftest grugru-backward-grugru-define-global-3-symbol-beginning-same-length ()
  (let (grugru--global-grugru-alist)
    (cursor-test/equal*
     :init "hoge |foo"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol (reverse '("foo" "bar" "baz")))
       (call-interactively #'grugru-backward))
     :expect "hoge |bar")
    (cursor-test/equal*
     :init "hoge |bar"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol (reverse '("foo" "bar" "baz")))
       (call-interactively #'grugru-backward))
     :expect "hoge |baz")

    (cursor-test/equal*
     :init "hoge |baz"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol (reverse '("foo" "bar" "baz")))
       (call-interactively #'grugru-backward))
     :expect "hoge |foo")))

(ert-deftest grugru-backward-grugru-define-global-2-symbol-beginning-different-length ()
  (let (grugru--global-grugru-alist)
    (cursor-test/equal*
     :init "hoge |foo"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol (reverse '("foo" "baar")))
       (call-interactively #'grugru-backward))
     :expect "hoge |baar")
    (cursor-test/equal*
     :init "hoge |baar"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol (reverse '("foo" "baar")))
       (call-interactively #'grugru-backward))
     :expect "hoge |foo")

    (cursor-test/equal*
     :init "hoge |foo"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol (reverse '("foo" "baar")))
       (call-interactively #'grugru-backward)
       (call-interactively #'grugru-backward))
     :expect "hoge |foo")))

(ert-deftest grugru-backward-grugru-define-global-3-symbol-beginning-different-length ()
  (let (grugru--global-grugru-alist)
    (cursor-test/equal*
     :init "hoge |foo"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol (reverse '("foo" "baar" "baaaz")))
       (call-interactively #'grugru-backward))
     :expect "hoge |baar")
    (cursor-test/equal*
     :init "hoge |baar"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol (reverse '("foo" "baar" "baaaz")))
       (call-interactively #'grugru-backward))
     :expect "hoge |baaaz")
    (cursor-test/equal*
     :init "hoge |baaaz"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol (reverse '("foo" "baar" "baaaz")))
       (call-interactively #'grugru-backward))
     :expect "hoge |foo")

    (cursor-test/equal*
     :init "hoge |foo"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol (reverse '("foo" "baar" "baaaz")))
       (call-interactively #'grugru-backward)
       (call-interactively #'grugru-backward)
       (call-interactively #'grugru-backward))
     :expect "hoge |foo")
    (cursor-test/equal*
     :init "hoge |baar"
     :exercise
     (lambda ()
       (grugru-define-global 'symbol (reverse '("foo" "baar" "baaaz")))
       (call-interactively #'grugru-backward)
       (call-interactively #'grugru-backward))
     :expect "hoge |foo")))


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


(ert-deftest grugru--find-function-integration ()
  (let ((file "find-function-test.el"))
    (unwind-protect
        (progn
          (when (file-exists-p file)
            (delete-file file))
          (with-temp-buffer
            (let (print-level print-length)
              (encode-coding-string
               "(grugru-define-function some-grugru-function ()
'(\"aaa\" \"bbb\" \"ccc\")
'(\"ddd\" \"eee\" \"fff\"))
(provide 'find-function-test)"
               'utf-8 nil (current-buffer))
              (write-region nil nil file)))
          (grugru-find-function-integration-mode +1)
          (should
           (cdr
            (find-function-search-for-symbol
             'some-grugru-function nil (expand-file-name file "."))))
          (grugru-find-function-integration-mode -1))
    (when (file-exists-p file)
      (delete-file file)))))


(ert-deftest grugru--highlight-add ()
  (let (grugru--global-grugru-alist)
    (grugru-define-global 'word '("aaa" "bbb"))
    (with-temp-buffer
      (insert "aaa")
      (grugru--highlight-add)
      (beginning-of-line)
      (should
       (member
        grugru--highlight-overlay
        (overlays-at (point)))))))

(ert-deftest grugru--highlight-remove ()
  (let (grugru--global-grugru-alist)
    (grugru-define-global 'word '("aaa" "bbb"))
    (with-temp-buffer
      (insert "aaa")
      (grugru--highlight-add)
      (grugru--highlight-remove)
      (beginning-of-line)
      (should-not
       (member
        grugru--highlight-overlay
        (overlays-at (point)))))))

(provide 'grugru-test)
;;; grugru-test.el ends here
