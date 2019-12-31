;;; grugru.el --- Rotate text at point             -*- lexical-binding: t; -*-

;; Copyright (C) 2019  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: convenience, abbrev, tools

;; Version: 1.1.0
;; Package-Requires: ((cl-lib "0.6.1") (emacs "24.4"))
;; URL: https://github.com/ROCKTAKEY/grugru

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

(require 'cl-lib)
(require 'thingatpt)
(require 'subword)

(defgroup grugru ()
  "Group for grugru."
  :group 'convenience
  :group 'abbrev
  :group 'tools
  :prefix "grugru")

(defcustom grugru-getter-alist
  '((symbol . (bounds-of-thing-at-point 'symbol))
    (word   . grugru--get-word)
    (char   . (cons (point) (1+ (point)))))
  "An alist of getter of current thing.
Each element should be (SYMBOL . FUNC-OR-SEXP).  SYMBOL is used to access to
SEXP by `grugru'.  FUNC-OR-SEXP should be sexp or function
which return cons cell whose car/cdr is begining/end point of current thing."
  :group 'grugru
  :risky t
  :type '(&rest (symbolp . [functionp sexp])))

(defcustom grugru-major-modes-grugru-alist
  '((c++-mode . ((symbol . ("true" "false"))
                 (symbol . ("vector" "array" "deque"))
                 (symbol . ("class" "struct"))
                 (symbol . ("float" "double"))
                 (symbol . ("private" "public" "protected"))))
    (emacs-lisp-mode . ((symbol . ("nil" "t"))
                        (symbol . ("let" "let*"))
                        (symbol . ("defun" "cl-defun"))
                        (symbol . ("defvar" "defcustom"))
                        (word   . ("add" "remove"))
                        (symbol . ("setq" "setq-default"))
                        (word . ("global" "local"))))
    ((tex-mode latex-mode yatex-mode) . ((symbol . ("figure" "table")))))
  "An alist of rotated text on each `major-mode'.
Each element should be (MAJOR-MODE . ALIST).

ALIST is compounded from (GETTER . STRINGS-OR-FUNCTION).
GETTER is symbol in `grugru-getter-alist'.  By default, `symbol', `word',
`char' is available as GETTER.
STRINGS-OR-FUNCTION can be a list of strings, or function which recieves
current thing as an argument and returns next text."
  :group 'grugru
  :risky t
  :type '(&rest ([symbolp (&rest symbolp)] .
                 (&rest (symbolp . [(&rest stringp) functionp])))))

(defvar grugru-buffer-global-grugru-alist '()
  "This variable keeps global list of (GETTER . STRINGS-OR-FUNCTION).
GETTER is symbol in `grugru-getter-alist'.  By default, `symbol', `word',
`char' is available as GETTER.
STRINGS-OR-FUNCTION can be a list of strings, or function which recieves
current thing as an argument and returns next text.

You can add element to this with `grugru-define-global'.")

(defvar-local grugru-buffer-local-grugru-alist '()
    "This variable keeps buffer-local list of (GETTER . STRINGS-OR-FUNCTION).
GETTER is symbol in `grugru-getter-alist'. By default, `symbol', `word',
`char' is available as GETTER.
STRINGS-OR-FUNCTION can be a list of strings, or function which recieves
current thing as an argument and returns next text.

You can add element to this with `grugru-define-local'.")

(defvar-local grugru--buffer-local-major-mode-grugru-alist '()
    "This variable keeps major-mode-specific list of (GETTER . STRINGS-OR-FUNCTION).
GETTER is symbol in `grugru-getter-alist'. By default, `symbol', `word',
`char' is available as GETTER.
 STRINGS-OR-FUNCTION can be a list of strings, or function which recieves
current thing as an argument and returns next text.

You can add element to this with `grugru-define-on-major-mode',
 or `grugru-define-on-major-mode'.")

(defvar-local grugru--loaded nil
  "Wheather the buffer load grugru list or not.")

(defvar grugru--point-cache nil
  "Cache for keep position on sequentally executed `grugru'.")


;; inner
(defun grugru--get-word ()
  "Get beginning/end of current point."
  (if (or (eq (point) (point-max))
          (string-match "[\\[\\]-_:;&+^~|#$!?%'()<>=*{}.,/\\\\ \n\t]"
                        (buffer-substring (point) (1+ (point)))))
      (save-excursion (cons (subword-left) (subword-right)))
    (save-excursion
      (let ((x (subword-right))
            (y (subword-left)))
        (cons y x)))))

(defun grugru--assq (key alist)
  "Like `assq', but key of ALIST is list, compare KEY to element of that, too.
In addition, This function return list of all cdr matched to the KEY."
  (cl-loop
   for (x . y) in alist
   if (or (eq key x) (and (listp x) (memq key x)))
   collect y))

(defun grugru--major-mode-load ()
  "Load grugru in current buffer."
  (setq grugru--buffer-local-major-mode-grugru-alist
        (apply #'append
               (grugru--assq major-mode grugru-major-modes-grugru-alist)))
  (setq grugru--loaded t))

(add-hook 'change-major-mode-after-body-hook 'grugru--major-mode-load)


;; For user interaction
(defun grugru ()
  "Rotate thing on point, if it is in `grugru-*-grugru-alist'.

You can directly add element to `grugru-buffer-global-grugru-alist',
`grugru-buffer-local-grugru-alist', and `grugru-major-modes-grugru-alist'.
However, directly asignment is risky, so Using `grugru-define-on-major-mode',
`grugru-define-on-local-major-mode', `grugru-define-local', or
`grugru-define-global' is recommended."
  (interactive)
  (unless grugru--loaded (grugru--major-mode-load))
  (let (begin end sexp str now cons cache tmp)
    (when
        (cl-loop
         for (getter . strs-or-func)
         in (append grugru-buffer-local-grugru-alist
                    grugru--buffer-local-major-mode-grugru-alist
                    grugru-buffer-global-grugru-alist)

         do (setq sexp (cdr (assq getter grugru-getter-alist)))

         unless sexp do (error "Getter %s is not set in grugru-getter-alist" getter)

         do
         (setq cons
               (or (setq tmp (cdr (assoc getter cache)))
                   (prog1 (if (functionp sexp) (funcall sexp) (eval sexp)))))
         (unless tmp (push (cons getter cons) cache))

         (setq begin (car cons) end (cdr cons))
         (setq now (- (point) begin))

         do
         (setq
          str
          (pcase strs-or-func
            ((pred functionp)
             (funcall strs-or-func (buffer-substring begin end)))
            ((pred listp)
             (let ((list (member (buffer-substring begin end) strs-or-func)))
               (when list
                 (if (eq (length list) 1)
                     (car strs-or-func)
                   (nth 1 list)))))
            (_
             (error "Wrong grugru is set in grugru-buffer-local-grugru-alist or \
grugru--buffer-local-major-mode-grugru-alist"))))
         if str return str
         finally return nil)
      (delete-region begin end)
      (insert str)
      (goto-char
       (if (eq this-command last-command)
           (+ begin (min grugru--point-cache (length str)))
         (setq grugru--point-cache now)
         (+ begin (min now (length str))))))))


;; For lisp user
(defun grugru-define-on-major-mode (major getter strings-or-function)
  "Add new grugru STRINGS-OR-FUNCTION in MAJOR major mode, with GETTER.

GETTER is symbol in `grugru-getter-alist'.  By default, `symbol', `word',
`char' is available as GETTER.
STRINGS-OR-FUNCTION can be a list of strings, or function which recieves
current thing as an argument and returns next text."
  (let ((x (assq major grugru-major-modes-grugru-alist)))
    (if x
        (setf (cdr (last (cdr x))) (list (cons getter strings-or-function)))
      (push (cons major (list (cons getter strings-or-function)))
            grugru-major-modes-grugru-alist))))

(defmacro grugru-define-on-local-major-mode (getter strings-or-function)
  "Same as (grugru-define-on-major-mode major-mode GETTER STRINGS-OR-FUNCTION)."
  `(grugru-define-on-major-mode ,major-mode ,getter ,strings-or-function))

(defun grugru-define-local (getter strings-or-function)
  "Add new grugru STRINGS-OR-FUNCTION with GETTER on buffer-local.

GETTER is symbol in `grugru-getter-alist'.  By default, `symbol', `word',
`char' is available as GETTER.
STRINGS-OR-FUNCTION can be a list of strings, or function which recieves
current thing as an argument and returns next text."
  (push (cons getter strings-or-function) grugru-buffer-local-grugru-alist))

(defun grugru-define-global (getter strings-or-function)
  "Add new grugru STRINGS-OR-FUNCTION with GETTER globally.

GETTER is symbol in `grugru-getter-alist'.  By default, `symbol', `word',
`char' is available as GETTER.
STRINGS-OR-FUNCTION can be a list of strings, or function which recieves
current thing as an argument and returns next text."
  (push (cons getter strings-or-function) grugru-buffer-global-grugru-alist))

(defmacro grugru-define-function (name _ &optional docstring &rest body)
  "You can define grugru function NAME with DOCSTRING and BODY.
The function defined with this rotates text at point only if it is matched to
one element of BODY.

DOCSTRING is optional argument, which is passed to defun as DOCSTRING,
and BODY is sequence of (GETTER . STRING-OR-FUNCTION).

GETTER is symbol in `grugru-getter-alist'.  By default, `symbol', `word',
`char' is available as GETTER.
STRINGS-OR-FUNCTION can be a list of strings, or function which recieves
current thing as an argument and returns next text.

\(fn NAME () &optional DOCSTRING &rest BODY)"
  (declare (indent defun) (doc-string 3))
  (let* ((args (cons docstring body))
         (docs (when (stringp (car args))
                 (car args)))
         (args (if docs (cdr args) args)))
   `(defun ,name ()
      ,docs
      (interactive)
      (let ((grugru-buffer-global-grugru-alist ',args)
            (grugru-buffer-local-grugru-alist nil)
            (grugru--buffer-local-major-mode-grugru-alist nil)
            (grugru--loaded t))
       (call-interactively #'grugru)))))

(with-eval-after-load 'find-func
  (defun grugru--function-advice (original symbol type library)
    "Advice for `find-function-search-for-symbol' from grugru."
    (let ((name (symbol-name symbol)))
      (or (funcall original symbol type library)
          (and (null type)
               (with-current-buffer (find-file-noselect library)
                 (when (re-search-forward
                        (format "\\|\\(^\\s-*(grugru-define-function\\s-*%s\\)"
                                (regexp-quote name))
                        nil t)
                   (cons (current-buffer) (match-beginning 0))))))))
  (advice-add 'find-function-search-for-symbol :around 'grugru--function-advice))

(provide 'grugru)
;;; grugru.el ends here
