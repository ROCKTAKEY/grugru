;;; grugru.el --- Rotate text at point.             -*- lexical-binding: t; -*-

;; Copyright (C) 2019  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: convenience, abbrev, tools

;; Version: 1.0.0
;; Package-Requires: ((cl-lib "1.0") (emacs "24"))
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
    (word   . (progn
                (let ((x (save-excursion (cons (subword-right) (subword-left))))
                      (y (save-excursion (cons (subword-left) (subword-right)))))
                  (if (< (- (- (car x) (cdr x)) (- (cdr y) (car y))))
                      (cons (cdr x) (car x)) y))))
    (char   . (cons (point) (1+ (point)))))
  "An alist of getter of current thing.
Each element should be (SYMBOL . SEXP). SYMBOL is used to access to SEXP by
`grugru'. SEXP should be sexp or function which return cons cell whose car/cdr
is begining/end point of current thing."
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
  "An alist of toggled text on each `major-mode'.
Each element should be (MAJOR-MODE . ALIST).

ALIST is compounded from (GETTER . (STRING...)).
GETTER is symbol in `grugru-getter-alist', \
and STRING is string which is toggled in order."
  :group 'grugru
  :risky t
  :type '(&rest ([symbolp (&rest symbolp)] .
                 (&rest (symbolp . [(&rest stringp) functionp])))))

(defvar-local grugru-buffer-local-grugru-alist '() "")

(defvar-local grugru-buffer-local-major-mode-grugru-alist '() "")

(defvar grugru--point-cache nil)


;; inner
(defun grugru--assq (key alist)
  "Same as assq, but if car of element of ALIST is list, compare KEY to element of that, too."
  (cl-loop
   for (x . y) in alist
   if (or (eq key x) (and (listp x) (memq key x)))
   collect y))

(defun grugru--major-mode-hook ()
  ""
  (setq grugru-buffer-local-major-mode-grugru-alist
        (apply #'append
               (grugru--assq major-mode grugru-major-modes-grugru-alist))))

(add-hook 'change-major-mode-after-body-hook 'grugru--major-mode-hook)


;; For user interaction
(defun grugru ()
  ""
  (interactive)
  (let (begin end sexp str now cons cache tmp)
    (when
        (cl-loop
         for (getter . strs-or-func)
         in (append grugru-buffer-local-grugru-alist
                    grugru-buffer-local-major-mode-grugru-alist)

         do (setq sexp (cdr (assq getter grugru-getter-alist)))

         unless sexp do (error "Getter %s is not set in grugru-getter-alist." getter)

         do
         (setq cons
               (or (setq tmp (cdr (assoc getter cache)))
                   (prog1 (if (functionp sexp) (apply sexp) (eval sexp)))))
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
grugru-buffer-local-major-mode-grugru-alist."))
            ))
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
(defun grugru-define-on-major-mode (major getter list)
  ""
  (let ((x (assq major grugru-major-modes-grugru-alist)))
    (if x
        (setf (cdr (last (cdr x))) (list (cons getter list)))
      (push (cons major (list (cons getter list)))
            grugru-major-modes-grugru-alist))))

(defmacro grugru-define-on-local-major-mode (getter list)
  ""
  `(grugru-define-on-major-mode ,major-mode ,getter ,list))

(defun grugru-define-local (getter list)
  ""
  (push (cons getter list) grugru-buffer-local-grugru-alist))

(provide 'grugru)
;;; grugru.el ends here
