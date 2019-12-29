;;; grugru.el --- Rotate text at point.             -*- lexical-binding: t; -*-

;; Copyright (C) 2019  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: convenience, abbrev, tools

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

(require 'cl-lib)
(require 'thingatpt)

(defgroup grugru ()
  "Group for grugru."
  :group 'convenience
  :group 'abbrev
  :group 'tools
  :prefix "grugru")

(defcustom grugru-getter-alist
  '((symbol . (bounds-of-thing-at-point 'symbol))
    (word   . (bounds-of-thing-at-point 'word))
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
                 (symbol . ("float" "double"))))
    (emacs-lisp-mode . ((symbol . ("nil" "t"))
                        (symbol . ("let" "let*"))
                        (symbol . ("defun" "cl-defun"))
                        (symbol . ("defvar" "defcustom"))
                        (symbol . ("add" "remove"))
                        (symbol . ("setq" "setq-default"))))
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
               (or (setq tmp (cdr (assoc sexp cache)))
                   (prog1 (if (functionp sexp) (apply sexp) (eval sexp)))))
         (unless tmp (push (cons sexp cons) cache))

         (setq begin (car cons) end (cdr cons))
         (setq now (- (point) begin))

         do
         (setq
          str
          (cond
           ((functionp strs-or-func)
            (apply strs-or-func (buffer-substring begin end)))
           ((listp strs-or-func)
            (let ((list (member (buffer-substring begin end) strs-or-func)))
              (when list
                (if (eq (length list) 1)
                    (car strs-or-func)
                  (nth 1 list)))))
           (t
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

(defun grugru-major-mode-hook ()
  ""
  (setq grugru-buffer-local-major-mode-grugru-alist
        (cdr (assq major-mode grugru-major-modes-grugru-alist))))

(add-hook 'change-major-mode-hook 'grugru-major-mode-hook)

(provide 'grugru)
;;; grugru.el ends here
