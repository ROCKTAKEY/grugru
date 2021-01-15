;;; grugru-utils.el --- Helpful utilities for users to define complex grugru  -*- lexical-binding: t; -*-

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


;;; Commentary:
;;

;;; Code:

(require 'cl-lib)

(defun grugru-utils-lisp-exchange-args (list-string permutation)
  "Permute argument of sexp read from LIST-STRING according to PERMUTATION.

For example, (grugru-utils-lisp-exchange-args \"(nth 1 '(x y z))\" '(2 1))
returns \"(nth '(x y z) 1)\".  Newlines and whitespaces are also kept.

This function is defined for user to define the function for grugru which rotate
not only fuction's name but also arguments' order."
  (ignore-errors
    (let* ((no-parenthesis (substring-no-properties list-string 1 -1))
           (function-name-pos (cdr (read-from-string no-parenthesis)))
           (args-string (substring-no-properties
                         no-parenthesis function-name-pos))
           (args-string-list
            (let ((pos 0)
                  new-pos result)
              (while (setq new-pos (ignore-errors (cdr (read-from-string args-string pos))))
                (push (substring-no-properties args-string pos new-pos) result)
                (setq pos new-pos))
              (nreverse result)))
           (permutation-length (length permutation))
           (result (make-vector permutation-length nil)))
      (cl-mapcar
       (lambda (arg num) (setf (aref result (1- num)) arg))
       args-string-list permutation)
      (apply #'concat
             "(" (substring-no-properties no-parenthesis nil function-name-pos)
             (append result (nthcdr permutation-length args-string-list)
                     '(")"))))))

(provide 'grugru-utils)
;;; grugru-utils.el ends here
