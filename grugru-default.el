;;; grugru-default.el --- Default setting of grugru  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: tools

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

;; Define default setupper.

;;; Code:

(defun grugru-default-setup ()
  "Setup default value.
If some are not confortable, you can remove some of them,
with `grugru-remove-on-major-mode' or `grugru-remove-global'."
  (grugru-define-multiple
   (c++-mode
    (symbol "true" "false")
    (symbol "vector" "array" "deque")
    (symbol "class" "struct")
    (symbol "float" "double")
    (symbol "private" "public" "protected"))
   (emacs-lisp-mode
    (symbol "nil" "t")
    (symbol "let" "let*")
    (symbol "defun" "cl-defun")
    (symbol "defvar" "defcustom")
    (word   "add" "remove")
    (symbol "setq" "setq-default")
    (word   "global" "local"))
   ((tex-mode latex-mode yatex-mode)
    (symbol "figure" "table"))))

(provide 'grugru-default)
;;; grugru-default.el ends here
