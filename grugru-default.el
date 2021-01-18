;;; grugru-default.el --- Default setting of grugru  -*- lexical-binding: t; -*-

;; Copyright (C) 2020-2021  ROCKTAKEY

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

(require 'grugru)
(require 'grugru-utils)

(defun grugru-default@emacs-lisp+nth!aref (str)
  (cond
   ((string-match "^(\\(\\_<nth\\_>\\)" str)
    (grugru-utils-lisp-exchange-args
     (replace-match "aref" nil nil str 1)
     '(2 1)))
   ((string-match "^(\\(\\_<aref\\_>\\)" str)
    (grugru-utils-lisp-exchange-args
     (replace-match "nth" nil nil str 1)
     '(2 1)))))

;;;###autoload
(defun grugru-default-setup ()
  "Setup default value.
If some are not confortable, you can remove some of them,
with `grugru-remove-on-major-mode' or `grugru-remove-global'."
  (grugru-define-multiple
   ((c-mode c++-mode)
    (non-alphabet "&&" "||")
    (non-alphabet "&" "|")
    (non-alphabet "+" "-")
    (non-alphabet "*" "/" "%")
    (non-alphabet "+=" "-=")
    (non-alphabet "*=" "/=" "%=")
    (non-alphabet "&=" "|=" "^=")
    (non-alphabet ">>=" "<<=")
    (non-alphabet "++" "--")
    (non-alphabet "==" "!=")
    (non-alphabet "<" "<=" ">" ">=")
    (symbol "break" "continue")
    (symbol "signed" "unsigned"))
   (c++-mode
    (symbol "true" "false")
    (symbol "vector" "array" "deque")
    (symbol "class" "struct")
    (symbol "float" "double")
    (symbol "private" "public" "protected")
    (symbol "pair" "tuple")
    (symbol "static_cast" "dynamic_cast" "reinterpret_cast" "const_cast"))
   (emacs-lisp-mode
    (symbol "nil" "t")
    (symbol "eq" "equal")
    (symbol "let" "let*")
    (symbol "defun" "cl-defun")
    (symbol "defvar" "defcustom" "defconst")
    (symbol "add-hook" "remove-hook")
    (symbol "setq" "setq-default")
    (word   "global" "local")
    (symbol "if" "when" "unless")
    (symbol "assoc" "assq" "alist-get")
    (symbol "assoc-delete-all" "assq-delete-all")
    (symbol "rassoc" "rassq")
    (symbol "member" "memq")
    (symbol "delete" "delq")
    (symbol "remove" "remq")
    (symbol "plist-get" "plist-put")
    (symbol "lax-plist-get" "lax-plist-put")
    (symbol "car" "cdr")
    (symbol "car-safe" "cdr-safe")
    (symbol "mapc" "mapcar")
    (list grugru-default@emacs-lisp+nth!aref))
   ((tex-mode latex-mode yatex-mode)
    (symbol "figure" "table"))
   (org-mode ;; v9.3.6
    (word ":t" ":nil")
    (word "overview" "showall")
    (word "fold" "unfold" "content" "showeverything")
    (word "indent" "noindent")
    (word "align" "noalign")
    (word "inlineimages" "noinlineimages")
    (word "latexpreview" "nolatexpreview")
    (word "hideblocks" "showblocks") ;; nohideblocks
    (word "odd" "oddeven")
    (word "nologrefile" "logrefile" "lognoterefile")
    (word "nologdone" "logdone" "lognotedone")
    (word "nologreschedule" "logreschedule" "lognotereschedule")
    (word "nologredeadline" "logredeadline" "lognoteredeadline")
    (word "lognoteclock-out" "nolognoteclock-out")
    (word "logdrawer" "nologdrawer")
    (word "logstatesreversed" "nologstatesreversed")
    (word "nologrepeat" "logrepeat" "lognoterepeat")
    (word "hidestars" "showstars")
    (word "fninline" "nofninline" "fnlocal")
    (word "fnauto" "fnprompt" "fnconfirm" "fnplain") ;; TODO
    (word "nofnadjust" "fnadjust")
    (word "entitiespretty" "entitiesplain")
    (word "title" "author" "email" "date")
    ;; source block
    ;; TODO
    )))

(provide 'grugru-default)
;;; grugru-default.el ends here
