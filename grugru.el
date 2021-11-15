;;; grugru.el --- Rotate text at point             -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2021  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: convenience, abbrev, tools

;; Version: 1.20.4
;; Package-Requires: ((emacs "24.4"))
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

;;; How to Use?
;;   You can interactively use the function `grugru'.  This function rotate the thing at point
;;   if assigned.  You can assign rotated things with
;;   `grugru-define-on-major-mode', `grugru-define-on-local-major-mode', and `grugru-define-local'.
;;   If you use ~grugru~, you should assign ~grugru~ to 1 stroke key like ~C-;~, or ~M-g~.
;;
;;     (global-set-key (kbd "C-;") #'grugru)   ; Or other key.
;;
;;
;;   If you want use default grugru, eval ~grugru-default-setup~.  In the other words,
;;   add to your init.el:
;;
;;   (grugru-default-setup)
;;
;;   If you want to change default action at point, you can use ~grugru-edit~,
;;   with which you can edit grugrus at point interactively.  The change edited by this
;;   function is saved in ~grugru-edit-save-file~,
;;   and loaded by run ~grugru-edit-load~.  So to load the change, you can write
;;   on init.el after ~(grugru-default-setup)~:
;;
;;   (grugru-edit-load)
;;
;;
;;   If you want to use ivy or ido as completing-read, set ~grugru-edit-completing-function~.
;;   Or, you can use ~grugru-redefine-\*~ or ~grugru-remove-\*~
;;   for non-interactive editing of default setup.
;;;; Examples
;;
;;     ;; Define grugru on major-mode.
;;     (grugru-define-on-major-mode 'c-mode 'symbol '("unsigned" "signed"))
;;     (grugru-define-on-major-mode 'c-mode 'word '("get" "set"))
;;     ;; Now, you can toggle unsigned <=> signed and get <=> set
;;     ;; by running the command grugru in c-mode.
;;
;;     ;; You can pass a list of symbol major-mode instead of one.
;;     (grugru-define-on-major-mode '(java-mode c++-mode) 'word '("get" "set"))
;;
;;     ;; Define grugru on current major-mode.
;;     ;; Same as (grugru-define-on-major-mode major-mode 'symbol '("red" "green" "yellow"))
;;     ;; This should be run in some hook or function,
;;     ;; because major-mode is not confirmed if in init.el.
;;     (add-hook 'c-mode-common-hook
;;      (lambda ()
;;       (grugru-define-on-local-major-mode 'symbol '("red" "green" "yellow"))))
;;
;;     ;; Define grugru on local.  Should be defined in some hook or function,
;;     ;; because it is saved buffer local.
;;     (add-hook 'text-mode-hook
;;               (lambda ()
;;                (grugru-define-local 'word '("is" "was"))
;;                (grugru-define-local 'word '("I" "my" "me" "mine"))))
;;
;;     ;; Define grugru globally.  This is applied in all buffers.
;;     (grugru-define-global 'symbol '("yes" "no"))
;;
;;     ;; You can use function instead of list of strings.
;;     (grugru-define-on-major-mode
;;      'c-mode 'symbol
;;      (lambda (arg)
;;       (cond
;;        ((string-match "a\\(.*\\)b" arg)
;;         (concat "b" (match-string 1 arg) "c"))
;;        ((string-match "b\\(.*\\)c" arg)
;;         (concat "a" (match-string 1 arg) "b")))))
;;
;;     ;; You can also write like:
;;     (grugru-define-multiple
;;      (fundamental-mode
;;       . ((word . ("aaa" "bbb" "ccc"))
;;          ;; (symbol "xxx" "yyy" "zzz") is same as below.
;;          ;; You can use both.
;;          (symbol . ("xxx" "yyy" "zzz"))
;;          (word . ("abc" "def" "ghi"))))
;;       (word . ("aaaa" "bbbb" "cccc"))
;;       (symbol . ("xxxx" "yyyyy" "zzzzz"))
;;       (word . ("abcd" "defd" "ghid")))
;;     ;; or
;;     (grugru-define-multiple
;;      (fundamental-mode
;;        (word "aaa" "bbb" "ccc")
;;        (symbol "xxx" "yyy" "zzz")
;;        (word "abc" "def" "ghi"))
;;       (word "aaaa" "bbbb" "cccc")
;;       (symbol "xxxx" "yyyyy" "zzzzz")
;;       (word "abcd" "defd" "ghid"))
;;
;;     ;; Above two examples are both expanded to:
;;     (progn
;;       (progn
;;          (grugru-define-on-major-mode 'fundamental-mode 'word '("aaa" "bbb" "ccc"))
;;          (grugru-define-on-major-mode 'fundamental-mode 'symbol '("xxx" "yyy" "zzz"))
;;          (grugru-define-on-major-mode 'fundamental-mode 'word '("abc" "def" "ghi")))
;;        (grugru-define-global 'word '("aaaa" "bbbb" "cccc"))
;;        (grugru-define-global 'symbol '("xxxx" "yyyyy" "zzzzz"))
;;        (grugru-define-global 'word '("abcd" "defd" "ghid")))
;;
;;
;;     ;; You can define function which rotate pre-specified texts.
;;     ;; For example, three-state can rotate only 2 tuples,
;;     ;; ("water" "ice" "vapor") and ("solid" "liquid" "gas"),
;;     ;; not any other tuples defined by grugru-define-global and so on.
;;     (grugru-define-function three-state ()
;;      "Docstring.  This is optional."
;;      (symbol . ("water" "ice" "vapor"))
;;      (symbol . ("solid" "liquid" "gas")))
;;     ;; If you want to find the functions defined by `grugru-define-function'
;;     ;; with `describe-function', execute this:
;;     (grugru-find-function-integration-mode +1)
;;
;;; Interactive Functions
;;;; ~grugru~
;;    This function rotates text at point.
;;    Rotated text is defined by ~grugru-define-*~ functions.
;;    If prefix argument is passed, repeatedly executed.  Negative prefix arguments means
;;    backward rotation.  Also, ~grugru-backward~ can be used for backward rotation.
;;;; ~grugru-select~
;;    This function replace text at point.
;;    You can select grugru and string replaced to.
;;
;;    You can assign completing function to ~grugru-completing-function~.
;;;; ~grugru-edit~
;;    This function edits grugru at point defined by default.
;;
;;    First, select grugru from grugrus available at point.
;;    Then, edit the list in minibuffer.
;;
;;    The change is saved to file ~grugru-edit-save-file~.
;;    You can assign completing function to ~grugru-completing-function~.
;;; Functions Defining grugru
;;;; ~(grugru-define-global GETTER STRINGS-OR-GENERATOR)~
;;    Define global grugru with GETTER and STRINGS-OR-GENERATOR.
;;
;;    GETTER is a function, or a symbol which is alias defined in ~grugru-getter-alist~.
;;    GETTER also can be positive or negative number, which means the number of characters.
;;    By default, symbol, word, char is available.
;;    If it is a function, it should return cons cell ~(begin . end)~
;;    which express things at point, and with no argument.
;;
;;    STRINGS-OR-GENERATOR is list of string or function.
;;
;;    List of string: If it includes string gotten by GETTER,
;;    the things gotten by GETTER is replaced to next string.
;;
;;    Function: It is passed things gotten by GETTER, and should return string
;;    to replace the things to.
;;
;;    You can use like:
;;
;;      ;; Replace "yes" at point, to "no".
;;      ;; Or replace "no" at point, to "yes".
;;      (grugru-define-global 'symbol '("yes" "no"))
;;
;;;; ~(grugru-define-on-major-mode MAJOR GETTER STRINGS-OR-GENERATOR)~
;;    Define major-mode local grugru with GETTER and STRINGS-OR-GENERATOR.
;;
;;    Same as ~grugru-define-global~, but grugru defined with this is applied
;;    only in buffer on MAJOR major-mode.  MAJOR can be list of major-modes.
;;
;;      ;; Replace "yes" at point, to "no", or replace "no" at point, to "yes",
;;      ;; only in lisp-interaction-mode.
;;      (grugru-define-on-major-mode lisp-interaction-mode 'symbol '("yes" "no"))
;;
;;;; ~(grugru-define-local GETTER STRINGS-OR-GENERATOR)~
;;    Define buffer-local grugru with GETTER and STRINGS-OR-GENERATOR.
;;
;;    Same as ~grugru-define-global~, but grugru defined with this is applied
;;    only in buffer where eval this expression.
;;
;;      ;; This should be used in hook or others.
;;      ;; Because this definition is buffer-local.
;;      (add-hook 'text-mode-hook
;;                 (lambda ()
;;                  (grugru-define-local 'word '("is" "was"))
;;                  (grugru-define-local 'word '("I" "my" "me" "mine"))))
;;
;;
;;    Also, you can run it interactively (though cannot set STRINGS-OR-GENERATOR to a function).
;;    On interactive usage, by default, GETTER is the length of car of STRINGS-OR-GENERATOR,
;;    and STRINGS-OR-GENERATOR is a list which has 2 elements, constructed interactively.
;;    With prefix argument, you can select GETTER and length of STRINGS-OR-GENERATOR.
;;    Default GETTER is set by ~grugru-local-interactively-default-getter~.
;;
;;;; ~(grugru-define-multiple &rest CLAUSES)~
;;    This function define multiple grugru.
;;
;;    Each ~CLAUSE~ is:
;;    - ~(GETTER . STRINGS-OR-GENERATOR)~: means ~(grugru-define-global GETTER  STRINGS-OR-GENERATOR)~.
;;    - ~(MAJOR (GETTER . STRINGS-OR-GENERATOR)...)~: means ~(grugru-define-on-major-mode MAJOR GETTER  STRINGS-OR-GENERATOR)...~.
;;    - List of above.
;;
;;
;;     (grugru-define-multiple
;;      (fundamental-mode
;;       . ((word . ("aaa" "bbb" "ccc"))
;;          ;; (symbol "xxx" "yyy" "zzz") is same as below.
;;          ;; You can use both.
;;          (symbol . ("xxx" "yyy" "zzz"))
;;          (word . ("abc" "def" "ghi"))))
;;       (word . ("aaaa" "bbbb" "cccc"))
;;       (symbol . ("xxxx" "yyyyy" "zzzzz"))
;;       (word . ("abcd" "defd" "ghid")))
;;     ;; or
;;     (grugru-define-multiple
;;      (fundamental-mode
;;        (word "aaa" "bbb" "ccc")
;;        (symbol "xxx" "yyy" "zzz")
;;        (word "abc" "def" "ghi"))
;;       (word "aaaa" "bbbb" "cccc")
;;       (symbol "xxxx" "yyyyy" "zzzzz")
;;       (word "abcd" "defd" "ghid"))
;;
;;     ;; Above two examples are both expanded to:
;;     (progn
;;       (progn
;;          (grugru-define-on-major-mode 'fundamental-mode 'word '("aaa" "bbb" "ccc"))
;;          (grugru-define-on-major-mode 'fundamental-mode 'symbol '("xxx" "yyy" "zzz"))
;;          (grugru-define-on-major-mode 'fundamental-mode 'word '("abc" "def" "ghi")))
;;        (grugru-define-global 'word '("aaaa" "bbbb" "cccc"))
;;        (grugru-define-global 'symbol '("xxxx" "yyyyy" "zzzzz"))
;;        (grugru-define-global 'word '("abcd" "defd" "ghid")))
;;
;;;; ~(grugru-define-function NAME () &optional DOCSTRING &rest BODY)~
;;    Define function which can roate only grugru defined by BODY.
;;    Each element of BODY is ~(GETTER . STRINGS-OR-GENERATOR)~,
;;    which meaning is same as ~grugru-define-*~ functions.
;;
;;      ;; The function `three-state' rotate like "water"=>"ice"=>"vapor"=>"water",
;;      ;; or "solid"=>"liquid"=>"gas"=>"solid".
;;      (grugru-define-function three-state ()
;;       "Docstring.  This is optional."
;;       (symbol . ("water" "ice" "vapor"))
;;       (symbol . ("solid" "liquid" "gas")))
;;
;;      ;; This sentense do NOT affect to the function `three-state'.
;;      (grugru-define-global 'symbol '("yes" "no"))
;;
;;; leaf-keyword ~:grugru~
;;   You can use ~:grugru~ keyword on [[https://github.com/conao3/leaf.el][leaf.el]], if you use [[https://github.com/conao3/leaf-keywords.el][leaf-keywords.el]].
;;
;;   By default, ~leaf--name~ is used as major-mode.
;;   Or you can write major-mode obviously.
;;
;;     (leaf lisp-mode
;;      :grugru
;;      (symbol "nil" "t")
;;      (emacs-lisp-mode
;;       (word "add" "remove"))
;;      ...)
;;     ;; The section of `:grugru' means:
;;     (grugru-define-multiple
;;      (symbol "nil" "t")
;;      (emacs-lisp-mode
;;       (word "add" "remove")))
;;
;;; Custom Variables
;;;; ~grugru-getter-alist~
;;    Alist of getter.
;;
;;    Each key (car) of element is a symbol, which is regarded as ~GETTER~.
;;
;;    Each value (cdr) of element is a function or sexp.
;;    It should return things at point.
;;
;;;; ~grugru-edit-save-file~
;;    The name of file saved the information by ~grugru-edit~.
;;    Default value is "~/.emacs.d/.grugru".
;;
;;;; ~grugru-completing-function~
;;    Completing function.  Default value is ~completing-read~.
;;    If you would like to use ivy or ido, write:
;;
;;      ;; For ivy:
;;      (setq grugru-completing-function #'ivy-completing-read)
;;      ;; For ido:
;;      (setq grugru-completing-function #'ido-completing-read)
;;
;;
;;;; ~grugru-select-function-generate-number~
;;    This variable have how many strings are generated from function
;;    in ~STRINGS-OR-GENERATOR~, on ~grugru-select~.
;;
;;;; ~grugru-local-interactively-default-getter~
;;    Indicate default getter on interactive usage of ~grugru-define-local~.
;;    0 means If 0, gets number from first string, otherwise it should be
;;    symbol in ~grugru-getter-alist~ or a function which gets things at point.
;;; License
;;   This package is licensed by GPLv3. See [[file:LICENSE][LICENSE]].

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
  '((defun  . (bounds-of-thing-at-point 'defun))
    (symbol . (bounds-of-thing-at-point 'symbol))
    (word   . grugru--getter-word)
    (char   . (unless (= (point) (point-max)) (cons (point) (1+ (point)))))
    (list   . (bounds-of-thing-at-point 'list))
    (line   . (bounds-of-thing-at-point 'line))
    (non-alphabet . grugru--getter-non-alphabet)
    (tex-command . grugru--getter-tex-command))
  "An alist of getter of current thing.
Each element should be (SYMBOL . FUNCTION-OR-SEXP).  SYMBOL is used to access to
SEXP by `grugru'.  FUNCTION-OR-SEXP should be sexp or getter function,
which return cons cell whose car/cdr is beginning/end point of current thing."
  :group 'grugru
  :risky t
  :type '(&rest (symbolp . [functionp sexp])))

(defcustom grugru-edit-save-file
  (expand-file-name ".grugru" user-emacs-directory)
  "File which has saved data, provided by `grugru-edit'."
  :group 'grugru
  :type 'string)

(define-obsolete-variable-alias 'grugru-edit-completing-function
  'grugru-completing-function
  "1.10.0")

(defcustom grugru-completing-function #'completing-read
  "Completing read function used `grugru-edit'.
You can also use `ivy-completing-read' or `ido-completing-read'."
  :group 'grugru
  :risky t
  :type 'function)

(defcustom grugru-select-function-generate-number 30
  "Max number of strings which are generated by generator on `grugru-select'."
  :group 'grugru
  :type 'number)

(defcustom grugru-local-interactively-default-getter 0
  "Default getter of `grugru-define-local' on interactive usage.
If 0, use the length of the first string."
  :group 'grugru
  :risky t
  :type '(choice (const 0) function symbol))

(defcustom grugru-point-after-rotate 'as-is
  "Where the point is after rotation by `grugru'.
`as-is' means keeping first position.
`beginning' means beginning of rotated things.
`end' means end of rotated things."
  :group 'grugru
  :type '(choice
          (const as-is)
          (const beginning)
          (const end)))

(defcustom grugru-indent-after-rotate t
  "Indent rotated text after `grugru' or not.
Indent happens only if text after rotation has a newline."
  :group 'grugru
  :type 'boolean)

(defcustom grugru-before-hook nil
  "Hooks run before rotation by `grugru'."
  :group 'grugru
  :type 'hook)

(defcustom grugru-after-hook nil
  "Hooks run after rotation by `grugru'."
  :group 'grugru
  :type 'hook)

(defcustom grugru-after-no-rotate-hook nil
  "Hooks run after `grugru' tries to rotate text but cannot rotate."
  :group 'grugru
  :type 'hook)



(defvar grugru--major-modes-grugru-alist '()
  "An alist of rotated text on each `major-mode'.
Each element should be (MAJOR-MODE . ALIST).

ALIST is compounded from (GETTER . STRINGS-OR-GENERATOR).

GETTER is symbol in `grugru-getter-alist', sexp or getter function.
See also `grugru-getter-alist'.

STRINGS-OR-GENERATOR can be a list of strings or generator,
function which recieves current thing as an argument and returns next text.

You can add element to this with `grugru-define-on-major-mode',
 or `grugru-define-on-major-mode'.")

(defvar grugru--global-grugru-alist '()
  "This variable keeps global list of (GETTER . STRINGS-OR-GENERATOR).

GETTER is symbol in `grugru-getter-alist', sexp or getter function.
See also `grugru-getter-alist'.

STRINGS-OR-GENERATOR can be a list of strings or generator,
function which recieves current thing as an argument and returns next text.

You can add element to this with `grugru-define-global'.")

(defvar grugru--local-interactively-history nil)

(defvar-local grugru--buffer-local-grugru-alist '()
  "This variable keeps buffer-local list of (GETTER . STRINGS-OR-GENERATOR).

GETTER is symbol in `grugru-getter-alist', sexp or getter function.
See also `grugru-getter-alist'.

STRINGS-OR-GENERATOR can be a list of strings or generator,
function which recieves current thing as an argument and returns next text.

You can add element to this with `grugru-define-local'.")

(defvar-local grugru--buffer-local-major-mode-grugru-alist '()
  "This variable keeps major-mode-specific list of (GETTER . STRINGS-OR-GENERATOR).

GETTER is symbol in `grugru-getter-alist', sexp or getter function.
See also `grugru-getter-alist'.

STRINGS-OR-GENERATOR can be a list of strings or generator,
function which recieves current thing as an argument and returns next text.

This variable is automatically loaded from `grugru--major-mode-grugru-alist'
by `grugru--major-mode-load'.")

(defvar-local grugru--loaded-local nil
  "Whether the buffer load grugru list or not, on the buffer.
Global grugru is not observed, because `grugru' is remake rotated sets of list.")

(defvar grugru--point-cache nil
  "Cache for keep position on sequentially executed `grugru'.")


;;; Metagetter

(defconst grugru--non-alphabet-regexp
  (mapconcat
   (lambda (arg)
     (regexp-quote (string arg)))
   "-^\\@;:,\\./=~|`+*<>?_!\"#$%&'"
   "\\|")
  "Regexp which match non alphabet character.
Used in `grugru--getter-non-alphabet'.")

(defun grugru--metagetter-from-regexp (regexp)
  "Get beginning/end of string at point matched to REGEXP repeatedly."
  (let ((beg
         (save-excursion
           (let ((bef (point)))
             (while (and (<= (point-min) (point))
                         (re-search-backward regexp nil t)
                         (eq (match-end 0) bef))
               (setq bef (point)))
             bef)))
        (end
         (save-excursion
           (let ((bef (point)))
             (while (and (<= (point) (point-max))
                         (re-search-forward regexp nil t)
                         (eq (match-beginning 0) bef))
               (setq bef (point)))
             bef))))
    (unless (= beg end)
      (cons beg end))))

(defun grugru--metagetter-with-integer (number)
  "Get beginning/end of string at point by NUMBER characters.
NUMBER can be negative."
  (let ((p (+ (point)))
        (q (+ (point) number)))
    (when (and (<= (point-min) p) (<= (point-min) q)
               (<= p (point-max)) (<= q (point-max)))
      (cons (min p q) (max p q)))))


;;; Getter

(defun grugru--getter-word ()
  "Get beginning/end of word at point."
  (if (or (eq (point) (point-at-eol))
          (string-match "[-\\[\\]_:;&+^~|#$!?%'()<>=*{}.,/\\\\\n\t]\\| "
                        (buffer-substring (point) (1+ (point)))))
      (save-excursion (cons (subword-left) (subword-right)))
    (save-excursion
      (let ((x (subword-right))
            (y (subword-left)))
        (cons y x)))))

(defun grugru--getter-non-alphabet ()
  "Get beginning/end non-alphabet string sequence at point."
  (grugru--metagetter-from-regexp grugru--non-alphabet-regexp))

(defun grugru--getter-tex-command ()
  "Get beginning/end of TeX command string sequence at point.
For example, \"\\alpha\", \"\\mathrm\" is valid sequence."
  (save-excursion
    (when (and
           (not (eq (point) (point-max)))
           (string= (buffer-substring-no-properties (point) (1+ (point))) "\\"))
      (goto-char (1+ (point))))
    (let ((cons (grugru--metagetter-from-regexp "[a-zA-Z]")))
      (when (and cons
                 (not (eq (point-min) (car cons)))
                 (string=
                  (buffer-substring-no-properties (1- (car cons)) (car cons))
                  "\\"))
        (cons (1- (car cons)) (cdr cons))))))


;;; Load major-mode-local `grugru'

(defun grugru--major-mode-load ()
  "Load major mode local grugru in current buffer."
  (setq grugru--buffer-local-major-mode-grugru-alist
        (cdr (assq major-mode grugru--major-modes-grugru-alist)))
  (setq grugru--loaded-local t))

(add-hook 'change-major-mode-after-body-hook #'grugru--major-mode-load)

(defun grugru--major-mode-set-as-unloaded (major)
  "Mark buffers on MAJOR `major-mode' as unloaded."
  (mapcar (lambda (arg)
            (with-current-buffer arg
              (when (or (eq major major-mode)
                        (and (listp major) (memq major-mode major)))
                (setq grugru--loaded-local nil))))
          (buffer-list)))


;;; Metagenerator

(defun grugru--metagenerator-simple (strings string &optional reverse)
  "Return string next to STRING in STRINGS.
If REVERSE is non-nil, return previous STRING."
  (when reverse
    (setq strings (reverse strings)))
  (let ((match-list (member string strings)))
    (when match-list
      (or (cadr match-list)
          (car strings)))))


;;; Miscs

(defmacro grugru--set-cons (car cdr value)
  "Set CAR and CDR to car and cdr of VALUE."
  (let ((temp (cl-gensym)))
    `(let ((,temp ,value))
         (setf (cons ,car ,cdr) ,temp))))

(defun grugru--get-valid-bound (point valid-bounds)
  "Return bound if POINT is among VALID-BOUNDS.
VALID-BOUNDS is list of cons cell (BEG . END), which is pair of numbers
indicating range valid to rotate."
  (cl-some
   (lambda (bound)
     (let ((begin (car bound))
           (end (cdr bound)))
       (when (and (<= begin point) (<= point end))
         bound)))
   valid-bounds))

(defun grugru--call-generator (generator string reverse)
  "Call GENERATOR with STRING and REVERSE.
When REVERSE is non-nil, ignore the `wrong-number-of-arguments' error."
  (if reverse
      (condition-case nil
          (funcall generator string t)
        ('wrong-number-of-arguments nil))
    (funcall generator string)))

(defun grugru--get-next-string (string strings-or-generator &optional point reverse)
  "Get next string of STRING with STRINGS-OR-GENERATOR.

POINT is relative from beggining of STRING.  POINT is used when valid-bounds are
detected.

This function returns cons cell (valid-bounds . next-string).

If REVERSE is non-nil, get previous string instead."
  (let* ((func (if (functionp strings-or-generator) strings-or-generator
                 (apply-partially #'grugru--metagenerator-simple strings-or-generator)))
         (result (grugru--call-generator func string reverse))
         (valid-bounds (car-safe result))
         (next-string (or (cdr-safe result) result))
         valid-bound)
    (when (and (not (null valid-bounds))
               (listp valid-bounds)
               (not (consp (car valid-bounds))))
      (setq valid-bounds (list valid-bounds)))

    (setq valid-bound
          (if valid-bounds
              (grugru--get-valid-bound point valid-bounds)
            (cons 0 (length string))))
    (when (and next-string valid-bound)
      (cons valid-bound next-string))))

(defun grugru--get-getter-function (getter)
  "Get getter function from GETTER."
  (setq getter (or (cdr (assq getter grugru-getter-alist)) getter))
  (pcase getter
   ((pred functionp)
    getter)
   ((pred integerp)
    (apply-partially #'grugru--metagetter-with-integer getter))
    (_ `(lambda () ,getter))))

(defun grugru--get-plist (alist &optional only-one reverse)
  "Return plist list constructed with ALIST.
If ONLY-ONE is non-nil, returned value is 1 plist, which matches first.
Each element of ALIST is (SYMBOL . GRUGRU-ALIST).
Each element of GRUGRU-ALIST is (GETTER . STRINGS-OR-GENERATOR), which is same
as `grugru-define-global'.
If optional argument REVERSE is non-nil, get string reversely."
  (let (result
        cache
        symbol
        grugru-alist)
    (while (and alist
                (if only-one (null result) t))
      (grugru--set-cons symbol grugru-alist (pop alist))
      (when (symbolp grugru-alist)
        (setq grugru-alist (symbol-value grugru-alist)))
      (setq result
            (append
             result
             (let (inner-result)
               (while (and grugru-alist
                           (if only-one (null inner-result) t))
                 (let (getter
                       strings-or-generator
                       begin
                       end
                       cached?
                       bound
                       valid-bound
                       next-string)
                   (grugru--set-cons getter strings-or-generator (pop grugru-alist))
                   (setq cached? (assoc getter cache))
                   (setq bound
                         (if cached?
                             (cdr cached?)
                           (funcall (grugru--get-getter-function getter))))
                   (setf (cons begin end) bound)
                   (unless cached? (push (cons getter bound) cache))
                   (when bound
                     (grugru--set-cons valid-bound next-string
                                       (grugru--get-next-string
                                        (buffer-substring begin end)
                                        strings-or-generator
                                        (- (point) begin) reverse)))
                   (when next-string
                     (push (list :symbol symbol
                                 :bound (cons begin end)
                                 :next next-string
                                 :getter getter
                                 :strings-or-generator strings-or-generator
                                 :valid-bound valid-bound)
                           inner-result))))
               (nreverse inner-result)))))
    (if only-one
        (car result)
      result)))

(defun grugru--strings-or-generator-p (object)
  "Return non-nil if OBJECT is acceptable as `strings-or-generator'."
  (when object
    (or
     (functionp object)
     (and (listp object)
          (cl-every #'stringp object)))))

(defun grugru--replace (begin end str)
  "Replace string between points, BEGIN and END, to STR."
  (delete-region begin end)
  (save-excursion (goto-char begin) (insert str)))

(defun grugru--load-and-cache-position (begin len bef)
  "Load and cache position.
`grugru--point-cache' has position from BEGIN, on first running of `grugru'.
If First running of `grugru', save BEF to `grugru--point-cache'.

Goto there if LEN is longer then `grugru--point-cache'.
Otherwise, goto the end of the thing (begin + len)."
  (when (or (not this-command)
            (not (eq this-command last-command)))
    (setq grugru--point-cache bef))
  (goto-char (+ begin (min grugru--point-cache len))))


;;; Main functions

(defvar grugru-highlight-mode)

;;;###autoload
(defun grugru (&optional prefix)
  "Rotate thing on point, if it is in `grugru-*-grugru-alist'.

You can directly add element to `grugru--global-grugru-alist',
`grugru--buffer-local-grugru-alist', and `grugru--major-modes-grugru-alist'.
However, directly assignment is risky, so Using `grugru-define-on-major-mode',
`grugru-define-on-local-major-mode', `grugru-define-local', or
`grugru-define-global' is recommended.

If PREFIX is 0, `grugru-select' is called.
If PREFIX is positive number, rotate text PREFIX times.
If PREFIX is negative number, rotate text previously - PREFIX times."
  (interactive "p")
  (unless grugru--loaded-local
    (grugru--major-mode-load)
    (setq grugru--loaded-local t))
  (run-hooks 'grugru-before-hook)
  (setq prefix (or prefix 1))
  (if (eq prefix 0)
      (call-interactively #'grugru-select)
    (dotimes (_ (abs prefix))
      (let ((plist
             (grugru--get-plist
              '((local       . grugru--global-grugru-alist)
                (major-mode . grugru--buffer-local-major-mode-grugru-alist)
                (global      . grugru--buffer-local-grugru-alist))
              'only-one (< prefix 0))))
        (if plist
            (let* ((bound (plist-get plist :bound))
                   (begin (car bound))
                   (end   (cdr bound))
                   (str (plist-get plist :next))
                   (bef (- (point) begin)))
              (grugru--replace begin end str)
              (pcase grugru-point-after-rotate
                (`as-is (grugru--load-and-cache-position begin (length str) bef))
                (`beginning (goto-char begin))
                (`end (goto-char (+ begin (length str)))))
              (when (and grugru-indent-after-rotate (string-match-p "\n" str))
                (indent-region begin (+ begin (length str))))
              (when grugru-highlight-mode (grugru--highlight-add))
              (run-hooks 'grugru-after-hook))
          (run-hooks 'grugru-after-no-rotate-hook))))))

;;;###autoload
(defalias 'grugru-forward 'grugru
  "`grugru' forwardly PREFIX times.
If PREFIX is positive, same as calling `grugru-backward' with - PREFIX.")

;;;###autoload
(defun grugru-backward (&optional prefix)
  "`grugru' backwardly PREFIX times.
If PREFIX is negative, same as calling `grugru-forward' with - PREFIX."
  (interactive "p")
  (grugru (and prefix (- prefix))))


;; `grugru-edit'

(defun grugru--edit-insert-sexp-append-to-file (sexp file)
  "Insert SEXP to the end of FILE."
  (with-temp-buffer
    (let (print-level print-length)
      (encode-coding-string
       (format "%S
" sexp)
       'utf-8 nil (current-buffer))
      (write-region nil nil file t))))

(defun grugru--edit-make-expression (symbol getter old-strings-or-generator &optional new-strings-or-generator)
  "Make sexp to change old grugru to NEW-STRINGS-OR-GENERATOR grugru.
Old one is specified by SYMBOL, GETTER and OLD-STRINGS-OR-GENERATOR."
  (pcase symbol
    (`global
     (if new-strings-or-generator
         `(grugru-redefine-global ',getter ',old-strings-or-generator ',new-strings-or-generator)
       `(grugru-remove-global ',getter ',old-strings-or-generator)))
    (`local
     (if new-strings-or-generator
         `(grugru-redefine-local ',getter ',old-strings-or-generator ',new-strings-or-generator)
       `(grugru-remove-local ',getter ',old-strings-or-generator)))
    (_
     (if new-strings-or-generator
         `(grugru-redefine-on-major-mode ',symbol ',getter ',old-strings-or-generator ',new-strings-or-generator)
       `(grugru-remove-on-major-mode ',symbol ',getter ',old-strings-or-generator)))))

;;;###autoload
(defun grugru-edit (symbol getter old-strings-or-generator new-strings-or-generator)
  "Edit grugru which can be rotated at point.
SYMBOL indicates what type (global, local or major mode) of grugru is edited.
GETTER and OLD-STRINGS-OR-GENERATOR are used by `grugru-define-global' edited.
NEW-STRINGS-OR-GENERATOR indicates to which grugru is changed, which is new
STRINGS-OR-GENERATOR.

The change made by this function is saved in file `grugru-edit-save-file' if
SYMBOL is not `local'."
  (interactive
   (progn
     (unless grugru--loaded-local
       (grugru--major-mode-load)
       (setq grugru--loaded-local t))
     (let* ((lst
             ;; lst has list of cons cell (prompt . plist)
             (mapcar
              (lambda (arg)
                (cons (format "%S(%S): %S"
                              (plist-get arg :symbol)
                              (plist-get arg :getter)
                              (plist-get arg :strings-or-generator))
                      arg))
              (grugru--get-plist
               `((local . grugru--buffer-local-grugru-alist)
                 (,major-mode . grugru--buffer-local-major-mode-grugru-alist)
                 (global . grugru--global-grugru-alist)))))
            (cons
             (assoc (funcall grugru-completing-function "Edit grugru: "
                             lst nil t nil nil (car lst))
                    lst))
            (prompt (car cons))
            (plist (cdr cons))
            (getter (plist-get plist :getter))
            (old-strings-or-generator (plist-get plist :strings-or-generator))
            (symbol (plist-get plist :symbol))
            (new-strings-or-generator (read (read-from-minibuffer
                        (format "Edit '%s' to: " prompt)
                        (format "%S" old-strings-or-generator)))))
       (list symbol getter old-strings-or-generator new-strings-or-generator))))
  (unless grugru--loaded-local
    (grugru--major-mode-load)
    (setq grugru--loaded-local t))
  (let* ((expression (grugru--edit-make-expression symbol getter old-strings-or-generator new-strings-or-generator)))
    (eval expression)
    (unless (eq symbol 'local)
      (grugru--edit-insert-sexp-append-to-file expression grugru-edit-save-file))))

;;;###autoload
(defun grugru-edit-load ()
  "Load edited grugru saved on `grugru-edit-save-file'."
  (interactive)
  (load grugru-edit-save-file t nil t))


;; `grugru-select'

(defun grugru--select-generate-strings (init generator num)
  "Generate string list.
INIT is initial term, and GENERATOR generates string recursively.
NUM is a maximum length of the generated list."
  (let (all
        (now init))
    (cl-loop
     for i below num
     if (or (null now) (member now all))
     return 0
     end
     do (push now all)
     do (setq now (funcall generator now)))
    (nreverse all)))

;;;###autoload
(defun grugru-select (begin end str)
  "Replace string between BEGIN and END to STR.
When this is called interactively, you can select grugru and STR
by `grugru-completing-function'."
  (interactive
   (progn
     (unless grugru--loaded-local
       (grugru--major-mode-load)
       (setq grugru--loaded-local t))
     (let* ((plists (grugru--get-plist
                     `((local . grugru--buffer-local-grugru-alist)
                       (,major-mode . grugru--buffer-local-major-mode-grugru-alist)
                       (global . grugru--global-grugru-alist))))
            (lst
             ;; lst has list of cons cell (prompt . plist)
             (mapcar
              (lambda (plist)
                (let* ((symbol (plist-get plist :symbol))
                       (getter (plist-get plist :getter))
                       (strings-or-generator (plist-get plist :strings-or-generator))
                       (bound (plist-get plist :bound))
                       (begin (car bound))
                       (end (cdr bound)))
                  (cons (format "%S(%S): %S" symbol getter
                                (if (functionp strings-or-generator)
                                    ;; Replace function to strings on strings-or-generator.
                                    (grugru--select-generate-strings
                                     (buffer-substring-no-properties begin end)
                                     strings-or-generator
                                     grugru-select-function-generate-number)
                                  strings-or-generator))
                        plist)))
              plists))
            (cons
             (if (eq (length plists) 1)
                 (car lst)
               (assoc (funcall grugru-completing-function " Choose grugru: "
                               lst nil t nil nil (car lst))
                      lst)))
            (plist (cdr cons))
            (bound (plist-get plist :bound))
            (begin (car bound))
            (end   (cdr bound))
            (strings-or-generator (plist-get plist :strings-or-generator))
            (strings
             (remove
              (buffer-substring-no-properties begin end)
              (if (functionp strings-or-generator)
                  (grugru--select-generate-strings
                   (buffer-substring-no-properties begin end)
                   strings-or-generator grugru-select-function-generate-number)
                strings-or-generator)))
            (str
             (funcall grugru-completing-function
                      (format "Replace \"%s\" with: "
                              (buffer-substring-no-properties begin end))
                      strings nil t nil nil (car strings))))
       (list begin end str))))

  (grugru--replace begin end str))


;;; Functions defining `grugru'

;;;###autoload
(defun grugru-define-on-major-mode (major getter strings-or-generator)
  "Add new grugru STRINGS-OR-GENERATOR in MAJOR major mode, with GETTER.

MAJOR is `major-mode' or list of that where the grugru is set.
Others are same as `grugru-define-global'."
  (if (listp major)
      (mapc (lambda (arg)
              (grugru-define-on-major-mode arg getter strings-or-generator))
            major)
    (let ((x (assoc major grugru--major-modes-grugru-alist)))
      (if x
          (unless (member (cons getter strings-or-generator) (cdr x))
            (setf (cdr (last (cdr x))) (list (cons getter strings-or-generator))))
        (push (cons major (list (cons getter strings-or-generator)))
              grugru--major-modes-grugru-alist))
      (grugru--major-mode-set-as-unloaded major))))

;;;###autoload
(defmacro grugru-define-on-local-major-mode (getter strings-or-generator)
  "Same as (grugru-define-on-major-mode major-mode GETTER STRINGS-OR-GENERATOR)."
  `(grugru-define-on-major-mode (eval 'major-mode) ,getter ,strings-or-generator))

;;;###autoload
(defun grugru-define-local (getter strings-or-generator)
  "Add new grugru STRINGS-OR-GENERATOR with GETTER on buffer-local.

All arguments are same as `grugru-define-global'.

On interactive usage, by default, STRINGS-OR-GENERATOR is a list which has
two elements, and GETTER is the length of the first string.
Default GETTER is set by `grugru-local-interactively-default-getter'.

With prefix argument, you can select GETTER and length of strings  as
STRINGS-OR-GENERATOR, and then input each string."
  (interactive
   (if current-prefix-arg
       (let* ((getter
               (read
                (funcall grugru-completing-function
                         "String getter(can be integer): "
                         grugru-getter-alist)))
              (num (read-number "How many string?: "))
              (strings
               (nreverse
                (let (result)
                  (dotimes (i num)
                    (push
                     (read-from-minibuffer
                      (concat "Grugru string " (number-to-string (1+ i)) ": ")
                      nil nil nil 'grugru--local-interactively-history)
                     result))
                  result))))
         (list getter strings))
     (let* ((string1 (read-string "First grugru string: " ))
            (getter
             (if (eq grugru-local-interactively-default-getter 0)
                 (- (length string1))
               grugru-local-interactively-default-getter))
            (string2
             (or (read-from-minibuffer
                  (concat
                   "Second grugru string"
                   (when mark-active
                     " (default: region)")
                   ": ")
                  (when mark-active
                    (buffer-substring-no-properties
                     (region-beginning) (region-end)))
                  nil nil
                  'grugru--local-interactively-history)
                 (error "Void string"))))
       (list getter (list string1 string2)))))
  (unless (member (cons getter strings-or-generator) grugru--buffer-local-grugru-alist)
    (push (cons getter strings-or-generator) grugru--buffer-local-grugru-alist)))

;;;###autoload
(defun grugru-define-global (getter strings-or-generator)
  "Add new grugru STRINGS-OR-GENERATOR with GETTER globally.

GETTER is symbol in `grugru-getter-alist', sexp or getter function,
which return cons cell whose car/cdr is beginning/end point of current thing.
GETTER also can be integer NUMBER.  It means getting string from point by NUMBER
characters backward.

STRINGS-OR-GENERATOR can be a list of strings or generator, which recieves
current thing as an argument and returns next text."
  (unless (member (cons getter strings-or-generator) grugru--global-grugru-alist)
    (push (cons getter strings-or-generator) grugru--global-grugru-alist)))

;;;###autoload
(defun grugru-remove-on-major-mode (major getter strings-or-generator)
  "Remove `major-mode' local grugru defined with MAJOR, GETTER and STRINGS-OR-GENERATOR."
  (if (listp major)
      (mapcar (lambda (arg)
                (grugru-remove-on-major-mode arg getter strings-or-generator))
              major)
    (grugru--major-mode-set-as-unloaded major)
    (let ((major-grugru (assq major grugru--major-modes-grugru-alist)))
      (setf (cdr major-grugru) (delete (cons getter strings-or-generator)
                                       (cdr major-grugru))))))

;;;###autoload
(defmacro grugru-remove-on-local-major-mode (getter strings-or-generator)
  "Same as (grugru-remove-on-major-mode major-mode GETTER STRINGS-OR-GENERATOR)."
  `(grugru-remove-on-major-mode (eval 'major-mode) ,getter ,strings-or-generator))

;;;###autoload
(defun grugru-remove-local (getter strings-or-generator)
  "Remove local grugru defined with GETTER and  STRINGS-OR-GENERATOR."
  (setq grugru--buffer-local-grugru-alist
        (delete (cons getter strings-or-generator) grugru--buffer-local-grugru-alist)))

;;;###autoload
(defun grugru-remove-global (getter strings-or-generator)
  "Remove global grugru defined with GETTER and  STRINGS-OR-GENERATOR."
  (setq grugru--global-grugru-alist
        (delete (cons getter strings-or-generator) grugru--global-grugru-alist)))

;;;###autoload
(defun grugru-redefine-on-major-mode (major getter old-strings-or-generator new-strings-or-generator)
  "Redefine grugru defined with GETTER and OLD-STRINGS-OR-GENERATOR on MAJOR to NEW-STRINGS-OR-GENERATOR."
  (if (listp major)
      (mapcar
       (lambda (arg)
         (grugru-redefine-on-major-mode arg getter old-strings-or-generator new-strings-or-generator))
       major)
    (let* ((major-lst (cdr (assq major grugru--major-modes-grugru-alist)))
           (lst (car (member (cons getter old-strings-or-generator) major-lst))))
      (if lst
          (setf (cdr lst) new-strings-or-generator)
        (error "No grugru defined on %s with %s, %s" major getter old-strings-or-generator)))))

;;;###autoload
(defun grugru-redefine-global (getter old-strings-or-generator new-strings-or-generator)
  "Redefine grugru defined with GETTER and OLD-STRINGS-OR-GENERATOR on to NEW-STRINGS-OR-GENERATOR."
  (let* ((lst (car (member (cons getter old-strings-or-generator) grugru--global-grugru-alist))))
    (if lst
        (setf (cdr lst) new-strings-or-generator)
      (error "No grugru defined with %s, %s" getter old-strings-or-generator))))

;;;###autoload
(defun grugru-redefine-local (getter old-strings-or-generator new-strings-or-generator)
  "Redefine grugru defined with GETTER and OLD-STRINGS-OR-GENERATOR on to NEW-STRINGS-OR-GENERATOR."
  (let* ((lst (car (member (cons getter old-strings-or-generator) grugru--buffer-local-grugru-alist))))
    (if lst
        (setf (cdr lst) new-strings-or-generator)
      (error "No grugru defined with %s, %s" getter old-strings-or-generator))))

;;;###autoload
(defmacro grugru-define-multiple (&rest clauses)
  "Define multiple grugru with CLAUSES.
Each element of CLAUSES can be:
 - (GETTER . STRINGS-OR-GENERATOR)
   This is regarded as (`grugru-define-global' GETTER STRINGS-OR-GENERATOR).
 - (MAJOR-MODE . ((GETTER . STRINGS-OR-GENERATOR)...))
   This is regarded as multiple sexps, each one is like
   (`grugru-define-on-major-mode' MAJOR-MODE GETTER STRINGS-OR-GENERATOR).
 - (CLAUSE...)
   List of cons cells like above is also acceptable."
  `(progn
     ,@(mapcar
        (lambda (arg)
          (cond
           ;; (MAJOR-MODE . ((GETTER . STRINGS-OR-GENERATOR)...))
           ((and
             (listp arg)
             (listp (cdr arg))
             (listp (cadr arg))
             (grugru--strings-or-generator-p (cdr (cadr arg)))
             ;; car is not (getter . strings-or-generator)
             (not
              (and (listp (car arg))
                   (grugru--strings-or-generator-p (cdar arg)))))
            `(progn
               ,@(cl-loop
                  for (getter . strings-or-generator) in (cdr arg)
                  collect
                  `(grugru-define-on-major-mode
                    ',(car arg) ',getter ',strings-or-generator))))
           ;;(CLAUSE...)
           ((and (listp (car arg)) (not (functionp (car arg)))
                 `(grugru-define-multiple ,@arg)))
           ;; (GETTER . STRINGS-OR-GENERATOR)
           ((or (functionp (car arg)) (assq (car arg) grugru-getter-alist))
            `(grugru-define-global ',(car arg) ',(cdr arg)))
           ;; Not acceptable
           (t (error "Wrong clauses on `grugru-define-multiple'"))))
        clauses)))


;;;  Functions Defining independent `grugru'

;;;###autoload
(defmacro grugru-define-function (name _ &optional docstring &rest body)
  "You can define grugru function NAME with DOCSTRING and BODY.
The function defined with this rotates text at point only if it is matched to
one element of BODY.

DOCSTRING is optional argument, which is passed to defun as DOCSTRING,
and BODY is sequence of (GETTER . STRINGS-OR-GENERATOR).

GETTER is symbol in `grugru-getter-alist'.  By default, `symbol', `word',
`char', `line', `defun', `non-alphabet', `list', integer or function are
available as GETTER.
Integer means getting string from point by NUMBER characters.
If function, it should return cons cell (BEG . END),
which indicates buffer substring.
STRINGS-OR-GENERATOR can be a list of strings, or function which recieves
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
       (let ((grugru--global-grugru-alist ',args)
             (grugru--buffer-local-grugru-alist nil)
             (grugru--buffer-local-major-mode-grugru-alist nil)
             (grugru--loaded-local t))
         (call-interactively #'grugru)))))

(defun grugru--function-advice (original symbol type library)
  "Advice for `find-function-search-for-symbol' from grugru.
ORIGINAL is original function.  SYMBOL, TYPE and LIBRARY is original arguments."
  (let ((name (symbol-name symbol)))
    (or (funcall original symbol type library)
        (and (null type)
             (with-current-buffer (find-file-noselect library)
               (when (re-search-forward
                      (format "\\|\\(^\\s-*(grugru-define-function\\s-*%s\\)"
                              (regexp-quote name))
                      nil t)
                 (cons (current-buffer) (match-beginning 0))))))))

;;;###autoload
(define-minor-mode grugru-find-function-integration-mode
  "`describe-function' can find functions defined by `grugru-define-function'."
  :global t
  :require 'find-func
  (if grugru-find-function-integration-mode
      (advice-add #'find-function-search-for-symbol :around #'grugru--function-advice)
    (advice-remove #'find-function-search-for-symbol #'grugru--function-advice)))


;;; Highlight

(defcustom grugru-highlight-idle-delay 1
  "Idle delay to add highlight added by command `grugru-highlight-mode'."
  :type 'number
  :group 'grugru)

(defface grugru-highlight-face
  '((t (:box (:line-width 1 :color "#ff0000" :style nil))))
  "Face used `grugru-highlight-mode'."
  :group 'grugru)

(defface grugru-highlight-sub-face
  '((t (:underline (:line-width 1 :color "#0000ff"))))
  "Face used `grugru-highlight-mode'."
  :group 'grugru)

(defvar-local grugru--highlight-overlay nil)

(defvar-local grugru--highlight-overlay-sub nil)

(defvar grugru--highlight-timer-cache nil)

(defun grugru--highlight-add ()
  "Put highlight if grugru is available at point.
This is used by command `grugru-highlight-mode'."
  (unless grugru--loaded-local
    (grugru--major-mode-load)
    (setq grugru--loaded-local t))
  (grugru--highlight-remove)
  (let* ((plist (grugru--get-plist
                 `((local . grugru--buffer-local-grugru-alist)
                   (major-mode . grugru--buffer-local-major-mode-grugru-alist)
                   (global . grugru--global-grugru-alist))
                 t))
         (bound (plist-get plist :bound))
         (begin (car bound))
         (end (cdr bound))
         (valid-bound (plist-get plist :valid-bound))
         (valid-begin (car valid-bound))
         (valid-end (cdr valid-bound)))
    (when bound
      (if (not (eq (- end begin)
                   (- valid-end valid-begin)))
          (progn
            (setq grugru--highlight-overlay-sub
                  (make-overlay (+ begin valid-begin)
                                (+ begin valid-end)))
            (overlay-put grugru--highlight-overlay-sub 'face 'grugru-highlight-face)
            (setq grugru--highlight-overlay (make-overlay begin end))
            (overlay-put grugru--highlight-overlay 'face 'grugru-highlight-sub-face))
        (setq grugru--highlight-overlay (make-overlay begin end))
        (overlay-put grugru--highlight-overlay 'face 'grugru-highlight-face)))))

(defun grugru--highlight-remove ()
  "Remove highlight added by command `grugru-highlight-mode' on current buffer."
  (mapc
   (lambda (ov)
     (when (symbol-value ov)
    (delete-overlay (symbol-value ov))
    (set ov nil)))
   '(grugru--highlight-overlay grugru--highlight-overlay-sub)))

;;;###autoload
(define-minor-mode grugru-highlight-mode
  "Highlight if any rotatable text is at point."
  :global t
  (if grugru-highlight-mode
      (setq grugru--highlight-timer-cache
            (run-with-idle-timer
             grugru-highlight-idle-delay
             t #'grugru--highlight-add))
    (cancel-timer grugru--highlight-timer-cache)
    (mapc
     (lambda (arg)
       (with-current-buffer arg
         (grugru--highlight-remove)))
     (buffer-list))))

(provide 'grugru)
;;; grugru.el ends here
