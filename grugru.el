;;; grugru.el --- Rotate text at point             -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: convenience, abbrev, tools

;; Version: 1.14.0
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
;;;; ~(grugru-define-global GETTER STRINGS-OR-FUNCTION)~
;;    Define global grugru with GETTER and STRINGS-OR-FUNCTION.
;;
;;    GETTER is a function, or a symbol which is alias defined in ~grugru-getter-alist~.
;;    GETTER also can be positive or negative number, which means the number of characters.
;;    By default, symbol, word, char is available.
;;    If it is a function, it should return cons cell ~(begin . end)~
;;    which express things at point, and with no argument.
;;
;;    STRINGS-OR-FUNCTION is list of string or function.
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
;;;; ~(grugru-define-on-major-mode MAJOR GETTER STRINGS-OR-FUNCTION)~
;;    Define major-mode local grugru with GETTER and STRINGS-OR-FUNCTION.
;;
;;    Same as ~grugru-define-global~, but grugru defined with this is applied
;;    only in buffer on MAJOR major-mode.  MAJOR can be list of major-modes.
;;
;;      ;; Replace "yes" at point, to "no", or replace "no" at point, to "yes",
;;      ;; only in lisp-interaction-mode.
;;      (grugru-define-on-major-mode lisp-interaction-mode 'symbol '("yes" "no"))
;;
;;;; ~(grugru-define-local GETTER STRINGS-OR-FUNCTION)~
;;    Define buffer-local grugru with GETTER and STRINGS-OR-FUNCTION.
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
;;;; ~(grugru-define-multiple &rest CLAUSES)~
;;    This function define multiple grugru.
;;
;;    Each ~CLAUSE~ is:
;;    - ~(GETTER . STRINGS-OR-FUNCTION)~: means ~(grugru-define-global GETTER  STRINGS-OR-FUNCTION)~.
;;    - ~(MAJOR (GETTER . STRINGS-OR-FUNCTION)...)~: means ~(grugru-define-on-major-mode MAJOR GETTER  STRINGS-OR-FUNCTION)...~.
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
;;    Each element of BODY is ~(GETTER . STRINGS-OR-FUNCTION)~,
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
;;    in ~STRINGS-OR-FUNCTION~, on ~grugru-select~.
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
  '((symbol . (bounds-of-thing-at-point 'symbol))
    (word   . grugru--get-word)
    (char   . (cons (point) (1+ (point))))
    (line   . (bounds-of-thing-at-point 'line))
    (defun  . (bounds-of-thing-at-point 'defun))
    (non-alphabet . grugru--get-non-alphabet))
  "An alist of getter of current thing.
Each element should be (SYMBOL . FUNC-OR-SEXP).  SYMBOL is used to access to
SEXP by `grugru'.  FUNC-OR-SEXP should be sexp or function
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
  'grugru-completing-function)

(defcustom grugru-completing-function #'completing-read
  "Completing read function used `grugru-edit'.
You can also use `ivy-completing-read' or `ido-completing-read'."
  :group 'grugru
  :risky t
  :type 'function)

(defcustom grugru-select-function-generate-number 30
  "Max strings which are generated by function in STRS-OR-FUNCTION, on `grugru-select'."
  :group 'grugru
  :type 'number)

(defcustom grugru-local-interactively-default-getter 0
  "Default getter of `grugru-define-local' on interactive usage.
If 0, gets number from first string."
  :group 'grugru
  :risky t
  :type '(choice (const 0) function symbol))

(defvar grugru--major-modes-grugru-alist '()
  "An alist of rotated text on each `major-mode'.
Each element should be (MAJOR-MODE . ALIST).

ALIST is compounded from (GETTER . STRINGS-OR-FUNCTION).
GETTER is symbol in `grugru-getter-alist'.  By default, `symbol', `word',
`char' is available as GETTER.
STRINGS-OR-FUNCTION can be a list of strings, or function which recieves
current thing as an argument and returns next text.")

(defvar grugru--global-grugru-alist '()
  "This variable keeps global list of (GETTER . STRINGS-OR-FUNCTION).
GETTER is symbol in `grugru-getter-alist'.  By default, `symbol', `word',
`char' is available as GETTER.
STRINGS-OR-FUNCTION can be a list of strings, or function which recieves
current thing as an argument and returns next text.

You can add element to this with `grugru-define-global'.")

(defvar grugru--local-interactively-history nil)

(defvar-local grugru--buffer-local-grugru-alist '()
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

(defvar-local grugru--loaded-local nil
  "Whether the buffer load grugru list or not, on the buffer.
Global grugru is not observed, because `grugru' is remake rotated sets of list.")

(defvar grugru--point-cache nil
  "Cache for keep position on sequentially executed `grugru'.")

(defvar grugru-before-hook nil
  "Hooks run before rotation by `grugru'.")

(defvar grugru-after-hook nil
  "Hooks run after rotation by `grugru'.")

(defvar grugru-after-no-rotate-hook nil
  "Hooks run after `grugru' tries to rotate text but cannot rotate.")

(defconst grugru--non-alphabet-regexp
  (mapconcat
   (lambda (arg)
     (regexp-quote (string arg)))
   "-^\\@;:,\\./=~|`+*<>?_!\"#$%&'"
   "\\|")
  "Regexp which match non alphabet character.
Used in `grugru--get-non-alphabet'.")


;; inner
(defun grugru--get-word ()
  "Get beginning/end of current point."
  (if (or (eq (point) (point-at-eol))
          (string-match "[-\\[\\]_:;&+^~|#$!?%'()<>=*{}.,/\\\\\n\t]\\| "
                        (buffer-substring (point) (1+ (point)))))
      (save-excursion (cons (subword-left) (subword-right)))
    (save-excursion
      (let ((x (subword-right))
            (y (subword-left)))
        (cons y x)))))

(defun grugru--get-non-alphabet ()
  "Get non-alphabet sequence at point."
  (let* ((point (point))
         (beg
          (progn
            (while (and (<= (point-min) point)
                        (string-match grugru--non-alphabet-regexp
                                      (buffer-substring-no-properties
                                       point (1+ point))))
              (setq point (1- point)))
            point))
         (point (point))
         (end
          (progn
            (while (and (<= point (point-max))
                        (string-match grugru--non-alphabet-regexp
                                      (buffer-substring-no-properties
                                       point (1+ point))))
              (setq point (1+ point)))
            point)))
    (unless (= beg end)
      (cons (1+ beg) (1- end)))))

(defun grugru--get-with-integer (number)
  "Get string of buffer from point by NUMBER characters.
NUMBER can be negative."
  (let ((p (+ (point)))
        (q (+ (point) number)))
    (when (and (<= (point-min) p) (<= (point-min) q)
               (<= p (point-max)) (<= q (point-max)))
      (cons (min p q) (max p q)))))

(defun grugru--major-mode-load ()
  "Load grugru in current buffer."
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

(defun grugru--get-next-string (string strs-or-function)
  "Get next string of STRING with STRS-OR-FUNCTION."
  (pcase strs-or-function
    ((pred functionp)
     (funcall strs-or-function string))
    ((pred listp)
     (let ((list (member string strs-or-function)))
       (when list
         (if (eq (length list) 1)
             (car strs-or-function)
           (nth 1 list)))))
    (_ nil)))

(defun grugru--get-previous-string (string strs-or-function)
  "Get previous string of STRING with STRS-OR-FUNCTION.
If STRS-OR-FUNCTION is a function which recieves 2 arguments, return nil."
  (pcase strs-or-function
    ((pred functionp)
     (condition-case nil
         (funcall strs-or-function string 'reverse)
       ('wrong-number-of-arguments nil)))
    ((pred listp)
     (let* ((reversed-strs (reverse strs-or-function))
            (list (member string reversed-strs)))
       (when list
         (if (eq (length list) 1)
             (car reversed-strs)
           (nth 1 list)))))
    (_ nil)))

(defun grugru--get-getter-function (getter)
  "Get getter function from GETTER."
  (setq getter (or (cdr (assq getter grugru-getter-alist)) getter))
  (or (if (functionp getter)
          getter
        `(lambda () ,getter))))

(defun grugru--get-tuple-list (alist &optional only-one reverse)
  "Return tuple list constructed with ALIST.
If ONLY-ONE is non-nil, returned value is 1 tuple, which matches first.
Each element of ALIST is (SYMBOL . GRUGRU-ALIST).
Each element of GRUGRU-ALIST is (GETTER . STRS-OR-FUNCTION), which is same as
`grugru-define-global'.
If optional argument REVERSE is non-nil, get string reversely."
  (eval
   `(let (cache cached? begin end cons next element)
      (cl-loop
       for (symbol . grugrus) in ',alist
       do
       (setq
        element
        (cl-loop
         for (getter . strs-or-func) in (symbol-value grugrus)
         do
         (setq cons
               (or (setq cached? (cdr (assoc getter cache)))
                   (if (integerp getter)
                       (grugru--get-with-integer getter)
                    (funcall (grugru--get-getter-function getter)))))
         (unless cached? (push (cons getter cons) cache))

         (setq begin (car cons) end (cdr cons))
         if (and
             cons
             (setq next
                   (if ,reverse
                       (grugru--get-previous-string (buffer-substring begin end) strs-or-func)
                     (grugru--get-next-string (buffer-substring begin end) strs-or-func))))
         ,(if only-one 'return 'collect)
         (list symbol (cons begin end) next getter strs-or-func)))
       when element
       ,@(if only-one
             '(return element)
           '(append element))))))

(defun grugru--insert-sexp-append-to-file (sexp file)
  "Insert SEXP to the end of FILE."
  (with-temp-buffer
    (let (print-level print-length)
      (encode-coding-string
       (format "%S
" sexp)
       'utf-8 nil (current-buffer))
      (write-region nil nil file t))))

(defun grugru--make-expression (less-tuple new)
  "Make sexp to change grugru from LESS-TUPLE to NEW."
  (if (eq (nth 0 less-tuple) 'global)
      (if new
          `(grugru-redefine-global ',(nth 1 less-tuple) ',(nth 2 less-tuple) ',new)
        `(grugru-remove-global ',(nth 1 less-tuple) ',(nth 2 less-tuple)))
    (if new
        `(grugru-redefine-on-major-mode ',(nth 0 less-tuple) ',(nth 1 less-tuple)
                                        ',(nth 2 less-tuple) ',new)
      `(grugru-remove-on-major-mode ',(nth 0 less-tuple) ',(nth 1 less-tuple)
                                    ',(nth 2 less-tuple)))))

(defun grugru--strings-or-function-p (object)
  "Return non-nil if OBJECT is acceptable as `strs-or-func'."
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

(defun grugru--select-generate-strings (init func num)
  "Generate string list.
INIT is initial term, and FUNC generates string recursively.
NUM is a maximum length of the generated list."
  (let (all
        (now init))
    (cl-loop
     for i below num
     if (or (null now) (member now all))
     return 0
     end
     do (push now all)
     do (setq now (funcall func now)))
    (nreverse all)))


;; For user interaction

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
      (let ((tuple
             (grugru--get-tuple-list
              `((local       . grugru--global-grugru-alist)
                (,major-mode . grugru--buffer-local-major-mode-grugru-alist)
                (global      . grugru--buffer-local-grugru-alist))
              'only-one (< prefix 0))))
        (if tuple
            (let* ((begin (car (nth 1 tuple)))
                   (end   (cdr (nth 1 tuple)))
                   (str (nth 2 tuple))
                   (bef (- (point) begin)))
              (grugru--replace begin end str)
              (grugru--load-and-cache-position begin (length str) bef)
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

;;;###autoload
(defun grugru-edit (less-tuple new)
  "Edit grugru which can be rotated at point.
LESS-TUPLE, (symbol getter strs-or-func), indicates which grugru is to edit.
NEW indicates to which grugru is changed, which is new strs-or-func.
The change made by this function is saved in file `grugru-edit-save-file'."
  (interactive
   (progn
     (unless grugru--loaded-local
       (grugru--major-mode-load)
       (setq grugru--loaded-local t))
     (let* ((lst
             ;; lst has list of cons cell (prompt . less-tuple)
             ;; less-tuple means (symbol getter strs-or-func).
             (mapcar
              (lambda (arg)
                (cons (format "%S(%S): %S" (nth 0 arg) (nth 3 arg) (nth 4 arg))
                      (mapcar (lambda (n) (nth n arg)) '(0 3 4))))
              (grugru--get-tuple-list
               `((,major-mode . grugru--buffer-local-major-mode-grugru-alist)
                 (global . grugru--global-grugru-alist)))))
            (cons (assoc (funcall grugru-completing-function "Edit grugru: "
                                  lst nil t nil nil (car lst))
                         lst))
            (new (read (read-from-minibuffer (format "Edit '%s' to: " (nth 0 cons))
                                             (format "%S" (nth 2 (cdr cons)))))))
       (list (cdr cons) new))))
  (unless grugru--loaded-local
    (grugru--major-mode-load)
    (setq grugru--loaded-local t))
  (let ((expression (grugru--make-expression less-tuple new)))
    (eval expression)
    (grugru--insert-sexp-append-to-file expression grugru-edit-save-file)))

;;;###autoload
(defun grugru-edit-load ()
  "Load edited grugru saved on `grugru-edit-save-file'."
  (interactive)
  (load grugru-edit-save-file t nil t))

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
     (let* ((tuples (grugru--get-tuple-list
                     `((local . grugru--buffer-local-grugru-alist)
                       (,major-mode . grugru--buffer-local-major-mode-grugru-alist)
                       (global . grugru--global-grugru-alist))))
            (lst
             ;; lst has list of cons cell (prompt . another-less-tuple)
             ;; another-less-tuple means (symbol (begin . end) getter strs-or-func).
             (mapcar
              (lambda (arg)
                (cons (format "%S(%S): %S" (nth 0 arg) (nth 3 arg)
                              (if (functionp (nth 4 arg))
                                  ;; Replace function to strings on strs-or-func.
                                  (grugru--select-generate-strings
                                   (buffer-substring-no-properties
                                    (car (nth 1 arg)) (cdr (nth 1 arg)))
                                   (nth 4 arg)
                                   grugru-select-function-generate-number)
                                (nth 4 arg)))
                      (mapcar
                       (lambda (n)
                         (nth n arg))
                       '(0 1 3 4))))
              tuples))
            (cons
             (if (eq (length tuples) 1)
                 (car lst)
               (assoc (funcall grugru-completing-function " Choose grugru: "
                               lst nil t nil nil (car lst))
                      lst)))
            (begin (car (nth 1 (cdr cons))))
            (end   (cdr (nth 1 (cdr cons))))
            (strs-or-func (nth 3 (cdr cons)))
            (strings
             (remove
              (buffer-substring-no-properties begin end)
              (if (functionp strs-or-func)
                  (grugru--select-generate-strings
                   (buffer-substring-no-properties begin end)
                   strs-or-func grugru-select-function-generate-number)
                strs-or-func)))
            (str
             (funcall grugru-completing-function
                      (format "Replace \"%s\" with: "
                              (buffer-substring-no-properties begin end))
                      strings nil t nil nil (car strings))))
       (list begin end str))))

  (grugru--replace begin end str))


;; For lisp user
;;;###autoload
(defun grugru-define-on-major-mode (major getter strings-or-function)
  "Add new grugru STRINGS-OR-FUNCTION in MAJOR major mode, with GETTER.

MAJOR is `major-mode' or list of that where the grugru is set.
GETTER is symbol in `grugru-getter-alist'.  By default, `symbol', `word',
`char', `line', `defun', `non-alphabet', integer or function are available as GETTER.
Integer means getting string from point by NUMBER characters.
If function, it should return cons cell (BEG END), which indicates buffer substring.
STRINGS-OR-FUNCTION can be a list of strings, or function which recieves
current thing as an argument and returns next text."
  (if (listp major)
      (mapc (lambda (arg)
              (grugru-define-on-major-mode arg getter strings-or-function))
            major)
    (let ((x (assoc major grugru--major-modes-grugru-alist)))
      (if x
          (unless (member (cons getter strings-or-function) (cdr x))
            (setf (cdr (last (cdr x))) (list (cons getter strings-or-function))))
        (push (cons major (list (cons getter strings-or-function)))
              grugru--major-modes-grugru-alist))
      (grugru--major-mode-set-as-unloaded major))))

;;;###autoload
(defmacro grugru-define-on-local-major-mode (getter strings-or-function)
  "Same as (grugru-define-on-major-mode major-mode GETTER STRINGS-OR-FUNCTION)."
  `(grugru-define-on-major-mode (eval 'major-mode) ,getter ,strings-or-function))

;;;###autoload
(defun grugru-define-local (getter strings-or-function)
  "Add new grugru STRINGS-OR-FUNCTION with GETTER on buffer-local.

GETTER is symbol in `grugru-getter-alist'.  By default, `symbol', `word',
`char', `line', `defun', `non-alphabet', integer or function are available as GETTER.
Integer means getting string from point by NUMBER characters.
If function, it should return cons cell (BEG END), which indicates buffer substring.
STRINGS-OR-FUNCTION can be a list of strings, or function which recieves
current thing as an argument and returns next text.

On interactive usage, by default, GETTER is the length of car of STRINGS-OR-FUNCTION,
and STRINGS-OR-FUNCTION is a list which has 2 elements, constructed interactively.
With prefix argument, you can select GETTER and length of STRINGS-OR-FUNCTION.
Default GETTER is set by `grugru-local-interactively-default-getter'."
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
  (unless (member (cons getter strings-or-function) grugru--buffer-local-grugru-alist)
    (push (cons getter strings-or-function) grugru--buffer-local-grugru-alist)))

;;;###autoload
(defun grugru-define-global (getter strings-or-function)
  "Add new grugru STRINGS-OR-FUNCTION with GETTER globally.

GETTER is symbol in `grugru-getter-alist'.  By default, `symbol', `word',
`char', `line', `defun', `non-alphabet', integer or function are available as GETTER.
Integer means getting string from point by NUMBER characters.
If function, it should return cons cell (BEG END), which indicates buffer substring.
STRINGS-OR-FUNCTION can be a list of strings, or function which recieves
current thing as an argument and returns next text."
  (unless (member (cons getter strings-or-function) grugru--global-grugru-alist)
    (push (cons getter strings-or-function) grugru--global-grugru-alist)))

;;;###autoload
(defmacro grugru-define-function (name _ &optional docstring &rest body)
  "You can define grugru function NAME with DOCSTRING and BODY.
The function defined with this rotates text at point only if it is matched to
one element of BODY.

DOCSTRING is optional argument, which is passed to defun as DOCSTRING,
and BODY is sequence of (GETTER . STRING-OR-FUNCTION).

GETTER is symbol in `grugru-getter-alist'.  By default, `symbol', `word',
`char', `line', `defun', `non-alphabet', integer or function are available as GETTER.
Integer means getting string from point by NUMBER characters.
If function, it should return cons cell (BEG END), which indicates buffer substring.
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
       (let ((grugru--global-grugru-alist ',args)
             (grugru--buffer-local-grugru-alist nil)
             (grugru--buffer-local-major-mode-grugru-alist nil)
             (grugru--loaded-local t))
         (call-interactively #'grugru)))))

;;;###autoload
(defun grugru-remove-on-major-mode (major getter strings-or-function)
  "Remove `major-mode' local grugru defined with MAJOR, GETTER and STRINGS-OR-FUNCTION."
  (if (listp major)
      (mapcar (lambda (arg)
                (grugru-remove-on-major-mode arg getter strings-or-function))
              major)
    (grugru--major-mode-set-as-unloaded major)
    (let ((major-grugru (assq major grugru--major-modes-grugru-alist)))
      (setf (cdr major-grugru) (delete (cons getter strings-or-function)
                                       (cdr major-grugru))))))

;;;###autoload
(defmacro grugru-remove-on-local-major-mode (getter strings-or-function)
  "Same as (grugru-remove-on-major-mode major-mode GETTER STRINGS-OR-FUNCTION)."
  `(grugru-remove-on-major-mode (eval 'major-mode) ,getter ,strings-or-function))

;;;###autoload
(defun grugru-remove-local (getter strings-or-function)
  "Remove local grugru defined with GETTER and  STRINGS-OR-FUNCTION."
  (setq grugru--buffer-local-grugru-alist
        (delete (cons getter strings-or-function) grugru--buffer-local-grugru-alist)))

;;;###autoload
(defun grugru-remove-global (getter strings-or-function)
  "Remove global grugru defined with GETTER and  STRINGS-OR-FUNCTION."
  (setq grugru--global-grugru-alist
        (delete (cons getter strings-or-function) grugru--global-grugru-alist)))

;;;###autoload
(defun grugru-redefine-on-major-mode (major getter old-str-or-func new-str-or-func)
  "Redefine grugru defined with GETTER and OLD-STR-OR-FUNC on MAJOR to NEW-STR-OR-FUNC."
  (if (listp major)
      (mapcar
       (lambda (arg)
         (grugru-redefine-on-major-mode arg getter old-str-or-func new-str-or-func))
       major)
    (let* ((major-lst (cdr (assq major grugru--major-modes-grugru-alist)))
           (lst (car (member (cons getter old-str-or-func) major-lst))))
      (if lst
          (setf (cdr lst) new-str-or-func)
        (error "No grugru defined on %s with %s, %s" major getter old-str-or-func)))))

;;;###autoload
(defun grugru-redefine-global (getter old-str-or-func new-str-or-func)
  "Redefine grugru defined with GETTER and OLD-STR-OR-FUNC on to NEW-STR-OR-FUNC."
  (let* ((lst (car (member (cons getter old-str-or-func) grugru--global-grugru-alist))))
    (if lst
        (setf (cdr lst) new-str-or-func)
      (error "No grugru defined with %s, %s" getter old-str-or-func))))

;;;###autoload
(defun grugru-redefine-local (getter old-str-or-func new-str-or-func)
  "Redefine grugru defined with GETTER and OLD-STR-OR-FUNC on to NEW-STR-OR-FUNC."
  (let* ((lst (car (member (cons getter old-str-or-func) grugru--buffer-local-grugru-alist))))
    (if lst
        (setf (cdr lst) new-str-or-func)
      (error "No grugru defined with %s, %s" getter old-str-or-func))))

;;;###autoload
(defmacro grugru-define-multiple (&rest clauses)
  "Define multiple grugru with CLAUSES.
Each element of CLAUSES can be:
 - (GETTER . STRINGS-OR-FUNCTION)
   This is regarded as (`grugru-define-global' GETTER STRINGS-OR-FUNCTION).
 - (MAJOR-MODE . ((GETTER . STRINGS-OR-FUNCTION)...))
   This is regarded as multiple sexps, each one is like
   (`grugru-define-on-major-mode' MAJOR-MODE GETTER STRINGS-OR-FUNCTION).
 - (CLAUSE...)
   List of cons cells like above is also acceptable."
  `(progn
     ,@(mapcar
        (lambda (arg)
          (cond
           ;; (MAJOR-MODE . ((GETTER . STRINGS-OR-FUNCTION)...))
           ((and
             (listp arg)
             (listp (cdr arg))
             (listp (cadr arg))
             (grugru--strings-or-function-p (cdr (cadr arg)))
             ;; car is not (getter . strs-or-func)
             (not
              (and (listp (car arg))
                   (grugru--strings-or-function-p (cdar arg)))))
            `(progn
               ,@(cl-loop
                  for (getter . strings-or-function) in (cdr arg)
                  collect
                  `(grugru-define-on-major-mode
                    ',(car arg) ',getter ',strings-or-function))))
           ;;(CLAUSE...)
           ((and (listp (car arg)) (not (functionp (car arg)))
                 `(grugru-define-multiple ,@arg)))
           ;; (GETTER . STRINGS-OR-FUNCTION)
           ((or (functionp (car arg)) (assq (car arg) grugru-getter-alist))
            `(grugru-define-global ',(car arg) ',(cdr arg)))
           ;; Not acceptable
           (t (error "Wrong clauses on `grugru-define-multiple'"))))
        clauses)))

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

(defvar-local grugru--highlight-overlay nil)

(defvar grugru--highlight-timer-cache nil)

(defun grugru--highlight-add ()
  "Put highlight if grugru is available at point.
This is used by command `grugru-highlight-mode'."
  (unless grugru--loaded-local
    (grugru--major-mode-load)
    (setq grugru--loaded-local t))
  (grugru--highlight-remove)
  (let* ((tuple (grugru--get-tuple-list
                 `((local . grugru--buffer-local-grugru-alist)
                   (,major-mode . grugru--buffer-local-major-mode-grugru-alist)
                   (global . grugru--global-grugru-alist))
                 t))
         (cons (nth 1 tuple)))
    (when cons
      (setq grugru--highlight-overlay (make-overlay (car cons) (cdr cons)))
      (overlay-put grugru--highlight-overlay 'face 'grugru-highlight-face))))

(defun grugru--highlight-remove ()
  "Remove highlight added by command `grugru-highlight-mode' on current buffer."
  (when grugru--highlight-overlay
    (delete-overlay grugru--highlight-overlay)
    (setq grugru--highlight-overlay nil)))

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
