;;; grugru.el --- Rotate text at point             -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2020  ROCKTAKEY

;; Author: ROCKTAKEY <rocktakey@gmail.com>
;; Keywords: convenience, abbrev, tools

;; Version: 1.7.1
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

;;; Grugru: Rotate text at point.
;;   With this package, you can rotate things at point.

;;; How to Use?
;;   You can interactively use the function `grugru'.
;;   This function rotate the thing at point
;;   if it is assigned to `grugru-*-grugru-alist'.  You can assign rotated things with
;;   `grugru-define-on-major-mode', `grugru-define-on-local-major-mode', and `grugru-define-local'.
;;
;;     ;; Define grugru on major-mode.
;;     (grugru-define-on-major-mode 'c-mode 'symbol '("unsigned" "signed"))
;;     (grugru-define-on-major-mode 'c-mode 'word '("get" "set"))
;;     ;; Now, you can toggle unsigned <=> signed and get <=> set
;;     ;; by running the command grugru in c-mode.
;;
;;     ;; Define grugru on current major-mode.
;;     ;; Same as (grugru-define-on-major-mode major-mode 'symbol '("red" "green" "yellow"))
;;     ;; This should be run in some hook or function,
;;     ;; because major-mode is not confirmed if in init.el.
;;     (grugru-define-on-local-major-mode 'symbol '("red" "green" "yellow"))
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
;;     ;; You can define function which rotate pre-specified texts.
;;     ;; For example, three-state can rotate only 2 tuples,
;;     ;; ("water" "ice" "vapor") and ("solid" "liquid" "gas"),
;;     ;; not any other tuples defined by grugru-define-global and so on.
;;     (grugru-define-function three-state ()
;;      "Docstring.  This is optional."
;;      (symbol . ("water" "ice" "vapor"))
;;      (symbol . ("solid" "liquid" "gas")))
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
which return cons cell whose car/cdr is beginning/end point of current thing."
  :group 'grugru
  :risky t
  :type '(&rest (symbolp . [functionp sexp])))

(defcustom grugru-edit-save-file
  (expand-file-name ".grugru" user-emacs-directory)
  "File which has saved data, provided by `grugru-edit'."
  :group 'grugru
  :type 'string)

(defcustom grugru-edit-completing-function #'completing-read
  "Completing read function used `grugru-edit'.
You can also use `ivy-completing-read' or `ido-completing-read'."
  :group 'grugru
  :risky t
  :type 'function)

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


;; inner
(defun grugru--get-word ()
  "Get beginning/end of current point."
  (if (or (eq (point) (point-max))
          (string-match "[-\\[\\]_:;&+^~|#$!?%'()<>=*{}.,/\\\\\n\t]\\| "
                        (buffer-substring (point) (1+ (point)))))
      (save-excursion (cons (subword-left) (subword-right)))
    (save-excursion
      (let ((x (subword-right))
            (y (subword-left)))
        (cons y x)))))

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

(defun grugru--get-getter-function (getter)
  "Get getter function from GETTER."
  (setq getter (or (cdr (assq getter grugru-getter-alist)) getter))
  (or (if (functionp getter)
          getter
        `(lambda () ,getter))))

(defun grugru--get-tuple-list (alist &optional only-one)
  "Return tuple list constructed with ALIST.
If ONLY-ONE is non-nil, returned value is 1 tuple, which matches first.
Each element of ALIST is (SYMBOL . GRUGRU-ALIST).
Each element of GRUGRU-ALIST is (GETTER . STRS-OR-FUNCTION), which is same as
`grugru-define-global'."
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
                   (funcall (grugru--get-getter-function getter))))
         (unless cached? (push (cons getter cons) cache))

         (setq begin (car cons) end (cdr cons))
         if (setq next
                  (grugru--get-next-string (buffer-substring begin end) strs-or-func))
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


;; For user interaction
;;;###autoload
(defun grugru ()
  "Rotate thing on point, if it is in `grugru-*-grugru-alist'.

You can directly add element to `grugru--global-grugru-alist',
`grugru--buffer-local-grugru-alist', and `grugru--major-modes-grugru-alist'.
However, directly assignment is risky, so Using `grugru-define-on-major-mode',
`grugru-define-on-local-major-mode', `grugru-define-local', or
`grugru-define-global' is recommended."
  (interactive)
  (unless grugru--loaded-local
    (grugru--major-mode-load)
    (setq grugru--loaded-local t))
  (run-hooks 'grugru-before-hook)
  (let ((tuple
         (grugru--get-tuple-list
          `((global      . grugru--buffer-local-grugru-alist)
            (,major-mode . grugru--buffer-local-major-mode-grugru-alist)
            (local       . grugru--global-grugru-alist))
          'only-one)))
    (if tuple
      (let* ((begin (car (nth 1 tuple)))
             (end   (cdr (nth 1 tuple)))
             (str (nth 2 tuple))
             (now (- (point) begin)))
        (delete-region begin end)
        (insert str)
        (goto-char
         (if (and this-command (eq this-command last-command))
             (+ begin (min grugru--point-cache (length str)))
           (setq grugru--point-cache now)
           (+ begin (min now (length str)))))
        (run-hooks 'grugru-after-hook))
      (run-hooks 'grugru-after-no-rotate-hook))))

;;;###autoload
(defun grugru-edit (less-tuple new)
  "Edit grugru which can be rotated at point.
LESS-TUPLE indicates which grugru is to edit, which is (symbol getter strs-or-func).
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
               `((global . grugru--global-grugru-alist)
                 (,major-mode . grugru--buffer-local-major-mode-grugru-alist)))))
            (cons (assoc (funcall grugru-edit-completing-function "Edit grugru: "
                                  lst nil t nil nil (car lst))
                         lst))
            (new (read (read-from-minibuffer (format "Edit '%s' to: " (nth 0 cons))
                                             (format "%S" (nth 2 (cdr cons)))))))
       (list (cdr cons) new))))
  (let ((expression (grugru--make-expression less-tuple new)))
    (eval expression)
    (grugru--insert-sexp-append-to-file expression grugru-edit-save-file)))

;;;###autoload
(defun grugru-edit-load ()
  "Load edited grugru saved on `grugru-edit-save-file'."
  (interactive)
  (load grugru-edit-save-file t nil t))


;; For lisp user
;;;###autoload
(defun grugru-define-on-major-mode (major getter strings-or-function)
  "Add new grugru STRINGS-OR-FUNCTION in MAJOR major mode, with GETTER.

MAJOR is `major-mode' or list of that where the grugru is set.
GETTER is symbol in `grugru-getter-alist'.  By default, `symbol', `word',
`char' is available as GETTER.
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
`char' is available as GETTER.
STRINGS-OR-FUNCTION can be a list of strings, or function which recieves
current thing as an argument and returns next text."
  (unless (member (cons getter strings-or-function) grugru--buffer-local-grugru-alist)
    (push (cons getter strings-or-function) grugru--buffer-local-grugru-alist)))

;;;###autoload
(defun grugru-define-global (getter strings-or-function)
  "Add new grugru STRINGS-OR-FUNCTION with GETTER globally.

GETTER is symbol in `grugru-getter-alist'.  By default, `symbol', `word',
`char' is available as GETTER.
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
           ((or (and (listp (car arg))
                     (string-match "-mode$" (symbol-name (caar arg))))
                (and (symbolp (car arg))
                     (string-match "-mode$" (symbol-name (car arg)))))
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

(provide 'grugru)
;;; grugru.el ends here
