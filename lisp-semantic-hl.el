;;; lisp-semantic-hl.el --- Semantic Syntax Highlighting for Lisp Languages  -*- lexical-binding:t -*-

;; Copyright (C) 2025-2026 The Calendrical System

;; Author: The Calendrical System <us@calsys.org>
;; Version: 0.1
;; Keywords: languages, lisp, maint
;; URL: https://github.com/calsys456/lisp-semantic-hl.el
;; Package-Requires: ((emacs "27.1"))

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

;; Semantic Syntax Highlighting for Common Lisp & Elisp in Emacs,
;; based on the informations of the global Lisp environment.

;;; Code:

(require 'eieio)

;;; Utils

(defcustom lisp-semantic-hl-loop-keywords-names
  '("named"
    "initially" "finally" "for" "as" "with"
    "do" "collect" "collecting" "append"
    "appending" "nconc" "nconcing" "into" "count"
    "counting" "sum" "summing" "maximize" "return" "loop-finish"
    "maximizing" "minimize" "minimizing" "doing"
    "thereis" "always" "never" "if" "when"
    "unless" "repeat" "while" "until" "by"
    "=" "and" "it" "else" "end" "from" "upfrom"
    "above" "below" "to" "upto" "downto" "downfrom"
    "in" "on" "in-ref" "on-ref" "then" "across"
    "being" "each" "the" "hash-key"
    "hash-keys" "of" "using" "hash-value" "hash-values"
    "symbol" "symbols" "present-symbol"
    "present-symbols" "external-symbol"
    "external-symbols" "fixnum" "float" "of-type"
    ;; Emacs cl-loop
    "frame" "frames" "screen" "screens" "buffer" "buffers"
    "window" "windows" "overlay" "overlays" "extent" "extents"
    "interval" "intervals" "property"
    "key-code" "key-codes" "key-seq" "key-seqs" "key-binding" "key-bindings"
    "concat" "vconcat")
  "Loop keywords from cl-macs.el & https://lispcookbook.github.io/cl-cookbook/iteration.html#appendix-list-of-loop-keywords ."
  :group 'lisp-semantic-hl
  :type '(list string))

;; From lisp-mode.el
(eval-when-compile
  (let ((lisp-fdefs '("defmacro" "defun"))
        (lisp-vdefs '("defvar"))
        (lisp-kw '("cond" "if" "while" "let" "let*" "progn" "prog1"
                   "prog2" "lambda" "unwind-protect" "condition-case"
                   "when" "unless" "with-output-to-string" "handler-bind"
                   "ignore-errors" "dotimes" "dolist" "declare"))
        (lisp-errs '("warn" "error" "signal"))
        (el-fdefs '("defsubst" "cl-defsubst" "define-inline"
                    "define-advice" "defadvice" "defalias"
                    "define-derived-mode" "define-minor-mode"
                    "define-generic-mode" "define-global-minor-mode"
                    "define-globalized-minor-mode" "define-skeleton"
                    "define-widget" "ert-deftest"))
        (el-vdefs '("defconst" "defcustom" "defvaralias" "defvar-local"
                    "defface" "define-error"))
        (el-tdefs '("defgroup" "deftheme"))
        (el-errs '("user-error"))
        (eieio-fdefs '("defgeneric" "defmethod"))
        (eieio-tdefs '("defclass"))
        (cl-lib-fdefs '("defmacro" "defsubst" "defun" "defmethod" "defgeneric"))
        (cl-lib-tdefs '("defstruct" "deftype"))
        (cl-lib-errs '("assert" "check-type"))
        (cl-fdefs '("defsetf" "define-method-combination"
                    "define-condition" "define-setf-expander"
                    "define-compiler-macro" "define-modify-macro"))
        (cl-vdefs '("define-symbol-macro" "defconstant" "defparameter"))
        (cl-tdefs '("defpackage" "defstruct" "deftype"))
        (cl-kw '("block" "break" "case" "ccase" "compiler-let" "ctypecase"
                 "declaim" "destructuring-bind" "do" "do*"
                 "ecase" "etypecase" "eval-when" "flet" "flet*"
                 "go" "handler-case" "in-package"
                 "labels" "letf" "locally" "loop"
                 "macrolet" "multiple-value-bind" "multiple-value-prog1"
                 "proclaim" "prog" "prog*" "progv"
                 "restart-case" "restart-bind" "return" "return-from"
                 "symbol-macrolet" "tagbody" "the" "typecase"
                 "with-accessors" "with-compilation-unit"
                 "with-condition-restarts" "with-hash-table-iterator"
                 "with-input-from-string" "with-open-file"
                 "with-open-stream" "with-package-iterator"
                 "with-simple-restart" "with-slots" "with-standard-io-syntax"))
        (cl-errs '("abort" "cerror")))
    (let ((el-defs (append lisp-fdefs lisp-vdefs
                           el-fdefs el-vdefs el-tdefs
                           (mapcar (lambda (s) (concat "cl-" s))
                                   (append cl-lib-fdefs cl-lib-tdefs))
                           eieio-fdefs eieio-tdefs))
          (cl-defs (append lisp-fdefs lisp-vdefs
                           cl-lib-fdefs cl-lib-tdefs
                           eieio-fdefs eieio-tdefs
                           cl-fdefs cl-vdefs cl-tdefs))
          (cl-kws (append lisp-kw cl-kw))
          (el-errs (append (mapcar (lambda (s) (concat "cl-" s)) cl-lib-errs)
                           lisp-errs el-errs))
          (cl-errs (append lisp-errs cl-lib-errs cl-errs)))
      (defcustom lisp-semantic-hl-el-keywords-names
        el-defs
        "Elisp keyword symbol names. From lisp-mode.el"
        :group 'lisp-semantic-hl
        :type '(list string))

      (defcustom lisp-semantic-hl-cl-keywords-names
        (append cl-defs cl-kws)
        "Common Lisp keyword symbol names. From lisp-mode.el"
        :group 'lisp-semantic-hl
        :type '(list string))

      (defcustom lisp-semantic-hl-el-errors-names
        el-errs
        "Elisp error symbol names. From lisp-mode.el"
        :group 'lisp-semantic-hl
        :type '(list string))

      (defcustom lisp-semantic-hl-cl-errors-names
        cl-errs
        "Common Lisp error symbol names. From lisp-mode.el"
        :group 'lisp-semantic-hl
        :type '(list string)))))

(defun lisp-semantic-hl-collect-forms (point)
  "Collect forms inside ansexp from starting.

POINT should be putted at (|foo bar)"
  (save-excursion
    (cl-loop initially (setq end point)
             for end = (ignore-errors (goto-char end) (forward-sexp 1) (point))
             for start = (ignore-errors (forward-sexp -1) (point))
             while (and start end)
             collect (list start end))))

(defun char-quoted-p (charpos)
  "Returns true if char at CHARPOS is quoted.

Elisp version of with char_quoted in src/syntax.c"
  (cl-loop for pos downfrom charpos above (point-min)
           for quoted = nil then (not quoted)
           while (memql (char-syntax (char-before pos)) '(?\' ?\\))
           finally return quoted))

(defun forward-prefix-chars ()
  "Move point forward over any number of chars with prefix syntax.

Return how many chars forwarded.

Reverse-directional version of `backward-prefix-chars'."
  (cl-loop for syntax = (syntax-after (point))
           while (and (< (point) (point-max))
                      (or (char-quoted-p (point))
                          (= (syntax-class syntax) 6)
                          (cl-plusp (logand (car syntax) (ash 1 20)))))
           do (forward-char)
           sum 1))

(defun lisp-semantic-hl-count-success-quotes-before (point)
  "Count success (not be quoted) string-quote before POINT."
  (save-excursion
    (goto-char 0)
    (cl-loop while (search-forward "\"" (1+ point) t)
             count (not (char-quoted-p (1- (point)))))))

(defun lisp-semantic-hl-try-skip-a-quote-backward (point)
  "Try to skip a success (not be quoted) string-quote backward from POINT."
  (save-excursion
    (goto-char point)
    (cl-loop while (search-backward "\"" (point-min) t)
             until (not (char-quoted-p (point))))
    (point)))

(defun lisp-semantic-hl-try-skip-a-quote-forward (point)
  "Try to skip a success (not be quoted) string-quote forward from POINT."
  (save-excursion
    (goto-char point)
    (cl-loop while (search-forward "\"" (point-max) t)
             until (not (char-quoted-p (1- (point)))))
    (point)))

(defsubst lisp-semantic-hl--apply-highlight (start end attr)
  "Unique API for applying highlight.

In Emacs it just forwards START, END and ATTR to
`font-lock-append-text-property'."
  (font-lock-append-text-property start end 'face attr))


;;; Fontify Symbols Functions

;; Elisp

(defun lisp-semantic-hl-fontify-symbol-elisp (start end)
  "Fontify single ELisp Symbol from START to END."
  (let* ((str (buffer-substring-no-properties start end))
         (sym (intern-soft str)))
    (let ((face (cond ((or (eq sym t)
                           (string-equal str "nil"))
                       'font-lock-keyword-face)
                      ((null sym) nil)
                      ((or (special-form-p sym)
                           (cl-member sym lisp-semantic-hl-el-keywords-names :test #'string=))
                       'font-lock-keyword-face)
                      ((cl-member sym lisp-semantic-hl-el-errors-names :test #'string=)
                       'font-lock-warning-face)
                      ((macrop sym)
                       'font-lock-type-face)
                      ((keywordp sym)
                       'font-lock-builtin-face)
                      ((= (aref str 0) ?\&)
                       'font-lock-operator-face)
                      ((or (find-class sym nil) (cl-find-class sym))
                       'font-lock-type-face)
                      ((fboundp sym) 'font-lock-function-name-face)
                      ((boundp sym) 'font-lock-variable-use-face))))
      (when face (lisp-semantic-hl--apply-highlight start end face)))))

;; Common Lisp

(defvar lisp-semantic-hl--symbols nil
  "Lexical bounded varable for Common Lisp symbol informations to be fontified.")

(defun lisp-semantic-hl-read-string (str)
  "Bulk read all symbols inside STR."
  (with-temp-buffer
    (insert str)
    (let ((end-place (point))
          (last-place -1)
          result)
      (goto-char 0)
      (while (and (< last-place end-place)
                  (/= (point) last-place))
        (setq last-place (point))
        (skip-chars-forward " \t\n\r\f\v(")
        (forward-prefix-chars)
        (let ((symbols (ignore-errors
                         (flatten-tree (read (current-buffer))))))
          (when symbols (setq result (nconc result symbols)))))
      (delete-dups result))))

(defcustom lisp-semantic-hl-compute-symbols-timeout 1.5
  "Timeout for collecting sym info from the CL env via Slime or Sly."
  :type 'number
  :group 'lisp-semantic-hl)

;; In Common Lisp, we can't query the symbol information by the time
;; we're parsing it, as the sly/slime-eval function will take
;; significant time. So we need to prepare the information of all
;; symbols inside the region in a single query, before we start
;; parsing the region.
(defun lisp-semantic-hl-compute-symbols-in-form (start end)
  "Compute symbol informations in form from START to END, for later use."
  (let ((lisp-eval (cond ((and (fboundp 'sly-connected-p)
                               (funcall #'sly-connected-p)
                               (and (boundp 'sly-mode)
                                    (symbol-value 'sly-mode)))
                          'sly-eval)
                         ((and (fboundp 'slime-connected-p)
                               (funcall #'slime-connected-p)
                               (and (boundp 'sly-mode)
                                    (symbol-value 'sly-mode)))
                          'slime-eval)))
        (buffer-pak (when-let* ((pak-func (cond ((fboundp 'sly-current-package)
                                                 #'sly-current-package)
                                                ((fboundp 'slime-current-package)
                                                 #'slime-current-package)))
                                (pak (funcall pak-func)))
                      (upcase (string-trim pak "[#:\"]" "[#:\"]"))))
        lst)
    (when lisp-eval
      (let ((obarray (obarray-make)))
        (dolist (obj (lisp-semantic-hl-read-string
                      (buffer-substring-no-properties start end)))
          (unless (or (cl-member (symbol-name obj) lisp-semantic-hl-cl-keywords-names :test #'string=)
                      (cl-member (symbol-name obj) lisp-semantic-hl-cl-errors-names :test #'string=))
            (push (split-string (symbol-name obj) ":") lst))))
      (with-timeout (lisp-semantic-hl-compute-symbols-timeout)
        (funcall
         lisp-eval
         `(cl:let (result)
           (cl:dolist (split (cl:quote ,lst))
             (cl:let*
              ((symname (cl:string-upcase (cl:car (cl:last split))))
               (sympak (cl:or (cl:when (cl:> (cl:length split) 1)
                                       (cl:if (cl:string= (cl:car split) "")
                                              "KEYWORD"
                                              (cl:find-package (cl:string-upcase (cl:car split)))))
                              (cl:find-package ,buffer-pak)
                              cl:*package*))
               (face
                (cl:when (cl:plusp (cl:length symname))
                  (cl:cond
                   ((cl:string= (cl:car split) "") 'font-lock-builtin-face)
                   ((cl:= (cl:char-code (cl:char symname 0)) 38) 'font-lock-type-face)
                   (cl:t (cl:multiple-value-bind (sym status)
                             (cl:find-symbol symname sympak)
                           (cl:if status
                                  (cl:cond ((cl:or (cl:member sym '(cl:t cl:nil))
                                                   (cl:special-operator-p sym))
                                            'font-lock-keyword-face)
                                           ((cl:macro-function sym)
                                            'font-lock-type-face)
                                           ((cl:or (cl:find-class sym cl:nil)
                                                   (cl:find-package sym))
                                            'font-lock-type-face)
                                           ((cl:fboundp sym) 'font-lock-function-call-face)
                                           ((cl:boundp sym) 'font-lock-variable-use-face)
                                           (cl:t cl:nil))
                                  (cl:cond ((cl:find-package symname)
                                            'font-lock-type-face)))))))))
               (cl:push (cl:cons split face) result)))
           result))))))

(defun lisp-semantic-hl-fontify-symbol-cl (start end)
  "Fontify single Common Lisp symbol from START to END.

`lisp-semantic-hl--symbols' should be bound."
  (when (or (and (fboundp 'sly-connected-p)
                 (funcall #'sly-connected-p))
            (and (fboundp 'slime-connected-p)
                 (funcall #'slime-connected-p)))
    (let* ((str (buffer-substring-no-properties start end))
           (split (split-string str ":"))
           (face (cond ((member str lisp-semantic-hl-cl-keywords-names)
                        'font-lock-keyword-face)
                       ((member str lisp-semantic-hl-cl-errors-names)
                        'font-lock-warning-face)
                       (t (cdr (assoc split lisp-semantic-hl--symbols))))))
      (if (and (> (length split) 1)
               (> (length (car split)) 0))
          (let ((sep (+ start (+ (length (car split))
                                 (1- (length split))))))
            (lisp-semantic-hl--apply-highlight start sep 'font-lock-type-face)
            (when face (lisp-semantic-hl--apply-highlight sep end face)))
        (when face (lisp-semantic-hl--apply-highlight start end face))))))


;;; Fontify Symbol - List - Symbol Recursion

(defun lisp-semantic-hl-fontify-symbol (start end)
  "Fontify single symbol from START to END, without prefix."
  (if (derived-mode-p major-mode 'emacs-lisp-mode)
      (lisp-semantic-hl-fontify-symbol-elisp start end)
    (lisp-semantic-hl-fontify-symbol-cl start end)))

(defun lisp-semantic-hl-fontify-single-form (start end)
  "Fontify a single form from START to END.

The form can be a symbol or a list, with prefix characters.

This function is used to separate prefix & form, colouring prefix
characters, sending rest of the form to fontify-list or
fontify-symbol."
  (goto-char start)
  (let* ((prefix-len (forward-prefix-chars))
         (point      (point))
         (syntax     (syntax-class (syntax-after point))))
    (when (cl-plusp prefix-len)
      (lisp-semantic-hl--apply-highlight start point 'font-lock-negation-char-face))
    (pcase syntax
      ((or 2 3) (lisp-semantic-hl-fontify-symbol point end))
      (4        (lisp-semantic-hl-fontify-list   point)))))

(defun lisp-semantic-hl-fontify-list (start)
  "Parse and dispatch items inside the list from START."
  (setq start (scan-lists start 1 -1))
  ;; Collect sub forms inside the list
  (when-let* ((forms (lisp-semantic-hl-collect-forms start)))
    (let ((1st (apply #'buffer-substring-no-properties (car forms))))
      ;; We can add conditions here, to apply custom fontify
      ;; function for specific clause
      (cond ((cl-member 1st '("declare" "proclaim" "declaim") :test #'string-equal)
             (lisp-semantic-hl-fontify-declaration-list forms))
            ((cl-member 1st '("loop" "cl-loop") :test #'string-equal)
             (lisp-semantic-hl-fontify-loop forms))
            ((cl-member 1st '("let" "let*" "when-let" "when-let*" "if-let" "if-let*"
                              "prog" "prog*"
                              "dolist" "cl-dolist" "dotimes" "cl-dotimes" "seq-doseq"
                              "do-symbols" "do-all-symbols" "cl-do-symbols" "cl-do-all-symbols"
                              "with-slots" "with-accessors")
                        :test #'string-equal)
             (lisp-semantic-hl-fontify-let forms))
            ((cl-member 1st '("lambda"
                              "multiple-value-bind" "cl-multiple-value-bind"
                              "destructuring-bind" "cl-destructuring-bind"
                              "with-gensyms" "with-unique-names")
                        :test #'string-equal)
             (lisp-semantic-hl-fontify-lambda-list-at-1 forms))
            ((cl-member 1st '("defun" "defmacro" "defsubst" "defalias")
                        :test #'string-equal)
             (lisp-semantic-hl-fontify-lambda-list-at-2 forms))
            (t (dolist (l forms) (apply #'lisp-semantic-hl-fontify-single-form l)))))))


;;; Special Fontify Forms

;; loop
(defun lisp-semantic-hl-fontify-loop (lst)
  "Highlight loop keywords for LST."
  (apply #'lisp-semantic-hl-fontify-symbol (pop lst))
  (dolist (form lst)
    (let* ((start (car form))
           (end (cadr form))
           (str (buffer-substring-no-properties start end)))
      (if (cl-member str lisp-semantic-hl-loop-keywords-names :test #'string-equal)
          (lisp-semantic-hl--apply-highlight start end 'font-lock-builtin-face)
        (lisp-semantic-hl-fontify-single-form start end)))))

;; declare, declaim, proclaim
(defun lisp-semantic-hl-fontify-declaration-list (lst)
  "Highlight declarations (declare, declaim, proclaim) for LST."
  (let ((1st (pop lst)))
    (lisp-semantic-hl--apply-highlight (car 1st) (cadr 1st) 'font-lock-preprocessor-face))
  (dolist (form lst)
    (cl-destructuring-bind (start end) form
      (goto-char start)
      (cond ((cl-plusp (forward-prefix-chars))
             (lisp-semantic-hl--apply-highlight start (point) 'font-lock-preprocessor-face)
             (lisp-semantic-hl-fontify-single-form (point) end))
            ((= (char-after) ?\()
             (when-let* ((sub (scan-lists (point) 1 -1)))
               (let ((children (lisp-semantic-hl-collect-forms sub)))
                 (when children (lisp-semantic-hl-fontify-declaration-list children)))))))))

;; let, ...let , do..., with-slots
(defun lisp-semantic-hl-fontify-let (lst)
  "Highlight LST as a `let' form, or something behave like `let'."
  (let (1st)
    (cl-destructuring-bind (start end) (pop lst)
      (lisp-semantic-hl-fontify-single-form start end)
      (setq 1st (buffer-substring-no-properties start end)))
    (when lst
      (cl-destructuring-bind (start end) (pop lst)
        (goto-char start)
        (cond ((cl-plusp (forward-prefix-chars))
               (lisp-semantic-hl--apply-highlight start (point) 'font-lock-negation-char-face)
               (lisp-semantic-hl-fontify-single-form (point) end))
              ((= (char-after) ?\()
               (when-let* ((sub (scan-lists (point) 1 -1))
                           (children (lisp-semantic-hl-collect-forms sub)))
                 (if (or (cl-member 1st '("dolist" "cl-dolist" "dotimes" "cl-dotimes"
                                          "do-symbols" "do-all-symbols")
                                    :test #'string-equal)
                         (and (cl-member 1st '("when-let" "when-let*" "if-let" "if-let*") :test #'string-equal)
                              (progn (goto-char (caar children))
                                     (skip-chars-forward " \t\n\r\f\v")
                                     (/= (char-after) ?\())))
                     (cl-destructuring-bind (start end) (car children)
                       (lisp-semantic-hl--apply-highlight start end 'font-lock-variable-name-face)
                       (dolist (c (cdr children))
                         (apply #'lisp-semantic-hl-fontify-single-form c)))
                   (dolist (child children)
                     (cl-destructuring-bind (start end) child
                       (goto-char start)
                       (cond ((cl-plusp (forward-prefix-chars))
                              (lisp-semantic-hl--apply-highlight start (point) 'font-lock-negation-char-face)
                              (lisp-semantic-hl-fontify-single-form (point) end))
                             ((= (char-after) ?\()
                              (when-let* ((sub (scan-lists (point) 1 -1)))
                                (when-let* ((children (lisp-semantic-hl-collect-forms sub)))
                                  (cl-destructuring-bind (start end)
                                      (car children)
                                    (lisp-semantic-hl--apply-highlight start end 'font-lock-variable-name-face))
                                  (dolist (c (cdr children))
                                    (apply #'lisp-semantic-hl-fontify-single-form c)))))
                             (t (lisp-semantic-hl--apply-highlight (point) end 'font-lock-variable-name-face))))))))
              (t (lisp-semantic-hl-fontify-symbol (point) end)))))
    (dolist (l lst)
      (apply #'lisp-semantic-hl-fontify-single-form l))))

;; function arglist
(defun lisp-semantic-hl-fontify-lambda-list-at-1 (lst)
  "Highlight LST with lambda list at 1 (typically lambda expression)."
  (cl-destructuring-bind (start end) (pop lst)
    (lisp-semantic-hl-fontify-single-form start end))
  (when lst
    (cl-destructuring-bind (start end) (pop lst)
      (goto-char start)
      (cond ((cl-plusp (forward-prefix-chars))
             (lisp-semantic-hl--apply-highlight start (point) 'font-lock-negation-char-face)
             (lisp-semantic-hl-fontify-single-form (point) end))
            ((= (char-after) ?\( )
             (when-let* ((sub (scan-lists (point) 1 -1)))
               (let ((children (lisp-semantic-hl-collect-forms sub)))
                 (dolist (child children)
                   (cl-destructuring-bind (start end) child
                     (goto-char start)
                     (if (cl-plusp (forward-prefix-chars))
                         (progn (lisp-semantic-hl--apply-highlight start (point) 'font-lock-negation-char-face)
                                (lisp-semantic-hl-fontify-single-form (point) end))
                       (pcase (char-after)
                         (?\( (lisp-semantic-hl-fontify-list (point)))
                         (?\& (lisp-semantic-hl--apply-highlight start end 'font-lock-type-face))
                         (_   (lisp-semantic-hl--apply-highlight start end 'font-lock-variable-name-face)))))))))
            (t (lisp-semantic-hl-fontify-symbol (point) end)))))
  (dolist (l lst)
    (apply 'lisp-semantic-hl-fontify-single-form l)))

(defun lisp-semantic-hl-fontify-lambda-list-at-2 (lst)
  "Highlight LST with lambda list at 2 (typically defun)."
  (cl-destructuring-bind (start end) (pop lst)
    (lisp-semantic-hl-fontify-single-form start end))
  (when lst (lisp-semantic-hl-fontify-lambda-list-at-1 lst)))


;;; `font-lock-mode' Integration

(defun lisp-semantic-hl-fontify-keywords-region (start end)
  "Highlight forms covered by START and END."
  (save-excursion
    (when (cl-oddp (lisp-semantic-hl-count-success-quotes-before start))
      (setq start (lisp-semantic-hl-try-skip-a-quote-backward start)))
    (when (cl-oddp (lisp-semantic-hl-count-success-quotes-before end))
      (setq end (lisp-semantic-hl-try-skip-a-quote-forward end)))
    (let ((form-start end))
      (cl-loop for pos = (ignore-errors (scan-lists form-start -1 1))
               while pos
               do (setq form-start pos)
               until (< form-start start))
      (if (< form-start start)
          ;; If the region is inside one form
          (let ((form-end (scan-sexps form-start 1)))
            (if (or (and (boundp 'sly-mode) (symbol-value 'sly-mode))
                    (and (boundp 'slime-mode) (symbol-value 'slime-mode)))
                (let ((lisp-semantic-hl--symbols
                       (lisp-semantic-hl-compute-symbols-in-form form-start form-end)))
                  (lisp-semantic-hl-fontify-single-form form-start form-end))
              (lisp-semantic-hl-fontify-single-form form-start form-end)))
        ;; If the region has crossed the top-level
        (progn
          (setq form-start start)
          (cl-loop for pos = (ignore-errors (scan-lists form-start -1 1))
                   while pos
                   do (setq form-start pos))
          (let ((form-end form-start))
            (cl-loop while (setq form-end (ignore-errors (scan-sexps form-end 1)))
                     do (setq form-start form-end)
                     (setq form-start (scan-sexps form-start -1))
                     (if (or (and (boundp 'sly-mode) (symbol-value 'sly-mode))
                             (and (boundp 'slime-mode) (symbol-value 'slime-mode)))
                         (let ((lisp-semantic-hl--symbols
                                (lisp-semantic-hl-compute-symbols-in-form form-start form-end)))
                           (lisp-semantic-hl-fontify-single-form form-start form-end))
                       (lisp-semantic-hl-fontify-single-form form-start form-end))
                     until (> form-end end)))))))
  nil)

(defun lisp-semantic-hl-keyword-advice (start end &optional _)
  "`:after' advice for `font-lock-fontify-keywords-region'.

Call `lisp-semantic-hl-fontify-keywords-region' with START and END.

LOUDLY is ignored."
  (when (and (symbol-value 'lisp-semantic-hl-mode)
             (member major-mode '(emacs-lisp-mode lisp-mode)))
    (lisp-semantic-hl-fontify-keywords-region start end)))


;;; Mode and Hooks

;;;###autoload
(define-minor-mode lisp-semantic-hl-mode
  "Semantic Syntax Highlighting for Common Lisp & Elisp in Emacs.

Recommend settings:
\(add-hook \\='emacs-lisp-mode-hook \\='lisp-semantic-hl-mode)
\(add-hook \\='lisp-mode-hook \\='lisp-semantic-hl-mode)"
  :group 'lisp-semantic-hl
  (if lisp-semantic-hl-mode
      (advice-add 'font-lock-fontify-keywords-region :after #'lisp-semantic-hl-keyword-advice)
    (advice-remove 'font-lock-fontify-keywords-region #'lisp-semantic-hl-keyword-advice))
  (font-lock-flush))

(defun lisp-semantic-hl-on-lisp-connection ()
  "Call `font-lock-flush'.

Intended to be used with `sly-connected-hook' and `slime-connected-hook'."
  (when (and lisp-semantic-hl-mode
             (or (and (boundp 'sly-mode) (symbol-value 'sly-mode))
                 (and (boundp 'slime-mode) (symbol-value 'slime-mode))))
    (font-lock-flush)))

(when (boundp 'slime-connected-hook)
  (add-hook 'slime-connected-hook #'lisp-semantic-hl-on-lisp-connection))

(when (boundp 'sly-connected-hook)
  (add-hook 'sly-connected-hook #'lisp-semantic-hl-on-lisp-connection))

(provide 'lisp-semantic-hl)

;;; lisp-semantic-hl.el ends here
