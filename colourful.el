;;; colourful.el -*- lexical-binding:t -*-
;; Copyright (C) 2025 The Calendrical System
;; SPDX-License-Identifier: 0BSD

;; Utils

(defcustom colourful-loop-keywords-names
  '("named"
    "initially" "finally" "for" "as" "with"
    "do" "collect" "collecting" "append"
    "appending" "nconc" "nconcing" "into" "count"
    "counting" "sum" "summing" "maximize" "return" "loop-finish"
    "maximizing" "minimize" "minimizing" "doing"
    "thereis" "always" "never" "if" "when"
    "unless" "repeat" "while" "until"
    "=" "and" "it" "else" "end" "from" "upfrom"
    "above" "below" "to" "upto" "downto" "downfrom"
    "in" "on" "then" "across" "being" "each" "the" "hash-key"
    "hash-keys" "of" "using" "hash-value" "hash-values"
    "symbol" "symbols" "present-symbol"
    "present-symbols" "external-symbol"
    "external-symbols" "fixnum" "float" "of-type")
  "Loop keywords from https://lispcookbook.github.io/cl-cookbook/iteration.html#appendix-list-of-loop-keywords"
  :group 'colourful
  :type '(list string))

;; From lisp-mode.el
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
    (defcustom colourful-el-keywords-names
      el-defs
      "Elisp keyword symbol names. From lisp-mode.el"
      :group 'colourful
      :type '(list string))

    (defcustom colourful-cl-keywords-names
      (append cl-defs cl-kws)
      "Common Lisp keyword symbol names. From lisp-mode.el"
      :group 'colourful
      :type '(list string))

    (defcustom colourful-el-errors-names
      el-errs
      "Elisp error symbol names. From lisp-mode.el"
      :group 'colourful
      :type '(list string))

    (defcustom colourful-cl-errors-names
      cl-errs
      "Common Lisp error symbol names. From lisp-mode.el"
      :group 'colourful
      :type '(list string))))

(defun colourful-collect-forms (point)
  "Collect forms inside ansexp from starting.

Point should be putted at (|foo bar)"
  (cl-loop initially (setq end point)
           for end = (ignore-errors (scan-sexps end 1))
           for start = (ignore-errors (scan-sexps end -1))
           while (and start end)
           collect (list start end)))

(defun colourful-count-success-quotes-before (point)
  "Count success (not be quoted) string-quote before POINT."
  (let ((count 0))
    (save-excursion
      (goto-char 0)
      (cl-loop
       (unless (search-forward "\"" (1+ point) t) (cl-return))
       (when (cl-evenp (cl-loop for i downfrom -1
                                for c = (char-before (+ (point) i))
                                until (not (eql c ?\\))
                                count (eql c ?\\)))
         (cl-incf count)))
      count)))

(defun colourful-try-skip-a-quote-backward (point)
  "Try to skip a success (not be quoted) string-quote backward."
  (cl-loop with min = (point-min)
           until (and (eql (char-after point) ?\")
                      (if (eql (char-before point) ?\\)
                          (let ((count (cl-loop for i downfrom -1
                                                for c = (char-after (+ point i))
                                                until (not (eql c ?\\))
                                                count (eql c ?\\))))
                            (cl-evenp count))
                        t))
           do (cl-decf point)
           while (> point min))
  point)

(defun colourful-try-skip-a-quote-forward (point)
  "Try to skip a success (not be quoted) string-quote forward."
  (cl-loop with max = (point-max)
           until (and (eql (char-before point) ?\")
                      (if (eql (char-before (1- point)) ?\\)
                          (let ((count (cl-loop for i downfrom -2
                                                for c = (char-after (+ point i))
                                                until (not (eql c ?\\))
                                                count (eql c ?\\))))
                            (cl-evenp count))
                        t))
           do (cl-incf point)
           while (< point max))
  point)

;; Fontifying

(defun colourful-apply-highlight (start end attr)
  "Unique API for applying highlight."
  (font-lock-append-text-property start end 'face attr))


;; Fontify Symbols Functions

(defun colourful-fontify-symbol-elisp (start end)
  "Fontify ELisp Symbol"
  (let* ((str (buffer-substring-no-properties start end))
         (sym (intern-soft str)))
    (let ((face (cond ((or (eq sym t)
                           (string-equal str "nil"))
                       'font-lock-keyword-face)
                      ((null sym) nil)
                      ((or (special-form-p sym)
                           (cl-member sym colourful-el-keywords-names :test #'string=))
                       'font-lock-keyword-face)
                      ((cl-member sym colourful-el-errors-names :test #'string=)
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
      (when face (colourful-apply-highlight start end face)))))

(defvar colourful-symbols nil
  "Lexical bounded varable for Common Lisp symbol informations to be fontified.")

(defun colourful-read-string (str)
  "Bulk read all symbols inside a string."
  (with-temp-buffer
    (insert str)
    (let ((end-place (point))
          (last-place -1)
          result)
      (goto-char 0)
      (while (and (< last-place end-place)
                  (/= (point) last-place))
        (setq last-place (point))
        (skip-chars-forward " \t\n\r\f\v(#`',@")
        (let ((symbols (ignore-errors
                         (mapcar #'bare-symbol
                                 (flatten-tree
                                  (read-positioning-symbols (current-buffer)))))))
          (when symbols (setq result (nconc result symbols)))))
      (delete-dups result))))

(defcustom colourful-compute-symbols-timeout 1.5
  "Timeout for collecting symbol informations."
  :type 'number
  :group 'colourful)

;; In Common Lisp, we can't query the symbol information by the time
;; we're parsing it, as the sly/slime-eval function will take
;; significant time. So we need to prepare the information of all
;; symbols inside the region in a single query, before we start
;; parsing the region.
(defun colourful-compute-symbols-in-form (start end)
  (let ((lisp-eval (cond ((and (fboundp 'sly-connected-p)
                               (funcall 'sly-connected-p))
                          'sly-eval)
                         ((and (fboundp 'slime-connected-p)
                               (funcall 'slime-connected-p))
                          'slime-eval)))
        (buffer-pak (when-let (pak (funcall (if (fboundp 'sly-current-package)
                                                'sly-current-package
                                              'slime-current-package)))
                      (upcase (string-trim pak "[#:\"]" "[#:\"]"))))
        lst)
    (when lisp-eval
      (let ((obarray (obarray-make)))
        (dolist (obj (colourful-read-string
                      (buffer-substring-no-properties start end)))
          (unless (or (cl-member (symbol-name obj) colourful-cl-keywords-names :test #'string=)
                      (cl-member (symbol-name obj) colourful-cl-errors-names :test #'string=))
            (push (split-string (symbol-name obj) ":") lst))))
      (with-timeout (colourful-compute-symbols-timeout)
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

(defun colourful-fontify-symbol-cl (start end)
  "Fontify Common Lisp Symbol"
  (when (or (and (fboundp 'sly-connected-p)
                 (funcall 'sly-connected-p))
            (and (fboundp 'slime-connected-p)
                 (funcall 'slime-connected-p)))
    (let* ((str (buffer-substring-no-properties start end))
           (split (split-string str ":"))
           (face (cond ((member str colourful-cl-keywords-names)
                        'font-lock-keyword-face)
                       ((member str colourful-cl-errors-names)
                        'font-lock-warning-face)
                       (t (cdr (assoc split colourful-symbols))))))
      (if (and (> (length split) 1)
               (> (length (car split)) 0))
          (let ((sep (+ start (+ (length (car split))
                                 (1- (length split))))))
            (colourful-apply-highlight start sep 'font-lock-type-face)
            (when face (colourful-apply-highlight sep end face)))
        (when face (colourful-apply-highlight start end face))))))


;; Fontify Symbol - List - Symbol recursion

(defun colourful-fontify-symbol (start end)
  "Fontify a symbol, without prefix."
  (if (eq major-mode 'emacs-lisp-mode)
      (colourful-fontify-symbol-elisp start end)
    (when (or (and (fboundp 'sly-connected-p)
                   (funcall 'sly-connected-p))
              (and (fboundp 'slime-connected-p)
                   (funcall 'slime-connected-p)))
      (colourful-fontify-symbol-cl start end))))

(defun colourful-fontify-single-form (start end)
  "Fontify a single form, can be a symbol or a list, with prefix characters.

This function is used to separate prefix & form, colouring prefix
characters, sending rest of the form to fontify-list or
fontify-symbol."
  (goto-char start)
  (let* ((prefix-len (skip-chars-forward "#'@,`"))
         (point      (point))
         (syntax     (syntax-class (syntax-after point))))
    (when (cl-plusp prefix-len)
      (colourful-apply-highlight start point 'font-lock-negation-char-face))
    (pcase syntax
      ((or 2 3) (colourful-fontify-symbol point end))
      (4        (colourful-fontify-list   point end)))))

(defun colourful-fontify-list (start &optional end)
  "Parse items inside the list, sends them to corresponding fontify
functions."
  (setq start (scan-lists start 1 -1))
  ;; Collect sub forms inside the list
  (when-let (forms (colourful-collect-forms start))
    (let ((1st (apply #'buffer-substring-no-properties (car forms))))
      ;; We can add conditions here, to apply custom fontify
      ;; function for specific clause
      (cond ((cl-member 1st '("declare" "proclaim" "declaim") :test #'string-equal)
             (colourful-fontify-declaration-list forms))
            ((cl-member 1st '("loop" "cl-loop") :test #'string-equal)
             (colourful-fontify-loop forms))
            ((cl-member 1st '("let" "let*" "when-let" "when-let*" "if-let" "if-let*"
                              "prog" "prog*"
                              "dolist" "cl-dolist" "dotimes" "cl-dotimes" "seq-doseq"
                              "do-symbols" "do-all-symbols" "cl-do-symbols" "cl-do-all-symbols"
                              "with-slots" "with-accessors")
                        :test #'string-equal)
             (colourful-fontify-let forms))
            ((cl-member 1st '("lambda"
                              "multiple-value-bind" "cl-multiple-value-bind"
                              "destructuring-bind" "cl-destructuring-bind"
                              "with-gensyms" "with-unique-names")
                        :test #'string-equal)
             (colourful-fontify-ordinary-lambda-list-at-1 forms))
            ((cl-member 1st '("defun" "defmacro" "defsubst" "defalias")
                        :test #'string-equal)
             (colourful-fontify-ordinary-lambda-list-at-2 forms))
            (t (dolist (l forms) (apply 'colourful-fontify-single-form l)))))))


;; Special fontify forms

;; loop
(defun colourful-fontify-loop (lst)
  "Highlight loop keywords"
  (apply #'colourful-fontify-symbol (pop lst))
  (dolist (form lst)
    (let* ((start (car form))
           (end (cadr form))
           (str (buffer-substring-no-properties start end)))
      (if (cl-member str colourful-loop-keywords-names :test #'string-equal)
          (colourful-apply-highlight start end 'font-lock-builtin-face)
        (colourful-fontify-single-form start end)))))

;; declare, declaim, proclaim
(defun colourful-fontify-declaration-list (lst)
  "Highlight declarations (declare, declaim, proclaim)"
  (let ((1st (pop lst)))
    (colourful-apply-highlight (car 1st) (cadr 1st) 'font-lock-preprocessor-face))
  (dolist (form lst)
    (let* ((start (car form)))
      (goto-char start)
      (when (cl-plusp (skip-chars-forward "#'@,`"))
        (colourful-apply-highlight start (point) 'font-lock-preprocessor-face))
      (when (= (char-after) ?\()
        (when-let (sub (scan-lists (point) 1 -1))
          (let ((children (colourful-collect-forms sub)))
            (when children (colourful-fontify-declaration-list children))))))))

;; let, ...let , do..., with-slots
(defun colourful-fontify-let (lst)
  "Highlight `let' form, or something behave like `let'."
  (let (1st)
    (cl-destructuring-bind (start end) (pop lst)
      (colourful-fontify-single-form start end)
      (setq 1st (string-trim-left
                 (buffer-substring-no-properties start end)
                 "#'@,`")))
    (when lst
      (cl-destructuring-bind (start end) (pop lst)
        (goto-char start)
        (when (cl-plusp (skip-chars-forward "#'@,`"))
          (colourful-apply-highlight start (point) 'font-lock-warning-face))
        (if (= (char-after) ?\()
            (when-let (sub (scan-lists (point) 1 -1))
              (when-let (children (colourful-collect-forms sub))
                (if (or (cl-member 1st '("dolist" "cl-dolist" "dotimes" "cl-dotimes"
                                         "do-symbols" "do-all-symbols")
                                   :test #'string-equal)
                        (and (cl-member 1st '("when-let" "when-let*" "if-let" "if-let*")
                                        :test #'string-equal)
                             (progn (goto-char (caar children))
                                    (skip-chars-forward "#'@,`")
                                    (/= (char-after) ?\( ))))
                    (cl-destructuring-bind (start end) (car children)
                      (colourful-apply-highlight start end 'font-lock-variable-name-face)
                      (dolist (c (cdr children))
                        (apply 'colourful-fontify-single-form c)))
                  (dolist (child children)
                    (cl-destructuring-bind (start end) child
                      (goto-char start)
                      (when (cl-plusp (skip-chars-forward "#'@,`"))
                        (colourful-apply-highlight start (point) 'font-lock-warning-face))
                      (if (= (char-after) ?\()
                          (when-let (sub (scan-lists (point) 1 -1))
                            (when-let (children (colourful-collect-forms sub))
                              (cl-destructuring-bind (start end)
                                  (car children)
                                (colourful-apply-highlight start end 'font-lock-variable-name-face))
                              (dolist (c (cdr children))
                                (apply 'colourful-fontify-single-form c))))
                        (colourful-apply-highlight (point) end 'font-lock-variable-name-face)))))))
          (colourful-fontify-symbol (point) end))))
    (dolist (l lst)
      (apply 'colourful-fontify-single-form l))))

;; function arglist
(defun colourful-fontify-ordinary-lambda-list-at-1 (lst)
  "Highlight ordinary lambda list which is at the second of
expresion (typically lambda expression)."
  (cl-destructuring-bind (start end) (pop lst)
    (colourful-fontify-single-form start end))
  (when lst
    (cl-destructuring-bind (start end) (pop lst)
      (goto-char start)
      (when (cl-plusp (skip-chars-forward "#'@,`"))
        (colourful-apply-highlight start (point) 'font-lock-warning-face))
      (if (= (char-after) ?\()
          (when-let (sub (scan-lists (point) 1 -1))
            (let ((children (colourful-collect-forms sub)))
              (dolist (child children)
                (cl-destructuring-bind (start end) child
                  (goto-char start)
                  (when (cl-plusp (skip-chars-forward "#'@,`"))
                    (colourful-apply-highlight start (point) 'font-lock-warning-face))
                  (pcase (char-after)
                    (?\( (colourful-fontify-list (point)))
                    (?\& (colourful-apply-highlight start end 'font-lock-type-face))
                    (_ (colourful-apply-highlight start end 'font-lock-variable-name-face)))))))
        (colourful-fontify-symbol (point) end))))
  (dolist (l lst)
    (apply 'colourful-fontify-single-form l)))

(defun colourful-fontify-ordinary-lambda-list-at-2 (lst)
  "Highlight ordinary lambda list which is at the third of
expresion (typically defun)."
  (cl-destructuring-bind (start end) (pop lst)
    (colourful-fontify-single-form start end))
  (when lst (colourful-fontify-ordinary-lambda-list-at-1 lst)))


;; font-lock-mode integration

(defun colourful-fontify-keywords-region (start end)
  "Colouring forms covered by START and END."
  (save-excursion
    (when (cl-oddp (colourful-count-success-quotes-before start))
      (setq start (colourful-try-skip-a-quote-backward start)))
    (when (cl-oddp (colourful-count-success-quotes-before end))
      (setq end (colourful-try-skip-a-quote-forward end)))
    (let ((form-start end))
      (cl-loop for pos = (ignore-errors (scan-lists form-start -1 1))
               while pos
               do (setq form-start pos)
               until (< form-start start))
      (if (< form-start start)
          ;; If the region is inside one form 
          (let ((form-end (scan-sexps form-start 1)))
            (if (or sly-mode slime-mode)
                (let ((colourful-symbols
                       (colourful-compute-symbols-in-form form-start form-end)))
                  (colourful-fontify-single-form form-start form-end))
              (colourful-fontify-single-form form-start form-end)))
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
                     (let ((colourful-symbols
                            (colourful-compute-symbols-in-form form-start form-end)))
                       (colourful-fontify-single-form form-start form-end))
                     until (> form-end end)))))))
  nil)

(defun colourful-keyword-advice (origin start end &optional loudly)
  (if colourful-mode
      (progn
        (when (fboundp 'rainbow-delimiters--propertize)
          (save-excursion
            (goto-char start)
            (rainbow-delimiters--propertize end)))
        (colourful-fontify-keywords-region start end))
    (funcall origin start end loudly)))


;; Minor mode definition

(define-minor-mode colourful-mode
  "Colourful highlight for Lisp.

Recommend bindings:
\(add-hook 'emacs-lisp-mode-hook 'colourful-mode)
\(add-hook 'lisp-mode-hook 'colourful-mode)"
  :group 'colourful
  (if colourful-mode
      (advice-add 'font-lock-fontify-keywords-region :around 'colourful-keyword-advice)
    (advice-remove 'font-lock-fontify-keywords-region 'colourful-keyword-advice))
  (font-lock-flush))

(defun colourful-font-lock-flush-when-lisp-connected ()
  (when colourful-mode
    (font-lock-flush)))

(provide 'colourful)

;;; colourful.el ends here
