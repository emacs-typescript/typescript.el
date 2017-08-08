;;; typescript-mode.el --- Major mode for editing TypeScript

;; -----------------------------------------------------------------------------------
;;     TypeScript support for Emacs
;;     Copyright (c) 2008 Free Software Foundation
;;     Portions Copyright (C) Microsoft Open Technologies, Inc. All rights reserved.
;;
;;     This program is free software: you can redistribute it and/or modify
;;     it under the terms of the GNU General Public License as published by
;;     the Free Software Foundation, either version 3 of the License, or
;;     (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public License
;;     along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; -------------------------------------------------------------------------------------------

;; URL: http://github.com/ananthakumaran/typescript.el
;; Version: 0.1
;; Keywords: typescript languages
;; Package-Requires: ()

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is based on Karl Landstrom's barebones javascript-mode.  This
;; is much more robust and works with cc-mode's comment filling
;; (mostly).
;;
;; The main features of this TypeScript mode are syntactic
;; highlighting (enabled with `font-lock-mode' or
;; `global-font-lock-mode'), automatic indentation and filling of
;; comments and C preprocessor fontification.
;;
;; General Remarks:
;;
;; XXX: This mode assumes that block comments are not nested inside block
;; XXX: comments
;;
;; Exported names start with "typescript-"; private names start with
;; "typescript--".

;;; Code:
(require 'js)
(require 'compile)

(eval-when-compile
  (require 'cl))




(defun typescript--regexp-opt-symbol (list)
  "Like `regexp-opt', but surround the result with `\\\\_<' and `\\\\_>'."
  (concat "\\_<" (regexp-opt list t) "\\_>"))

(defconst typescript--keyword-re
  (js--regexp-opt-symbol
   '("abstract" "any" "as" "async" "await" "boolean" "break" "case" "catch" "class" "const"
     "constructor" "continue" "declare" "default" "delete" "do" "else"
     "enum" "export" "extends" "extern" "false" "finally" "for"
     "function" "from" "get" "goto" "if" "implements" "import" "in" "instanceof"
     "interface" "keyof" "let" "module" "namespace" "new" "null" "number" "object" "of"
     "private" "protected" "public" "readonly" "return" "set" "static" "string"
     "super" "switch"  "this" "throw" "true"
     "try" "type" "typeof" "var" "void"
     "while" ))
  "Regexp matching any typescript keyword.")

(defconst typescript--basic-type-re
  (js--regexp-opt-symbol
   '("bool" "boolean" "string" "number" "any" "void"))
  "Regular expression matching any predefined type in typescript.")


(defconst typescript--font-lock-keywords-2
  (append js--font-lock-keywords-1
          (list (list typescript--keyword-re 1 font-lock-keyword-face)
                (list "\\_<for\\_>"
                      "\\s-+\\(each\\)\\_>" nil nil
                      (list 1 'font-lock-keyword-face))
                (cons typescript--basic-type-re font-lock-type-face)
                (cons js--constant-re font-lock-constant-face)))
  "Level two font lock keywords for `js-mode'.")


;;; User Customization

(defgroup typescript nil
  "Customization variables for typescript mode."
  :tag "typescript"
  :group 'languages)

(defcustom typescript-indent-level 4
  "Number of spaces for each indentation step in `typescript-mode'."
  :type 'integer
  :group 'typescript)

(defcustom typescript-expr-indent-offset 0
  "Number of additional spaces used for indentation of continued expressions.
The value must be no less than minus `typescript-indent-level'."
  :type 'integer
  :group 'typescript)

(defcustom typescript-auto-indent-flag t
  "Whether to automatically indent when typing punctuation characters.
If non-nil, the characters {}();,: also indent the current line
in typescript mode."
  :type 'boolean
  :group 'typescript)

(defcustom typescript-comment-lineup-func #'c-lineup-C-comments
  "Lineup function for `cc-mode-style', for C comments in `typescript-mode'."
  :type 'function
  :group 'typescript)

(defconst typescript--font-lock-keywords-3
  `(
    ;; This goes before keywords-2 so it gets used preferentially
    ;; instead of the keywords in keywords-2. Don't use override
    ;; because that will override syntactic fontification too, which
    ;; will fontify commented-out directives as if they weren't
    ;; commented out.
    ,@cpp-font-lock-keywords ; from font-lock.el

    ,@typescript--font-lock-keywords-2

    ("\\.\\(prototype\\)\\_>"
     (1 font-lock-constant-face))

    ;; Highlights class being declared, in parts
    (js--class-decl-matcher
     ,(concat "\\(" js--name-re "\\)\\(?:\\.\\|.*$\\)")
     (goto-char (match-beginning 1))
     nil
     (1 font-lock-type-face))

    ;; Highlights parent class, in parts, if available
    (js--class-decl-matcher
     ,(concat "\\(" js--name-re "\\)\\(?:\\.\\|.*$\\)")
     (if (match-beginning 2)
         (progn
           (setq js--tmp-location (match-end 2))
           (goto-char js--tmp-location)
           (insert "=")
           (goto-char (match-beginning 2)))
       (setq js--tmp-location nil)
       (goto-char (point-at-eol)))
     (when js--tmp-location
       (save-excursion
         (goto-char js--tmp-location)
         (delete-char 1)))
     (1 font-lock-type-face))

    ;; Highlights parent class
    (js--class-decl-matcher
     (2 font-lock-type-face nil t))

    ;; Dojo needs its own matcher to override the string highlighting
    (,(js--make-framework-matcher
       'dojo
       "^\\s-*dojo\\.declare\\s-*(\""
       "\\(" js--dotted-name-re "\\)"
       "\\(?:\"\\s-*,\\s-*\\(" js--dotted-name-re "\\)\\)?")
     (1 font-lock-type-face t)
     (2 font-lock-type-face nil t))

    ;; Match Dojo base classes. Of course Mojo has to be different
    ;; from everything else under the sun...
    (,(js--make-framework-matcher
       'dojo
       "^\\s-*dojo\\.declare\\s-*(\""
       "\\(" js--dotted-name-re "\\)\"\\s-*,\\s-*\\[")
     ,(concat "[[,]\\s-*\\(" js--dotted-name-re "\\)\\s-*"
              "\\(?:\\].*$\\)?")
     (backward-char)
     (end-of-line)
     (1 font-lock-type-face))

    ;; continued Dojo base-class list
    (,(js--make-framework-matcher
       'dojo
       "^\\s-*" js--dotted-name-re "\\s-*[],]")
     ,(concat "\\(" js--dotted-name-re "\\)"
              "\\s-*\\(?:\\].*$\\)?")
     (if (save-excursion (backward-char)
                         (js--inside-dojo-class-list-p))
         (forward-symbol -1)
       (end-of-line))
     (end-of-line)
     (1 font-lock-type-face))

    ;; variable declarations
    ,(list
      (concat "\\_<\\(const\\|var\\|let\\)\\_>\\|" js--basic-type-re)
      (list #'js--variable-decl-matcher nil nil nil))

    ;; class instantiation
    ,(list
      (concat "\\_<new\\_>\\s-+\\(" js--dotted-name-re "\\)")
      (list 1 'font-lock-type-face))

    ;; instanceof
    ,(list
      (concat "\\_<instanceof\\_>\\s-+\\(" js--dotted-name-re "\\)")
      (list 1 'font-lock-type-face))

    ;; formal parameters
    ,(list
      (concat
       "\\_<function\\_>\\(\\s-+" js--name-re "\\)?\\s-*(\\s-*"
       js--name-start-re)
      (list (concat "\\(" js--name-re "\\)\\(\\s-*).*\\)?")
            '(backward-char)
            '(end-of-line)
            '(1 font-lock-variable-name-face)))

    ;; continued formal parameter list
    ,(list
      (concat
       "^\\s-*" js--name-re "\\s-*[,)]")
      (list js--name-re
            '(if (save-excursion (backward-char)
                                 (js--inside-param-list-p))
                 (forward-symbol -1)
               (end-of-line))
            '(end-of-line)
            '(0 font-lock-variable-name-face))))
  "Level three font lock for `js-mode'.")


(defconst typescript--font-lock-keywords
  '(typescript--font-lock-keywords-3 js--font-lock-keywords-1
                                   typescript--font-lock-keywords-2
                                   typescript--font-lock-keywords-3)
  "Font lock keywords for `js-mode'.  See `font-lock-keywords'.")

;;; Indentation

(defun typescript--beginning-of-macro (&optional lim)
  (let ((here (point)))
    (save-restriction
      (if lim (narrow-to-region lim (point-max)))
      (beginning-of-line)
      (while (eq (char-before (1- (point))) ?\\)
        (forward-line -1))
      (back-to-indentation)
      (if (and (<= (point) here)
               (looking-at js--opt-cpp-start))
          t
        (goto-char here)
        nil))))


(defun typescript--backward-syntactic-ws (&optional lim)
  "Simple implementation of `c-backward-syntactic-ws' for `typescript-mode'."
  (save-restriction
    (when lim (narrow-to-region lim (point-max)))

    (let ((in-macro (save-excursion (typescript--beginning-of-macro)))
          (pos (point)))

      (while (progn (unless in-macro (typescript--beginning-of-macro))
                    (forward-comment most-negative-fixnum)
                    (/= (point)
                        (prog1
                            pos
                          (setq pos (point)))))))))


(defun typescript--re-search-forward-inner (regexp &optional bound count)
  "Helper function for `typescript--re-search-forward'."
  (let ((parse)
        str-terminator
        (orig-macro-end (save-excursion
                          (when (typescript--beginning-of-macro)
                            (c-end-of-macro)
                            (point)))))
    (while (> count 0)
      (re-search-forward regexp bound)
      (setq parse (syntax-ppss))
      (cond ((setq str-terminator (nth 3 parse))
             (when (eq str-terminator t)
               (setq str-terminator ?/))
             (re-search-forward
              (concat "\\([^\\]\\|^\\)" (string str-terminator))
              (save-excursion (end-of-line) (point)) t))
            ((nth 7 parse)
             (forward-line))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?\/) (eq (char-after) ?\*)))
             (re-search-forward "\\*/"))
            ((and (not (and orig-macro-end
                            (<= (point) orig-macro-end)))
                  (typescript--beginning-of-macro))
             (c-end-of-macro))
            (t
             (setq count (1- count))))))
  (point))


(defun typescript--re-search-forward (regexp &optional bound noerror count)
  "Search forward, ignoring strings, cpp macros, and comments.
This function invokes `re-search-forward', but treats the buffer
as if strings, cpp macros, and comments have been removed.

If invoked while inside a macro, it treats the contents of the
macro as normal text."
  (let ((saved-point (point))
        (search-expr
         (cond ((null count)
                '(typescript--re-search-forward-inner regexp bound 1))
               ((< count 0)
                '(typescript--re-search-backward-inner regexp bound (- count)))
               ((> count 0)
                '(typescript--re-search-forward-inner regexp bound count)))))
    (condition-case err
        (eval search-expr)
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (error (error-message-string err)))))))


(defun typescript--re-search-backward-inner (regexp &optional bound count)
  "Auxiliary function for `typescript--re-search-backward'."
  (let ((parse)
        str-terminator
        (orig-macro-start
         (save-excursion
           (and (typescript--beginning-of-macro)
                (point)))))
    (while (> count 0)
      (re-search-backward regexp bound)
      (when (and (> (point) (point-min))
                 (save-excursion (backward-char) (looking-at "/[/*]")))
        (forward-char))
      (setq parse (syntax-ppss))
      (cond ((setq str-terminator (nth 3 parse))
             (when (eq str-terminator t)
               (setq str-terminator ?/))
             (re-search-backward
              (concat "\\([^\\]\\|^\\)" (string str-terminator))
              (save-excursion (beginning-of-line) (point)) t))
            ((nth 7 parse)
             (goto-char (nth 8 parse)))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?/) (eq (char-after) ?*)))
             (re-search-backward "/\\*"))
            ((and (not (and orig-macro-start
                            (>= (point) orig-macro-start)))
                  (typescript--beginning-of-macro)))
            (t
             (setq count (1- count))))))
  (point))


(defun typescript--re-search-backward (regexp &optional bound noerror count)
  "Search backward, ignoring strings, preprocessor macros, and comments.

This function invokes `re-search-backward' but treats the buffer
as if strings, preprocessor macros, and comments have been
removed.

If invoked while inside a macro, treat the macro as normal text."
  (let ((saved-point (point))
        (search-expr
         (cond ((null count)
                '(typescript--re-search-backward-inner regexp bound 1))
               ((< count 0)
                '(typescript--re-search-forward-inner regexp bound (- count)))
               ((> count 0)
                '(typescript--re-search-backward-inner regexp bound count)))))
    (condition-case err
        (eval search-expr)
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (error (error-message-string err)))))))


(defconst typescript--possibly-braceless-keyword-re
  (typescript--regexp-opt-symbol
   '("catch" "do" "else" "finally" "for" "if" "try" "while" "with"
     "each"))
  "Regexp matching keywords optionally followed by an opening brace.")

(defconst typescript--indent-keyword-re
  (typescript--regexp-opt-symbol '("in" "instanceof"))
  "Regexp matching keywords that affect indentation of continued expressions.")

(defconst typescript--indent-operator-re
  (concat "[-+*/%<>=&^|?:.]\\([^-+*/]\\|$\\)\\|" typescript--indent-keyword-re)
  "Regexp matching operators that affect indentation of continued expressions.")


(defun typescript--looking-at-operator-p ()
  "Return non-nil if point is on a typescript operator, other than a comma."
  (save-match-data
    (and (looking-at typescript--indent-operator-re)
         (or (not (looking-at ":"))
             (save-excursion
               (and (typescript--re-search-backward "[?:{]\\|\\_<case\\_>" nil t)
                    (looking-at "?"))))
         ;; Do not identify forward slashes appearing in a "list" as
         ;; an operator. The lists are: arrays, or lists of
         ;; arguments. In this context, they must be part of regular
         ;; expressions, and not math operators.
         (not (and (looking-at "/")
                   (save-excursion
                     (typescript--backward-syntactic-ws)
                     (memq (char-before) '(?, ?\[ ?\()))))
         ;; Do not identify methods, or fields, that are named "in" or
         ;; "instanceof" as being operator keywords.
         (not (and
               (looking-at typescript--indent-keyword-re)
               (save-excursion
                 (typescript--backward-syntactic-ws)
                 (memq (char-before) '(?, ?{ ?} ?\;)))))
         (not (and
               (looking-at "*")
               ;; Generator method (possibly using computed property).
               (looking-at (concat "\\* *\\(?:\\[\\|" js--name-re
                                   " *(\\)"))
               (save-excursion
                 (typescript--backward-syntactic-ws)
                 ;; We might misindent some expressions that would
                 ;; return NaN anyway.  Shouldn't be a problem.
                 (memq (char-before) '(?, ?} ?{ ?\;)))))))
)


(defun typescript--continued-expression-p ()
  "Return non-nil if the current line continues an expression."
  (save-excursion
    (back-to-indentation)
    (and
     ;; Don't identify the spread syntax or rest operator as a
     ;; "continuation".
     (not (looking-at "\\.\\.\\."))
     (or (typescript--looking-at-operator-p)
         (and (typescript--re-search-backward "\n" nil t)
              (progn
                (skip-chars-backward " \t")
                (or (bobp) (backward-char))
                (and (> (point) (point-min))
                     (save-excursion (backward-char) (not (looking-at "[/*]/")))
                     (typescript--looking-at-operator-p)
                     (and (progn (backward-char)
                                 (not (looking-at "++\\|--\\|/[/*]")))))))))))


(defun typescript--end-of-do-while-loop-p ()
  "Return non-nil if point is on the \"while\" of a do-while statement.
Otherwise, return nil.  A braceless do-while statement spanning
several lines requires that the start of the loop is indented to
the same column as the current line."
  (interactive)
  (save-excursion
    (save-match-data
      (when (looking-at "\\s-*\\_<while\\_>")
	(if (save-excursion
	      (skip-chars-backward "[ \t\n]*}")
	      (looking-at "[ \t\n]*}"))
	    (save-excursion
	      (backward-list) (forward-symbol -1) (looking-at "\\_<do\\_>"))
	  (typescript--re-search-backward "\\_<do\\_>" (point-at-bol) t)
	  (or (looking-at "\\_<do\\_>")
	      (let ((saved-indent (current-indentation)))
		(while (and (typescript--re-search-backward "^\\s-*\\_<" nil t)
			    (/= (current-indentation) saved-indent)))
		(and (looking-at "\\s-*\\_<do\\_>")
		     (not (typescript--re-search-forward
			   "\\_<while\\_>" (point-at-eol) t))
		     (= (current-indentation) saved-indent)))))))))


(defun typescript--ctrl-statement-indentation ()
  "Helper function for `typescript--proper-indentation'.
Return the proper indentation of the current line if it starts
the body of a control statement without braces; otherwise, return
nil."
  (save-excursion
    (back-to-indentation)
    (when (save-excursion
            (and (not (eq (point-at-bol) (point-min)))
                 (not (looking-at "[{]"))
                 (progn
                   (typescript--re-search-backward "[[:graph:]]" nil t)
                   (or (eobp) (forward-char))
                   (when (= (char-before) ?\)) (backward-list))
                   (skip-syntax-backward " ")
                   (skip-syntax-backward "w_")
                   (looking-at typescript--possibly-braceless-keyword-re))
                 (not (typescript--end-of-do-while-loop-p))))
      (save-excursion
        (goto-char (match-beginning 0))
        (+ (current-indentation) typescript-indent-level)))))

(defun typescript--get-c-offset (symbol anchor)
  (let ((c-offsets-alist
         (list (cons 'c typescript-comment-lineup-func))))
    (c-get-syntactic-indentation (list (cons symbol anchor)))))

(defun typescript--backward-over-generic-parameter-list ()
  "Search backward for the start of a generic's parameter list and move to it.

This is a utility function for
`typescript--backward-to-parameter-list'.

This function must be called with the point placed on the final >
of the generic's parameter list.  It will scan backwards to find
the start.  If successful, it will move the point to the start of
the list.  If not, it does not move the point.

Returns nil on failure, or the position to which the point was
moved on success."
  (when (eq (char-after) ?>)
    (let ((depth 1))
      (loop named search-loop
            while (> depth 0)
            do (progn
                 (unless (re-search-backward "[<>]" nil t)
                   (cl-return-from search-loop nil))
                 (cond
                  ((looking-at ">")
                   (unless (eq (char-before) ?=)
                     (setq depth (1+ depth))))
                  ((looking-at "<") (setq depth (1- depth)))))
            finally return (point)))))

(defun typescript--backward-to-parameter-list ()
  "Search backward for the end of a parameter list and move to it.

This is a utility function for `typescript--proper-indentation'.

This function must be called with the point placed before an
opening curly brace.  It will try to skip over the type
annotation that would mark the return value of a function and
move to the end of the parameter list.  If it is unsuccessful, it
does not move the point. \"Unsuccessful\" here also means that
the position at which we started did not in fact mark the
beginning of a function. The curly brace belonged to some other
syntactic construct than a function.

Returns nil on failure, or the position to which the point was
moved on success."
  (let ((location
         (or
          ;; This handles the case of a function with return type annotation.
          (save-excursion
            (loop named search-loop
                  do (progn
                       (if (eq (char-before) ?>)
                           (if (looking-back "=>" (- (point) 2))
                               ;; Move back over the arrow of an arrow function.
                               (backward-char 2)
                             ;; Otherwise, we are looking at the end of the parameters
                             ;; list of a generic. We need to move back over the list.
                             (backward-char)
                             (typescript--backward-over-generic-parameter-list))
                         ;; General case: we just move back over the current sexp.
                         (condition-case nil
                             (backward-sexp)
                           (scan-error nil)))
                       (typescript--backward-syntactic-ws)
                       (let ((before (char-before)))
                         ;; Check whether we are at "):".
                         (when (and (eq before ?\:)
                                    (progn
                                      (backward-char)
                                      (skip-syntax-backward " ")
                                      (eq (char-before) ?\))))
                           ;; Success! This the end of the parameter list.
                           (cl-return-from search-loop (point)))
                         ;; All the following cases are constructs that are allowed to
                         ;; appear between the opening brace of a function and the
                         ;; end of a parameter list.
                         (unless
                             (or
                              ;; End of a generic.
                              (eq before ?>)
                              ;; Union of types
                              (eq before ?|)
                              ;; Dotted names
                              (eq before ?.)
                              ;; Typeguard (eg. foo is SomeClass)
                              (looking-back "is" (- (point) 2))
                              ;; This is also dealing with dotted names. This may come
                              ;; into play if a jump back moves over an entire dotted
                              ;; name at once.
                              ;;
                              ;; The earlier test for dotted names comes into play if the
                              ;; logic moves over one part of a dotted name at a time (which
                              ;; is what `backward-sexp` normally does).
                              (looking-back js--dotted-name-re nil)
                             )
                           ;; We did not encounter a valid construct, so
                           ;; the search is unsuccessful.
                           (cl-return-from search-loop nil))))))
          ;; This handles the case of a function without return type annotation.
          (progn
            (typescript--backward-syntactic-ws)
            (when (eq (char-before) ?\))
              (point))))))
    (when location
      (goto-char location))))

(defun typescript--proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (cond ((nth 4 parse-status)
           (typescript--get-c-offset 'c (nth 8 parse-status)))
          ((nth 8 parse-status) 0) ; inside string
          ((typescript--ctrl-statement-indentation))
          ((eq (char-after) ?#) 0)
          ((save-excursion (typescript--beginning-of-macro)) 4)
          ((nth 1 parse-status)
           (let ((same-indent-p (looking-at
                                 "[]})]\\|\\_<case\\_>\\|\\_<default\\_>"))
                 (continued-expr-p (typescript--continued-expression-p)))
             (goto-char (nth 1 parse-status))
             (if (looking-at "[({[]\\s-*\\(/[/*]\\|$\\)")
                 (progn
                   (skip-syntax-backward " ")
                   (when (or (typescript--backward-to-parameter-list)
                             (eq (char-before) ?\)))
                     (backward-list))
                   ;; If the parameter list is preceded by (, take the
                   ;; start of the parameter list as our reference.
                   ;; This allows handling functions in parameter
                   ;; lists. Otherwise, we want to go back to the
                   ;; start of function declaration.
                   (back-to-indentation)
                   (cond (same-indent-p
                          (current-column))
                         (continued-expr-p
                          (+ (current-column) (* 2 typescript-indent-level)
                             typescript-expr-indent-offset))
                         (t
                          (+ (current-column) typescript-indent-level))))
               (unless same-indent-p
                 (forward-char)
                 (skip-chars-forward " \t"))
               (current-column))))

          ((typescript--continued-expression-p)
           (+ typescript-indent-level typescript-expr-indent-offset))
          (t 0))))

(defun typescript-indent-line ()
  "Indent the current line as typescript."
  (interactive)
  (save-restriction
    (widen)
    (let* ((parse-status
            (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation))))
      (indent-line-to (typescript--proper-indentation parse-status))
      (when (> offset 0) (move-to-column (+ offset (current-indentation)))))))



;;; compilation-mode support

;; handle compiler-errors like the following when doing M-x compile<ret>tsc<ret>
;; greeter.ts(24,9): error TS2362: The left-hand side of an arithmetic operation must be of type 'any', 'number' or an enum type.
;; greeter.ts(30,12): error TS2339: Property 'indexOf' does not exist on type 'number'.
(defconst typescript-tsc-error-regexp
  (concat
   "^[[:blank:]]*"
   "\\([^(\r\n)]+\\)(\\([0-9]+\\),\\([0-9]+\\)):[[:blank:]]+"
   "error [[:alnum:]]+: [^\r\n]+$")
  "Regexp to match errors generated by tsc.")

;;
;; Should handle output like:
;; src/modules/authenticator.ts[1, 83]: ' should be "
;; (quotemarks) src/modules/authenticator.ts[2, 26]: ' should be "
;; ERROR: (quotemarks) src/modules/authenticator.ts[2, 26]: ' should be "
;; WARNING: src/modules/authenticator.ts[2, 26]: ' should be "
;;
;; "(quotemarks)" it the rule name. It is produced when using the
;; "verbose" formatter. The "verbose" formatter is identical to the
;; default ("prose") formatter, except for the additional rule name.
;;
;; "ERROR:" and "WARNING:" are the severity. This was added in tslint
;; 5.0. Prior versions have no notion of severity and simply omit this
;; part.
;;
(defconst typescript-tslint-report-regexp
  (concat
   "^[[:blank:]]*"
   ;; severity ("type" in Emacs' parlance)
   "\\(?:\\(?:ERROR\\|\\(WARNING\\)\\):[[:blank:]]+\\)?"
   ;; rule name
   "\\((.*)[[:blank:]]+\\)?"
   ;; filename
   "\\([^(\r\n)]+\\)"
   "\\["
   ;; line
   "\\([[:digit:]]+\\)"
   ", "
   ;; column
   "\\([[:digit:]]+\\)"
   "\\]: "
   ;; message
   ".*$"
   )
  "Regexp to match reports generated by tslint.")

(dolist
    (regexp
     `((typescript-tsc
        ,typescript-tsc-error-regexp
        1 2 3 2)

       (typescript-tslint
        ,typescript-tslint-report-regexp
        3 4 5 (1))))
  (add-to-list 'compilation-error-regexp-alist-alist regexp)
  (add-to-list 'compilation-error-regexp-alist (car regexp)))



;;;###autoload
(define-derived-mode typescript-mode js-mode "TypeScript"
  "Major mode for editing TypeScript."
  :group 'typescript
  (setq-local indent-line-function #'typescript-indent-line)
  (setq-local beginning-of-defun-function #'js-beginning-of-defun)
  (setq-local end-of-defun-function #'js-end-of-defun)
  (setq-local open-paren-in-column-0-is-defun-start nil)
  (setq-local font-lock-defaults (list typescript--font-lock-keywords))
  (setq-local syntax-propertize-function #'js-syntax-propertize)
  ;; (setq-local prettify-symbols-alist js--prettify-symbols-alist)

  (setq-local parse-sexp-ignore-comments t)
  (setq-local parse-sexp-lookup-properties t)

  ;; Comments
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local fill-paragraph-function #'js-c-fill-paragraph)

  ;; Parse cache
  (add-hook 'before-change-functions #'js--flush-caches t t)

  ;; Frameworks
  (js--update-quick-match-re)

  ;; Imenu
  (setq imenu-case-fold-search nil)
  (setq imenu-create-index-function #'js--imenu-create-index)

  ;; for filling, pretend we're cc-mode
  (setq c-comment-prefix-regexp "//+\\|\\**"
        c-paragraph-start "\\(@[[:alpha:]]+\\>\\|$\\)"
        c-paragraph-separate "$"
        c-block-comment-prefix "* "
        c-line-comment-starter "//"
        c-comment-start-regexp "/[*/]\\|\\s!"
        comment-start-skip "\\(//+\\|/\\*+\\)\\s *")

  (setq-local electric-indent-chars
	      (append "{}():;," electric-indent-chars)) ;FIXME: js2-mode adds "[]*".
  (setq-local electric-layout-rules
	      '((?\; . after) (?\{ . after) (?\} . before)))

  (let ((c-buffer-is-cc-mode t))
    ;; FIXME: These are normally set by `c-basic-common-init'.  Should
    ;; we call it instead?  (Bug#6071)
    (make-local-variable 'paragraph-start)
    (make-local-variable 'paragraph-separate)
    (make-local-variable 'paragraph-ignore-fill-prefix)
    (make-local-variable 'adaptive-fill-mode)
    (make-local-variable 'adaptive-fill-regexp)
    (c-setup-paragraph-variables))

  ;; Important to fontify the whole buffer syntactically! If we don't,
  ;; then we might have regular expression literals that aren't marked
  ;; as strings, which will screw up parse-partial-sexp, scan-lists,
  ;; etc. and produce maddening "unbalanced parenthesis" errors.
  ;; When we attempt to find the error and scroll to the portion of
  ;; the buffer containing the problem, JIT-lock will apply the
  ;; correct syntax to the regular expression literal and the problem
  ;; will mysteriously disappear.
  ;; FIXME: We should instead do this fontification lazily by adding
  ;; calls to syntax-propertize wherever it's really needed.
  ;;(syntax-propertize (point-max))
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))

(provide 'typescript-mode)

;;; typescript-mode.el ends here
