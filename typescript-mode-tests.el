
;;; typescript-mode-tests --- This file contains automated tests for typescript-mode.el

;;; Commentary:
;; Run tests using (ert-run-tests-interactively t).

;;; Code:

(require 'ert)
(require 'typescript-mode)
(require 'cl)

(defun typescript-test-get-doc ()
  (buffer-substring-no-properties (point-min) (point-max)))

(defun typescript-test-indent-all ()
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(ert-deftest indentation-reference-document-is-reflowed-correctly ()
  (let ((buffer (find-file "test-files/indentation-reference-document.ts")))
    ;; double ensure mode is active
    (typescript-mode)

    (let ((test-reference (typescript-test-get-doc)))
      (typescript-test-indent-all)
      (should (string-equal test-reference
                            (typescript-test-get-doc)))
      (let ((typescript-indent-switch-clauses nil))
        (typescript-test-indent-all)
        (should (string-equal test-reference
                              (typescript-test-get-doc)))))
    (kill-buffer buffer)))

(ert-deftest switch-case-indent-default ()
  (let ((buffer (find-file "test-files/switch-case-indent-default.ts")))
    (typescript-mode)
    (let ((test-reference (typescript-test-get-doc)))
      (typescript-test-indent-all)
      (should (string-equal test-reference
                            (typescript-test-get-doc))))
    (kill-buffer buffer)))

(ert-deftest switch-case-indent-disabled ()
  (let ((buffer (find-file "test-files/switch-case-indent-disabled.ts"))
        (typescript-indent-switch-clauses nil))
    (typescript-mode)
    (let ((test-reference (typescript-test-get-doc)))
      (typescript-test-indent-all)
      (should (string-equal test-reference
                            (typescript-test-get-doc))))
    (kill-buffer buffer)))

(defun get-all-matched-strings (to-match)
  (let (result)
    (dotimes (x (/ (length (match-data)) 2))
      (setq result (nconc result (list (match-string x to-match)))))
    result))

(ert-deftest typescript-tslint-report-regexp-matches ()
  "typescript-tslint-report-regexp matches a line that does not
have a rule name or a severity."
  (let* ((to-match
          "src/modules/authenticator.ts[1, 83]: ' should be \"")
         (match (string-match typescript-tslint-report-regexp
                              to-match))
         (matches (and match (get-all-matched-strings to-match))))
    (should match)
    (should (not (nth 1 matches)))
    (should (not (nth 2 matches)))
    (should (string-equal (nth 3 matches)
                          "src/modules/authenticator.ts"))
    (should (string-equal (nth 4 matches) "1"))
    (should (string-equal (nth 5 matches) "83"))))

(ert-deftest typescript-tslint-report-regexp-matches-with-name ()
  "typescript-tslint-report-regexp matches a line that has
a rule name, no severity."
  (let* ((to-match
          "(quotemark) src/modules/authenticator.ts[1, 83]: ' should be \"")
         (match (string-match typescript-tslint-report-regexp
                              to-match))
         (matches (and match (get-all-matched-strings to-match))))
    (should match)
    (should (not (nth 1 matches)))
    (should (string-equal (nth 2 matches) "(quotemark) "))
    (should (string-equal (nth 3 matches)
                          "src/modules/authenticator.ts"))
    (should (string-equal (nth 4 matches) "1"))
    (should (string-equal (nth 5 matches) "83"))))

(ert-deftest typescript-tslint-report-regexp-matches-with-error ()
  "typescript-tslint-report-regexp matches a line that has
a severity set to ERROR, no rule name."
  (let* ((to-match
          "ERROR: src/modules/authenticator.ts[1, 83]: ' should be \"")
         (match (string-match typescript-tslint-report-regexp
                              to-match))
         (matches (and match (get-all-matched-strings to-match))))
    (should match)
    (should (not (nth 1 matches)))
    (should (not (nth 2 matches)))
    (should (string-equal (nth 3 matches)
                          "src/modules/authenticator.ts"))
    (should (string-equal (nth 4 matches) "1"))
    (should (string-equal (nth 5 matches) "83"))))

(ert-deftest typescript-tslint-report-regexp-matches-with-warning ()
  "typescript-tslint-report-regexp matches a line that has
a severity set to WARNING, no rule name."
  (let* ((to-match
          "WARNING: src/modules/authenticator.ts[1, 83]: ' should be \"")
         (match (string-match typescript-tslint-report-regexp
                              to-match))
         (matches (and match (get-all-matched-strings to-match))))
    (should match)
    (should (string-equal (nth 1 matches) "WARNING"))
    (should (not (nth 2 matches)))
    (should (string-equal (nth 3 matches)
                          "src/modules/authenticator.ts"))
    (should (string-equal (nth 4 matches) "1"))
    (should (string-equal (nth 5 matches) "83"))))

(ert-deftest typescript--number-literal-re-matches-numbers ()
  "`typescript--number-literal-re' matches numbers."
  (dolist (to-match '("NaN" "Infinity" "-Infinity" "-1" "1" "0.1" ".1" "-.1" "8e23"
                      "9E-2" ".1e23" "0b1" "-0B1" "0o7" "-0O13" "0xaf" "-0XAF"))
    (should (string-match typescript--number-literal-re to-match))
    ;; The regular expression does not begin with ^ and end with $ so
    ;; we need to check ourselves that the whole string is matched.
    (should (string-equal (match-string 0 to-match) to-match))))

(ert-deftest typescript--number-literal-re-does-not-match-non-numbers ()
  "`typescript--number-literal-re' does not match non-numbers."
  (dolist (to-match '("NaNa" "Inf" "1." "." "0xPQ" "e" "2.3e2.4"))
    ;; For the same reason as for the positive test above, what we want is either no match
    ;; or a match that fails to match the whole string.
    (should-not (and (string-match typescript--number-literal-re to-match)
                     (string-equal (match-string 0 to-match) to-match)))))

(ert-deftest correctly-indents-lines-with-wide-chars ()
  "Otsuka Ai and other multi-char users should be a happy to write typescript."

  (with-temp-buffer
    (ignore-errors (typescript-mode))
    (insert "let x = '大塚愛'")
    (let ((pos1 (current-column)))
      (typescript-indent-line)
      (let ((pos2 (current-column)))
        (should (= pos1 pos2))))))

(ert-deftest correctly-indents-lines-with-tabs ()
  (with-temp-buffer
    (ignore-errors (typescript-mode))

    (insert "class Example {")
    (newline-and-indent)
    (insert "constructor() {")
    (newline-and-indent)
    (insert "const a = new Promise")

    (should (= 29 (current-column)))
    (typescript-indent-line)
    (should (= 29 (current-column)))

    ;; verify tab was used
    (move-beginning-of-line nil)
    (should (= 0 (current-column)))
    (forward-char 1)
    (should (= 8 (current-column)))))

(ert-deftest indentation-does-not-hang-on-multiline-string ()
  "Testcase for https://github.com/ananthakumaran/typescript.el/issues/20"

  (with-temp-buffer
    (typescript-mode)

    (insert "let multiLineString = \"line 1")
    (newline-and-indent)
    (insert "// and so we continue")
    (newline-and-indent)
    ;; completing and not locking up is test-success!
    ))

(defun test-re-search (searchee contents offset)
  (with-temp-buffer
    (typescript-mode)

    (insert contents)
    (goto-char (- (point-max) offset))

    (should (= 5 (typescript--re-search-backward-inner searchee nil 1)))))

(ert-deftest re-search-backwards-skips-single-line-strings ()
  (test-re-search "token" "let token = \"token in string-thing\";" 2))

(ert-deftest re-search-backwards-skips-multi-line-strings ()
  (test-re-search "token" "let token = \"token in\n multi-line token string\";" 2))

(ert-deftest re-search-backwards-skips-single-line-comments ()
  (test-re-search "token" "let token; // token in comment" 0))

(ert-deftest re-search-backwards-skips-multi-line-comments ()
  (test-re-search "token" "let token; /* token in \nmulti-line token comment" 0))

;; Adapted from jdee-mode's test suite.
(defmacro test-with-temp-buffer (content &rest body)
  "Fill a temporary buffer with `CONTENT' and eval `BODY' in it."
  (declare (debug t)
           (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (typescript-mode)
     (goto-char (point-min))
     ;; We need this so that tests that simulate user actions operate on the right buffer.
     (switch-to-buffer (current-buffer))
     ,@body))

(defmacro test-with-fontified-buffer (content &rest body)
  "Fill a temporary buffer with `CONTENT' and eval `BODY' in it."
  (declare (debug t)
           (indent 1))
  `(test-with-temp-buffer
    ,content
     (font-lock-fontify-buffer)
     ,@body))

(defun get-face-at (loc)
  "Get the face at `LOC'. If it is not a number, then we `re-search-forward' with `LOC'
as the search pattern."
  (when (not (numberp loc))
    (save-excursion
      (re-search-forward loc)
      (setq loc (match-beginning 0))))
  (get-text-property loc 'face))

(setq font-lock-contents
 " * @param {Something} bar A parameter. References [[moo]] and [[foo]].
 * @param second May hold ``x`` or ``y``.")

(defun font-lock-test (contents expected)
  "Perform a test on our template. `CONTENTS' is the string to
put in the temporary buffer. `EXPECTED' is the expected
results. It should be a list of (LOCATION . FACE) pairs, where
LOCATION can be either a single location, or list of locations,
that are all expected to have the same face."
  (test-with-fontified-buffer
   contents
   ;; Make sure our propertize function has been applied to the whole
   ;; buffer.
   (syntax-propertize (point-max))
   (dolist (spec expected)
     (if (listp (car spec))
         (dolist (source (car spec))
           (should (eq (get-face-at source) (cdr spec))))
       (should (eq (get-face-at (car spec)) (cdr spec)))))))

(ert-deftest font-lock/documentation-in-documentation-comments ()
  "Documentation in documentation comments should be fontified as
documentation."
  (font-lock-test
   (concat "/**\n" font-lock-contents "\n*/")
   '((1 . font-lock-comment-delimiter-face)
     (5 . font-lock-comment-face)
     ("@param" . typescript-jsdoc-tag)
     ("{Something}" . typescript-jsdoc-type)
     ("bar" . typescript-jsdoc-value)
     ("\\[\\[moo\\]\\]" . typescript-jsdoc-value)
     ("\\[\\[foo\\]\\]" . typescript-jsdoc-value)
     ("``x``" . typescript-jsdoc-value)
     ("``y``" . typescript-jsdoc-value))))

(ert-deftest font-lock/no-documentation-in-non-documentation-comments ()
  "Documentation tags that are not in documentation comments
should not be fontified as documentation."
  (test-with-fontified-buffer
   (concat "/*\n" font-lock-contents "\n*/\n")
   (let ((loc 3))
     ;; Make sure we start with the right face.
     (should (eq (get-face-at loc) font-lock-comment-face))
     (should (eq (text-property-not-all loc (point-max) 'face font-lock-comment-face)
                 (1- (point-max)))))))

(ert-deftest font-lock/no-documentation-in-strings ()
  "Documentation tags that are not in strings should not be
fontified as documentation."
  (test-with-fontified-buffer
   (concat "const x = \"/**" font-lock-contents "*/\";")
   (let ((loc (search-forward "\"")))
     ;; Make sure we start with the right face.
     (should (eq (get-face-at loc) font-lock-string-face))
     ;; Make sure the face does not change later.
     (should (eq (text-property-not-all loc (point-max) 'face font-lock-string-face)
                 (1- (point-max)))))))

(ert-deftest font-lock/immediate-doc ()
  "Tests that it is not necessary to have the documentation tag on a
new line after the start of '/**'."
  (font-lock-test
   ;; We have 4 comments here because we need to cover the multiple
   ;; regexes that deal with the different types of jsdoc tags.
   "/** @type {foo} */\n
/** @alias bar */\n
/** @author me */\n
/** @param meow */"
   '((1 . font-lock-comment-delimiter-face)
     ("@type" . typescript-jsdoc-tag)
     ("{foo}" . typescript-jsdoc-type)
     ("@alias" . typescript-jsdoc-tag)
     ("bar" . typescript-jsdoc-value)
     ("@author" . typescript-jsdoc-tag)
     ("me" . font-lock-comment-face)
     ("@param" . typescript-jsdoc-tag)
     ("meow" . typescript-jsdoc-value))))

(ert-deftest font-lock/function-definition-prefixes ()
  "Tests that function names are highlighted in definitions, even
when prefixed with module modifiers."
  (font-lock-test
   "function basicDefn(x0: xty0, y0: yty0): ret0 {}\n
export function exportedDefn(x1: xty1, y1: yty1): ret1 {}\n
export default function exportedDefaultDefn(x2: xty2, y2: yty2): ret2 {}\n
declare function declareFunctionDefn(x3: xty3, y3: yty3): ret3;"
    '(("basicDefn" . font-lock-function-name-face)
      ("exportedDefn" . font-lock-function-name-face)
      ("exportedDefaultDefn" . font-lock-function-name-face)
      ("declareFunctionDefn" . font-lock-function-name-face)
      (("x0" "x1" "x2" "x3") . font-lock-variable-name-face)
      (("y0" "y1" "y2" "y3") . font-lock-variable-name-face)
      (("ret0" "ret1" "ret2" "ret3") . nil))))

(ert-deftest font-lock/regexp ()
  "Regular expresssions should be fontified as string constant."
  (let ((content "=/foo/ (/bar/ ,/baz/ :/buzz/"))
    (font-lock-test content
                    '(("=" . nil) ("/foo/" . font-lock-string-face)
                      ("(" . nil) ("/bar/" . font-lock-string-face)
                      ("," . nil) ("/baz/" . font-lock-string-face)
                      (":" . nil) ("/buzz/" . font-lock-string-face))))
  ;; Make sure that escaped forward slashes are handled too.
  (font-lock-test "var a = /flip\\/flop/;"
                  '(("=" . nil)
                    (("/flip" "\\\\" "/" "flop/") . font-lock-string-face)
                    (";" . nil)))
  ;; Make sure a forward slash in a character class is handled fine.
  ;; It must not terminate the regular expression.
  (font-lock-test "var a = /[/]/;"
                  '(("=" . nil)
                    (("/" "\\[/" "\\]/") . font-lock-string-face)
                    (";" . nil)))
  ;; Make sure an open bracket in a character class does not
  ;; throw off fontification.
  (font-lock-test "var a = /[[]/;"
                  '(("=" . nil)
                    (("/" "\\[\\[\\]" "/") . font-lock-string-face)
                    (";" . nil)))
  ;; A sequence of two forward slashes is never a regex, so there is
  ;; no such thing as an \"empty regex\" when we use the forward slash
  ;; notation.
  (font-lock-test "=//g something // comment"
                  '(("g something" . font-lock-comment-face))))

(ert-deftest font-lock/yield ()
  "`yield' and `yield*' should be fontified as keywords."
  (font-lock-test
   "function* gen(x0: xty0, y0: yty0): ret0 {
    yield 123;
    yield* subIter;
}"
   '(("yield 123" . font-lock-keyword-face)
     ("yield\\*" . font-lock-keyword-face)
     ("\\* subIter" . font-lock-keyword-face))))

(ert-deftest font-lock/yielder ()
  "`yielder' should not be fontified as a keyword."
  (font-lock-test
   "function* gen(x0: xty0, y0: yty0): ret0 {
    const yielder = 123;
    yield abc;
    return yielder;
}"
   '(("yielder =" . font-lock-variable-name-face)
     ("yielder;" . nil))))

(ert-deftest font-lock/text-after-trailing-regexp-delim-should-not-be-fontified ()
  "Text after trailing regular expression delimiter should not be fontified."
  (test-with-fontified-buffer
   "=/foo/g something // comment"
   (should (eq (get-face-at "g something") nil)))
  (test-with-fontified-buffer
   "=/foo\\bar/g something // comment"
   (should (eq (get-face-at "g something") nil)))
  (test-with-fontified-buffer
   "=/foo\\\\bar/g something // comment"
   (should (eq (get-face-at "g something") nil)))
  (test-with-fontified-buffer
   "=/foo\\\\/g something // comment"
   (should (eq (get-face-at "g something") nil))))

(ert-deftest font-lock/type-names ()
  "Type names should be highlighted in definitions."
  ;; Typical case.
  (test-with-fontified-buffer
      "export class Foo extends Bar implements Qux {}"
    (should (eq (get-face-at "Foo") 'font-lock-type-face))
    (should (eq (get-face-at "Bar") 'font-lock-type-face))
    (should (eq (get-face-at "Qux") 'font-lock-type-face)))
  ;; Ensure we require symbol boundaries.
  (test-with-fontified-buffer
      "Notclass Foo"
    (should (not (eq (get-face-at "Foo") 'font-lock-type-face))))
  ;; Other common ways of defining types.
  (test-with-fontified-buffer
      "interface Thing {}"
    (should (eq (get-face-at "Thing") 'font-lock-type-face)))
  (test-with-fontified-buffer
      "enum Thing {}"
    (should (eq (get-face-at "Thing") 'font-lock-type-face)))
  (test-with-fontified-buffer
      "type Thing = number;"
    (should (eq (get-face-at "Thing") 'font-lock-type-face))))

(defun flyspell-predicate-test (search-for)
  "This function runs a test on
`typescript--flyspell-mode-predicate'.  `SEARCH-FOR' is a string
to search for in the current buffer before running
`typescript--flyspell-mode-predicate'.  This test checks that the
point has not moved.  It returns the value of returned by the
invocation of `typescript--flyspell-mode-predicate'."
  (search-forward search-for)
  (let ((point-before (point)))
    (prog1
        (typescript--flyspell-mode-predicate)
      ;; We should not have moved.
      (should (eq (point) point-before)))
  ))

(ert-deftest flyspell-mode-predicate-skips-what-it-should ()
  "Check that the custom flyspell predicate filters strings in
import... from...."
  (let (flyspell-generic-progmode-verify)
    (fset 'flyspell-generic-progmode-verify (lambda () t))
    ;; In the following searches we search for the starting quote of the strings
    ;; to avoid hitting keywords. Moreover, the end position of the search is important.
    ;; Flyspell puts point at the end of the word before calling the predicate. We must
    ;; replicate that behavior here.
    (test-with-fontified-buffer
     "import 'a';\nimport { x } from 'b';\nconst foo = 'c';import { x }\nfrom 'd';"
     (should (not (flyspell-predicate-test "'a")))
     (should (not (flyspell-predicate-test "'b")))
     (should (flyspell-predicate-test "'c"))
     (should (not (flyspell-predicate-test "'d"))))
    (test-with-fontified-buffer
     ;; This is valid TypeScript.
     "const from = 'a';"
     (should (flyspell-predicate-test "'a")))
    (test-with-fontified-buffer
     ;; TypeScript does not allow a function named "import" but object
     ;; members may be named "import". So this *can* be valid
     ;; TypeScript.
     "x.import('a');"
     (should (flyspell-predicate-test "'a")))))


(ert-deftest typescript--move-to-end-of-plain-string ()
  "Unit tests for `typescript--move-to-end-of-plain-string'."
  (cl-flet
      ((should-fail ()
                    (let ((point-before (point)))
                      (should (not (typescript--move-to-end-of-plain-string)))
                      (should (eq (point) point-before))))
       (should-not-fail (expected)
                        (let ((result (typescript--move-to-end-of-plain-string)))
                          (should (eq result expected))
                          (should (eq (point) expected)))))
    ;;
    ;; The tests below are structured as follows. For each case:
    ;;
    ;; 1. Move point to a new location in the buffer.
    ;;
    ;; 2. Check whether typescript--move-to-end-of-plain-string returns the value we expected
    ;;    and changes (point) when successful.
    ;;
    ;; Cases often start with a check right away: (point) equal to
    ;; (point-min) for those cases.
    ;;
    (dolist (delimiter '("'" "\""))
      (test-with-temp-buffer
       (replace-regexp-in-string "'" delimiter "const a = 'not terminated")
       (should-fail)
       (re-search-forward delimiter)
       (should-fail))
      (test-with-temp-buffer
       (replace-regexp-in-string "'" delimiter "const a = 'terminated'")
       (should-fail)
       ;; This checks that the function works when invoked on the start delimiter of
       ;; a terminated string.
       (re-search-forward delimiter)
       (should-not-fail (1- (point-max)))
       (goto-char (point-min))
       (re-search-forward "term")
       (should-not-fail (1- (point-max)))
       ;; This checks that the function works when invoked on the end delimiter of
       ;; a terminated string.
       (goto-char (1- (point-max)))
       (should-not-fail (1- (point-max))))
      (test-with-temp-buffer
       (replace-regexp-in-string "'" delimiter "const a = 'terminated aaa';\n
const b = 'not terminated bbb")
       (should-fail)
       (re-search-forward "term")
       (should-not-fail (save-excursion (re-search-forward "aaa")))
       (re-search-forward "const b")
       (should-fail)
       (re-search-forward "not terminated")
       (should-fail))
      ;; Case with escaped delimiter.
      (test-with-temp-buffer
       (replace-regexp-in-string "'" delimiter "const a = 'terminat\\'ed aaa';\n
 const b = 'not terminated bbb")
       (re-search-forward "term")
       (should-not-fail (save-excursion (re-search-forward "aaa"))))
      ;; Delimiters in comments.
      (test-with-temp-buffer
       (replace-regexp-in-string "'" delimiter "const a = 'terminated aaa';\n
// Comment 'or'\n
const b = 'not terminated bbb")
       (re-search-forward "term")
       (should-not-fail (save-excursion (re-search-forward "aaa")))
       (re-search-forward "Comment ")
       (should-fail)
       (forward-char)
       (should-fail)
       (re-search-forward "or")
       (should-fail)))
    ;; Ignores template strings.
    (test-with-temp-buffer
     "const a = `terminated aaa`"
     (re-search-forward "term")
     (should-fail))))

(ert-deftest typescript-convert-to-template ()
  "Unit tests for `typescript-convert-to-template'."
  (cl-flet
      ((should-do-nothing (str regexp)
                    (test-with-temp-buffer
                     str
                     (re-search-forward regexp)
                     (typescript-convert-to-template)
                     (should (string-equal (typescript-test-get-doc) str))))
       (should-modify (str delimiter regexp)
                    (test-with-temp-buffer
                     str
                     (re-search-forward regexp)
                     (typescript-convert-to-template)
                     (should (string-equal (typescript-test-get-doc)
                                           (replace-regexp-in-string delimiter "`" str))))))
    (dolist (delimiter '("'" "\""))
      (let ((str (replace-regexp-in-string "'" delimiter "const a = 'not terminated")))
        (dolist (move-to '("const" "not"))
          (should-do-nothing str move-to)))
      (let ((str (replace-regexp-in-string "'" delimiter "const a = 'terminated'")))
        (should-do-nothing str "const")
        (should-modify str delimiter delimiter)
        (should-modify str delimiter "term")
        (should-modify str delimiter "terminated"))
      ;; Delimiters in comments.
      (let ((str (replace-regexp-in-string "'" delimiter "const a = 'terminated aaa';\n
// Comment 'or'\n
const b = 'not terminated bbb")))
        (should-do-nothing str "Comment ")))
    ;; Ignores template strings.
    (let ((str "const a = `terminated aaa`"))
      (should-do-nothing str "terminated"))))

(ert-deftest typescript-autoconvert-to-template ()
  "Unit tests for `typescript-autoconvert-to-template'."
  (cl-flet
      ((should-do-nothing (str regexp)
                    (test-with-temp-buffer
                     str
                     (re-search-forward regexp)
                     (typescript-autoconvert-to-template)
                     (should (string-equal (typescript-test-get-doc) str))))
       (should-modify (str delimiter regexp)
                    (test-with-temp-buffer
                     str
                     (re-search-forward regexp)
                     (typescript-autoconvert-to-template)
                     (should (string-equal (typescript-test-get-doc)
                                           (replace-regexp-in-string delimiter "`" str))))))
    (dolist (delimiter '("'" "\""))
      (let ((str (replace-regexp-in-string "'" delimiter "const a = 'terminated'")))
        (should-do-nothing str "= ")
        (should-do-nothing str "terminated"))
      (let ((str (replace-regexp-in-string "'" delimiter "const a = '${foo}'")))
        (should-do-nothing str "= ")
        (should-modify str delimiter (concat "foo}" delimiter))))))

(ert-deftest typescript-autoconvert-to-template-is-invoked ()
  "Test that we call `typescript-autoconvert-to-template' as needed."
  (cl-flet
      ((should-do-nothing (str delimiter)
                    (test-with-temp-buffer
                     str
                     (goto-char (point-max))
                     (execute-kbd-macro delimiter)
                     (should (string-equal (typescript-test-get-doc) (concat str delimiter)))))
       (should-modify (str delimiter)
                    (test-with-temp-buffer
                     str
                     (goto-char (point-max))
                     (execute-kbd-macro delimiter)
                     (should (string-equal (typescript-test-get-doc)
                                           (replace-regexp-in-string delimiter "`" (concat str delimiter)))))))
    (dolist (delimiter '("'" "\""))
      (let ((str (replace-regexp-in-string "'" delimiter "const a = '${foo}")))
        (should-do-nothing str delimiter)
        (let ((typescript-autoconvert-to-template-flag t))
          (should-modify str delimiter))))))

(provide 'typescript-mode-tests)

;;; typescript-mode-tests.el ends here
