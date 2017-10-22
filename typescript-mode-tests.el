
;;; typescript-mode-tests --- This file contains automated tests for typescript-mode.el

;;; Commentary:
;; Run tests using (ert-run-tests-interactively t).

;;; Code:

(require 'ert)
(require 'typescript-mode)

(defun typescript-test-get-doc ()
  (buffer-substring-no-properties (point-min) (point-max)))

(defun typescript-test-indent-all ()
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

(ert-deftest indentation-reference-document-is-reflowed-correctly ()
  (let* ((buffer (find-file "test-files/indentation-reference-document.ts")))
    ;; double ensure mode is active
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
     (font-lock-fontify-buffer)
     (goto-char (point-min))
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
  (test-with-temp-buffer
   contents
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
  (test-with-temp-buffer
   (concat "/*\n" font-lock-contents "\n*/\n")
   (let ((loc 3))
     ;; Make sure we start with the right face.
     (should (eq (get-face-at loc) font-lock-comment-face))
     (should (eq (text-property-not-all loc (point-max) 'face font-lock-comment-face)
                 (1- (point-max)))))))

(ert-deftest font-lock/no-documentation-in-strings ()
  "Documentation tags that are not in strings should not be
fontified as documentation."
  (test-with-temp-buffer
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
                      (":" . nil) ("/buzz/" . font-lock-string-face)))))

(ert-deftest font-lock/text-after-trailing-regexp-delim-should-not-be-fontified ()
  "Text after trailing regular expression delimiter should not be fontified."
  (test-with-temp-buffer
      "=/foo/g something // comment"
    (should (eq (get-face-at "g something") nil)))
  (test-with-temp-buffer
      "=/foo\\bar/g something // comment"
    (should (eq (get-face-at "g something") nil)))
  (test-with-temp-buffer
      "=/foo\\\\bar/g something // comment"
    (should (eq (get-face-at "g something") nil)))
  (test-with-temp-buffer
      "=/foo\\\\/g something // comment"
    (should (eq (get-face-at "g something") nil))))

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
    (test-with-temp-buffer
     "import 'a';\nimport { x } from 'b';\nconst foo = 'c';import { x }\nfrom 'd';"
     (should (not (flyspell-predicate-test "'a")))
     (should (not (flyspell-predicate-test "'b")))
     (should (flyspell-predicate-test "'c"))
     (should (not (flyspell-predicate-test "'d"))))
    (test-with-temp-buffer
     ;; This is valid TypeScript.
     "const from = 'a';"
     (should (flyspell-predicate-test "'a")))
    (test-with-temp-buffer
     ;; TypeScript does not allow a function named "import" but object
     ;; members may be named "import". So this *can* be valid
     ;; TypeScript.
     "x.import('a');"
     (should (flyspell-predicate-test "'a")))))

(provide 'typescript-mode-tests)

;;; typescript-mode-tests.el ends here
