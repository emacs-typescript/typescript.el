;;; typescript-mode-jsdoc-tests --- This file contains JSDoc related tests for typescript-mode.el

;;; Commentary:
;; To understand the definition of JSDoc tag,
;; see  https://github.com/jsdoc3/jsdoc/blob/b21427343c7294bbf1f14c718a390f3e955e37cb/lib/jsdoc/tag.js#L153-L195
;; To know how to run the tests, see typescript-mode-tests.el

;;; Code:

(require 'ert)
(require 'typescript-mode)
(require 'typescript-mode-test-utilities)

(defun jsdoc-multiline-test (jsdoc-lines specs)
  "Perform a multi-line JSDoc test against specs.
`JSDOC-LINES' are converted into a multi-line JS comment, and
the comment is tested against `SPECS'.
For more information about how to write `SPECS', see `font-lock-test'."
  (let* ((unwrapped-comment-lines
          (mapcar (lambda (line) (concat " * " line "\n")) jsdoc-lines))
         (comment-lines
          (append '("/**\n") unwrapped-comment-lines '(" */"))))
    (font-lock-test
     (apply #'concat comment-lines)
     (cons
      '(1 . font-lock-comment-delimiter-face)
      specs))))

;; Internal tags (or essential tags)
;; See https://github.com/jsdoc3/jsdoc/blob/b21427343c7294bbf1f14c718a390f3e955e37cb/lib/jsdoc/tag/dictionary/definitions.js#L222-L256

(ert-deftest jsdoc/also-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @also tags and its alias."
  (jsdoc-multiline-test
   '(
     "@also"
     )
   '(
     ;; ("@also$" . typescript-jsdoc-tag)
     )))

(ert-deftest jsdoc/also-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @also tags and its alias."
  (jsdoc-multiline-test
   '(
     "@also extra text"
     )
   '(
     ;; ("@also extra text" . typescript-jsdoc-tag)
     ("extra text" . font-lock-comment-face)
     ("text" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/also-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @also tags and its alias."
  (jsdoc-multiline-test
   '(
     )
   '(
     )))

(ert-deftest jsdoc/description-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @description tags and its alias."
  (jsdoc-multiline-test
   '(
     "@description descriptionInSingleWord"
     "@description description about something"
     "@description"
     "multi-line description about other things"
     "@desc descInSingleWord"
     "@desc desc about something"
     "@desc"
     "multi-line desc about other things"
     )
   '(
     ("@description descriptionInSingleWord" . typescript-jsdoc-tag)
     ("descriptionInSingleWord" . font-lock-comment-face)
     ("@description description about something" . typescript-jsdoc-tag)
     ("description about something" . font-lock-comment-face)
     ("about something" . font-lock-comment-face)
     ("something" . font-lock-comment-face)
     ("@description$" . typescript-jsdoc-tag)
     ("multi-line description about other things" . font-lock-comment-face)
     ("@desc descInSingleWord" . typescript-jsdoc-tag)
     ("descInSingleWord" . font-lock-comment-face)
     ("@desc desc about something" . typescript-jsdoc-tag)
     ("desc about something" . font-lock-comment-face)
     ("about something" . font-lock-comment-face)
     ("something" . font-lock-comment-face)
     ("@desc$" . typescript-jsdoc-tag)
     ("multi-line desc about other things" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/description-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @description tags and its alias."
  (jsdoc-multiline-test
   '(
     "@description {DescriptionType}"
     "@desc {DescType}"
     )
   '(
     ("@description {DescriptionType}" . typescript-jsdoc-tag)
     ("{DescriptionType}" . font-lock-comment-face)
     ("@desc {DescType}" . typescript-jsdoc-tag)
     ("{DescType}" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/description-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @description tags and its alias."
  (jsdoc-multiline-test
   '(
     "@description"
     "@desc"
     )
   '(
     ;; ("@description$" . font-lock-comment-face)
     ;; ("@desc$" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/kind-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @kind tags and its alias."
  (jsdoc-multiline-test
   '(
     "@kind class"
     "@kind constant"
     "@kind event"
     "@kind external"
     "@kind file"
     "@kind function"
     "@kind member"
     "@kind mixin"
     "@kind module"
     "@kind namespace"
     "@kind typedef"
     )
   '(
     ("@kind class" . typescript-jsdoc-tag)
     ("class" . typescript-jsdoc-value)
     ("@kind constant" . typescript-jsdoc-tag)
     ("constant" . typescript-jsdoc-value)
     ("@kind event" . typescript-jsdoc-tag)
     ("event" . typescript-jsdoc-value)
     ("@kind external" . typescript-jsdoc-tag)
     ("external" . typescript-jsdoc-value)
     ("@kind file" . typescript-jsdoc-tag)
     ("file" . typescript-jsdoc-value)
     ("@kind function" . typescript-jsdoc-tag)
     ("function" . typescript-jsdoc-value)
     ("@kind member" . typescript-jsdoc-tag)
     ("member" . typescript-jsdoc-value)
     ("@kind mixin" . typescript-jsdoc-tag)
     ("mixin" . typescript-jsdoc-value)
     ("@kind module" . typescript-jsdoc-tag)
     ("module" . typescript-jsdoc-value)
     ("@kind namespace" . typescript-jsdoc-tag)
     ("namespace" . typescript-jsdoc-value)
     ("@kind typedef" . typescript-jsdoc-tag)
     ("typedef" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/kind-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @kind tags and its alias."
  (jsdoc-multiline-test
   '(
     "@kind meaninglessKind"
     "@kind {KindType}"
     )
   '(
     ("@kind meaninglessKind" . typescript-jsdoc-tag)
     ("meaninglessKind" . typescript-jsdoc-value)
     ("@kind {KindType}" . typescript-jsdoc-tag)
     ("{KindType}" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/kind-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @kind tags and its alias."
  (jsdoc-multiline-test
   '(
     "@kind"
     )
   '(
     ("@kind$" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/name-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @name tags and its alias."
  (jsdoc-multiline-test
   '(
     "@name someName"
     )
   '(
     ("@name someName" . typescript-jsdoc-tag)
     ("someName" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/name-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @name tags and its alias."
  (jsdoc-multiline-test
   '(
     "@name {NameType}"
     )
   '(
     ("@name {NameType}" . typescript-jsdoc-tag)
     ("{NameType}" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/name-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @name tags and its alias."
  (jsdoc-multiline-test
   '(
     "@name"
     )
   '(
     ("@name$" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/undocumented-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @undocumented tags and its alias."
  (jsdoc-multiline-test
   '(
     "@undocumented"
     )
   '(
     ;; ("@undocumented" . typescript-jsdoc-tag)
     )))

(ert-deftest jsdoc/undocumented-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @undocumented tags and its alias."
  (jsdoc-multiline-test
   '(
     )
   '(
     )))

(ert-deftest jsdoc/undocumented-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @undocumented tags and its alias."
  (jsdoc-multiline-test
   '(
     "@undocumented invalidValue"
     "@undocumented {InvalidType}"
     )
   '(
     ("@undocumented invalidValue" . font-lock-comment-face)
     ("invalidValue" . font-lock-comment-face)
     ("@undocumented {InvalidType}" . font-lock-comment-face)
     ("{InvalidType}" . font-lock-comment-face)
     )))

;; Basic tags
;; See https://github.com/jsdoc3/jsdoc/blob/b21427343c7294bbf1f14c718a390f3e955e37cb/lib/jsdoc/tag/dictionary/definitions.js#L260-L858
;; for details of following tag tests

(ert-deftest jsdoc/abstract-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @abstract tags and its alias."
  (jsdoc-multiline-test
   '(
     "@abstract"
     "@virtual"
     )
   '(
     ("@abstract" . typescript-jsdoc-tag)
     ("@virtual" . typescript-jsdoc-tag)
     )))

(ert-deftest jsdoc/abstract-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @abstract tags and its alias."
  (jsdoc-multiline-test
   '(
     )
   '(
     )))

(ert-deftest jsdoc/abstract-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @abstract tags and its alias."
  (jsdoc-multiline-test
   '(
     "@abstract invalid extra value0"
     "@abstract {InvalidType0}"
     "@virtual invalid extra value1"
     "@virtual {InvalidType1}"
     )
   '(
     ("@abstract invalid extra value0" . typescript-jsdoc-tag)
     ("invalid extra value0" . font-lock-comment-face)
     ("extra value0" . font-lock-comment-face)
     ("value0" . font-lock-comment-face)
     ("@abstract {InvalidType0}" . typescript-jsdoc-tag)
     ("{InvalidType0}" . font-lock-comment-face)
     ("@virtual invalid extra value1" . typescript-jsdoc-tag)
     ("invalid extra value1" . font-lock-comment-face)
     ("extra value1" . font-lock-comment-face)
     ("value1" . font-lock-comment-face)
     ("@virtual {InvalidType1}" . typescript-jsdoc-tag)
     ("{InvalidType1}" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/access-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @access tags and its alias."
  (jsdoc-multiline-test
   '(
     "@access package"
     "@access private"
     "@access protected"
     "@access public"
     )
   '(
     ("@access package" . typescript-jsdoc-tag)
     ("package" . typescript-jsdoc-value)
     ("@access private" . typescript-jsdoc-tag)
     ("private" . typescript-jsdoc-value)
     ("@access protected" . typescript-jsdoc-tag)
     ("protected" . typescript-jsdoc-value)
     ("@access public" . typescript-jsdoc-tag)
     ("public" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/access-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @access tags and its alias."
  (jsdoc-multiline-test
   '(
     "@access meaninglessAccess"
     "@access {AccessType}"
     )
   '(
     ("@access meaninglessAccess" . typescript-jsdoc-tag)
     ("meaninglessAccess" . typescript-jsdoc-value)
     ("@access {AccessType}" . typescript-jsdoc-tag)
     ("{AccessType}" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/access-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @access tags and its alias."
  (jsdoc-multiline-test
   '(
     "@access"
     )
   '(
     ("@access$" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/alias-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @alias tags and its alias."
  (jsdoc-multiline-test
   '(
     "@alias exampleAlias0"
     "@alias somenamespace.exampleAlias1"
     )
   '(
     ("@alias exampleAlias0" . typescript-jsdoc-tag)
     ("exampleAlias0" . typescript-jsdoc-value)
     ("@alias somenamespace.exampleAlias1" . typescript-jsdoc-tag)
     ("somenamespace.exampleAlias1" . typescript-jsdoc-value)
     ("exampleAlias1" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/alias-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @alias tags and its alias."
  (jsdoc-multiline-test
   '(
     "@alias multiple words"
     "@alias {AliasType}"
     )
   '(
     ("@alias multiple words" . typescript-jsdoc-tag)
     ("multiple words" . typescript-jsdoc-value)
     ;; ("words" . typescript-jsdoc-value)
     ("@alias {AliasType}" . typescript-jsdoc-tag)
     ("{AliasType}" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/alias-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @alias tags and its alias."
  (jsdoc-multiline-test
   '(
     "@alias"
     )
   '(
     ("@alias$" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/async-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @async tags and its alias."
  (jsdoc-multiline-test
   '(
     "@async"
     )
   '(
     ("@async" . typescript-jsdoc-tag)
     )))

(ert-deftest jsdoc/async-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @async tags and its alias."
  (jsdoc-multiline-test
   '(
     )
   '(
     )))

(ert-deftest jsdoc/async-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @async tags and its alias."
  (jsdoc-multiline-test
   '(
     "@async invalid extra value"
     "@async {InvalidType}"
     )
   '(
     ("@async invalid extra value" . typescript-jsdoc-tag)
     ("invalid extra value" . font-lock-comment-face)
     ("extra value" . font-lock-comment-face)
     ("value" . font-lock-comment-face)
     ("@async {InvalidType}" . typescript-jsdoc-tag)
     ("{InvalidType}" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/augments-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @augments tags and its alias."
  (jsdoc-multiline-test
   '(
     "@augments ExampleAugmented0"
     "@augments {ExampleAugmented1}"
     "@extends ExampleExtended0"
     "@extends {ExampleExtended1}"
     )
   '(
     ("@augments ExampleAugmented0" . typescript-jsdoc-tag)
     ("ExampleAugmented0" . typescript-jsdoc-value)
     ("@augments {ExampleAugmented1}" . typescript-jsdoc-tag)
     ;; ("{ExampleAugmented1}" . typescript-jsdoc-type)
     ("@extends ExampleExtended0" . typescript-jsdoc-tag)
     ("ExampleExtended0" . typescript-jsdoc-value)
     ("@extends {ExampleExtended1}" . typescript-jsdoc-tag)
     ;; ("{ExampleExtended1}" . typescript-jsdoc-type)
     )))

(ert-deftest jsdoc/augments-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @augments tags and its alias."
  (jsdoc-multiline-test
   '(
     "@augments {ExampleAugmented0} extra augments value"
     "@extends {ExampleExtended0} extra extends value"
     )
   '(
     ("@augments {ExampleAugmented0} extra augments value" . typescript-jsdoc-tag)
     ;; ("{ExampleAugmented0} extra augments value" . typescript-jsdoc-type)
     ("extra augments value" . font-lock-comment-face)
     ("augments value" . font-lock-comment-face)
     ("value" . font-lock-comment-face)
     ("@extends {ExampleExtended0} extra extends value" . typescript-jsdoc-tag)
     ;; ("{ExampleExtended0} extra extends value" . typescript-jsdoc-type)
     ("extra extends value" . font-lock-comment-face)
     ("extends value" . font-lock-comment-face)
     ("value" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/augments-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @augments tags and its alias."
  (jsdoc-multiline-test
   '(
     "@augments"
     "@extends"
     )
   '(
     ("@augments$" . font-lock-comment-face)
     ;; ("@extends$" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/author-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @author tags and its alias."
  (jsdoc-multiline-test
   '(
     "@author Exampleauthor"
     "@author Exampleauthor Withfamilyname"
     "@author Exampleauthor <example@author.email>"
     )
   '(
     ("@author Exampleauthor$" . typescript-jsdoc-tag)
     ;; ("Exampleauthor" . typescript-jsdoc-value)
     ("@author Exampleauthor Withfamilyname" . typescript-jsdoc-tag)
     ;; ("Exampleauthor Withfamilyname" . typescript-jsdoc-value)
     ;; ("Withfamilyname" . typescript-jsdoc-value)
     ("@author Exampleauthor <example@author.email>" . typescript-jsdoc-tag)
     ;; ("Exampleauthor <example@author.email>" . typescript-jsdoc-value)
     ;; ("<example@author.email>" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/author-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @author tags and its alias."
  (jsdoc-multiline-test
   '(
     "@author {AuthorType}"
     )
   '(
     ("@author {AuthorType}" . typescript-jsdoc-tag)
     ;; ("{AuthorType}" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/author-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @author tags and its alias."
  (jsdoc-multiline-test
   '(
     "@author"
     )
   '(
     ;; ("@author$" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/borrows-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @borrows tags and its alias."
  (jsdoc-multiline-test
   '(
     "@borrows something0"
     "@borrows originalName as borrowedName"
     )
   '(
     ("@borrows something0" . typescript-jsdoc-tag)
     ("something0" . typescript-jsdoc-value)
     ("@borrows originalName as borrowedName" . typescript-jsdoc-tag)
     ("originalName as borrowedName" . typescript-jsdoc-value)
     ;; ("as borrowedName" . typescript-jsdoc-value)
     ;; ("borrowedName" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/borrows-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @borrows tags and its alias."
  (jsdoc-multiline-test
   '(
     "@borrows meaningless multiple words"
     "@borrows {BorrowType}"
     )
   '(
     ("@borrows meaningless multiple words" . typescript-jsdoc-tag)
     ("meaningless multiple words" . typescript-jsdoc-value)
     ;; ("multiple words" . typescript-jsdoc-value)
     ;; ("words" . typescript-jsdoc-value)
     ("@borrows {BorrowType}" . typescript-jsdoc-tag)
     ("{BorrowType}" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/borrows-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @borrows tags and its alias."
  (jsdoc-multiline-test
   '(
     "@borrows"
     "@borrows {InvalidType}"
     )
   '(
     ("@borrows$" . font-lock-comment-face)
     ;; ("@borrows {InvalidType}" . font-lock-comment-face)
     ;; ("{InvalidType}" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/class-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @class tags and its alias."
  (jsdoc-multiline-test
   '(
     ;; See https://github.com/jsdoc3/jsdoc/blob/b21427343c7294bbf1f14c718a390f3e955e37cb/lib/jsdoc/tag/dictionary/definitions.js#L318-L340
     "@class"
     "@class ExampleClass"
     "@class Class tag for description0"
     "@class"
     "Class tag for description1"
     "@constructor"
     "@constructor ExampleClassConstructor"
     ;; You cannot use @constructor to describe class
     )
   '(
     ("@class$" . typescript-jsdoc-tag)
     ("@class ExampleClass" . typescript-jsdoc-tag)
     ;; ("ExampleClass" . typescript-jsdoc-value)
     ("@class Class tag for description0" . typescript-jsdoc-tag)
     ("Class tag for description0" . font-lock-comment-face)
     ("tag for description0" . font-lock-comment-face)
     ("for description0" . font-lock-comment-face)
     ("description0" . font-lock-comment-face)
     ("Class tag for description1" . font-lock-comment-face)
     ("tag for description1" . font-lock-comment-face)
     ("for description1" . font-lock-comment-face)
     ("description1" . font-lock-comment-face)
     ("@constructor" . typescript-jsdoc-tag)
     ;; ("ExampleClassConstructor" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/class-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @class tags and its alias."
  (jsdoc-multiline-test
   '(
     "@class {ClassType}"
     "@constructor {ConstructorType}"
     )
   '(
     ("@class {ClassType}" . typescript-jsdoc-tag)
     ;; ("{ClassType}" . typescript-jsdoc-value)
     ("@constructor {ConstructorType}" . typescript-jsdoc-tag)
     ;; ("{ConstructorType}" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/class-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @class tags and its alias."
  (jsdoc-multiline-test
   '(
     )
   '(
     )))

(ert-deftest jsdoc/classdesc-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @classdesc tags and its alias."
  (jsdoc-multiline-test
   '(
     "@classdesc"
     "@classdesc This is class description0"
     "@classdesc"
     "This is class description1"
     )
   '(
     ("@classdesc$" . typescript-jsdoc-tag)
     ("@classdesc This is class description0" . typescript-jsdoc-tag)
     ("This is class description0" . font-lock-comment-face)
     ("is class description0" . font-lock-comment-face)
     ("class description0" . font-lock-comment-face)
     ("description0" . font-lock-comment-face)
     ("This is class description1" . font-lock-comment-face)
     ("is class description1" . font-lock-comment-face)
     ("class description1" . font-lock-comment-face)
     ("description1" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/classdesc-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @classdesc tags and its alias."
  (jsdoc-multiline-test
   '(
     "@classdesc {ClassDescType}"
     )
   '(
     ("@classdesc {ClassDescType}" . typescript-jsdoc-tag)
     ("{ClassDescType}" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/classdesc-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @classdesc tags and its alias."
  (jsdoc-multiline-test
   '(
     )
   '(
     )))

(ert-deftest jsdoc/constant-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @constant tags and its alias."
  (jsdoc-multiline-test
   '(
     "@constant"
     "@constant {ConstantType0}"
     "@constant constantName0"
     "@constant {ConstantType1} constantName1"
     "@const"
     "@const {ConstType0}"
     "@const constName0"
     "@const {ConstType1} constName1"
     )
   '(
     ("@constant$" . typescript-jsdoc-tag)
     ("@constant {ConstantType0}" . typescript-jsdoc-tag)
     ;; ("{ConstantType0}" . typescript-jsdoc-type)
     ("@constant constantName0" . typescript-jsdoc-tag)
     ;; ("constantName0" . typescript-jsdoc-value)
     ("@constant {ConstantType1} constantName1" . typescript-jsdoc-tag)
     ;; ("{ConstantType1} constantName1" . typescript-jsdoc-type)
     ;; ("constantName1" . typescript-jsdoc-value)
     ("@const$" . typescript-jsdoc-tag)
     ("@const {ConstType0}" . typescript-jsdoc-tag)
     ;; ("{ConstType0}" . typescript-jsdoc-type)
     ("@const constName0" . typescript-jsdoc-tag)
     ;; ("constName0" . typescript-jsdoc-value)
     ("@const {ConstType1} constName1" . typescript-jsdoc-tag)
     ;; ("{ConstType1} constName1" . typescript-jsdoc-type)
     ;; ("constName1" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/constant-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @constant tags and its alias."
  (jsdoc-multiline-test
   '(
     "@constant extra value other than name for constant0"
     "@const extra value other than name for const0"
     )
   '(
     ("@constant extra value other than name for constant0" . typescript-jsdoc-tag)
     ;; ("extra value other than name for constant0" . typescript-jsdoc-value)
     ("value other than name for constant0" . font-lock-comment-face)
     ("other than name for constant0" . font-lock-comment-face)
     ("than name for constant0" . font-lock-comment-face)
     ("name for constant0" . font-lock-comment-face)
     ("for constant0" . font-lock-comment-face)
     ("constant0" . font-lock-comment-face)
     ("@const extra value other than name for const0" . typescript-jsdoc-tag)
     ;; ("extra value other than name for const0" . typescript-jsdoc-value)
     ("value other than name for const0" . font-lock-comment-face)
     ("other than name for const0" . font-lock-comment-face)
     ("than name for const0" . font-lock-comment-face)
     ("name for const0" . font-lock-comment-face)
     ("for const0" . font-lock-comment-face)
     ("const0" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/constant-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @constant tags and its alias."
  (jsdoc-multiline-test
   '(
     )
   '(
     )))

(ert-deftest jsdoc/constructs-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @constructs tags and its alias."
  (jsdoc-multiline-test
   '(
     "@constructs"
     "@constructs className"
     "@constructs {@thisClass}"
     )
   '(
     ("@constructs$" . typescript-jsdoc-tag)
     ("@constructs className" . typescript-jsdoc-tag)
     ;; ("className" . typescript-jsdoc-value)
     ("@constructs {@thisClass}" . typescript-jsdoc-tag)
     ;; ("{@thisClass}" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/constructs-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @constructs tags and its alias."
  (jsdoc-multiline-test
   '(
     "@constructs className and extra description"
     "@constructs {ClassType}"
     )
   '(
     ("@constructs className and extra description" . typescript-jsdoc-tag)
     ;; ("className and extra description" . typescript-jsdoc-value)
     ("and extra description" . font-lock-comment-face)
     ("extra description" . font-lock-comment-face)
     ("description" . font-lock-comment-face)
     ("@constructs {ClassType}" . typescript-jsdoc-tag)
     ;; ("{ClassType}" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/constructs-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @constructs tags and its alias."
  (jsdoc-multiline-test
   '(
     )
   '(
     )))

(ert-deftest jsdoc/copyright-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @copyright tags and its alias."
  (jsdoc-multiline-test
   '(
     "@copyright Some copyright texts"
     )
   '(
     ("@copyright Some copyright texts" . typescript-jsdoc-tag)
     ("Some copyright texts" . font-lock-comment-face)
     ("copyright texts" . font-lock-comment-face)
     ("texts" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/copyright-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @copyright tags and its alias."
  (jsdoc-multiline-test
   '(
     "@copyright {InvalidType}"
     )
   '(
     ("@copyright {InvalidType}" . typescript-jsdoc-tag)
     ("{InvalidType}" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/copyright-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @copyright tags and its alias."
  (jsdoc-multiline-test
   '(
     "@copyright"
     )
   '(
     ;; ("@copyright$" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/default-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @default tags and its alias."
  (jsdoc-multiline-test
   '(
     "@default"
     "@default 5"
     "@default true"
     "@defaultvalue"
     "@defaultvalue null"
     "@defaultvalue \"abc\""
     )
   '(
     ("@default$" . typescript-jsdoc-tag)
     ("@default 5" . typescript-jsdoc-tag)
     ;; ("5" . typescript-jsdoc-value)
     ("@default true" . typescript-jsdoc-tag)
     ;; ("true" . typescript-jsdoc-value)
     ("@defaultvalue$" . typescript-jsdoc-tag)
     ("@defaultvalue null" . typescript-jsdoc-tag)
     ;; ("null" . typescript-jsdoc-value)
     ("@defaultvalue \"abc\"" . typescript-jsdoc-tag)
     ;; ("\"abc\"" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/default-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @default tags and its alias."
  (jsdoc-multiline-test
   '(
     "@default multiple values for default0"
     "@default {DefaultType}"
     "@defaultvalue multiple values for defaultvalue0"
     "@defaultvalue {DefaultValueType}"
     )
   '(
     ("@default multiple values for default0" . typescript-jsdoc-tag)
     ;; ("multiple values for default0" . typescript-jsdoc-value)
     ;; ("values for default0" . typescript-jsdoc-value)
     ;; ("for default0" . typescript-jsdoc-value)
     ;; ("default0" . typescript-jsdoc-value)
     ("@default {DefaultType}" . typescript-jsdoc-tag)
     ;; ("{DefaultType}" . typescript-jsdoc-value)
     ("@defaultvalue multiple values for defaultvalue0" . typescript-jsdoc-tag)
     ;; ("multiple values for defaultvalue0" . typescript-jsdoc-value)
     ;; ("values for defaultvalue0" . typescript-jsdoc-value)
     ;; ("for defaultvalue0" . typescript-jsdoc-value)
     ;; ("defaultvalue0" . typescript-jsdoc-value)
     ("@defaultvalue {DefaultValueType}" . typescript-jsdoc-tag)
     ;; ("{DefaultValueType}" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/default-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @default tags and its alias."
  (jsdoc-multiline-test
   '(
     )
   '(
     )))

(ert-deftest jsdoc/deprecated-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @deprecated tags and its alias."
  (jsdoc-multiline-test
   '(
     "@deprecated"
     "@deprecated Some explanations about deprecation"
     )
   '(
     ("@deprecated$" . typescript-jsdoc-tag)
     ("@deprecated Some explanations about deprecation" . typescript-jsdoc-tag)
     ("Some explanations about deprecation" . font-lock-comment-face)
     ("explanations about deprecation" . font-lock-comment-face)
     ("about deprecation" . font-lock-comment-face)
     ("deprecation" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/deprecated-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @deprecated tags and its alias."
  (jsdoc-multiline-test
   '(
     "@deprecated {DeprecatedType}"
     )
   '(
     ("@deprecated {DeprecatedType}" . typescript-jsdoc-tag)
     ("{DeprecatedType}" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/deprecated-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @deprecated tags and its alias."
  (jsdoc-multiline-test
   '(
     )
   '(
     )))

(ert-deftest jsdoc/enum-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @enum tags and its alias."
  (jsdoc-multiline-test
   '(
     "@enum"
     "@enum {EnumItemType}"
     )
   '(
     ("@enum$" . typescript-jsdoc-tag)
     ("@enum {EnumItemType}" . typescript-jsdoc-tag)
     ("{EnumItemType}" . typescript-jsdoc-type)
     )))

(ert-deftest jsdoc/enum-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @enum tags and its alias."
  (jsdoc-multiline-test
   '(
     "@enum meaningless description0"
     "@enum {EnumItemType} meaningless description1"
     )
   '(
     ("@enum meaningless description0" . typescript-jsdoc-tag)
     ("meaningless description0" . font-lock-comment-face)
     ("description0" . font-lock-comment-face)
     ("@enum {EnumItemType} meaningless description1" . typescript-jsdoc-tag)
     ("{EnumItemType} meaningless description1" . typescript-jsdoc-type)
     ("meaningless description1" . font-lock-comment-face)
     ("description1" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/enum-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @enum tags and its alias."
  (jsdoc-multiline-test
   '(
     )
   '(
     )))

(ert-deftest jsdoc/event-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @event tags and its alias."
  (jsdoc-multiline-test
   '(
     "@event"
     "@event SomeClass#exampleEvent"
     )
   '(
     ("@event$" . typescript-jsdoc-tag)
     ("@event SomeClass#exampleEvent" . typescript-jsdoc-tag)
     ;; ("SomeClass#exampleEvent" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/event-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @event tags and its alias."
  (jsdoc-multiline-test
   '(
     "@event justText"
     "@event {EventType}"
     )
   '(
     ("@event justText" . typescript-jsdoc-tag)
     ;; ("justText" . typescript-jsdoc-value)
     ("@event {EventType}" . typescript-jsdoc-tag)
     ;; ("{EventType}" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/event-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @event tags and its alias."
  (jsdoc-multiline-test
   '(
     )
   '(
     )))

(ert-deftest jsdoc/example-tag-in-multiline-doc-comment-with-meaninglful-cases ()
  "Test for meaningful and valid usage of @example tags and its alias."
  (jsdoc-multiline-test
   '(
     "@example inlineExample with multiple terms0"
     "@example"
     "With multiline and multiple terms1"
     )
   '(
     ("@example inlineExample with multiple terms" . typescript-jsdoc-tag)
     ;; ("inlineExample with multiple terms" . typescript-jsdoc-value)
     ;; ("with multiple terms" . typescript-jsdoc-value)
     ;; ("multiple terms" . typescript-jsdoc-value)
     ;; ("terms" . typescript-jsdoc-value)
     ("@example$" . typescript-jsdoc-tag)
     ;; ("With multiline and multiple terms1" . typescript-jsdoc-value)
     ;; ("multiline and multiple terms1" . typescript-jsdoc-value)
     ;; ("and multiple terms1" . typescript-jsdoc-value)
     ;; ("multiple terms1" . typescript-jsdoc-value)
     ;; ("terms1" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/example-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @example tags and its alias."
  (jsdoc-multiline-test
   '(
     )
   '(
     )))

(ert-deftest jsdoc/example-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @example tags and its alias."
  (jsdoc-multiline-test
   '(
     "@example"
     )
   '(
     ;; ("@example$" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/exports-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @exports tags and its alias."
  (jsdoc-multiline-test
   '(
     "@exports some/module"
     )
   '(
     ("@exports some/module" . typescript-jsdoc-tag)
     ;; ("some/module" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/exports-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @exports tags and its alias."
  (jsdoc-multiline-test
   '(
     "@exports justText"
     "@exports {ExportType}"
     )
   '(
     ("@exports justText" . typescript-jsdoc-tag)
     ;; ("justText" . typescript-jsdoc-value)
     ("@exports {ExportType}" . typescript-jsdoc-tag)
     ;; ("{ExportType}" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/exports-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @exports tags and its alias."
  (jsdoc-multiline-test
   '(
     "@exports"
     )
   '(
     ;; ("@exports$" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/external-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @external tags and its alias."
  (jsdoc-multiline-test
   '(
     "@external"
     "@external String"
     "@external \"jQuery.fn\""
     "@external {ExternalType0}"
     "@external {ExternalType1} ExternalValue"
     "@host"
     "@host Boolean"
     "@host \"jQuery.fn.pluginNamespace\""
     "@host {HostType0}"
     "@host {HostType1} HostValue"
     )
   '(
     ;; ("@external$" . typescript-jsdoc-tag)
     ("@external String" . typescript-jsdoc-tag)
     ("String" . typescript-jsdoc-value)
     ("@external \"jQuery.fn\"" . typescript-jsdoc-tag)
     ("\"jQuery.fn\"" . typescript-jsdoc-value)
     ("@external {ExternalType0}" . typescript-jsdoc-tag)
     ;; ("{ExternalType0}" . typescript-jsdoc-type)
     ("@external {ExternalType1} ExternalValue" . typescript-jsdoc-tag)
     ;; ("{ExternalType1} ExternalValue" . typescript-jsdoc-type)
     ;; ("ExternalValue" . typescript-jsdoc-value)
     ;; ("@host$" . typescript-jsdoc-tag)
     ("@host Boolean" . typescript-jsdoc-tag)
     ("Boolean" . typescript-jsdoc-value)
     ("@host \"jQuery.fn.pluginNamespace\"" . typescript-jsdoc-tag)
     ("\"jQuery.fn.pluginNamespace\"" . typescript-jsdoc-value)
     ("@host {HostType0}" . typescript-jsdoc-tag)
     ;; ("{HostType0}" . typescript-jsdoc-type)
     ("@host {HostType1} HostValue" . typescript-jsdoc-tag)
     ;; ("{HostType1} HostValue" . typescript-jsdoc-type)
     ;; ("HostValue" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/external-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @external tags and its alias."
  (jsdoc-multiline-test
   '(
     "@external multiple words0"
     "@host multiple words1"
     )
   '(
     ("@external multiple words0" . typescript-jsdoc-tag)
     ("multiple words0" . typescript-jsdoc-value)
     ;; ("words0" . typescript-jsdoc-value)
     ("@host multiple words1" . typescript-jsdoc-tag)
     ("multiple words1" . typescript-jsdoc-value)
     ;; ("words1" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/external-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @external tags and its alias."
  (jsdoc-multiline-test
   '(
     "@external"
     )
   '(
     ;; ("@external$" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/file-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @file tags and its alias."
  (jsdoc-multiline-test
   '(
     "@file"
     "@file description for file0"
     "@fileoverview"
     "@fileoverview description for file1"
     "@overview"
     "@overview description for file2"
     )
   '(
     ("@file$" . typescript-jsdoc-tag)
     ("@file description for file0" . typescript-jsdoc-tag)
     ("description for file0" . font-lock-comment-face)
     ("for file0" . font-lock-comment-face)
     ("file0" . font-lock-comment-face)
     ("@fileoverview$" . typescript-jsdoc-tag)
     ("@fileoverview description for file1" . typescript-jsdoc-tag)
     ("description for file1" . font-lock-comment-face)
     ("for file1" . font-lock-comment-face)
     ("file1" . font-lock-comment-face)
     ("@overview$" . typescript-jsdoc-tag)
     ("@overview description for file2" . typescript-jsdoc-tag)
     ("description for file2" . font-lock-comment-face)
     ("for file2" . font-lock-comment-face)
     ("file2" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/file-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @file tags and its alias."
  (jsdoc-multiline-test
   '(
     "@file {FileType}"
     "@fileoverview {FileOverviewType}"
     "@overview {OverviewType}"
     )
   '(
     ("@file {FileType}" . typescript-jsdoc-tag)
     ("{FileType}" . font-lock-comment-face)
     ("@fileoverview {FileOverviewType}" . typescript-jsdoc-tag)
     ("{FileOverviewType}" . font-lock-comment-face)
     ("@overview {OverviewType}" . typescript-jsdoc-tag)
     ("{OverviewType}" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/file-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @file tags and its alias."
  (jsdoc-multiline-test
   '(
     )
   '(
     )))

(ert-deftest jsdoc/fires-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @fires tags and its alias."
  (jsdoc-multiline-test
   '(
     "@fires SomeClass#eventName0"
     "@emits SomeClass#eventName1"
     )
   '(
     ("@fires SomeClass#eventName0" . typescript-jsdoc-tag)
     ("SomeClass#eventName0" . typescript-jsdoc-value)
     ("@emits SomeClass#eventName1" . typescript-jsdoc-tag)
     ("SomeClass#eventName1" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/fires-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @fires tags and its alias."
  (jsdoc-multiline-test
   '(
     "@fires just text"
     "@fires {FiresType}"
     "@emits any text"
     "@emits {EmitsType}"
     )
   '(
     ("@fires just text" . typescript-jsdoc-tag)
     ("just text" . typescript-jsdoc-value)
     ("text" . font-lock-comment-face)
     ("@fires {FiresType}" . typescript-jsdoc-tag)
     ("{FiresType}" . typescript-jsdoc-value)
     ("@emits any text" . typescript-jsdoc-tag)
     ("any text" . typescript-jsdoc-value)
     ("text" . font-lock-comment-face)
     ("@emits {EmitsType}" . typescript-jsdoc-tag)
     ("{EmitsType}" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/fires-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @fires tags and its alias."
  (jsdoc-multiline-test
   '(
     "@fires"
     "@emits"
     )
   '(
     ("@fires$" . font-lock-comment-face)
     ("@emits$" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/function-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @function tags and its alias."
  (jsdoc-multiline-test
   '(
     "@function"
     "@function functionName"
     "@func"
     "@func funcName"
     "@method"
     "@method methodName"
     )
   '(
     ("@function$" . typescript-jsdoc-tag)
     ("@function functionName" . typescript-jsdoc-tag)
     ("functionName" . typescript-jsdoc-value)
     ("@func$" . typescript-jsdoc-tag)
     ("@func funcName" . typescript-jsdoc-tag)
     ("funcName" . typescript-jsdoc-value)
     ("@method$" . typescript-jsdoc-tag)
     ("@method methodName" . typescript-jsdoc-tag)
     ("methodName" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/function-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @function tags and its alias."
  (jsdoc-multiline-test
   '(
     "@function multiple words for function0"
     "@function {FunctionType}"
     "@func multiple words for func0"
     "@func {FuncType}"
     "@method multiple words for method0"
     "@method {MethodType}"
     )
   '(
     ("@function multiple words for function0" . typescript-jsdoc-tag)
     ("multiple words for function0" . typescript-jsdoc-value)
     ("words for function0" . font-lock-comment-face)
     ("for function0" . font-lock-comment-face)
     ("function0" . font-lock-comment-face)
     ("@function {FunctionType}" . typescript-jsdoc-tag)
     ("{FunctionType}" . typescript-jsdoc-value)
     ("@func multiple words for func0" . typescript-jsdoc-tag)
     ("multiple words for func0" . typescript-jsdoc-value)
     ("words for func0" . font-lock-comment-face)
     ("for func0" . font-lock-comment-face)
     ("func0" . font-lock-comment-face)
     ("@func {FuncType}" . typescript-jsdoc-tag)
     ("{FuncType}" . typescript-jsdoc-value)
     ("@method multiple words for method0" . typescript-jsdoc-tag)
     ("multiple words for method0" . typescript-jsdoc-value)
     ("words for method0" . font-lock-comment-face)
     ("for method0" . font-lock-comment-face)
     ("method0" . font-lock-comment-face)
     ("@method {MethodType}" . typescript-jsdoc-tag)
     ("{MethodType}" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/function-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @function tags and its alias."
  (jsdoc-multiline-test
   '(
     )
   '(
     )))

(ert-deftest jsdoc/generator-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @generator tags and its alias."
  (jsdoc-multiline-test
   '(
     "@generator"
     )
   '(
     ("@generator$" . typescript-jsdoc-tag)
     )))

(ert-deftest jsdoc/generator-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @generator tags and its alias."
  (jsdoc-multiline-test
   '(
     )
   '(
     )))

(ert-deftest jsdoc/generator-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @generator tags and its alias."
  (jsdoc-multiline-test
   '(
     "@generator invalid extra values"
     "@generator {InvalidType}"
     )
   '(
     ("@generator invalid extra values" . typescript-jsdoc-tag)
     ("invalid extra values" . font-lock-comment-face)
     ("extra values" . font-lock-comment-face)
     ("values" . font-lock-comment-face)
     ("@generator {InvalidType}" . typescript-jsdoc-tag)
     ("{InvalidType}" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/global-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @global tags and its alias."
  (jsdoc-multiline-test
   '(
     "@global"
     )
   '(
     ("@global$" . typescript-jsdoc-tag)
     )))

(ert-deftest jsdoc/global-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @global tags and its alias."
  (jsdoc-multiline-test
   '(
     )
   '(
     )))

(ert-deftest jsdoc/global-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @global tags and its alias."
  (jsdoc-multiline-test
   '(
     "@global invalid extra values"
     "@global {InvalidType}"
     )
   '(
     ("@global invalid extra values" . typescript-jsdoc-tag)
     ("invalid extra values" . font-lock-comment-face)
     ("extra values" . font-lock-comment-face)
     ("values" . font-lock-comment-face)
     ("@global {InvalidType}" . typescript-jsdoc-tag)
     ("{InvalidType}" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/hideconstructor-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @hideconstructor tags and its alias."
  (jsdoc-multiline-test
   '(
     "@hideconstructor"
     )
   '(
     ("@hideconstructor$" . typescript-jsdoc-tag)
     )))

(ert-deftest jsdoc/hideconstructor-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @hideconstructor tags and its alias."
  (jsdoc-multiline-test
   '(
     )
   '(
     )))

(ert-deftest jsdoc/hideconstructor-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @hideconstructor tags and its alias."
  (jsdoc-multiline-test
   '(
     "@hideconstructor invalid extra values"
     "@hideconstructor {InvalidType}"
     )
   '(
     ("@hideconstructor invalid extra values" . typescript-jsdoc-tag)
     ("invalid extra values" . font-lock-comment-face)
     ("extra values" . font-lock-comment-face)
     ("values" . font-lock-comment-face)
     ("@hideconstructor {InvalidType}" . typescript-jsdoc-tag)
     ("{InvalidType}" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/ignore-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @ignore tags and its alias."
  (jsdoc-multiline-test
   '(
     "@ignore"
     )
   '(
     ("@ignore$" . typescript-jsdoc-tag)
     )))

(ert-deftest jsdoc/ignore-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @ignore tags and its alias."
  (jsdoc-multiline-test
   '(
     )
   '(
     )))

(ert-deftest jsdoc/ignore-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @ignore tags and its alias."
  (jsdoc-multiline-test
   '(
     "@ignore invalid extra values"
     "@ignore {InvalidType}"
     )
   '(
     ("@ignore invalid extra values" . typescript-jsdoc-tag)
     ("invalid extra values" . font-lock-comment-face)
     ("extra values" . font-lock-comment-face)
     ("values" . font-lock-comment-face)
     ("@ignore {InvalidType}" . typescript-jsdoc-tag)
     ("{InvalidType}" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/implements-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @implements tags and its alias."
  (jsdoc-multiline-test
   '(
     "@implements SomeInterface"
     "@implements {OtherInterface}"
     )
   '(
     ("@implements SomeInterface" . typescript-jsdoc-tag)
     ;; ("SomeInterface" . typescript-jsdoc-value)
     ("@implements {OtherInterface}" . typescript-jsdoc-tag)
     ("{OtherInterface}" . typescript-jsdoc-type)
     )))

(ert-deftest jsdoc/implements-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @implements tags and its alias."
  (jsdoc-multiline-test
   '(
     "@implements {Interface0} Interface1"
     )
   '(
     ("@implements {Interface0} Interface1" . typescript-jsdoc-tag)
     ("{Interface0} Interface1" . typescript-jsdoc-type)
     ("Interface1" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/implements-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @implements tags and its alias."
  (jsdoc-multiline-test
   '(
     "@implements"
     )
   '(
     ;; ("@implements$" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/inheritdoc-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @inheritdoc tags and its alias."
  (jsdoc-multiline-test
   '(
     "@inheritdoc"
     )
   '(
     ("@inheritdoc$" . typescript-jsdoc-tag)
     )))

(ert-deftest jsdoc/inheritdoc-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @inheritdoc tags and its alias."
  (jsdoc-multiline-test
   '(
     )
   '(
     )))

(ert-deftest jsdoc/inheritdoc-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @inheritdoc tags and its alias."
  (jsdoc-multiline-test
   '(
     "@inheritdoc invalid extra values"
     "@inheritdoc {InvalidType}"
     )
   '(
     ("@inheritdoc invalid extra values" . typescript-jsdoc-tag)
     ("invalid extra values" . font-lock-comment-face)
     ("extra values" . font-lock-comment-face)
     ("values" . font-lock-comment-face)
     ("@inheritdoc {InvalidType}" . typescript-jsdoc-tag)
     ("{InvalidType}" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/inner-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @inner tags and its alias."
  (jsdoc-multiline-test
   '(
     "@inner"
     )
   '(
     ("@inner$" . typescript-jsdoc-tag)
     )))

(ert-deftest jsdoc/inner-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @inner tags and its alias."
  (jsdoc-multiline-test
   '(
     "@inner extra values"
     "@inner {InnerType}"
     )
   '(
     ("@inner extra values" . typescript-jsdoc-tag)
     ("extra values" . font-lock-comment-face)
     ("values" . font-lock-comment-face)
     ("@inner {InnerType}" . typescript-jsdoc-tag)
     ("{InnerType}" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/inner-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @inner tags and its alias."
  (jsdoc-multiline-test
   '(
     )
   '(
     )))

(ert-deftest jsdoc/instance-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @instance tags and its alias."
  (jsdoc-multiline-test
   '(
     "@instance"
     )
   '(
     ("@instance$" . typescript-jsdoc-tag)
     )))

(ert-deftest jsdoc/instance-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @instance tags and its alias."
  (jsdoc-multiline-test
   '(
     "@instance extra values"
     "@instance {InstanceType}"
     )
   '(
     ("@instance extra values" . typescript-jsdoc-tag)
     ("extra values" . font-lock-comment-face)
     ("values" . font-lock-comment-face)
     ("@instance {InstanceType}" . typescript-jsdoc-tag)
     ("{InstanceType}" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/instance-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @instance tags and its alias."
  (jsdoc-multiline-test
   '(
     )
   '(
     )))

(ert-deftest jsdoc/interface-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @interface tags and its alias."
  (jsdoc-multiline-test
   '(
     "@interface"
     "@interface InterfaceName"
     )
   '(
     ("@interface$" . typescript-jsdoc-tag)
     ("@interface InterfaceName" . typescript-jsdoc-tag)
     ;; ("InterfaceName" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/interface-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @interface tags and its alias."
  (jsdoc-multiline-test
   '(
     "@interface extra values"
     "@interface {InterfaceType0}"
     "@interface {InterfaceType1} AndName"
     )
   '(
     ("@interface extra values" . typescript-jsdoc-tag)
     ;; ("extra values" . typescript-jsdoc-value)
     ("values" . font-lock-comment-face)
     ("@interface {InterfaceType0}" . typescript-jsdoc-tag)
     ;; ("{InterfaceType0}" . typescript-jsdoc-value)
     ("@interface {InterfaceType1} AndName" . typescript-jsdoc-tag)
     ;; ("{InterfaceType1} AndName" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/interface-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @interface tags and its alias."
  (jsdoc-multiline-test
   '(
     )
   '(
     )))

(ert-deftest jsdoc/lends-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @lends tags and its alias."
  (jsdoc-multiline-test
   '(
     "@lends"
     "@lends Something"
     "@lends Something.deeper"
     )
   '(
     ("@lends$" . typescript-jsdoc-tag)
     ("@lends Something" . typescript-jsdoc-tag)
     ;; ("Something" . typescript-jsdoc-value)
     ("@lends Something.deeper" . typescript-jsdoc-tag)
     ;; ("Something.deeper" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/lends-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @lends tags and its alias."
  (jsdoc-multiline-test
   '(
     "@lends {LendsType0}"
     "@lends {LendsType1} With.some.path"
     )
   '(
     ("@lends {LendsType0}" . typescript-jsdoc-tag)
     ;; ("{LendsType0}" . nil)
     ("@lends {LendsType1} With.some.path" . typescript-jsdoc-tag)
     ;; ("{LendsType1} With.some.path" . nil)
     ;; ("With.some.path" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/lends-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @lends tags and its alias."
  (jsdoc-multiline-test
   '(
     )
   '(
     )))

(ert-deftest jsdoc/license-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @license tags and its alias."
  (jsdoc-multiline-test
   '(
     "@license Some-License-Name"
     "@license"
     "Some long license texts"
     )
   '(
     ("@license Some-License-Name" . typescript-jsdoc-tag)
     ("Some-License-Name" . font-lock-comment-face)
     ("@license$" . typescript-jsdoc-tag)
     ("Some long license texts" . font-lock-comment-face)
     ("long license texts" . font-lock-comment-face)
     ("license texts" . font-lock-comment-face)
     ("texts" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/license-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @license tags and its alias."
  (jsdoc-multiline-test
   '(
     "@license {LicenseType0}"
     "@license {LicenseType1} With more text"
     )
   '(
     ("@license {LicenseType0}" . typescript-jsdoc-tag)
     ("{LicenseType0}" . font-lock-comment-face)
     ("@license {LicenseType1} With more text" . typescript-jsdoc-tag)
     ("{LicenseType1} With more text" . font-lock-comment-face)
     ("With more text" . font-lock-comment-face)
     ("more text" . font-lock-comment-face)
     ("text" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/license-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @license tags and its alias."
  (jsdoc-multiline-test
   '(
     "@license"
     )
   '(
     ;; ("@license$" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/listens-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @listens tags and its alias."
  (jsdoc-multiline-test
   '(
     "@listens event:nameOfEvent"
     )
   '(
     ("@listens event:nameOfEvent" . typescript-jsdoc-tag)
     ("event:nameOfEvent" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/listens-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @listens tags and its alias."
  (jsdoc-multiline-test
   '(
     "@listens extra text0"
     "@listens {ListenType0}"
     "@listens {ListenType1} and text1"
     )
   '(
     ("@listens extra text0" . typescript-jsdoc-tag)
     ("extra text0" . typescript-jsdoc-value)
     ;; ("text0" . typescript-jsdoc-value)
     ("@listens {ListenType0}" . typescript-jsdoc-tag)
     ("{ListenType0}" . typescript-jsdoc-value)
     ("@listens {ListenType1} and text1" . typescript-jsdoc-tag)
     ("{ListenType1} and text1" . typescript-jsdoc-value)
     ;; ("and text1" . typescript-jsdoc-value)
     ;; ("text1" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/listens-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @listens tags and its alias."
  (jsdoc-multiline-test
   '(
     "@listens"
     )
   '(
     ("@listens$" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/member-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @member tags and its alias."
  (jsdoc-multiline-test
   '(
     "@member"
     "@member {MemberType0}"
     "@member memberName0"
     "@member {MemberType1} memberName1"
     "@var"
     "@var {VarType0}"
     "@var varName0"
     "@var {VarType1} varName1"
     )
   '(
     ;; ("@member$" . typescript-jsdoc-tag)
     ("@member {MemberType0}" . typescript-jsdoc-tag)
     ;; ("{MemberType0}" . typescript-jsdoc-type)
     ("@member memberName0" . typescript-jsdoc-tag)
     ("memberName0" . typescript-jsdoc-value)
     ("@member {MemberType1} memberName1" . typescript-jsdoc-tag)
     ;; ("{MemberType1} memberName1" . typescript-jsdoc-type)
     ;; ("memberName1" . typescript-jsdoc-value)
     ;; ("@var$" . typescript-jsdoc-tag)
     ("@var {VarType0}" . typescript-jsdoc-tag)
     ;; ;; ("{VarType0}" . typescript-jsdoc-type)
     ("@var varName0" . typescript-jsdoc-tag)
     ("varName0" . typescript-jsdoc-value)
     ("@var {VarType1} varName1" . typescript-jsdoc-tag)
     ;; ("{VarType1} varName1" . typescript-jsdoc-type)
     ;; ("varName1" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/member-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @member tags and its alias."
  (jsdoc-multiline-test
   '(
     "@member extra value0"
     "@var extra value1"
     )
   '(
     ("@member extra value0" . typescript-jsdoc-tag)
     ("extra value0" . typescript-jsdoc-value)
     ("value0" . font-lock-comment-face)
     ("@var extra value1" . typescript-jsdoc-tag)
     ("extra value1" . typescript-jsdoc-value)
     ("value1" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/member-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @member tags and its alias."
  (jsdoc-multiline-test
   '(
     )
   '(
     )))

(ert-deftest jsdoc/memberof-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @memberof tags and its alias."
  (jsdoc-multiline-test
   '(
     "@memberof SomeClass0"
     "@memberof SomeClass1#"
     "@memberof <global>"
     "@memberof! OtherClass0"
     "@memberof! OtherClass1#"
     ;; "@memberof! <global>" is valid, but it is hard to test.
     )
   '(
     ("@memberof SomeClass0" . typescript-jsdoc-tag)
     ("SomeClass0" . typescript-jsdoc-value)
     ("@memberof SomeClass1#" . typescript-jsdoc-tag)
     ("SomeClass1#" . typescript-jsdoc-value)
     ("@memberof <global>" . typescript-jsdoc-tag)
     ("<global>" . typescript-jsdoc-value)
     ;; ("@memberof! OtherClass0" . typescript-jsdoc-tag)
     ;; ("OtherClass0" . typescript-jsdoc-value)
     ;; ("@memberof! OtherClass1#" . typescript-jsdoc-tag)
     ;; ("OtherClass1#" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/memberof-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @memberof tags and its alias."
  (jsdoc-multiline-test
   '(
     "@memberof extra text0"
     "@memberof {MemberOfType0}"
     "@memberof {MemberOfType1} and text1"
     "@memberof! again string0"
     "@memberof! {MemberOfForcedType0}"
     "@memberof! {MemberOfForcedType1} and more string1"
     )
   '(
     ("@memberof extra text0" . typescript-jsdoc-tag)
     ("extra text0" . typescript-jsdoc-value)
     ;; ("text0" . typescript-jsdoc-value)
     ("@memberof {MemberOfType0}" . typescript-jsdoc-tag)
     ("{MemberOfType0}" . typescript-jsdoc-value)
     ("@memberof {MemberOfType1} and text1" . typescript-jsdoc-tag)
     ("{MemberOfType1} and text1" . typescript-jsdoc-value)
     ;; ("and text1" . typescript-jsdoc-value)
     ;; ("text1" . typescript-jsdoc-value)
     ;; ("@memberof! again string0" . typescript-jsdoc-tag)
     ;; ("again string0" . typescript-jsdoc-value)
     ;; ("string0" . typescript-jsdoc-value)
     ;; ("@memberof! {MemberOfForcedType0}" . typescript-jsdoc-tag)
     ;; ("{MemberOfForcedType0}" . typescript-jsdoc-value)
     ;; ("@memberof! {MemberOfForcedType1} and more string1" . typescript-jsdoc-tag)
     ;; ("{MemberOfForcedType1} and more string1" . typescript-jsdoc-value)
     ;; ("and more string1" . typescript-jsdoc-value)
     ;; ("more string1" . typescript-jsdoc-value)
     ;; ("string1" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/memberof-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @memberof tags and its alias."
  (jsdoc-multiline-test
   '(
     "@memberof"
     "@memberof!"
     )
   '(
     ("@memberof$" . font-lock-comment-face)
     ("@memberof!$" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/mixes-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @mixes tags and its alias."
  (jsdoc-multiline-test
   '(
     "@mixes OtherObject"
     )
   '(
     ("@mixes OtherObject" . typescript-jsdoc-tag)
     ("OtherObject" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/mixes-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @mixes tags and its alias."
  (jsdoc-multiline-test
   '(
     "@mixes extra text0"
     "@mixes {MixesType0}"
     "@mixes {MixesType1} and text1"
     )
   '(
     ("@mixes extra text0" . typescript-jsdoc-tag)
     ("extra text0" . typescript-jsdoc-value)
     ("text0" . font-lock-comment-face)
     ("@mixes {MixesType0}" . typescript-jsdoc-tag)
     ("{MixesType0}" . typescript-jsdoc-value)
     ("@mixes {MixesType1} and text1" . typescript-jsdoc-tag)
     ("{MixesType1} and text1" . typescript-jsdoc-value)
     ("and text1" . font-lock-comment-face)
     ("text1" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/mixes-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @mixes tags and its alias."
  (jsdoc-multiline-test
   '(
     "@mixes"
     )
   '(
     ("@mixes$" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/mixin-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @mixin tags and its alias."
  (jsdoc-multiline-test
   '(
     "@mixin"
     "@mixin MixinName"
     )
   '(
     ("@mixin$" . typescript-jsdoc-tag)
     ("@mixin MixinName" . typescript-jsdoc-tag)
     ;; ("MixinName" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/mixin-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @mixin tags and its alias."
  (jsdoc-multiline-test
   '(
     "@mixin and extra text0"
     "@mixin {MixinType0}"
     "@mixin {MixinType1} with more text1"
     )
   '(
     ("@mixin and extra text0" . typescript-jsdoc-tag)
     ;; ("and extra text0" . typescript-jsdoc-value)
     ("extra text0" . font-lock-comment-face)
     ("text0" . font-lock-comment-face)
     ("@mixin {MixinType0}" . typescript-jsdoc-tag)
     ;; ("{MixinType0}" . typescript-jsdoc-value)
     ("@mixin {MixinType1} with more text1" . typescript-jsdoc-tag)
     ;; ("{MixinType1} with more text1" . typescript-jsdoc-value)
     ("with more text1" . font-lock-comment-face)
     ("more text1" . font-lock-comment-face)
     ("text1" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/mixin-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @mixin tags and its alias."
  (jsdoc-multiline-test
   '(
     )
   '(
     )))

;; @modified tag is not yet released (It is a new tag of JSDoc 3.6.0)
(ert-deftest jsdoc/modifies-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @modifies tags and its alias."
  (jsdoc-multiline-test
   '(
     "@modifies"
     "@modifies {ModifiedType}"
     )
   '(
     ;; ("@modifies$" . typescript-jsdoc-tag)
     ;; ("@modifies {ModifiedType}" . typescript-jsdoc-tag)
     ;; ("{ModifiedType}" . typescript-jsdoc-type)
     )))

(ert-deftest jsdoc/modifies-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @modifies tags and its alias."
  (jsdoc-multiline-test
   '(
     "@modifies name0"
     "@modifies {ModifiedType} and name1"
     )
   '(
     ;; ("@modifies name0" . typescript-jsdoc-tag)
     ;; ("name0" . font-lock-comment-face)
     ;; ("@modifies {ModifiedType} and name1" . typescript-jsdoc-tag)
     ;; ("{ModifiedType} and name1" . typescript-jsdoc-type)
     ;; ("and name1" . font-lock-comment-face)
     ;; ("name1" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/modifies-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @modifies tags and its alias."
  (jsdoc-multiline-test
   '(
     "@modifies"
     )
   '(
     ("@modifies$" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/module-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @module tags and its alias."
  (jsdoc-multiline-test
   '(
     "@module"
     "@module ModuleName0"
     "@module {ModuleType0}"
     "@module {ModuleType1} ModuleName1"
     )
   '(
     ;; ("@module$" . typescript-jsdoc-tag)
     ("@module ModuleName0" . typescript-jsdoc-tag)
     ("ModuleName0" . typescript-jsdoc-value)
     ("@module {ModuleType0}" . typescript-jsdoc-tag)
     ;; ("{ModuleType0}" . typescript-jsdoc-type)
     ("@module {ModuleType1} ModuleName1" . typescript-jsdoc-tag)
     ;; ("{ModuleType1} ModuleName1" . typescript-jsdoc-type)
     ;; ("ModuleName1" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/module-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @module tags and its alias."
  (jsdoc-multiline-test
   '(
     "@module with multiple words0"
     "@module {ModuleType} with multiple words1"
     )
   '(
     ("@module with multiple words0" . typescript-jsdoc-tag)
     ("with multiple words0" . typescript-jsdoc-value)
     ("multiple words0" . font-lock-comment-face)
     ("words0" . font-lock-comment-face)
     ("@module {ModuleType} with multiple words1" . typescript-jsdoc-tag)
     ;; ("{ModuleType} with multiple words1" . typescript-jsdoc-type)
     ;; ("with multiple words1" . typescript-jsdoc-value)
     ("multiple words1" . font-lock-comment-face)
     ("words1" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/module-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @module tags and its alias."
  (jsdoc-multiline-test
   '(
     )
   '(
     )))

(ert-deftest jsdoc/namespace-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @namespace tags and its alias."
  (jsdoc-multiline-test
   '(
     "@namespace"
     "@namespace Namespace0"
     "@namespace {NamespaceType0}"
     "@namespace {NamespaceType1} Namespace1"
     )
   '(
     ;; ("@namespace$" . typescript-jsdoc-tag)
     ("@namespace Namespace0" . typescript-jsdoc-tag)
     ("Namespace0" . typescript-jsdoc-value)
     ("@namespace {NamespaceType0}" . typescript-jsdoc-tag)
     ;; ("{NamespaceType0}" . typescript-jsdoc-type)
     ("@namespace {NamespaceType1} Namespace1" . typescript-jsdoc-tag)
     ;; ("{NamespaceType1} Namespace1" . typescript-jsdoc-type)
     ;; ("Namespace1" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/namespace-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @namespace tags and its alias."
  (jsdoc-multiline-test
   '(
     "@namespace with multiple words0"
     "@namespace {NamespaceType} with multiple words1"
     )
   '(
     ("@namespace with multiple words0" . typescript-jsdoc-tag)
     ("with multiple words0" . typescript-jsdoc-value)
     ("multiple words0" . font-lock-comment-face)
     ("words0" . font-lock-comment-face)
     ("@namespace {NamespaceType} with multiple words1" . typescript-jsdoc-tag)
     ;; ("{NamespaceType} with multiple words1" . typescript-jsdoc-type)
     ;; ("with multiple words1" . typescript-jsdoc-value)
     ("multiple words1" . font-lock-comment-face)
     ("words1" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/namespace-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @namespace tags and its alias."
  (jsdoc-multiline-test
   '(
     )
   '(
     )))

(ert-deftest jsdoc/package-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @package tags and its alias."
  (jsdoc-multiline-test
   '(
     "@package"
     )
   '(
     ("@package$" . typescript-jsdoc-tag)
     )))

(ert-deftest jsdoc/package-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @package tags and its alias."
  (jsdoc-multiline-test
   '(
     )
   '(
     )))

(ert-deftest jsdoc/package-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @package tags and its alias."
  (jsdoc-multiline-test
   '(
     "@package invalid extra values"
     "@package {InvalidType}"
     )
   '(
     ("@package invalid extra values" . typescript-jsdoc-tag)
     ("invalid extra values" . font-lock-comment-face)
     ("extra values" . font-lock-comment-face)
     ("values" . font-lock-comment-face)
     ("@package {InvalidType}" . typescript-jsdoc-tag)
     ("{InvalidType}" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/param-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @param tags and its alias."
  (jsdoc-multiline-test
   '(
     "@param"
     "@param {ParamType0}"
     "@param paramName0"
     "@param {ParamType1} paramName1"
     "@param {ParamType2} paramName2 with paramDescription2"
     "@arg"
     "@arg {ArgType0}"
     "@arg argName0"
     "@arg {ArgType1} argName1"
     "@arg {ArgType2} argName2 with argDescription2"
     "@argument"
     "@argument {ArgumentType0}"
     "@argument argumentName0"
     "@argument {ArgumentType1} argumentName1"
     "@argument {ArgumentType2} argumentName2 with argumentDescription2"
     )
   '(
     ("@param$" . typescript-jsdoc-tag)
     ("@param {ParamType0}" . typescript-jsdoc-tag)
     ;; ("{ParamType0}" . typescript-jsdoc-type)
     ("@param paramName0" . typescript-jsdoc-tag)
     ("paramName0" . typescript-jsdoc-value)
     ("@param {ParamType1} paramName1" . typescript-jsdoc-tag)
     ("{ParamType1} paramName1" . typescript-jsdoc-type)
     ("paramName1" . typescript-jsdoc-value)
     ("@param {ParamType2} paramName2 with paramDescription2" . typescript-jsdoc-tag)
     ("{ParamType2} paramName2 with paramDescription2" . typescript-jsdoc-type)
     ("paramName2 with paramDescription2" . typescript-jsdoc-value)
     ("with paramDescription2" . font-lock-comment-face)
     ("paramDescription2" . font-lock-comment-face)
     ("@arg$" . typescript-jsdoc-tag)
     ("@arg {ArgType0}" . typescript-jsdoc-tag)
     ;; ("{ArgType0}" . typescript-jsdoc-type)
     ("@arg argName0" . typescript-jsdoc-tag)
     ("argName0" . typescript-jsdoc-value)
     ("@arg {ArgType1} argName1" . typescript-jsdoc-tag)
     ("{ArgType1} argName1" . typescript-jsdoc-type)
     ("argName1" . typescript-jsdoc-value)
     ("@arg {ArgType2} argName2 with argDescription2" . typescript-jsdoc-tag)
     ("{ArgType2} argName2 with argDescription2" . typescript-jsdoc-type)
     ("argName2 with argDescription2" . typescript-jsdoc-value)
     ("with argDescription2" . font-lock-comment-face)
     ("argDescription2" . font-lock-comment-face)
     ("@argument$" . typescript-jsdoc-tag)
     ("@argument {ArgumentType0}" . typescript-jsdoc-tag)
     ;; ("{ArgumentType0}" . typescript-jsdoc-type)
     ("@argument argumentName0" . typescript-jsdoc-tag)
     ("argumentName0" . typescript-jsdoc-value)
     ("@argument {ArgumentType1} argumentName1" . typescript-jsdoc-tag)
     ("{ArgumentType1} argumentName1" . typescript-jsdoc-type)
     ("argumentName1" . typescript-jsdoc-value)
     ("@argument {ArgumentType2} argumentName2 with argumentDescription2" . typescript-jsdoc-tag)
     ("{ArgumentType2} argumentName2 with argumentDescription2" . typescript-jsdoc-type)
     ("argumentName2 with argumentDescription2" . typescript-jsdoc-value)
     ("with argumentDescription2" . font-lock-comment-face)
     ("argumentDescription2" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/param-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @param tags and its alias."
  (jsdoc-multiline-test
   '(
     "@param extra value0"
     "@arg extra value1"
     "@argument extra value2"
     )
   '(
     ("@param extra value0" . typescript-jsdoc-tag)
     ("extra value0" . typescript-jsdoc-value)
     ("value0" . font-lock-comment-face)
     ("@arg extra value1" . typescript-jsdoc-tag)
     ("extra value1" . typescript-jsdoc-value)
     ("value1" . font-lock-comment-face)
     ("@argument extra value2" . typescript-jsdoc-tag)
     ("extra value2" . typescript-jsdoc-value)
     ("value2" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/param-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @param tags and its alias."
  (jsdoc-multiline-test
   '(
     )
   '(
     )))

(ert-deftest jsdoc/private-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @private tags and its alias."
  (jsdoc-multiline-test
   '(
     "@private"
     )
   '(
     ("@private$" . typescript-jsdoc-tag)
     )))

(ert-deftest jsdoc/private-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @private tags and its alias."
  (jsdoc-multiline-test
   '(
     )
   '(
     )))

(ert-deftest jsdoc/private-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @private tags and its alias."
  (jsdoc-multiline-test
   '(
     "@private invalid extra values"
     "@private {InvalidType}"
     )
   '(
     ("@private invalid extra values" . typescript-jsdoc-tag)
     ("invalid extra values" . font-lock-comment-face)
     ("extra values" . font-lock-comment-face)
     ("values" . font-lock-comment-face)
     ("@private {InvalidType}" . typescript-jsdoc-tag)
     ("{InvalidType}" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/property-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @property tags and its alias."
  (jsdoc-multiline-test
   '(
     "@property {PropertyType0}"
     "@property propertyName0"
     "@property {PropertyType1} propertyName1"
     "@property {PropertyType2} propertyName2 with propertyDescription2"
     "@prop"
     "@prop {PropType0}"
     "@prop propName0"
     "@prop {PropType1} propName1"
     "@prop {PropType2} propName2 with propDescription2"
     )
   '(
     ("@property {PropertyType0}" . typescript-jsdoc-tag)
     ;; ("{PropertyType0}" . typescript-jsdoc-type)
     ("@property propertyName0" . typescript-jsdoc-tag)
     ("propertyName0" . typescript-jsdoc-value)
     ("@property {PropertyType1} propertyName1" . typescript-jsdoc-tag)
     ("{PropertyType1} propertyName1" . typescript-jsdoc-type)
     ("propertyName1" . typescript-jsdoc-value)
     ("@property {PropertyType2} propertyName2 with propertyDescription2" . typescript-jsdoc-tag)
     ("{PropertyType2} propertyName2 with propertyDescription2" . typescript-jsdoc-type)
     ("propertyName2 with propertyDescription2" . typescript-jsdoc-value)
     ("with propertyDescription2" . font-lock-comment-face)
     ("propertyDescription2" . font-lock-comment-face)
     ("@prop {PropType0}" . typescript-jsdoc-tag)
     ;; ("{PropType0}" . typescript-jsdoc-type)
     ("@prop propName0" . typescript-jsdoc-tag)
     ("propName0" . typescript-jsdoc-value)
     ("@prop {PropType1} propName1" . typescript-jsdoc-tag)
     ("{PropType1} propName1" . typescript-jsdoc-type)
     ("propName1" . typescript-jsdoc-value)
     ("@prop {PropType2} propName2 with propDescription2" . typescript-jsdoc-tag)
     ("{PropType2} propName2 with propDescription2" . typescript-jsdoc-type)
     ("propName2 with propDescription2" . typescript-jsdoc-value)
     ("with propDescription2" . font-lock-comment-face)
     ("propDescription2" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/property-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @property tags and its alias."
  (jsdoc-multiline-test
   '(
     )
   '(
     )))

(ert-deftest jsdoc/property-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @property tags and its alias."
  (jsdoc-multiline-test
   '(
     "@property"
     "@prop"
     )
   '(
     ;; ("@property$" . font-lock-comment-face)
     ;; ("@prop$" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/protected-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @protected tags and its alias."
  (jsdoc-multiline-test
   '(
     "@protected"
     )
   '(
     ("@protected$" . typescript-jsdoc-tag)
     )))

(ert-deftest jsdoc/protected-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @protected tags and its alias."
  (jsdoc-multiline-test
   '(
     )
   '(
     )))

(ert-deftest jsdoc/protected-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @protected tags and its alias."
  (jsdoc-multiline-test
   '(
     "@protected invalid extra values"
     "@protected {InvalidType}"
     )
   '(
     ("@protected invalid extra values" . typescript-jsdoc-tag)
     ("invalid extra values" . font-lock-comment-face)
     ("extra values" . font-lock-comment-face)
     ("values" . font-lock-comment-face)
     ("@protected {InvalidType}" . typescript-jsdoc-tag)
     ("{InvalidType}" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/public-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @public tags and its alias."
  (jsdoc-multiline-test
   '(
     "@public"
     )
   '(
     ("@public$" . typescript-jsdoc-tag)
     )))

(ert-deftest jsdoc/public-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @public tags and its alias."
  (jsdoc-multiline-test
   '(
     )
   '(
     )))

(ert-deftest jsdoc/public-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @public tags and its alias."
  (jsdoc-multiline-test
   '(
     "@public invalid extra values"
     "@public {InvalidType}"
     )
   '(
     ("@public invalid extra values" . typescript-jsdoc-tag)
     ("invalid extra values" . font-lock-comment-face)
     ("extra values" . font-lock-comment-face)
     ("values" . font-lock-comment-face)
     ("@public {InvalidType}" . typescript-jsdoc-tag)
     ("{InvalidType}" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/readonly-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @readonly tags and its alias."
  (jsdoc-multiline-test
   '(
     "@readonly"
     )
   '(
     ("@readonly$" . typescript-jsdoc-tag)
     )))

(ert-deftest jsdoc/readonly-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @readonly tags and its alias."
  (jsdoc-multiline-test
   '(
     )
   '(
     )))

(ert-deftest jsdoc/readonly-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @readonly tags and its alias."
  (jsdoc-multiline-test
   '(
     "@readonly invalid extra values"
     "@readonly {InvalidType}"
     )
   '(
     ("@readonly invalid extra values" . typescript-jsdoc-tag)
     ("invalid extra values" . font-lock-comment-face)
     ("extra values" . font-lock-comment-face)
     ("values" . font-lock-comment-face)
     ("@readonly {InvalidType}" . typescript-jsdoc-tag)
     ("{InvalidType}" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/requires-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @requires tags and its alias."
  (jsdoc-multiline-test
   '(
     "@requires SomeModuleName"
     "@requires module:path/to/module"
     "@requires {@link something}"
     )
   '(
     ("@requires SomeModuleName" . typescript-jsdoc-tag)
     ("SomeModuleName" . typescript-jsdoc-value)
     ("@requires module:path/to/module" . typescript-jsdoc-tag)
     ("module:path/to/module" . typescript-jsdoc-value)
     ("@requires {@link something}" . typescript-jsdoc-tag)
     ("{@link something}" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/requires-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @requires tags and its alias."
  (jsdoc-multiline-test
   '(
     "@requires extra text0"
     "@requires {RequiresType0}"
     "@requires {RequiresType1} and text1"
     )
   '(
     ("@requires extra text0" . typescript-jsdoc-tag)
     ("extra text0" . typescript-jsdoc-value)
     ("text0" . font-lock-comment-face)
     ("@requires {RequiresType0}" . typescript-jsdoc-tag)
     ("{RequiresType0}" . typescript-jsdoc-value)
     ("@requires {RequiresType1} and text1" . typescript-jsdoc-tag)
     ("{RequiresType1} and text1" . typescript-jsdoc-value)
     ("and text1" . font-lock-comment-face)
     ("text1" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/requires-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @requires tags and its alias."
  (jsdoc-multiline-test
   '(
     "@requires"
     )
   '(
     ;; ("@requires$" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/returns-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @returns tags and its alias."
  (jsdoc-multiline-test
   '(
     "@returns description for returned value0"
     "@returns {ReturnType1} description for returned value1"
     "@return description for returned value2"
     "@return {ReturnType3} description for returned value3"
     )
   '(
     ("@returns description for returned value0" . typescript-jsdoc-tag)
     ("description for returned value0" . font-lock-comment-face)
     ("for returned value0" . font-lock-comment-face)
     ("returned value0" . font-lock-comment-face)
     ("value0" . font-lock-comment-face)
     ("@returns {ReturnType1} description for returned value1" . typescript-jsdoc-tag)
     ("{ReturnType1} description for returned value1" . typescript-jsdoc-type)
     ("description for returned value1" . font-lock-comment-face)
     ("for returned value1" . font-lock-comment-face)
     ("returned value1" . font-lock-comment-face)
     ("value1" . font-lock-comment-face)
     ("@return description for returned value2" . typescript-jsdoc-tag)
     ("description for returned value2" . font-lock-comment-face)
     ("for returned value2" . font-lock-comment-face)
     ("returned value2" . font-lock-comment-face)
     ("value2" . font-lock-comment-face)
     ("@return {ReturnType3} description for returned value3" . typescript-jsdoc-tag)
     ("{ReturnType3} description for returned value3" . typescript-jsdoc-type)
     ("description for returned value3" . font-lock-comment-face)
     ("for returned value3" . font-lock-comment-face)
     ("returned value3" . font-lock-comment-face)
     ("value3" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/returns-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @returns tags and its alias."
  (jsdoc-multiline-test
   '(
     )
   '(
     )))

(ert-deftest jsdoc/returns-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @returns tags and its alias."
  (jsdoc-multiline-test
   '(
     "@returns"
     "@return"
     )
   '(
     ;; ("@returns$" . font-lock-comment-face)
     ;; ("@return$" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/see-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @see tags and its alias."
  (jsdoc-multiline-test
   '(
     "@see someName.path"
     "@see {@link something}"
     "@see any other text"
     )
   '(
     ;; ("@see someName.path" . typescript-jsdoc-tag)
     ;; ("someName.path" . typescript-jsdoc-value)
     ;; ("@see {@link something}" . typescript-jsdoc-tag)
     ;; ("{@link something}" . typescript-jsdoc-value)
     ;; ("@see any other text" . typescript-jsdoc-tag)
     ;; ("any other text" . typescript-jsdoc-value)
     ;; ("other text" . typescript-jsdoc-value)
     ;; ("text" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/see-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @see tags and its alias."
  (jsdoc-multiline-test
   '(
     "@see {SeeType0}"
     "@see {SeeType1} and text1"
     )
   '(
     ;; ("@see {SeeType0}" . typescript-jsdoc-tag)
     ;; ("{SeeType0}" . typescript-jsdoc-value)
     ;; ("@see {SeeType1} and text1" . typescript-jsdoc-tag)
     ;; ("{SeeType1} and text1" . typescript-jsdoc-value)
     ;; ("and text1" . typescript-jsdoc-value)
     ;; ("text1" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/see-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @see tags and its alias."
  (jsdoc-multiline-test
   '(
     "@see"
     )
   '(
     ("@see$" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/since-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @since tags and its alias."
  (jsdoc-multiline-test
   '(
     "@since 1.0.1"
     )
   '(
     ("@since 1.0.1" . typescript-jsdoc-tag)
     ("1.0.1" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/since-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @since tags and its alias."
  (jsdoc-multiline-test
   '(
     "@since some extra text0"
     "@since {SinceType0}"
     "@since {SinceType1} and text1"
     )
   '(
     ("@since some extra text0" . typescript-jsdoc-tag)
     ("some extra text0" . typescript-jsdoc-value)
     ("extra text0" . font-lock-comment-face)
     ("text0" . font-lock-comment-face)
     ("@since {SinceType0}" . typescript-jsdoc-tag)
     ("{SinceType0}" . typescript-jsdoc-value)
     ("@since {SinceType1} and text1" . typescript-jsdoc-tag)
     ("{SinceType1} and text1" . typescript-jsdoc-value)
     ("and text1" . font-lock-comment-face)
     ("text1" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/since-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @since tags and its alias."
  (jsdoc-multiline-test
   '(
     "@since"
     )
   '(
     ("@since$" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/static-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @static tags and its alias."
  (jsdoc-multiline-test
   '(
     "@static"
     )
   '(
     ("@static$" . typescript-jsdoc-tag)
     )))

(ert-deftest jsdoc/static-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @static tags and its alias."
  (jsdoc-multiline-test
   '(
     "@static extra values"
     "@static {StaticType}"
     )
   '(
     ("@static extra values" . typescript-jsdoc-tag)
     ("extra values" . font-lock-comment-face)
     ("values" . font-lock-comment-face)
     ("@static {StaticType}" . typescript-jsdoc-tag)
     ("{StaticType}" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/static-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @static tags and its alias."
  (jsdoc-multiline-test
   '(
     )
   '(
     )))

(ert-deftest jsdoc/summary-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @summary tags and its alias."
  (jsdoc-multiline-test
   '(
     "@summary some summary text"
     )
   '(
     ("@summary some summary text" . typescript-jsdoc-tag)
     ("some summary text" . font-lock-comment-face)
     ("summary text" . font-lock-comment-face)
     ("text" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/summary-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @summary tags and its alias."
  (jsdoc-multiline-test
   '(
     "@summary {SummaryType0}"
     "@summary {SummaryType1} and text1"
     )
   '(
     ("@summary {SummaryType0}" . typescript-jsdoc-tag)
     ("{SummaryType0}" . font-lock-comment-face)
     ("@summary {SummaryType1} and text1" . typescript-jsdoc-tag)
     ("{SummaryType1} and text1" . font-lock-comment-face)
     ("and text1" . font-lock-comment-face)
     ("text1" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/summary-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @summary tags and its alias."
  (jsdoc-multiline-test
   '(
     "@summary"
     )
   '(
     ;; ("@summary$" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/this-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @this tags and its alias."
  (jsdoc-multiline-test
   '(
     "@this ClassForThis"
     )
   '(
     ("@this ClassForThis" . typescript-jsdoc-tag)
     ("ClassForThis" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/this-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @this tags and its alias."
  (jsdoc-multiline-test
   '(
     "@this and multiple words0"
     "@this {SummaryType0}"
     "@this {SummaryType1} and multiple words1"
     )
   '(
     ("@this and multiple words0" . typescript-jsdoc-tag)
     ("and multiple words0" . typescript-jsdoc-value)
     ("multiple words0" . font-lock-comment-face)
     ("words0" . font-lock-comment-face)
     ("@this {SummaryType0}" . typescript-jsdoc-tag)
     ("{SummaryType0}" . typescript-jsdoc-value)
     ("@this {SummaryType1} and multiple words1" . typescript-jsdoc-tag)
     ("{SummaryType1} and multiple words1" . typescript-jsdoc-value)
     ("and multiple words1" . font-lock-comment-face)
     ("multiple words1" . font-lock-comment-face)
     ("words1" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/this-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @this tags and its alias."
  (jsdoc-multiline-test
   '(
     "@this"
     )
   '(
     ("@this$" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/todo-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @todo tags and its alias."
  (jsdoc-multiline-test
   '(
     "@todo todo things"
     "@todo more things to do!"
     )
   '(
     ("@todo todo things" . typescript-jsdoc-tag)
     ("todo things" . font-lock-comment-face)
     ("things" . font-lock-comment-face)
     ("@todo more things to do!" . typescript-jsdoc-tag)
     ("more things to do!" . font-lock-comment-face)
     ("things to do!" . font-lock-comment-face)
     ("to do!" . font-lock-comment-face)
     ("do!" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/todo-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @todo tags and its alias."
  (jsdoc-multiline-test
   '(
     "@todo {TodoType0}"
     "@todo {TodoType1} and text1"
     )
   '(
     ("@todo {TodoType0}" . typescript-jsdoc-tag)
     ("{TodoType0}" . font-lock-comment-face)
     ("@todo {TodoType1} and text1" . typescript-jsdoc-tag)
     ("{TodoType1} and text1" . font-lock-comment-face)
     ("and text1" . font-lock-comment-face)
     ("text1" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/todo-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @todo tags and its alias."
  (jsdoc-multiline-test
   '(
     "@todo"
     )
   '(
     ;; ("@todo$" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/throws-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @throws tags and its alias."
  (jsdoc-multiline-test
   '(
     "@throws explanation for throws cases0"
     "@throws {ThrowsType0}"
     "@throws {ThrowsType1} explanation for throws cases1"
     "@exception explanation for exception0"
     "@exception {ExceptionType0}"
     "@exception {ExceptionType1} explanation for exception1"
     )
   '(
     ("@throws explanation for throws cases0" . typescript-jsdoc-tag)
     ;; ("explanation for throws cases0" . font-lock-comment-face)
     ("for throws cases0" . font-lock-comment-face)
     ("throws cases0" . font-lock-comment-face)
     ("cases0" . font-lock-comment-face)
     ("@throws {ThrowsType0}" . typescript-jsdoc-tag)
     ;; ("{ThrowsType0}" . typescript-jsdoc-type)
     ("@throws {ThrowsType1} explanation for throws cases1" . typescript-jsdoc-tag)
     ;; ("{ThrowsType1} explanation for throws cases1" . typescript-jsdoc-type)
     ("explanation for throws cases1" . font-lock-comment-face)
     ("for throws cases1" . font-lock-comment-face)
     ("throws cases1" . font-lock-comment-face)
     ("cases1" . font-lock-comment-face)
     ("@exception explanation for exception0" . typescript-jsdoc-tag)
     ;; ("explanation for exception0" . font-lock-comment-face)
     ("for exception0" . font-lock-comment-face)
     ("exception0" . font-lock-comment-face)
     ("@exception {ExceptionType0}" . typescript-jsdoc-tag)
     ;; ("{ExceptionType0}" . typescript-jsdoc-type)
     ("@exception {ExceptionType1} explanation for exception1" . typescript-jsdoc-tag)
     ;; ("{ExceptionType1} explanation for exception1" . typescript-jsdoc-type)
     ("explanation for exception1" . font-lock-comment-face)
     ("for exception1" . font-lock-comment-face)
     ("exception1" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/throws-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @throws tags and its alias."
  (jsdoc-multiline-test
   '(
     )
   '(
     )))

(ert-deftest jsdoc/throws-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @throws tags and its alias."
  (jsdoc-multiline-test
   '(
     "@throws"
     "@exception"
     )
   '(
     ;; ("@throws$" . font-lock-comment-face)
     ;; ("@exception$" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/tutorial-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @tutorial tags and its alias."
  (jsdoc-multiline-test
   '(
     "@tutorial tutorial-file0"
     )
   '(
     ("@tutorial tutorial-file0" . typescript-jsdoc-tag)
     ;; ("tutorial-file0" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/tutorial-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @tutorial tags and its alias."
  (jsdoc-multiline-test
   '(
     "@tutorial multiple words0"
     "@tutorial {TutorialType0}"
     "@tutorial {TutorialType1} multiple words1"
     )
   '(
     ("@tutorial multiple words0" . typescript-jsdoc-tag)
     ;; ("multiple words0" . typescript-jsdoc-value)
     ("words0" . font-lock-comment-face)
     ("@tutorial {TutorialType0}" . typescript-jsdoc-tag)
     ;; ("{TutorialType0}" . typescript-jsdoc-value)
     ("@tutorial {TutorialType1} multiple words1" . typescript-jsdoc-tag)
     ;; ("{TutorialType1} multiple words1" . typescript-jsdoc-value)
     ("multiple words1" . font-lock-comment-face)
     ("words1" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/tutorial-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @tutorial tags and its alias."
  (jsdoc-multiline-test
   '(
     "@tutorial"
     )
   '(
     ;; ("@tutorial$" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/type-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @type tags and its alias."
  (jsdoc-multiline-test
   '(
     "@type TypeOfSomething0"
     "@type {TypeofSomething1}"
     )
   '(
     ("@type TypeOfSomething0" . typescript-jsdoc-tag)
     ;; ("TypeOfSomething0" . typescript-jsdoc-type)
     ("@type {TypeOfSomething1}" . typescript-jsdoc-tag)
     ("{TypeOfSomething1}" . typescript-jsdoc-type)
     )))

(ert-deftest jsdoc/type-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @type tags and its alias."
  (jsdoc-multiline-test
   '(
     "@type multiple words0"
     "@type {Type0} multiple words1"
     )
   '(
     ("@type multiple words0" . typescript-jsdoc-tag)
     ;; ("multiple words0" . typescript-jsdoc-type)
     ("words0" . font-lock-comment-face)
     ("@type {Type0} multiple words1" . typescript-jsdoc-tag)
     ("{Type0} multiple words1" . typescript-jsdoc-type)
     ("multiple words1" . font-lock-comment-face)
     ("words1" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/type-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @type tags and its alias."
  (jsdoc-multiline-test
   '(
     "@type"
     )
   '(
     ;; ("@type$" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/typedef-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @typedef tags and its alias."
  (jsdoc-multiline-test
   '(
     "@typedef nameOfNewType0"
     "@typedef namePath~OfNewType0"
     "@typedef {TypeOfNewType0} nameOfNewType1"
     "@typedef {TypeOfNewType1} namePath~OfNewType1"
     "@callback nameOfNewCallback"
     "@callback namePath~OfNewCallback"
     )
   '(
     ("@typedef nameOfNewType0" . typescript-jsdoc-tag)
     ("nameOfNewType0" . typescript-jsdoc-value)
     ("@typedef namePath~OfNewType0" . typescript-jsdoc-tag)
     ("namePath~OfNewType0" . typescript-jsdoc-value)
     ("OfNewType0" . typescript-jsdoc-value)
     ("@typedef {TypeOfNewType0} nameOfNewType1" . typescript-jsdoc-tag)
     ("{TypeOfNewType0} nameOfNewType1" . typescript-jsdoc-type)
     ("nameOfNewType1" . typescript-jsdoc-value)
     ("@typedef {TypeOfNewType1} namePath~OfNewType1" . typescript-jsdoc-tag)
     ("{TypeOfNewType1} namePath~OfNewType1" . typescript-jsdoc-type)
     ("namePath~OfNewType1" . typescript-jsdoc-value)
     ("OfNewType1" . typescript-jsdoc-value)
     ("@callback nameOfNewCallback" . typescript-jsdoc-tag)
     ("nameOfNewCallback" . typescript-jsdoc-value)
     ("@callback namePath~OfNewCallback" . typescript-jsdoc-tag)
     ("namePath~OfNewCallback" . typescript-jsdoc-value)
     ("OfNewCallback" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/typedef-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @typedef tags and its alias."
  (jsdoc-multiline-test
   '(
     "@typedef"
     "@typedef multiple words0"
     "@typedef {TypeOfNewType0}"
     "@typedef {TypeOfNewType1} multiple words1"
     "@callback"
     "@callback multiple words for callback0"
     "@callback {CallbackType0}"
     "@callback {CallbackType1} nameOfNewCallback"
     "@callback {CallbackType2} namePath~OfNewCallback"
     "@callback {CallbackType3} multiple words for callback1"
     )
   '(
     ("@typedef$" . typescript-jsdoc-tag)
     ("@typedef multiple words0" . typescript-jsdoc-tag)
     ("multiple words0" . typescript-jsdoc-value)
     ("words0" . font-lock-comment-face)
     ("@typedef {TypeOfNewType0}" . typescript-jsdoc-tag)
     ;; ("{TypeOfNewType0}" . typescript-jsdoc-type)
     ("@typedef {TypeOfNewType1} multiple words1" . typescript-jsdoc-tag)
     ("{TypeOfNewType1} multiple words1" . typescript-jsdoc-type)
     ("multiple words1" . typescript-jsdoc-value)
     ("words1" . font-lock-comment-face)
     ;; ("@callback$" . typescript-jsdoc-tag)
     ("@callback multiple words for callback0" . typescript-jsdoc-tag)
     ("multiple words for callback0" . typescript-jsdoc-value)
     ("words for callback0" . font-lock-comment-face)
     ("for callback0" . font-lock-comment-face)
     ("callback0" . font-lock-comment-face)
     ("@callback {CallbackType0}" . typescript-jsdoc-tag)
     ;; ("{CallbackType0}" . typescript-jsdoc-type)
     ("@callback {CallbackType1} nameOfNewCallback" . typescript-jsdoc-tag)
     ;; ("{CallbackType1} nameOfNewCallback" . typescript-jsdoc-type)
     ;; ("nameOfNewCallback" . typescript-jsdoc-value)
     ("@callback {CallbackType2} namePath~OfNewCallback" . typescript-jsdoc-tag)
     ;; ("{CallbackType2} namePath~OfNewCallback" . typescript-jsdoc-type)
     ;; ("namePath~OfNewCallback" . typescript-jsdoc-value)
     ;; ("OfNewCallback" . typescript-jsdoc-value)
     ("@callback {CallbackType3} multiple words for callback1" . typescript-jsdoc-tag)
     ;; ("{CallbackType3} multiple words for callback1" . typescript-jsdoc-type)
     ;; ("multiple words for callback1" . typescript-jsdoc-value)
     ("words for callback1" . font-lock-comment-face)
     ("for callback1" . font-lock-comment-face)
     ("callback1" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/typedef-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @typedef tags and its alias."
  (jsdoc-multiline-test
   '(
     )
   '(
     )))

(ert-deftest jsdoc/variation-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @variation tags and its alias."
  (jsdoc-multiline-test
   '(
     "@variation 0"
     "@variation (1)"
     )
   '(
     ("@variation 0" . typescript-jsdoc-tag)
     ("0" . typescript-jsdoc-value)
     ("@variation (1)" . typescript-jsdoc-tag)
     ("(1)" . typescript-jsdoc-value)
     ("1)" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/variation-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @variation tags and its alias."
  (jsdoc-multiline-test
   '(
     "@variation multiple words0"
     "@variation {VariationType0}"
     "@variation {VariationType1} multiple words1"
     )
   '(
     ("@variation multiple words0" . typescript-jsdoc-tag)
     ("multiple words0" . typescript-jsdoc-value)
     ("words0" . font-lock-comment-face)
     ("@variation {VariationType0}" . typescript-jsdoc-tag)
     ("{VariationType0}" . typescript-jsdoc-value)
     ("@variation {VariationType1} multiple words1" . typescript-jsdoc-tag)
     ("{VariationType1} multiple words1" . typescript-jsdoc-value)
     ("multiple words1" . font-lock-comment-face)
     ("words1" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/variation-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @variation tags and its alias."
  (jsdoc-multiline-test
   '(
     "@variation"
     )
   '(
     ("@variation$" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/version-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @version tags and its alias."
  (jsdoc-multiline-test
   '(
     "@version 1.2.3"
     "@version (4.7.1)"
     )
   '(
     ("@version 1.2.3" . typescript-jsdoc-tag)
     ("1.2.3" . typescript-jsdoc-value)
     ("2.3" . typescript-jsdoc-value)
     ("3" . typescript-jsdoc-value)
     ("@version (4.7.1)" . typescript-jsdoc-tag)
     ("(4.7.1)" . typescript-jsdoc-value)
     ("7.1)" . typescript-jsdoc-value)
     ("1)" . typescript-jsdoc-value)
     )))

(ert-deftest jsdoc/version-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @version tags and its alias."
  (jsdoc-multiline-test
   '(
     "@version multiple words0"
     "@version {VersionType0}"
     "@version {VersionType1} multiple words1"
     )
   '(
     ("@version multiple words0" . typescript-jsdoc-tag)
     ("multiple words0" . typescript-jsdoc-value)
     ("words0" . font-lock-comment-face)
     ("@version {VersionType0}" . typescript-jsdoc-tag)
     ("{VersionType0}" . typescript-jsdoc-value)
     ("@version {VersionType1} multiple words1" . typescript-jsdoc-tag)
     ("{VersionType1} multiple words1" . typescript-jsdoc-value)
     ("multiple words1" . font-lock-comment-face)
     ("words1" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/version-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @version tags and its alias."
  (jsdoc-multiline-test
   '(
     "@version"
     )
   '(
     ("@version$" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/yields-tag-in-multiline-doc-comment-with-meaningful-cases ()
  "Test for meaningful and valid usage of @yields tags and its alias."
  (jsdoc-multiline-test
   '(
     "@yields Text for yields0"
     "@yields {YieldsType0}"
     "@yields {YieldsType1} Text for yields1"
     "@yield Text for yield0"
     "@yield {YieldType0}"
     "@yield {YieldType1} Text for yield1"
     )
   '(
     ("@yields Text for yields0" . typescript-jsdoc-tag)
     ("Text for yields0" . font-lock-comment-face)
     ("for yields0" . font-lock-comment-face)
     ("yields0" . font-lock-comment-face)
     ("@yields {YieldsType0}" . typescript-jsdoc-tag)
     ("{YieldsType0}" . typescript-jsdoc-type)
     ("@yields {YieldsType1} Text for yields1" . typescript-jsdoc-tag)
     ("{YieldsType1} Text for yields1" . typescript-jsdoc-type)
     ("Text for yields1" . font-lock-comment-face)
     ("for yields1" . font-lock-comment-face)
     ("yields1" . font-lock-comment-face)
     ("@yield Text for yield0" . typescript-jsdoc-tag)
     ("Text for yield0" . font-lock-comment-face)
     ("for yield0" . font-lock-comment-face)
     ("yield0" . font-lock-comment-face)
     ("@yield {YieldType0}" . typescript-jsdoc-tag)
     ("{YieldType0}" . typescript-jsdoc-type)
     ("@yield {YieldType1} Text for yield1" . typescript-jsdoc-tag)
     ("{YieldType1} Text for yield1" . typescript-jsdoc-type)
     ("Text for yield1" . font-lock-comment-face)
     ("for yield1" . font-lock-comment-face)
     ("yield1" . font-lock-comment-face)
     )))

(ert-deftest jsdoc/yields-tag-in-multiline-doc-comment-with-meaningless-cases ()
  "Test for meaningless though valid usage of @yields tags and its alias."
  (jsdoc-multiline-test
   '(
     )
   '(
     )))

(ert-deftest jsdoc/yields-tag-in-multiline-doc-comment-with-invalid-cases ()
  "Test for invalid usage of @yields tags and its alias."
  (jsdoc-multiline-test
   '(
     "@yields"
     )
   '(
     ;; ("@yields$" . font-lock-comment-face)
     )))

;; Do we need to add closure tags?
;; See https://github.com/jsdoc3/jsdoc/blob/b21427343c7294bbf1f14c718a390f3e955e37cb/lib/jsdoc/tag/dictionary/definitions.js#L870-L1031

(provide 'typescript-mode-jsdoc-tests)
;;; typescript-mode-jsdoc-tests.el ends here
