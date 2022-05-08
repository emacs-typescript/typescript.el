;;; typescript-ts.el --- tree sitter support for Typescript  -*- lexical-binding: t; -*-

;; Copyright (C) Theodor Thornhill

;; Author     : Theodor Thornhill <theo@thornhill.no>
;; Maintainer : Theodor Thornhill <theo@thornhill.no>
;; Created    : April 2022
;; Modified   : 2022
;; Version    : 0.4
;; Keywords   : typescript languages
;; X-URL      : https://github.com/emacs-typescript/typescript.el
;; Package-Requires: ((emacs "26.1") (tree-sitter "0.12.1") (tree-sitter-indent "0.1") (tree-sitter-langs "0.9.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'treesit)


(defcustom typescript-ts-indent-offset 2
  "Number of spaces for each indentation step in `typescript-mode'."
  :type 'integer
  :safe 'integerp
  :group 'typescript)

(defvar typescript-ts-syntax-table
  (let ((table (make-syntax-table)))
    ;; Taken from the cc-langs version
    (modify-syntax-entry ?_  "_"     table)
    (modify-syntax-entry ?$ "_"      table)
    (modify-syntax-entry ?\\ "\\"    table)
    (modify-syntax-entry ?+  "."     table)
    (modify-syntax-entry ?-  "."     table)
    (modify-syntax-entry ?=  "."     table)
    (modify-syntax-entry ?%  "."     table)
    (modify-syntax-entry ?<  "."     table)
    (modify-syntax-entry ?>  "."     table)
    (modify-syntax-entry ?&  "."     table)
    (modify-syntax-entry ?|  "."     table)
    (modify-syntax-entry ?` "\""     table)
    (modify-syntax-entry ?\240 "."   table)
    table)
  "Syntax table for `typescript-ts-mode'.")

(defun ts-backward-up-list ()
  (lambda (node parent bol &rest _)
    (save-excursion
      (backward-up-list 1 nil t)
      (goto-char
       (treesit-node-start
        (treesit-node-at (point) (point) 'tsx)))
      (back-to-indentation)
      (treesit-node-start
       (treesit-node-at (point) (point) 'tsx)))))

(defvar typescript-ts-indent-rules
  `((tsx
     (no-node (ts-backward-up-list) ,typescript-ts-indent-offset)
     ((node-is "}") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is ">") parent-bol 0)
     ((node-is ".") parent-bol ,typescript-ts-indent-offset)
     ((parent-is "named_imports") parent-bol ,typescript-ts-indent-offset)
     ((parent-is "statement_block") parent-bol ,typescript-ts-indent-offset)
     ((parent-is "type_arguments") parent-bol ,typescript-ts-indent-offset)
     ((parent-is "variable_declarator") parent-bol ,typescript-ts-indent-offset)
     ((parent-is "arguments") parent-bol ,typescript-ts-indent-offset)
     ((parent-is "array") parent-bol ,typescript-ts-indent-offset)
     ((parent-is "formal_parameters") parent-bol ,typescript-ts-indent-offset)
     ((parent-is "template_substitution") parent-bol ,typescript-ts-indent-offset)
     ((parent-is "object_pattern") parent-bol ,typescript-ts-indent-offset)
     ((parent-is "object") parent-bol ,typescript-ts-indent-offset)
     ((parent-is "object_type") parent-bol ,typescript-ts-indent-offset)
     ((parent-is "enum_body") parent-bol ,typescript-ts-indent-offset)
     ((parent-is "arrow_function") parent-bol ,typescript-ts-indent-offset)
     ((parent-is "parenthesized_expression") parent-bol ,typescript-ts-indent-offset)
     
     ;; JSX
     ((parent-is "jsx_opening_element") parent ,typescript-ts-indent-offset)
     ((node-is "jsx_closing_element") parent 0)
     ((parent-is "jsx_element") parent ,typescript-ts-indent-offset)
     ;; TODO(Theo): This one is a little off.  Meant to hit the dangling '/' in
     ;; a jsx-element.  But it is also division operator...
     ((node-is "/") parent 0)
     ((parent-is "jsx_self_closing_element") parent ,typescript-ts-indent-offset))))

(defvar typescript-ts-font-lock-settings-1
  '((tsx
     (
      ((identifier) @font-lock-constant-face
       (:match "^[A-Z_][A-Z_\\d]*$" @font-lock-constant-face))

      (nested_type_identifier module: (identifier) @font-lock-type-face)
      (type_identifier) @font-lock-type-face
      (predefined_type) @font-lock-type-face

      (new_expression
       constructor: (identifier) @font-lock-type-face)

      (function
       name: (identifier) @font-lock-function-name-face)

      (function_declaration
       name: (identifier) @font-lock-function-name-face)

      (method_definition
       name: (property_identifier) @font-lock-function-name-face)

      (variable_declarator
       name: (identifier) @font-lock-function-name-face
       value: [(function) (arrow_function)])

      (variable_declarator
       name: (array_pattern (identifier) (identifier) @font-lock-function-name-face)
       value: (array (number) (function)))

      (assignment_expression
       left: [(identifier) @font-lock-function-name-face
              (member_expression property: (property_identifier) @font-lock-function-name-face)]
       right: [(function) (arrow_function)])

      (call_expression
       function: [(identifier) @font-lock-function-name-face
                  (member_expression
                   property: (property_identifier) @font-lock-function-name-face)])

      (variable_declarator
       name: (identifier) @font-lock-variable-name-face)

      (enum_declaration (identifier) @font-lock-type-face)

      (enum_body (property_identifier) @font-lock-type-face)

      (enum_assignment name: (property_identifier) @font-lock-type-face)

      (assignment_expression
       left: [(identifier) @font-lock-variable-name-face
              (member_expression property: (property_identifier) @font-lock-variable-name-face)])

      (for_in_statement
       left: (identifier) @font-lock-variable-name-face)

      (arrow_function
       parameter: (identifier) @font-lock-variable-name-face)

      (arrow_function
       parameters: [(_ (identifier) @font-lock-variable-name-face)
                    (_ (_ (identifier) @font-lock-variable-name-face))
                    (_ (_ (_ (identifier) @font-lock-variable-name-face)))])


      (pair key: (property_identifier) @font-lock-variable-name-face)

      (pair value: (identifier) @font-lock-variable-name-face)

      (pair
       key: (property_identifier) @font-lock-function-name-face
       value: [(function) (arrow_function)])

      (property_signature name: (property_identifier) @font-lock-variable-name-face)

      ((shorthand_property_identifier) @font-lock-variable-name-face)

      (pair_pattern key: (property_identifier) @font-lock-variable-name-face)

      ((shorthand_property_identifier_pattern) @font-lock-variable-name-face)

      (array_pattern (identifier) @font-lock-variable-name-face)

      (jsx_opening_element [(nested_identifier (identifier)) (identifier)] @font-lock-function-name-face)
      (jsx_closing_element [(nested_identifier (identifier)) (identifier)] @font-lock-function-name-face)
      (jsx_self_closing_element [(nested_identifier (identifier)) (identifier)] @font-lock-function-name-face)
      (jsx_attribute (property_identifier) @font-lock-constant-face)

      [(this) (super)] @font-lock-keyword-face
      
      [(true) (false) (null)] @font-lock-constant-face
      ;; (regex pattern: (regex_pattern))
      (number) @font-lock-constant-face

      (string) @font-lock-string-face

      ;; template strings need to be last in the file for embedded expressions
      ;; to work properly
      (template_string) @font-lock-string-face

      (template_substitution
       "${" @font-lock-constant-face
       (_)
       "}" @font-lock-constant-face
       )

      ["!"
       "abstract"
       "as"
       "async"
       "await"
       "break"
       "case"
       "catch"
       "class"
       "const"
       "continue"
       "debugger"
       "declare"
       "default"
       "delete"
       "do"
       "else"
       "enum"
       "export"
       "extends"
       "finally"
       "for"
       "from"
       "function"
       "get"
       "if"
       "implements"
       "import"
       "in"
       "instanceof"
       "interface"
       "keyof"
       "let"
       "namespace"
       "new"
       "of"
       "private"
       "protected"
       "public"
       "readonly"
       "return"
       "set"
       "static"
       "switch"
       "target"
       "throw"
       "try"
       "type"
       "typeof"
       "var"
       "void"
       "while"
       "with"
       "yield"
       ] @font-lock-keyword-face

      (comment) @font-lock-comment-face
      ))))

(defun typescript-ts-move-to-node (fn)
  (when-let ((found-node (treesit-parent-until
                          (treesit-node-at (point) (point) 'tsx)
                          (lambda (parent)
                            (let ((parent-type (treesit-node-type parent)))
                              (or (equal "function_declaration" parent-type)
                                  (equal "interface_declaration" parent-type)))))))
    (goto-char (funcall fn found-node))))

(defun typescript-ts-beginning-of-defun (&optional arg)
  (typescript-ts-move-to-node #'treesit-node-start))

(defun typescript-ts-end-of-defun (&optional arg)
  (typescript-ts-move-to-node #'treesit-node-end))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-ts-mode))

(define-derived-mode typescript-ts-mode prog-mode "typescriptreact"
  "Major mode for editing typescript.

Key bindings:

\\{typescript-mode-map}"

  :group 'typescript
  :syntax-table typescript-ts-syntax-table

  (unless (or (treesit-should-enable-p)
              (treesit-language-available-p 'tsx))
    (error "Tree sitter isn't available.  Did you compile emacs from the feature/tree-sitter branch?"))

  ;; Comments
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "\\(?://+\\|/\\*+\\)\\s *")
  (setq-local comment-end "")

  (treesit-get-parser-create 'tsx)
  (setq-local treesit-simple-indent-rules typescript-ts-indent-rules)
  (setq-local indent-line-function #'treesit-indent)
  (setq-local beginning-of-defun-function #'typescript-ts-beginning-of-defun)
  (setq-local end-of-defun-function #'typescript-ts-end-of-defun)

  ;; This needs to be non-nil, because reasons
  (unless font-lock-defaults
    (setq font-lock-defaults '(nil t)))

  (setq-local treesit-font-lock-defaults
              '((typescript-ts-font-lock-settings-1)))

  (treesit-font-lock-enable))

(provide 'typescript-ts)

;;; typescript-ts.el ends here
