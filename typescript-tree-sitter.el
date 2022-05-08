;;; typescript-tree-sitter.el --- tree sitter support for Typescript  -*- lexical-binding: t; -*-

;; Copyright (C) Theodor Thornhill

;; Author     : Theodor Thornhill <theo@thornhill.no>
;; Maintainer : Jostein Kjønigsen <jostein@gmail.com>
;;              Theodor Thornhill <theo@thornhill.no>
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

;; Note about indentation:
;; The indentation mechanics are adapted from Felipe Lemas tree-sitter-indent
;; package.  We don't need a generic solution for now, because we are waiting
;; for Emacs proper.  Let's just make it work for us, then move over to emacs
;; when that is ready.  No need for a dependency.

;;; Code:
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(when t
  ;; In order for the package to be usable and installable (and hence
  ;; compilable) without tree-sitter, wrap the `require's within a dummy `when'
  ;; so they're only executed when loading this file but not when compiling it.

  (require 'tree-sitter)
  (require 'tree-sitter-hl)
  (require 'tree-sitter-langs))
;; Vars and functions defined by the above packages:
(defvar tree-sitter-major-mode-language-alist)
(declare-function tree-sitter-hl-mode "ext:tree-sitter-hl")
(declare-function tsc-node-end-position "ext:tree-sitter")
(declare-function tsc-node-start-position "ext:tree-sitter")

(defvar typescript-tree-sitter-syntax-table)
(defvar typescript-tree-sitter-map)

(defvar typescript-tree-sitter-scopes
  '((indent
     ;; if parent node is one of these and node is not first → indent
     . (try_statement
        if_statement
        object
        template_substitution
        ;; function_declaration
        interface_declaration
        lexical_declaration
        expression_statement
        return_statement
        named_imports
        arguments
        jsx_self_closing_element
        jsx_element
        jsx_opening_element))
    (outdent
     ;; these nodes always outdent (1 shift in opposite direction)
     . (")"
        "}"
        "]"
        "/")))
  "Current scopes in use for tree-sitter-indent.")

(defun typescript-tree-sitter--indent (node)
  (let-alist typescript-tree-sitter-scopes
    (member (tsc-node-type node) .indent)))

(defun typescript-tree-sitter--outdent (node)
  (let-alist typescript-tree-sitter-scopes
    (member (tsc-node-type node) .outdent)))

(defun typescript-tree-sitter--highest-node-at-position (position)
  (save-excursion
    (goto-char position)
    (let ((current-node (tree-sitter-node-at-pos)))
      (while (and
	            current-node
	            (when-let ((parent-node (tsc-get-parent current-node)))
                (when (and ;; parent and current share same position
                       (eq (tsc-node-start-byte parent-node)
                           (tsc-node-start-byte current-node)))
		              (setq current-node parent-node)))))
      current-node)))

(defun typescript-tree-sitter--parentwise-path (node)
  (let ((path (list node))
        (next-parent-node (tsc-get-parent node)))
    (while next-parent-node
      (push next-parent-node path)
      (setq next-parent-node (tsc-get-parent next-parent-node)))
    path))

(cl-defun typescript-tree-sitter--indents-in-path (parentwise-path)
  (seq-map
   (lambda (current-node)
     (cond
      ((typescript-tree-sitter--outdent current-node) 'outdent)
      ((typescript-tree-sitter--indent current-node) 'indent)
      (t 'no-indent)))
   parentwise-path))

(defun typescript-tree-sitter--updated-column (column indent)
  (pcase indent
    (`no-indent column)
    (`indent (+ column typescript-tree-sitter-indent-offset))
    (`outdent (- column typescript-tree-sitter-indent-offset))
    (_ (error "Unexpected indent instruction: %s" indent))))

(cl-defun typescript-tree-sitter--indent-column ()
  (seq-reduce
   #'typescript-tree-sitter--updated-column
   (typescript-tree-sitter--indents-in-path
    (typescript-tree-sitter--parentwise-path
     (typescript-tree-sitter--highest-node-at-position
      (save-excursion (back-to-indentation) (point)))))
   0))

;;;; Public API

;;;###autoload
(defun typescript-tree-sitter-indent-line ()
  (let ((first-non-blank-pos ;; see savep in `smie-indent-line'
         (save-excursion
           (forward-line 0)
           (skip-chars-forward " \t")
           (point)))
        (new-column
         (typescript-tree-sitter--indent-column)))
    (when (numberp new-column)
      (if (< first-non-blank-pos (point))
          (save-excursion (indent-line-to new-column))
        (indent-line-to new-column)))))

(defgroup typescript-tree-sitter-indent nil "Indent lines using Tree-sitter as backend"
  :group 'tree-sitter)

(defcustom typescript-tree-sitter-indent-offset 2
  "Indent offset for typescript-tree-sitter-mode."
  :type 'integer
  :group 'typescript)

(defvar typescript-tree-sitter-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap used in typescript-tree-sitter buffers.")

(defvar typescript-tree-sitter-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?@ "_" table)
    table))

;;;###autoload
(define-derived-mode typescript-tree-sitter-mode prog-mode "typescriptreact"
  "Major mode for editing Typescript code.

Key bindings:
\\{typescript-tree-sitter-mode-map}"
  :group 'typescript
  :syntax-table typescript-tree-sitter-mode-syntax-table

  (setq-local indent-line-function #'typescript-tree-sitter-indent-line)
  ;; (setq-local beginning-of-defun-function #'typescript-beginning-of-defun)
  ;; (setq-local end-of-defun-function #'typescript-end-of-defun)

  ;; https://github.com/ubolonton/emacs-tree-sitter/issues/84
  (unless font-lock-defaults
    (setq font-lock-defaults '(nil)))

  ;; Comments
  (setq-local comment-start "// ")
  (setq-local comment-start-skip "\\(?://+\\|/\\*+\\)\\s *")
  (setq-local comment-end "")

  (tree-sitter-hl-mode))

(add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tree-sitter-mode . tsx))

(provide 'typescript-tree-sitter)

;;; typescript-tree-sitter.el ends here
