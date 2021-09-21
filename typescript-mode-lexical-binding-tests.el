;; -*- lexical-binding: t -*-
;;; typescript-mode-lexical-binding-tests --- This file contains test for typescript-mode.el under enabled lexical-binding

;;; Commentary:
;; To know how to run the tests, see typescript-mode-tests.el

(require 'ert)

;; reload typescript-mode with lexical-binding enabled
(with-temp-buffer
  (insert-file-contents "typescript-mode.el")
  (ignore-errors
    (while (setq sexp (read (current-buffer)))
      (eval sexp t))))

(require 'typescript-mode-test-utilities)

(ert-deftest lexical-binding--indentation-does-not-throw-error ()
  (with-temp-buffer
    (insert-file-contents "test-files/indentation-reference-document.ts")
    (typescript-mode)
    (goto-line 2)
    (typescript-indent-line)))

(provide 'typescript-mode-lexical-binding-tests)
