;;; typescript-mode-tests --- This file contains automated tests for typescript-mode.el

;;; Commentary:
;; Run tests using (ert-run-tests-interactively t).

;;; Code:

(require 'ert)
(require 'typescript-mode)
(require 'cl-lib)
(require 'typescript-mode-test-utilities)
(require 'typescript-mode-general-tests)
(require 'typescript-mode-jsdoc-tests)
(require 'typescript-mode-lexical-binding-tests)

(provide 'typescript-mode-tests)

;;; typescript-mode-tests.el ends here
