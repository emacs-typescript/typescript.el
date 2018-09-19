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
  ""
  (let* ((unwrapped-comment-lines
          (mapcar (lambda (line) (concat " * " line "\n")) jsdoc-lines))
         (comment-lines
          (append '("/**\n") unwrapped-comment-lines '(" */"))))
    (font-lock-test
     (apply #'concat comment-lines)
     (cons
      '(1 . font-lock-comment-delimiter-face)
      specs))))

(provide 'typescript-mode-jsdoc-tests)

;;; typescript-mode-jsdoc-tests.el ends here
