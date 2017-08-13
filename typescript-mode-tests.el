
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

(ert-deftest should-not-trigger-js-mode-hook ()
  (with-temp-buffer

    ;; prepare js-mode and hooks
    (require 'js)
    (let ((js-activated nil))
      (add-hook 'js-mode-hook (lambda ()
                           (setq js-activated t)))

      ;; activate our code
      (typescript-mode)
      (should (= nil js-activated)))))

(provide 'typescript-mode-tests)

;;; typescript-mode-tests.el ends here
