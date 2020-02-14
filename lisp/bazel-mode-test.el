;;; bazel-mode-test.el --- unit tests for bazel-mode.el  -*- lexical-binding: t; -*-

;; Copyright 2020 Google LLC
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     https://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:

;; Unit tests for bazel-mode.el.

;;; Code:

(require 'bazel-mode)

(require 'ert)

(ert-deftest bazel-mode/indent-after-colon ()
  (with-temp-buffer
    (bazel-mode)
    (insert "def foo():")
    (newline-and-indent)
    (should (= (current-column) 4))))

(ert-deftest bazel-mode/indent-region ()
  (with-temp-buffer
    (bazel-mode)
    (insert-file-contents "BUILD")
    (let ((before (buffer-string)))
      (indent-region (point-min) (point-max))
      (should (equal (buffer-string) before)))))

(ert-deftest bazel-mode--make-diagnostics ()
  "Unit test for ‘bazel-mode--make-diagnostics’.
We test that function instead of the Flymake backend directly so
we don’t have to start or mock a process."
  (with-temp-buffer
    (let ((output-buffer (current-buffer))
          (diagnostics nil))
      (insert-file-contents "testdata/buildifier.json")
      (with-temp-buffer
        (insert-file-contents "testdata/buildifier.bzl")
        (dolist (diag (bazel-mode--make-diagnostics output-buffer))
          (ert-info ((prin1-to-string diag))
            (should (eq (flymake-diagnostic-buffer diag) (current-buffer)))
            (should (eq (flymake-diagnostic-type diag) :warning))
            (push (list (buffer-substring-no-properties
                         (flymake-diagnostic-beg diag)
                         (flymake-diagnostic-end diag))
                        (flymake-diagnostic-text diag))
                  diagnostics))))
      (should (equal (nreverse diagnostics)
                     `(("def foo(bar):"
                        ,(concat "The file has no module docstring. "
                                 "[module-docstring] "
                                 "(https://github.com/bazelbuild/buildtools/blob/master/WARNINGS.md#module-docstring)"))
                       ("\"\"\" \"\"\""
                        ,(concat "The docstring for the function \"foo\" "
                                 "should start with a one-line summary. "
                                 "[function-docstring-header] "
                                 "(https://github.com/bazelbuild/buildtools/blob/master/WARNINGS.md#function-docstring-header)"))
                       ("1 / 2"
                        ,(concat "The \"/\" operator for integer division "
                                 "is deprecated in favor of \"//\". "
                                 "[integer-division] "
                                 "(https://github.com/bazelbuild/buildtools/blob/master/WARNINGS.md#integer-division)"))))))))

;;; bazel-mode-test.el ends here
