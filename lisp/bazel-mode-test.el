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
    (let ((output-buffer (current-buffer)))
      ;; Example from
      ;; https://github.com/bazelbuild/buildtools/blob/master/buildifier/README.md#file-diagnostics-in-json.
      (insert-file-contents "testdata/buildifier.json")
      (with-temp-buffer
        ;; The exact contents of the input buffer don’t matter, but it should
        ;; be large enough for the diagnostic to point to a valid position.
        (insert "01234567890123456789\n")
        (should (equal (bazel-mode--make-diagnostics output-buffer)
                       (list (flymake-make-diagnostic
                              (current-buffer) 6 11 :warning
                              (concat "The \"/\" operator for integer division "
                                      "is deprecated in favor of \"//\". "
                                      "[integer-division] "
                                      "(https://github.com/bazelbuild/buildtools/blob/master/WARNINGS.md#integer-division)")))))))))

;;; bazel-mode-test.el ends here
