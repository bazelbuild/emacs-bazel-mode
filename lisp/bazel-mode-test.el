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

(require 'cl-lib)
(require 'eieio)
(require 'ert)
(require 'rx)
(require 'xref)

(defconst bazel-mode-test--directory
  ;; https://docs.bazel.build/versions/2.2.0/test-encyclopedia.html#initial-conditions.
  (substitute-in-file-name "$TEST_SRCDIR/$TEST_WORKSPACE/lisp/")
  "Directory with data dependencies for this package.")

(ert-deftest bazel-mode/indent-after-colon ()
  (with-temp-buffer
    (bazel-mode)
    (insert "def foo():")
    (newline-and-indent)
    (should (= (current-column) 4))))

(ert-deftest bazel-mode/indent-region ()
  (with-temp-buffer
    (bazel-mode)
    (insert-file-contents (expand-file-name "BUILD" bazel-mode-test--directory))
    (let ((before (buffer-string)))
      (indent-region (point-min) (point-max))
      (should (equal (buffer-string) before)))))

(ert-deftest bazel-mode--make-diagnostics ()
  "Unit test for ‘bazel-mode--make-diagnostics’.
We test that function instead of the Flymake backend directly so
we don’t have to start or mock a process."
  ;; This test doesn’t work in Emacs 27 due to
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=39971.
  (skip-unless (not (eql emacs-major-version 27)))
  (with-temp-buffer
    (let ((output-buffer (current-buffer))
          (diagnostics nil))
      (insert-file-contents
       (expand-file-name "testdata/buildifier.json" bazel-mode-test--directory))
      (with-temp-buffer
        (insert-file-contents
         (expand-file-name "testdata/buildifier.bzl"
                           bazel-mode-test--directory))
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

(defmacro bazel-mode-test--with-temp-directory (name &rest body)
  "Create a new temporary directory.
Bind the name of the directory to NAME and execute BODY while the
directory exists.  Remove the directory and all its contents once
BODY finishes."
  (declare (indent 1) (debug (sexp body)))
  (cl-check-type name symbol)
  `(let ((,name (make-temp-file "bazel-mode-test-" :dir-flag)))
     (unwind-protect
         ,(macroexp-progn body)
       (delete-directory ,name :recursive))))

(defmacro bazel-mode-test--with-file-buffer (filename &rest body)
  "Visit FILENAME in a temporary buffer.
Execute BODY with the buffer that visits FILENAME current.  Kill
that buffer once BODY finishes."
  (declare (indent 1) (debug (sexp body)))
  (let ((buffer (make-symbol "buffer")))
    `(let ((,buffer (find-file-noselect ,filename)))
       (unwind-protect
           (with-current-buffer ,buffer ,@body)
         (kill-buffer ,buffer)))))

(ert-deftest bazel-mode/xref ()
  "Unit test for XRef support."
  (let ((definitions ()))
    (bazel-mode-test--with-temp-directory dir
      (make-directory (expand-file-name "root" dir))
      (copy-file
       (expand-file-name "testdata/xref.BUILD" bazel-mode-test--directory)
       (expand-file-name "root/BUILD" dir))
      ;; Create empty files that the labels in the test BUILD file refer to.
      (dolist (file '("WORKSPACE" "aaa.cc" "dir/bbb.cc" "pkg/BUILD" "pkg/ccc.cc"
                      "bazel-root/external/ws/WORKSPACE"
                      "bazel-root/external/ws/pkg/ddd.cc"))
        (let ((full-filename (expand-file-name (concat "root/" file) dir)))
          (make-directory (file-name-directory full-filename) :parents)
          (write-region "" nil full-filename nil nil nil :mustbenew)))
      (bazel-mode-test--with-file-buffer (expand-file-name "root/BUILD" dir)
        (forward-comment (point-max))
        ;; Search for all sources and dependencies.  These are strings that
        ;; stand on their own in a line.
        (while (re-search-forward (rx bol (* blank) ?\") nil t)
          (let ((backend (xref-find-backend)))
            (should (eq backend 'bazel-mode))
            (ert-info ((format "line %d, %s" (line-number-at-pos)
                               (buffer-substring-no-properties
                                (1- (point)) (line-end-position))))
              (let ((identifier (xref-backend-identifier-at-point backend)))
                (should (stringp identifier))
                (should
                 (equal (expand-file-name
                         (get-text-property 0 'bazel-mode-workspace identifier))
                        (file-name-as-directory (expand-file-name "root" dir))))
                (let* ((defs (xref-backend-definitions backend identifier))
                       (def (car-safe defs)))
                  (should (consp defs))
                  ;; Check that ‘xref-backend-definitions’ still works if the
                  ;; magic text properties aren’t present.  This allows users
                  ;; to invoke ‘xref-find-definitions’ and enter a target
                  ;; manually.
                  (should
                   (equal defs
                          (xref-backend-definitions
                           backend (substring-no-properties identifier))))
                  ;; We only expect one definition for now.
                  (should-not (cdr defs))
                  (should (xref-item-p def))
                  (should (equal (xref-item-summary def) identifier))
                  (push (list identifier
                              (file-relative-name
                               (buffer-file-name
                                (marker-buffer
                                 (xref-location-marker
                                  (xref-item-location def))))
                               (expand-file-name "root" dir)))
                        definitions))))))
        ;; Test completions.
        (should
         (equal (all-completions
                 ":" (xref-backend-identifier-completion-table 'bazel-mode))
                '(":lib" ":bin" ":aaa.cc")))))
    (should (equal (nreverse definitions)
                   '(("//:aaa.cc" "aaa.cc")
                     ("//:dir/bbb.cc" "dir/bbb.cc")
                     ("//:aaa.cc" "aaa.cc")
                     ("//:aaa.cc" "aaa.cc")
                     ("//pkg:ccc.cc" "pkg/ccc.cc")
                     ("@ws//pkg:ddd.cc" "bazel-root/external/ws/pkg/ddd.cc")
                     ("//:lib" "BUILD")
                     ("//:lib" "BUILD")
                     ("//pkg:pkg" "pkg/BUILD")
                     ("//pkg:lib" "pkg/BUILD"))))))

;;; bazel-mode-test.el ends here
