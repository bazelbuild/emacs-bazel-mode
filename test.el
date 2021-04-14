;;; test.el --- unit tests for bazel.el  -*- lexical-binding: t; -*-

;; Copyright 2020, 2021 Google LLC
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

;; Unit tests for bazel.el.

;;; Code:

(require 'bazel)

(require 'cl-lib)
(require 'compile)
(require 'eieio)
(require 'ert)
(require 'faces)
(require 'ffap)
(require 'font-lock)
(require 'imenu)
(require 'rx)
(require 'speedbar)
(require 'syntax)
(require 'xref)

(defconst bazel-test--directory
  ;; https://docs.bazel.build/versions/2.2.0/test-encyclopedia.html#initial-conditions.
  (substitute-in-file-name "$TEST_SRCDIR/$TEST_WORKSPACE/")
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
    (insert-file-contents (expand-file-name "BUILD" bazel-test--directory))
    (let ((before (buffer-string)))
      (indent-region (point-min) (point-max))
      (should (equal (buffer-string) before)))))

(ert-deftest bazel--make-diagnostics ()
  "Unit test for ‘bazel--make-diagnostics’.
We test that function instead of the Flymake backend directly so
we don’t have to start or mock a process."
  ;; This test doesn’t work in Emacs 27 due to
  ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=39971.
  (skip-unless (not (eql emacs-major-version 27)))
  (with-temp-buffer
    (let ((output-buffer (current-buffer))
          (diagnostics nil))
      (insert-file-contents
       (expand-file-name "testdata/buildifier.json" bazel-test--directory))
      (with-temp-buffer
        (insert-file-contents
         (expand-file-name "testdata/buildifier.bzl" bazel-test--directory))
        (dolist (diag (bazel--make-diagnostics output-buffer))
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

(ert-deftest bazel-mode-flymake ()
  "Unit test for the ‘bazel-mode-flymake’ Flymake backend."
  (with-temp-buffer
    (let ((bazel-buildifier-command
           (expand-file-name "testdata/fake_buildifier" bazel-test--directory))
          (flymake-diagnostic-functions '(bazel-mode-flymake))
          (warning-minimum-log-level :debug)
          (diagnostics ()))
      (skip-unless (file-executable-p bazel-buildifier-command))
      (insert-file-contents
       (expand-file-name "testdata/buildifier.bzl" bazel-test--directory))
      (flymake-mode)
      (flymake-start)
      (should (flymake-is-running))
      (should (equal (flymake-running-backends) '(bazel-mode-flymake)))
      ;; Wait for the backend to start reporting.
      (while (not (memq #'bazel-mode-flymake (flymake-reporting-backends)))
        (sleep-for 0.1))
      ;; Give the backend some time to report.  This isn’t 100% robust, but
      ;; should be good enough in typical cases.
      (sleep-for 1)
      (dolist (diag (flymake-diagnostics))
        (ert-info ((prin1-to-string diag))
          (should (eq (flymake-diagnostic-buffer diag) (current-buffer)))
          (should (eq (flymake-diagnostic-type diag) :warning))
          (push (list (buffer-substring-no-properties
                       (flymake-diagnostic-beg diag)
                       (flymake-diagnostic-end diag))
                      (flymake-diagnostic-text diag))
                diagnostics)))
      (should (equal diagnostics
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

(defmacro bazel-test--with-temp-directory (name &rest body)
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

(defmacro bazel-test--with-file-buffer (filename &rest body)
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
    (bazel-test--with-temp-directory dir
      (make-directory (expand-file-name "root" dir))
      (copy-file
       (expand-file-name "testdata/xref.BUILD" bazel-test--directory)
       (expand-file-name "root/BUILD" dir))
      ;; Create empty files that the labels in the test BUILD file refer to.
      (dolist (file '("WORKSPACE" "aaa.cc" "dir/bbb.cc" "pkg/BUILD" "pkg/ccc.cc"
                      "bazel-root/external/ws/WORKSPACE"
                      "bazel-root/external/ws/pkg/ddd.cc"))
        (let ((full-filename (expand-file-name (concat "root/" file) dir)))
          (make-directory (file-name-directory full-filename) :parents)
          (write-region "" nil full-filename nil nil nil :mustbenew)))
      (bazel-test--with-file-buffer (expand-file-name "root/BUILD" dir)
        (forward-comment (point-max))
        ;; Search for all sources and dependencies.  These are strings that
        ;; stand on their own in a line.
        (while (re-search-forward (rx bol (* blank) ?\") nil t)
          (let ((backend (xref-find-backend))
                (root (expand-file-name "root" dir)))
            (should (eq backend 'bazel-mode))
            (ert-info ((format "line %d, %s" (line-number-at-pos)
                               (buffer-substring-no-properties
                                (1- (point)) (line-end-position))))
              (let ((identifier (xref-backend-identifier-at-point backend)))
                (should (stringp identifier))
                (should
                 (equal (expand-file-name
                         (get-text-property 0 'bazel-mode-workspace identifier))
                        (file-name-as-directory root)))
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
                  (let ((ref-file (buffer-file-name
                                   (marker-buffer
                                    (xref-location-marker
                                     (xref-item-location def))))))
                    ;; Work around
                    ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=46219.
                    (cl-callf file-name-unquote root)
                    (cl-callf file-name-unquote ref-file)
                    (push (list (substring-no-properties identifier)
                                (file-relative-name ref-file root))
                          definitions)))))))
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

(ert-deftest bazel-mode/ffap ()
  "Unit test for ‘find-file-at-point’ support."
  (bazel-test--with-temp-directory dir
    (make-directory (expand-file-name "root" dir))
    (write-region "" nil (expand-file-name "root/WORKSPACE" dir)
                  nil nil nil 'excl)
    (write-region "" nil (expand-file-name "root/aaa.h" dir) nil nil nil 'excl)
    (make-directory (expand-file-name "root/pkg" dir))
    (write-region "#include \"aaa.h\"\n#include \"bbb.h\"\n" nil
                  (expand-file-name "root/pkg/aaa.c" dir) nil nil nil 'excl)
    (make-directory (expand-file-name "root/bazel-root/external/ws" dir)
                    :parents)
    (write-region "" nil
                  (expand-file-name "root/bazel-root/external/ws/WORKSPACE" dir)
                  nil nil nil 'excl)
    (write-region "" nil
                  (expand-file-name "root/bazel-root/external/ws/bbb.h" dir)
                  nil nil nil 'excl)
    (bazel-test--with-file-buffer (expand-file-name "root/pkg/aaa.c" dir)
      (search-forward "\"" (line-end-position))
      (should (equal (ffap-file-at-point)
                     (file-name-unquote (expand-file-name "root/aaa.h" dir))))
      (forward-line)
      (search-forward "\"" (line-end-position))
      (forward-comment (point-max))
      (should (equal
               (ffap-file-at-point)
               (file-name-unquote
                (expand-file-name "root/bazel-root/external/ws/bbb.h" dir)))))))

(ert-deftest bazel-mode/fill ()
  "Check that “keep sorted” comments are left alone."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "testdata/fill.BUILD" bazel-test--directory))
    (bazel-mode)
    (search-forward "# The Foobar files")
    (let ((before (buffer-string)))
      (fill-paragraph)
      (should (equal (buffer-string) before)))))

(ert-deftest bazel-build-mode/beginning-of-defun ()
  "Check that ‘beginning-of-defun’ in BUILD buffers moves to the
beginning of the rule."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "BUILD" bazel-test--directory))
    (bazel-build-mode)
    (search-forward "bazel.el")
    (beginning-of-defun)
    (should (looking-at-p (rx bol "elisp_library(" ?\n
                              "    name = \"bazel\",")))))

(ert-deftest bazel-build-mode/end-of-defun ()
  "Check that ‘end-of-defun’ in BUILD buffers moves to the end of
the rule."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "BUILD" bazel-test--directory))
    (bazel-build-mode)
    (search-forward "bazel.el")
    (end-of-defun)
    (should (looking-back (rx "\n)\n") nil))))

(ert-deftest bazel-mode/compile ()
  "Check that \\[next-error] jumps to the correct places."
  (bazel-test--with-temp-directory dir
    (copy-file
     (expand-file-name "testdata/test.WORKSPACE" bazel-test--directory)
     (expand-file-name "WORKSPACE" dir))
    (make-directory (expand-file-name "package" dir))
    (copy-file
     (expand-file-name "testdata/compile.BUILD" bazel-test--directory)
     (expand-file-name "package/BUILD" dir))
    (copy-file
     (expand-file-name "testdata/test.cc" bazel-test--directory)
     (expand-file-name "package/test.cc" dir))
    (with-temp-buffer
      (let* ((file nil) (line nil)
             (next-error-move-function
              (lambda (_from to)
                (goto-char to)
                (setq file buffer-file-name
                      line (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position)))))
             (case-fold-search nil)
             (compilation-skip-to-next-location nil))
        (insert-file-contents
         (expand-file-name "testdata/compile.out" bazel-test--directory))
        ;; Replace the %WORKSPACE% placeholder added by
        ;; testdata/make_compile_out by our temporary root.
        (goto-char (point-min))
        (while (search-forward "%WORKSPACE%" nil t)
          (replace-match (file-name-unquote dir) :fixedcase :literal))
        (goto-char (point-min))
        (compilation-minor-mode)
        (ert-info ("Deprecation warning")
          (compilation-next-error 1)
          (should (looking-at-p
                   (rx bol "WARNING: " (+ nonl) "/package/BUILD:15:1: target "
                       "'//package:test' is deprecated: Deprecated!" eol)))
          (save-current-buffer (compile-goto-error))
          (should (equal file (file-name-unquote
                               (expand-file-name "package/BUILD" dir))))
          (should (equal line "cc_library(")))
        (ert-info ("Target failure")
          (compilation-next-error 1)
          (should (looking-at-p
                   (rx bol "ERROR: " (+ nonl) "/package/BUILD:15:1: "
                       "C++ compilation")))
          (save-current-buffer (compile-goto-error))
          (should (equal file (file-name-unquote
                               (expand-file-name "package/BUILD" dir))))
          (should (equal line "cc_library(")))
        (ert-info ("Compiler error")
          (compilation-next-error 1)
          (save-current-buffer (compile-goto-error))
          (should (equal file (file-name-unquote
                               (expand-file-name "package/test.cc" dir))))
          (should (equal line "UnknownType foo;")))
        (ert-info ("No more errors")
          (should-error (compilation-next-error 1)))))))

(ert-deftest bazel-build-mode/imenu ()
  "Check that ‘imenu’ finds BUILD rules."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "testdata/xref.BUILD" bazel-test--directory))
    (bazel-build-mode)
    (let ((imenu-use-markers nil))
      (should (equal (funcall imenu-create-index-function)
                     '(("lib" . 577) ("bin" . 761)))))))

(ert-deftest bazel-mode/speedbar ()
  "Check that \\[speedbar] detects BUILD files."
  (with-temp-buffer
    (speedbar-default-directory-list bazel-test--directory 0)
    (goto-char (point-min))
    (let ((case-fold-search nil))
      (search-forward "BUILD"))))

(ert-deftest bazel-mode/triple-quoted-strings ()
  "Check that triple-quoted strings work as expected."
  ;; See https://docs.bazel.build/versions/3.1.0/skylark/lib/string.html.
  (with-temp-buffer
    (bazel-build-mode)
    (insert "\"\"\"\n\"foo\"\n\"\"\"\n")
    (font-lock-flush)
    (font-lock-ensure)
    (goto-char (point-min))
    (while (not (eobp))
      (ert-info ((format "at position %d; rest of buffer is %S"
                         (point)
                         (buffer-substring-no-properties (point) (point-max))))
        (cl-destructuring-bind
            (depth _ _ in-string-p in-comment-p _ _ _ string-start . rest)
            (syntax-ppss)
          (should (eq depth 0))
          (when (< 4 (point) (- (point-max) 5)) (should in-string-p))
          (should-not in-comment-p)
          (should (eq string-start (and in-string-p 1))))
        (should (eq (face-at-point)
                    (and (< (point) (1- (point-max))) 'font-lock-string-face)))
        (forward-char)))))

(put #'looking-at-p 'ert-explainer #'bazel-test--explain-looking-at-p)

(defun bazel-test--explain-looking-at-p (regexp)
  "ERT explainer for ‘looking-at-p’.
See Info node ‘(ert) Defining Explanation Functions’.  REGEXP is
the expected regular expression."
  (unless (looking-at-p regexp)
    `(rest-of-line ,(buffer-substring-no-properties
                     (point) (line-end-position)))))

;;; test.el ends here
