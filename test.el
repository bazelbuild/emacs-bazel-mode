;;; test.el --- unit tests for bazel.el  -*- lexical-binding: t; -*-

;; Copyright 2020, 2021, 2022 Google LLC
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

(require 'add-log)
(require 'cl-lib)
(require 'compile)
(require 'eieio)
(require 'ert)
(require 'ert-x)
(require 'faces)
(require 'ffap)
(require 'flymake)
(require 'font-lock)
(require 'imenu)
(require 'org)
(require 'ob-shell)
(require 'project)
(require 'rx)
(require 'speedbar)
(require 'subr-x)
(require 'syntax)
(require 'url)
(require 'warnings)
(require 'which-func)
(require 'xref)

(defconst bazel-test--directory
  ;; https://docs.bazel.build/versions/2.2.0/test-encyclopedia.html#initial-conditions.
  (substitute-in-file-name "$TEST_SRCDIR/$TEST_WORKSPACE/")
  "Directory with data dependencies for this package.")

;;;; Helper macros

(eval-when-compile
  (defmacro bazel-test--with-temp-directory (name org-file &rest body)
    "Create a new temporary directory.
Bind the name of the directory to NAME and execute BODY while the
directory exists.  Remove the directory and all its contents once
BODY finishes successfully.  NAME will be a directory name, not a
directory file name; see Info node ‘(elisp) Directory Names’.  If
ORG-FILE is non-nil, tangle code blocks in ORG-FILE into the
directory first using ‘bazel-test--tangle’."
    (declare (indent 2) (debug (symbolp form body)))
    (cl-check-type name symbol)
    (let ((directory (make-symbol "directory"))
          (file (make-symbol "file")))
      `(let ((,directory (make-temp-file "bazel-mode-test-" :dir-flag)))
         (ert-info (,directory :prefix "Temporary directory: ")
           (prog2 (when-let ((,file ,org-file))
                    (bazel-test--tangle ,directory ,file))
               (let ((,name (file-name-as-directory ,directory)))
                 ,@body)
             (delete-directory ,directory :recursive))))))

  (defmacro bazel-test--with-file-buffer (filename &rest body)
    "Visit FILENAME in a temporary buffer.
Execute BODY with the buffer that visits FILENAME current.  Kill
that buffer once BODY finishes."
    (declare (indent 1) (debug t))
    (let ((previous-buffers (make-symbol "previous-buffers"))
          (buffer (make-symbol "buffer")))
      (macroexp-let2 nil filename filename
        `(ert-info (,filename :prefix "Visited test file: ")
           (let ((,previous-buffers (buffer-list)))
             (access-file ,filename "Visiting test file")
             (save-current-buffer
               (find-file-existing ,filename)
               (let ((,buffer (current-buffer)))
                 (unwind-protect
                     ,(macroexp-progn body)
                   ;; Kill the buffer only if ‘find-file-existing’ has generated
                   ;; a new one.
                   (unless (memq ,buffer ,previous-buffers)
                     (kill-buffer ,buffer))))))))))

  (defmacro bazel-test--with-buffers (&rest body)
    "Evaluate BODY and kill all buffers that it created."
    (declare (indent 0) (debug t))
    (let ((buffers (make-symbol "buffers")))
      `(let ((,buffers (buffer-list)))
         (unwind-protect
             ,(macroexp-progn body)
           (dolist (buffer (buffer-list))
             (unless (memq buffer ,buffers) (kill-buffer buffer)))))))

  (defmacro bazel-test--with-suppressed-warnings (warnings &rest body)
    "Suppress WARNINGS in BODY.
This is the same as ‘with-suppressed-warnigns’ if available.
Otherwise, just evaluate BODY."
    (declare (indent 1) (debug (sexp body)))
    (if (macrop 'with-suppressed-warnings)
        `(with-suppressed-warnings ,warnings ,@body)
      (macroexp-progn body)))

  (defmacro bazel-test--wait-for (message condition)
    "Busy wait for CONDITION to become non-nil.
MESSAGE is a message for ‘ert-info’."
    (declare (indent 1) (debug t))
    `(ert-info (,message)
       (ert-info (,(prin1-to-string condition) :prefix "Condition: ")
         (with-timeout (10 (ert-fail "Timed out waiting for condition"))
           (while (not ,condition)
             (sleep-for 0.1)))))))

;;;; Unit tests

(ert-deftest bazel-mode/indent-after-colon ()
  (with-temp-buffer
    (bazel-mode)
    (insert "def foo():")
    (newline-and-indent)
    (should (= (current-column) 4))))

(ert-deftest bazel-mode/indent-region ()
  (bazel-test--with-temp-directory dir "indent.org"
    (bazel-test--with-file-buffer (expand-file-name "BUILD" dir)
      (should (equal (buffer-string) (ert-buffer-string-reindented))))))

(ert-deftest bazel-mode-flymake ()
  "Unit test for the ‘bazel-mode-flymake’ Flymake backend."
  (bazel-test--with-temp-directory dir "flymake.org"
    (bazel-test--with-file-buffer (expand-file-name "buildifier.bzl" dir)
      (let ((bazel-buildifier-command (expand-file-name "buildifier" dir))
            (flymake-diagnostic-functions '(bazel-mode-flymake))
            (warning-minimum-log-level :debug)
            (diagnostics ()))
        (skip-unless (file-executable-p bazel-buildifier-command))
        ;; Make the fake Buildifier report immediately.
        (write-region "" nil (expand-file-name "signal" dir) nil nil nil 'excl)
        (flymake-mode)
        (should (flymake-is-running))
        (should (equal (flymake-running-backends) '(bazel-mode-flymake)))
        ;; Wait for the backend to start reporting.
        (bazel-test--wait-for "Waiting for backend to start reporting"
          (memq #'bazel-mode-flymake (flymake-reporting-backends)))
        ;; Give the backend some time to report.
        (bazel-test--wait-for "Waiting for diagnostics to arrive"
          (flymake-diagnostics))
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
                                   "(https://github.com/bazelbuild/buildtools/blob/master/WARNINGS.md#integer-division)")))))))))

(ert-deftest bazel-mode-flymake/source-buffer-killed ()
  "Unit test for the ‘bazel-mode-flymake’ Flymake backend.
Check that the Flymake backend behaves well if the source buffer
gets killed early."
  (ert-with-message-capture messages
    (bazel-test--with-temp-directory dir "flymake.org"
      (let ((bazel-buildifier-command (expand-file-name "buildifier" dir))
            (warning-minimum-log-level :debug))
        (skip-unless (file-executable-p bazel-buildifier-command))
        (bazel-test--with-file-buffer (expand-file-name "buildifier.bzl" dir)
          (let ((flymake-diagnostic-functions '(bazel-mode-flymake)))
            (flymake-mode)
            (should (flymake-is-running))
            (should (equal (flymake-running-backends) '(bazel-mode-flymake))))
          ;; Wait for a Buildifier process to start before killing the buffer.
          (bazel-test--wait-for "Waiting for Buildifier process to start"
            (bazel-test--buildifier-running-p)))
        ;; Now allow the fake Buildifier to proceed.
        (write-region "" nil (expand-file-name "signal" dir) nil nil nil 'excl)
        ;; Wait for fake Buildifier to finish.
        (bazel-test--wait-for "Waiting for Buildifier process to finish"
          (not (bazel-test--buildifier-running-p)))))
    ;; This shouldn’t have caused any warnings or errors.
    (should-not (string-match-p (rx bol (or "Emergency" "Error" "Warning"))
                                messages))))

(ert-deftest bazel-mode/xref ()
  "Unit test for XRef support."
  (let ((definitions ()))
    (bazel-test--with-temp-directory dir "xref.org"
      (bazel-test--with-file-buffer (expand-file-name "root/BUILD" dir)
        (let ((case-fold-search nil)
              (search-spaces-regexp nil))
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
                  (should (equal (expand-file-name
                                  (get-text-property 0 'bazel-mode-workspace
                                                     identifier))
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
                      ;; Work around Bug#46219.
                      (cl-callf file-name-unquote root)
                      (cl-callf file-name-unquote ref-file)
                      (push (list (substring-no-properties identifier)
                                  (file-relative-name ref-file root))
                            definitions))))))))
        ;; Test completions.
        (let ((table (xref-backend-identifier-completion-table 'bazel-mode)))
          (should (equal (try-completion ":" table) ":"))
          (should (equal (all-completions ":" table) '("lib" "bin" "aaa.cc")))
          (should (equal (completion-boundaries ":" table nil "") '(1 . 0))))
        ;; Test ‘bazel-show-consuming-rule’.
        (let* ((build-buffer (current-buffer))
               (jumps 0)
               (xref-after-jump-hook (list (lambda () (cl-incf jumps)))))
          (bazel-test--with-file-buffer (expand-file-name "root/aaa.cc" dir)
            (bazel-show-consuming-rule))
          (should (eql jumps 1))
          (should (eq (current-buffer) build-buffer))
          (should (looking-at-p (rx "lib"))))))
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
  (bazel-test--with-temp-directory dir "find-file-at-point.org"
    (bazel-test--with-file-buffer (expand-file-name "root/pkg/aaa.c" dir)
      (let ((case-fold-search nil)
            (search-spaces-regexp nil))
        (search-forward "\"" (line-end-position))
        (should (equal (ffap-file-at-point)
                       (file-name-unquote (expand-file-name "root/aaa.h" dir))))
        (forward-line)
        (search-forward "\"" (line-end-position))
        (forward-comment (point-max))
        (should (equal (ffap-file-at-point)
                       (file-name-unquote
                        (expand-file-name "root/bazel-root/external/ws/bbb.h"
                                          dir))))))))

(ert-deftest bazel-mode/fill ()
  "Check that magic comments are left alone."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "testdata/fill.BUILD" bazel-test--directory))
    (bazel-mode)
    (let ((before (buffer-string)))
      (while (search-forward "# Test paragraph" nil t)
        (ert-info ((format "fill-paragraph on line %d" (line-number-at-pos)))
          (fill-paragraph)
          (should (equal (buffer-string) before)))))))

(ert-deftest bazel-build-mode/beginning-of-defun ()
  "Check that ‘beginning-of-defun’ in BUILD buffers moves to the
beginning of the rule."
  (bazel-test--with-temp-directory dir "defun-navigation.org"
    (bazel-test--with-file-buffer (expand-file-name "BUILD" dir)
      (search-forward "bazel.el")
      (beginning-of-defun)
      (should (looking-at-p (rx bol "elisp_library(" ?\n
                                "    name = \"bazel\","))))))

(ert-deftest bazel-build-mode/end-of-defun ()
  "Check that ‘end-of-defun’ in BUILD buffers moves to the end of
the rule."
  (bazel-test--with-temp-directory dir "defun-navigation.org"
    (bazel-test--with-file-buffer (expand-file-name "BUILD" dir)
      (search-forward "bazel.el")
      (end-of-defun)
      (should (looking-back (rx "\n)\n") nil)))))

(ert-deftest bazel-mode/compile ()
  "Check that \\[next-error] jumps to the correct places."
  (bazel-test--with-temp-directory dir "compile.org"
    (with-temp-buffer
      (let* ((file nil) (line nil)
             (next-error-move-function
              (lambda (_from to)
                (goto-char to)
                (setq file buffer-file-name
                      line (buffer-substring-no-properties
                            (line-beginning-position) (line-end-position)))))
             (case-fold-search nil)
             (search-spaces-regexp nil)
             (compilation-skip-to-next-location nil))
        (insert-file-contents (expand-file-name "bazel.out" dir))
        (compilation-minor-mode)
        (ert-info ("Deprecation warning")
          (compilation-next-error 1)
          (should (looking-at-p
                   (rx bol "WARNING: " (+ nonl) "/package/BUILD:1:11: target "
                       "'//package:test' is deprecated: Deprecated!" eol)))
          (save-current-buffer (compile-goto-error))
          (should (equal file (file-name-unquote
                               (expand-file-name "package/BUILD" dir))))
          (should (equal line "cc_library(")))
        (ert-info ("Target failure")
          (compilation-next-error 1)
          (should (looking-at-p
                   (rx bol "ERROR: " (+ nonl) "/package/BUILD:1:11: "
                       "Compiling package/test.cc failed: ")))
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
  (bazel-test--with-temp-directory dir "xref.org"
    (with-temp-buffer
      (insert-file-contents (expand-file-name "root/BUILD" dir))
      (bazel-build-mode)
      (let ((imenu-use-markers nil))
        (should (equal (funcall imenu-create-index-function)
                       '(("lib" . 1) ("bin" . 185)))))
      (let ((imenu-use-markers t))
        (should (equal (funcall imenu-create-index-function)
                       `(("lib" . ,(copy-marker 1))
                         ("bin" . ,(copy-marker 185)))))))))

(ert-deftest bazel-mode/speedbar ()
  "Check that \\[speedbar] detects BUILD files."
  (bazel-test--with-temp-directory dir "speedbar.org"
    (with-temp-buffer
      (speedbar-default-directory-list dir 0)
      (goto-char (point-min))
      (let ((case-fold-search nil)
            (search-spaces-regexp nil))
        (search-forward "BUILD")))))

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
          ;; The syntactic start of a triple-quoted string could be on the first
          ;; (Emacs 26 and 27) or the last (Emacs 28) quote, cf. Bug#49518.
          (should (eq (not in-string-p) (not string-start))))
        (should (eq (face-at-point)
                    (and (< (point) (1- (point-max))) 'font-lock-string-face)))
        (forward-char)))))

(ert-deftest bazel/project ()
  "Test project support for Bazel workspaces."
  (bazel-test--with-temp-directory dir "project.org"
    (let ((project (project-current nil dir)))
      (should project)
      (should (bazel-workspace-p project))
      (should (bazel-workspace-root project))
      (should (directory-name-p (bazel-workspace-root project)))
      (should (file-directory-p (bazel-workspace-root project)))
      (should (bazel-test--file-equal-p (bazel-workspace-root project) dir))
      (should (bazel-test--file-equal-p (project-root project) dir))
      (bazel-test--with-suppressed-warnings ((obsolete project-roots))
        (should (consp (project-roots project)))
        (should-not (cdr (project-roots project)))
        (should (bazel-test--file-equal-p (car (project-roots project)) dir)))
      (should-not (project-external-roots project)))))

(ert-deftest bazel/project-files ()
  "Test ‘project-files’ support for Bazel workspaces."
  ;; Try to work around Bug#48471 by picking GNU find on macOS.
  (let ((find-program (if (eq system-type 'darwin) "gfind" find-program)))
    (skip-unless (executable-find find-program))
    (bazel-test--with-temp-directory dir "project.org"
      (let* ((dir (file-name-unquote dir))  ; unquote to work around Bug#47799
             (project (project-current nil dir))
             (files (cond ((fboundp 'project-files)  ; Emacs 27
                           (project-files project))
                          ((fboundp 'project-file-completion-table)  ; Emacs 26
                           (all-completions "" (project-file-completion-table
                                                project (list dir)))))))
        (should project)
        (should (bazel-workspace-p project))
        (should files)
        (should (equal (sort (cl-loop for file in files
                                      collect (file-relative-name file dir))
                             #'string-lessp)
                       '(".bazelignore" "WORKSPACE" "package/BUILD")))))))

(ert-deftest bazel-test/coverage ()
  "Test coverage parsing and display."
  ;; Set up a fake workspace and execution root.  We use DIR for both.
  (bazel-test--with-temp-directory dir "coverage.org"
    (let* ((package-dir (expand-file-name "src/main/java/example" dir))
           (library (expand-file-name "Example.java" package-dir)))
      (bazel-test--with-file-buffer library
        (let ((case-fold-search nil)
              (search-spaces-regexp nil))
          ;; The coverage overlays don’t logically change the buffer contents.
          ;; Ensure that the code works even if the buffer is read-only.
          (setq buffer-read-only t)
          (with-temp-buffer
            (let ((default-directory dir)
                  (bazel-display-coverage 'local))
              (insert-file-contents (expand-file-name "bazel.out" dir))
              ;; Simulate successful exit of the Bazel process.
              (compilation-handle-exit 'exit 0 "finished\n")))
          (ert-info ("Comment line")
            ;; Package declaration isn’t covered at all.
            (should (looking-at-p (rx bol "package ")))
            (should-not (face-at-point)))
          (ert-info ("Line with branching statement")
            (search-forward "if (arg)")
            (should (eq (face-at-point) 'bazel-covered-line))
            (should (> left-margin-width 0))
            (let ((before-string (get-char-property (point) 'before-string)))
              (should (stringp before-string))
              (pcase (get-text-property 0 'display before-string)
                (`((margin left-margin) ,(and (pred stringp) margin-string))
                 (should (ert-equal-including-properties
                          margin-string
                          (ert-propertized-string
                           '(face bazel-covered-line) "+"
                           '(face bazel-uncovered-line) "−"))))
                (otherwise
                 (ert-fail (list "Unexpected ‘display’ property" otherwise))))))
          (ert-info ("Covered line")
            (search-forward "return 137;")
            (backward-char)  ; overlay doesn’t extend beyond the end of the line
            (should (eq (face-at-point) 'bazel-covered-line)))
          (ert-info ("Uncovered line")
            (search-forward "return 42;")
            (backward-char)  ; overlay doesn’t extend beyond the end of the line
            (should (eq (face-at-point) 'bazel-uncovered-line)))
          (ert-info ("Removing coverage")
            (bazel-remove-coverage-display)
            ;; Now there shouldn’t be any faces or margins left in the buffer.
            (should (eql (next-single-char-property-change (point-min) 'face)
                         (point-max)))
            (should (eq left-margin-width 0))))))))

(ert-deftest bazel--target-completion-table/root-package ()
  "Test target completion in the root package."
  (bazel-test--with-temp-directory dir "target-completion-root.org"
    (make-symbolic-link dir (expand-file-name "bazel-out" dir))
    ;; The test cases are of the form (STRING PATTERN TRY ALL TEST BOUND).
    ;; STRING is the input string.  PATTERN specifies whether to complete target
    ;; patterns; if PATTERNS is ‘*’, try both with and without pattern
    ;; completion.  TRY, ALL, and TEST are the expected results of
    ;; ‘try-completion’, ‘all-completions’, and ‘test-completion’, respectively.
    ;; BOUND is the expected prefix completion bound returned by
    ;; ‘completion-bounds’.
    (pcase-dolist
        (`(,string ,pattern ,try ,all ,test ,bound)
         '(("" nil ""
            ("test" "foo_test" "foo_test.py" "test.cc" "//" "@") nil 0)
           ("" t ""
            ("test" "foo_test" ":all" ":all-targets" ":*" "..." "//" "@") nil 0)
           ("te" nil "test" ("test" "test.cc") nil 0)
           ("te" t "test" ("test") nil 0)
           ("test" nil "test" ("test" "test.cc") t 0)
           ("test" t t ("test") t 0)
           ("test.c" nil "test.cc" ("test.cc") nil 0)
           ("test.c" t nil () nil 0)
           ("a" * nil () nil 0)
           (":" t ":" ("test" "foo_test" "all" "all-targets" "*") nil 1)
           (":" nil ":" ("test" "foo_test" "foo_test.py" "test.cc") nil 1)
           (":te" nil ":test" ("test" "test.cc") nil 1)
           (":te" t ":test" ("test") nil 1)
           (":test" nil ":test" ("test" "test.cc") t 1)
           (":test" t t ("test") t 1)
           (":a" nil nil () nil 1)
           (":a" t ":all" ("all" "all-targets") nil 1)
           (":all" nil nil () nil 1)
           (":all" t ":all" ("all" "all-targets") t 1)
           (":all-targets" nil nil () nil 1)
           (":all-targets" t t ("all-targets") t 1)
           (":foo/bar" * nil () nil 1)
           ("package/" * nil () nil 0)
           ("package:" nil nil () nil 0)
           ("package:" t nil () nil 8)
           ("@" * "@//" ("//") nil 1)
           ("@w" * nil () nil 1)
           ("/" * "//" ("//") nil 0)
           ("//" nil "//:" (":") nil 2)
           ("//" t "//" (":" "...") nil 2)
           ("//:" nil "//:" ("test" "foo_test" "foo_test.py" "test.cc") nil 3)
           ("//:" t "//:" ("test" "foo_test" "all" "all-targets" "*") nil 3)
           ("//:te" nil "//:test" ("test" "test.cc") nil 3)
           ("//:te" t "//:test" ("test") nil 3)
           ("//:test" nil "//:test" ("test" "test.cc") t 3)
           ("//:test" t t ("test") t 3)
           ("//:a" nil nil () nil 3)
           ("//:a" t "//:all" ("all" "all-targets") nil 3)
           ("//:all" nil nil () nil 3)
           ("//:all" t "//:all" ("all" "all-targets") t 3)
           ("//:all-targets" nil nil () nil 3)
           ("//:all-targets" t t ("all-targets") t 3)
           ("//:*" nil nil () nil 3)
           ("//:*" t t ("*") t 3)
           ("//pack" * nil () nil 2)
           ("//package/" * nil () nil 10)
           ("//package:" * nil () nil 10)
           ("//." nil nil () nil 0)
           ("//." t "//..." ("...") nil 2)
           ("//..." nil nil () nil 0)
           ("//..." t "//..." ("..." "...:") t 2)
           ("//...:" nil nil () nil 0)
           ("//...:" t "//...:" ("all" "all-targets" "*") nil 6)))
      (dolist (pattern (if (eq pattern '*) '(nil t) (list pattern)))
        (ert-info ((if pattern "yes" "no") :prefix "Pattern completion: ")
          (let* ((default-directory dir)
                 (table (bazel--target-completion-table pattern nil)))
            (ert-info ((prin1-to-string string) :prefix "Input: ")
              (should (equal (try-completion string table) try))
              (should (equal (all-completions string table) all))
              (should (eq (test-completion string table) test))
              (should (equal (completion-boundaries string table nil "suffix")
                             (cons bound 6))))))))
    ;; The test cases are of the form (STRING ONLY-TESTS TRY ALL TEST BOUND).
    ;; STRING is the input string.  ONLY-TESTS specifies whether to restrict the
    ;; search to test rules; if TEST-ONLY is ‘*’, try both with and without this
    ;; restriction.  TRY, ALL, and TEST are the expected results of
    ;; ‘try-completion’, ‘all-completions’, and ‘test-completion’, respectively.
    ;; BOUND is the expected prefix completion bound returned by
    ;; ‘completion-bounds’.
    (pcase-dolist
        (`(,string ,only-tests ,try ,all ,test ,bound)
         '(("" nil ""
            ("test" "foo_test" ":all" ":all-targets" ":*" "..." "//" "@") nil 0)
           ("" t ""
            ("foo_test" ":all" ":all-targets" ":*" "..." "//" "@") nil 0)
           ("te" nil "test" ("test") nil 0)
           ("te" t nil () nil 0)
           ("fo" * "foo_test" ("foo_test") nil 0)
           ("a" * nil () nil 0)
           (":" nil ":" ("test" "foo_test" "all" "all-targets" "*") nil 1)
           (":" t ":" ("foo_test" "all" "all-targets" "*") nil 1)
           (":f" * ":foo_test" ("foo_test") nil 1)
           (":te" nil ":test" ("test") nil 1)
           (":te" t nil () nil 1)
           (":test" nil t ("test") t 1)
           (":test" t nil () nil 1)
           (":a" * ":all" ("all" "all-targets") nil 1)))
      (dolist (only-tests (if (eq only-tests '*) '(nil t) (list only-tests)))
        (ert-info ((if only-tests "yes" "no") :prefix "Only tests: ")
          (let* ((default-directory dir)
                 (table (bazel--target-completion-table :pattern only-tests)))
            (ert-info ((prin1-to-string string) :prefix "Input: ")
              (should (equal (try-completion string table) try))
              (should (equal (all-completions string table) all))
              (should (eq (test-completion string table) test))
              (should (equal (completion-boundaries string table nil "suffix")
                             (cons bound 6))))))))))

(ert-deftest bazel--target-completion-table/subpackage ()
  "Test target completion in a subpackage."
  (bazel-test--with-temp-directory dir "target-completion-package.org"
    ;; The test cases are of the form (PACKAGE STRING TRY ALL TEST BOUND).
    ;; PACKAGE is the package name (t stands for both "" and "package").  STRING
    ;; is the input string.  TRY, ALL, and TEST are the expected results of
    ;; ‘try-completion’, ‘all-completions’, and ‘test-completion’, respectively.
    ;; BOUND is the expected prefix completion bound returned by
    ;; ‘completion-bounds’.
    (pcase-dolist
        (`(,package ,string ,try ,all ,test ,bound)
         '(("" "" "" ("package" "..." "//" "@") nil 0)
           ("" "test" nil () nil 0)
           ("" "all" nil () nil 0)
           ("" "all-targets" nil () nil 0)
           ("" "*" nil () nil 0)
           ("" ":" nil () nil 1)
           ("" ":test" nil () nil 1)
           ("" ":all" nil () nil 1)
           ("" ":*" nil () nil 1)
           ("" ":all-targets" nil () nil 1)
           ("" "pack" "package" ("package") nil 0)
           ("" "package" "package" ("package") t 0)
           ("" "package/" "package/..." ("...") nil 8)
           ("" "package/." "package/..." ("...") nil 8)
           ("" "package/..." "package/..." ("..." "...:") t 8)
           ("" "package/...:" "package/...:" ("all" "all-targets" "*") nil 12)
           ("" "package:" "package:" ("test" "all" "all-targets" "*") nil 8)
           ("" "package:te" "package:test" ("test") nil 8)
           ("" "package:test" t ("test") t 8)
           ("" "package:a" "package:all" ("all" "all-targets") nil 8)
           ("" "package:all" "package:all" ("all" "all-targets") t 8)
           ("" "package:all-targets" t ("all-targets") t 8)
           ("" "package:*" t ("*") t 8)
           ("package" "" ""
            ("test" ":all" ":all-targets" ":*" "..." "//" "@") nil 0)
           ("package" "te" "test" ("test") nil 0)
           ("package" "test" t ("test") t 0)
           ("package" "a" nil () nil 0)
           ("package" "all" nil () nil 0)
           ("package" ":" ":" ("test" "all" "all-targets" "*") nil 1)
           ("package" ":te" ":test" ("test") nil 1)
           ("package" ":test" t ("test") t 1)
           ("package" ":a" ":all" ("all" "all-targets") nil 1)
           ("package" ":all" ":all" ("all" "all-targets") t 1)
           ("package" ":all-targets" t ("all-targets") t 1)
           ("package" ":*" t ("*") t 1)
           (t "@" "@//" ("//") nil 1)
           (t "@w" nil () nil 1)
           (t "/" "//" ("//") nil 0)
           (t "//" "//" (":" "package" "...") nil 2)
           (t "//:" nil () nil 3)
           (t "//:test" nil () nil 3)
           (t "//:all" nil () nil 3)
           (t "//:all-targets" nil () nil 3)
           (t "//:*" nil () nil 3)
           (t "//." "//..." ("...") nil 2)
           (t "//..." "//..." ("..." "...:") t 2)
           (t "//pack" "//package" ("package") nil 2)
           (t "//package" "//package" ("package") t 2)
           (t "//package/" "//package/..." ("...") nil 10)
           (t "//package/." "//package/..." ("...") nil 10)
           (t "//package/..." "//package/..." ("..." "...:") t 10)
           (t "//package/...:"
              "//package/...:" ("all" "all-targets" "*") nil 14)
           (t "//package:"
              "//package:" ("test" "all" "all-targets" "*") nil 10)
           (t "//package:te" "//package:test" ("test") nil 10)
           (t "//package:test" t ("test") t 10)
           (t "//package:a" "//package:all" ("all" "all-targets") nil 10)
           (t "//package:all" "//package:all" ("all" "all-targets") t 10)
           (t "//package:all-targets" t ("all-targets") t 10)
           (t "//package:*" t ("*") t 10)))
      (ert-info ((prin1-to-string string) :prefix "Input: ")
        (let ((packages (cond ((stringp package) (list package))
                              ((eq package t) '("" "package")))))
          (should (consp packages))
          (dolist (package packages)
            (ert-info ((prin1-to-string package) :prefix "Package: ")
              (let* ((default-directory (file-name-as-directory
                                         (expand-file-name package dir)))
                     (table (bazel--target-completion-table :pattern nil)))
                (should (equal (try-completion string table) try))
                (should (equal (all-completions string table) all))
                (should (eq (test-completion string table) test))
                (should (equal (completion-boundaries string table nil "suffix")
                               (cons bound 6)))))))))))

(ert-deftest bazel--target-completion-table/workspace ()
  "Test workspace name completion."
  (bazel-test--with-temp-directory dir "target-completion-workspace.org"
    (make-symbolic-link dir (expand-file-name "bazel-out" dir))
    ;; The test cases are of the form (STRING TRY ALL TEST BOUND).  STRING is
    ;; the input string.  TRY, ALL, and TEST are the expected results of
    ;; ‘try-completion’, ‘all-completions’, and ‘test-completion’, respectively.
    ;; BOUND is the expected prefix completion bound returned by
    ;; ‘completion-bounds’.
    (pcase-dolist
        (`(,string ,try ,all ,test ,bound)
         '(("@" "@" ("ext" "//") nil 1)
           ("@/" "@//" ("//") nil 1)
           ("@//" "@//" (":" "main-pkg" "...") nil 3)
           ("@//:" "@//:" ("all" "all-targets" "*") nil 4)
           ("@//:all" "@//:all" ("all" "all-targets") t 4)
           ("@//." "@//..." ("...") nil 3)
           ("@//..." "@//..." ("..." "...:") t 3)
           ("@//...:*" t ("*") t 7)
           ("@//m" "@//main-pkg" ("main-pkg") nil 3)
           ("@//main-pkg" "@//main-pkg" ("main-pkg") t 3)
           ("@e" "@ext" ("ext") nil 1)
           ("@ext" t ("ext" "ext//") t 1)
           ("@ext/" "@ext//" ("//") nil 4)
           ("@ext//" "@ext//" (":" "ext-pkg" "...") nil 6)
           ("@ext//:" "@ext//:" ("all" "all-targets" "*") nil 7)
           ("@ext//:a" "@ext//:all" ("all" "all-targets") nil 7)
           ("@ext//:all" "@ext//:all" ("all" "all-targets") t 7)
           ("@ext//." "@ext//..." ("...") nil 6)
           ("@ext//..." "@ext//..." ("..." "...:") t 6)
           ("@ext//...:*" t ("*") t 10)
           ("@ext//e" "@ext//ext-pkg" ("ext-pkg") nil 6)
           ("@ext//ext-pkg" "@ext//ext-pkg" ("ext-pkg") t 6)
           ("@ext//ext-pkg/" "@ext//ext-pkg/..." ("...") nil 14)
           ("@ext//ext-pkg:" "@ext//ext-pkg:" ("all" "all-targets" "*") nil 14)
           ("@q" nil () nil 1)
           ("@qux" nil () nil 1)
           ("@qux//:" nil () nil 7)
           ("@qux//p" nil () nil 6)))
      (ert-info ((prin1-to-string string) :prefix "Input: ")
        (let* ((default-directory (expand-file-name "main/" dir))
               (table (bazel--target-completion-table :pattern nil)))
          (should (equal (try-completion string table) try))
          (should (equal (all-completions string table) all))
          (should (eq (test-completion string table) test))
          (should (equal (completion-boundaries string table nil "suffix")
                         (cons bound 6))))))))

(ert-deftest bazel-mode/which-function ()
  "Verify that ‘which-function’ and ‘add-log-current-defun’ work
in ‘bazel-mode’."
  (bazel-test--with-temp-directory dir "xref.org"
    (bazel-test--with-file-buffer (expand-file-name "root/BUILD" dir)
      (bazel-mode)
      (let ((case-fold-search nil)
            (search-spaces-regexp nil))
        (pcase-dolist (`(,search-string ,expected-name)
                       '(("name = \"lib\"" "lib")
                         ("aaa.cc" "lib")
                         ("name = \"bin\"" "bin")
                         ("//pkg:lib" "bin")))
          (ert-info ((format "Search string: %s" search-string))
            (goto-char (point-min))
            (search-forward search-string)
            (let ((begin (match-beginning 0))
                  (end (match-end 0)))
              (pcase-dolist (`(,symbol ,position)
                             `((begin ,begin)
                               (end ,end)
                               (middle ,(/ (+ begin end) 2))))
                (ert-info ((format "Location: %s" symbol))
                  (goto-char position)
                  (should (equal (add-log-current-defun) expected-name))
                  (should (equal (which-function) expected-name)))))))))))

(ert-deftest bazel-build ()
  (cl-letf* ((commands ())
             ((symbol-function #'compile)
              (lambda (command &optional _comint)
                (push command commands))))
    (bazel-build "//foo:bar")
    (should (equal commands '("bazel build -- //foo\\:bar")))))

(ert-deftest bazel-run/options ()
  (cl-letf* ((commands ())
             ((symbol-function #'compile)
              (lambda (command &optional _comint)
                (push command commands)))
             (bazel-command-options '("--tool_tag=emacs")))
    (bazel-run "//foo:bar")
    (should (equal commands '("bazel run --tool_tag\\=emacs -- //foo\\:bar")))))

(ert-deftest bazel-build-mode/font-lock ()
  "Test Font Locking in ‘bazel-build-mode’."
  (let ((text (ert-propertized-string
               '(face font-lock-comment-delimiter-face) "# "
               '(face font-lock-comment-face) "comment\n" nil "\n"
               '(face font-lock-keyword-face) "load" nil "("
               '(face font-lock-string-face) "\"foo.bzl\"" nil ", "
               '(face font-lock-string-face) "\"\"\"foo\"\"\"" nil ")\n\n"
               nil "cc_library(\n"
               nil "    name = " '(face font-lock-string-face) "\""
               '(face (font-lock-variable-name-face font-lock-string-face))
               "foo"
               '(face font-lock-string-face) "\"" nil ",\n"
               nil "    "
               '(face font-lock-comment-delimiter-face) "# "
               '(face (font-lock-preprocessor-face font-lock-comment-face))
               "keep sorted" '(face font-lock-comment-face) "\n"
               nil "    srcs = "
               '(face font-lock-builtin-face) "glob" nil "(["
               '(face font-lock-string-face) "\"*.cc\"" nil "]),\n"
               nil "    alwayslink = "
               '(face font-lock-constant-face) "True" nil ",\n"
               nil "    linkstatic="
               '(face font-lock-constant-face) "True" nil ",\n"
               nil")\n\n"
               nil "some_rule(name = "
               '(face font-lock-string-face) "\"foo\"" nil " + SUFFIX)\n\n"
               nil "name_only(name = " '(face font-lock-string-face) "\""
               '(face (font-lock-variable-name-face font-lock-string-face))
               "foo"
               '(face font-lock-string-face) "\"" nil ")\n\n"
               nil "some_rule(\n"
               nil "    filename = "
               '(face font-lock-string-face) "\"file.txt\"" nil ",\n"
               nil ")\n\n")))
    (with-temp-buffer
      (bazel-build-mode)
      (insert (substring-no-properties text))
      (font-lock-flush)
      (font-lock-ensure)
      ;; Emacs 26 adds a ‘syntax-table’ text property, which we don’t care
      ;; about.
      (remove-list-of-text-properties (point-min) (point-max) '(syntax-table))
      (should (ert-equal-including-properties (buffer-string) text)))))

(ert-deftest bazel-compile-current-file ()
  "Test for ‘bazel-compile-current-file’."
  (bazel-test--with-temp-directory dir nil
    (write-region "" nil (expand-file-name "WORKSPACE" dir))
    (write-region "" nil (expand-file-name "BUILD" dir))
    (make-directory (expand-file-name "package" dir))
    (write-region "" nil (expand-file-name "package/BUILD" dir))
    (write-region "" nil (expand-file-name "package/test.cc" dir))
    (bazel-test--with-file-buffer (expand-file-name "package/test.cc" dir)
      (pcase-dolist
          (`(,default-directory ,want-commands)
           `((,dir ("bazel build --compile_one_dependency -- package/test.cc"))
             (,(expand-file-name "package" dir)
              ("bazel build --compile_one_dependency -- test.cc"))))
        (cl-letf* ((got-commands ())
                   ((symbol-function #'compile)
                    (lambda (command &optional _comint)
                      (push command got-commands))))
          (bazel-compile-current-file)
          (should (equal got-commands want-commands)))))))

(ert-deftest bazel-completion-at-point ()
  "Test for ‘completion-at-point’ in ‘bazel-mode’."
  (bazel-test--with-temp-directory dir "completion-at-point.org"
    (bazel-test--with-file-buffer (expand-file-name "BUILD" dir)
      (let* ((case-fold-search nil)
             (search-spaces-regexp nil)
             (got-args ())
             (completion-in-region-function
              (lambda (start end collection predicate)
                (push (list start end collection predicate) got-args))))
        (ert-info ("outside of a label")
          (goto-char (point-min))
          (completion-at-point)
          (should-not got-args))
        (ert-info ("within a label")
          (re-search-forward
           (rx bol (+ blank) "deps = [\"" (group "//:l") (group "i") "\"]"))
          (let ((want-begin (match-beginning 1))
                (want-end (match-end 2))
                (point (match-beginning 2))
                (string (match-string-no-properties 1))
                (suffix (match-string-no-properties 2)))
            (goto-char point)
            (completion-at-point)
            (pcase got-args
              (`((,begin ,end ,collection nil))
               (should (integer-or-marker-p begin))
               (should (integer-or-marker-p end))
               (should (= begin want-begin))
               (should (= end want-end))
               (should (equal (try-completion string collection) "//:lib"))
               (should (equal (all-completions string collection) '("lib")))
               (should (equal
                        (completion-boundaries string collection nil suffix)
                        '(3 . 1))))
              (_ (ert-fail (format "Unexpected arguments %S" got-args))))))))))

(ert-deftest bazel-test-at-point ()
  (bazel-test--with-temp-directory dir "test-at-point-elisp.org"
    (bazel-test--with-file-buffer (expand-file-name "foo.el" dir)
      (let ((case-fold-search nil)
            (search-spaces-regexp nil))
        (re-search-forward (rx bol "(ert-deftest foo/not-really-a-test ()"))
        ;; Point is on a test case, but the consuming rule is not a test rule.
        (should-error (bazel-test-at-point) :type 'user-error)))
    (bazel-test--with-file-buffer (expand-file-name "foo-test.el" dir)
      (cl-letf* ((case-fold-search nil)
                 (search-spaces-regexp nil)
                 (commands ())
                 ((symbol-function #'compile)
                  (lambda (command &optional _comint)
                    (push command commands))))
        (should-error (bazel-test-at-point) :type 'user-error)
        (re-search-forward (rx bol "(ert-deftest foo/test ()"))
        (bazel-test-at-point)
        (should
         (equal commands
                '("bazel test --test_filter\\=foo/test -- //\\:foo_test")))))))

(ert-deftest bazel-test-at-point/python-mode ()
  "Test ‘bazel-test-at-point’ in ‘python-mode’."
  (bazel-test--with-temp-directory dir "test-at-point-python.org"
    (bazel-test--with-file-buffer (expand-file-name "py_test.py" dir)
      (cl-letf* ((case-fold-search nil)
                 (search-spaces-regexp nil)
                 (commands ())
                 ((symbol-function #'compile)
                  (lambda (command &optional _comint)
                    (push command commands))))
        (should-error (bazel-test-at-point) :type 'user-error)
        (search-forward "# Test case class")
        (bazel-test-at-point)
        (search-forward "# Test method")
        (bazel-test-at-point)
        (should
         (equal (reverse commands)
                '("bazel test --test_filter\\=MyTest -- //\\:py_test"
                  "bazel test --test_filter\\=MyTest.testFoo -- //\\:py_test")))))))

(ert-deftest bazel-test-at-point/c++-mode ()
  "Test ‘bazel-test-at-point’ in ‘c++-mode’."
  (bazel-test--with-temp-directory dir "test-at-point-c++.org"
    (bazel-test--with-file-buffer (expand-file-name "cc_test.cc" dir)
      (cl-letf* ((case-fold-search nil)
                 (search-spaces-regexp nil)
                 (commands ())
                 ((symbol-function #'compile)
                  (lambda (command &optional _comint)
                    (push command commands))))
        (should-error (bazel-test-at-point) :type 'user-error)
        (search-forward "EXPECT_EQ(1, 2)")
        (bazel-test-at-point)
        (search-forward "EXPECT_EQ(4, 5)")
        (bazel-test-at-point)
        (should
         (equal (reverse commands)
                '("bazel test --test_filter\\=FooTest.Bar -- //\\:cc_test"
                  "bazel test --test_filter\\=BazTest.Qux -- //\\:cc_test")))))))

(ert-deftest bazel-test-at-point/go-mode ()
  "Test ‘bazel-test-at-point’ in ‘go-mode’."
  (bazel-test--with-temp-directory dir "test-at-point-go.org"
    (bazel-test--with-file-buffer (expand-file-name "go_test.go" dir)
      (cl-letf* ((case-fold-search nil)
                 (search-spaces-regexp nil)
                 (commands ())
                 ((symbol-function #'compile)
                  (lambda (command &optional _comint)
                    (push command commands))))
        (unless (derived-mode-p 'go-mode)
          ;; ‘go-mode’ might not be installed or available.  Simulate it as
          ;; best as possible.
          (setq-local beginning-of-defun-function
                      (lambda (&optional arg)
                        (let ((case-fold-search nil)
                              (search-spaces-regexp nil))
                          (re-search-backward (rx bol "func" blank)
                                              nil t arg))))
          (run-hooks 'go-mode-hook))
        (should-error (bazel-test-at-point) :type 'user-error)
        (search-forward "t.Error(")
        (bazel-test-at-point)
        (search-forward "b.Error(")
        (bazel-test-at-point)
        (should
         (equal (reverse commands)
                '("bazel test --test_filter\\=\\^\\\\QTest\\\\E\\$ -- //\\:go_test"
                  "bazel test --test_filter\\=\\^\\\\QBenchmarkFoo_Bar\\\\E\\$ -- //\\:go_test")))))))

(ert-deftest bazel-test-at-point-functions ()
  "Test that ‘which-function’ is at the end of
‘bazel-test-at-point-functions’."
  (skip-unless (>= emacs-major-version 27))  ; older versions lack hook depths
  (let ((bazel-test-at-point-functions bazel-test-at-point-functions)
        (which-func-functions
         (list (lambda () (ert-fail "‘which-function’ has been called")))))
    (add-hook 'bazel-test-at-point-functions (lambda () "foobar") 50)
    (should (equal
             (run-hook-with-args-until-success 'bazel-test-at-point-functions)
             "foobar"))))

(ert-deftest bazel-buildifier/success ()
  (bazel-test--with-temp-directory dir nil
    (let ((bash (executable-find "bash"))
          (bazel-buildifier-command (expand-file-name "buildifier" dir)))
      (skip-unless bash)
      (with-temp-file bazel-buildifier-command
        (insert "#!" (file-name-unquote bash) ?\n
                "set -efu\n"
                "cd " (shell-quote-argument (file-name-unquote dir)) ?\n
                "cat > input\n"
                "echo \"$*\" > args\n"
                "echo output\n"
                "echo error >&2\n"))
      (set-file-modes bazel-buildifier-command #o0500)
      (pcase-dolist (`(,type ,args)
                     '((nil "-type=bzl")
                       (workspace "-type=workspace")
                       (default "-type=default")))
        (ert-info ((symbol-name type) :prefix "Explicit type: ")
          (with-temp-buffer
            (insert "input")
            (bazel-starlark-mode)
            (bazel-buildifier type)
            (should (equal (buffer-string) "output\n")))
          (pcase-dolist (`(,file ,expected)
                         `(("input" "input")
                           ("args" ,(concat args "\n"))))
            (ert-info (file :prefix "File: ")
              (with-temp-buffer
                (insert-file-contents (expand-file-name file dir))
                (should (equal (buffer-string) expected))))))))))

(ert-deftest bazel-buildifier/failure ()
  (bazel-test--with-temp-directory dir "buildifier.org"
    (let* ((bash (executable-find "bash"))
           (bazel-buildifier-command (expand-file-name "buildifier" dir))
           (error-file (expand-file-name "buildifier.err" dir))
           (temp-buffers nil))
      (skip-unless bash)
      (with-temp-file bazel-buildifier-command
        (insert "#!" (file-name-unquote bash) ?\n
                "set -Cefu\n"
                "cat > /dev/null\n"  ; don’t exit before reading input
                "echo output\n"
                "cat -- " (shell-quote-argument (file-name-unquote error-file))
                " >&2\n"
                "exit 1\n"))
      (set-file-modes bazel-buildifier-command #o0500)
      (with-temp-buffer
        (insert-file-contents (expand-file-name "pkg/BUILD" dir) :visit)
        (bazel-starlark-mode)
        (let ((tick-before (buffer-modified-tick))
              (temp-buffer-window-show-hook
               (list (lambda () (push (current-buffer) temp-buffers)))))
          (bazel-buildifier)
          (should (equal (buffer-string) "cc_library(\n"))  ; no change
          (should (eql (buffer-modified-tick) tick-before))))
      (should (eql (length temp-buffers) 1))
      (with-current-buffer (car temp-buffers)
        (ert-info ("Error buffer")
          (should (bazel-test--file-equal-p default-directory dir))
          (should (equal (buffer-string) "pkg/BUILD:3:1: syntax error
pkg/BUILD # reformat

Process buildifier exited abnormally with code 1
")))))))

(ert-deftest bazel-buildifier-before-save ()
  (bazel-test--with-temp-directory dir nil
    (let ((bash (executable-find "bash"))
          (input-file (expand-file-name "input" dir))
          (marker-file (expand-file-name "ok" dir))
          (bazel-buildifier-command (expand-file-name "buildifier" dir)))
      (skip-unless bash)
      (write-region "" nil input-file nil nil nil 'excl)
      (with-temp-file bazel-buildifier-command
        (insert "#!" (file-name-unquote bash) ?\n
                "set -efu\n"
                "cd " (shell-quote-argument (file-name-unquote dir)) ?\n
                "cat > /dev/null\n"   ; don’t exit before reading input
                "echo output\n"
                "touch ok\n"))
      (set-file-modes bazel-buildifier-command #o0500)
      (pcase-dolist (`(,bazel-buildifier-before-save ,expected)
                     '((nil "input\n") (t "output\n")))
        (ert-info ((symbol-name bazel-buildifier-before-save)
                   :prefix "Value of ‘bazel-buildifier-before-save’: ")
          (delete-file marker-file)
          (bazel-test--with-file-buffer input-file
            (bazel-starlark-mode)
            (insert "input\n")
            (should (buffer-modified-p))
            (save-buffer)
            (should-not (buffer-modified-p))
            (should (equal (buffer-string) expected)))
          (should (eq bazel-buildifier-before-save
                      (file-exists-p marker-file))))))))

(ert-deftest bazel-insert-http-archive ()
  (bazel-test--with-temp-directory dir "http-archive.org"
    (should (set-file-times (expand-file-name "prefix" dir)
                            (encode-time 0 0 0 2 5 2019 t)))
    (let* ((archive (file-name-unquote
                     (expand-file-name "archive.tar.gz" dir)))
           (url-unreserved-chars (cons ?/ url-unreserved-chars))
           (url (concat "file://" (url-hexify-string archive)))
           (tar (executable-find "tar"))
           (sha256sum (executable-find "sha256sum"))
           (default-directory dir))
      (skip-unless tar)
      (skip-unless sha256sum)
      (process-lines tar "-c" "-z" "-f" archive "--" "prefix")
      (let ((actual
             (with-temp-buffer
               (bazel-workspace-mode)
               (when (version< emacs-version "26.2")
                 ;; Work around Bug#31950.
                 (set-window-buffer nil (current-buffer)))
               (bazel-insert-http-archive url)
               (buffer-substring-no-properties (point-min) (point-max))))
            (expected
             (with-temp-buffer
               (insert-file-contents
                (expand-file-name "WORKSPACE.expected" dir))
               (let ((case-fold-search nil)
                     (search-spaces-regexp nil))
                 (pcase (process-lines sha256sum "-b" "--" archive)
                   ;; “sha256sum” should print exactly one line, the hash
                   ;; followed by a space and the filename.
                   ;; See Info node ‘(coreutils) sha2 utilities’
                   ;; and Info node ‘(coreutils) md5sum invocation’.
                   (`(,(rx bos (let hash (+ xdigit)) ?\s))
                    ;; Replace variables in expected snippet with their values.
                    (dolist (pair `(("%sha256%" . ,hash)
                                    ("%url%" . ,url)))
                      (cl-destructuring-bind (variable . value) pair
                        (goto-char (point-min))
                        (while (search-forward variable nil t)
                          (replace-match value :fixedcase :literal)))))
                   (o (ert-fail (format "Invalid sha256sum output: %S" o)))))
               ;; We don’t expect a trailing newline.
               (goto-char (point-max))
               (skip-chars-backward "\n")
               (buffer-substring-no-properties (point-min) (point)))))
        (should (equal actual expected))))))

(ert-deftest bazel-insert-http-archive/empty ()
  (bazel-test--with-temp-directory dir nil
    (let* ((archive (file-name-unquote
                     (expand-file-name "archive.tar.gz" dir)))
           (url-unreserved-chars (cons ?/ url-unreserved-chars))
           (url (concat "file://" (url-hexify-string archive)))
           (tar (executable-find "tar"))
           (default-directory dir))
      (skip-unless tar)
      ;; https://superuser.com/a/448624
      (process-lines tar "-c" "-z" "-f" archive "-T" "/dev/null")
      (with-temp-buffer
        (let ((tick-before (buffer-modified-tick)))
          (bazel-workspace-mode)
          (when (version< emacs-version "26.2")
            ;; Work around Bug#31950.
            (set-window-buffer nil (current-buffer)))
          (should-error (bazel-insert-http-archive url) :type 'user-error)
          (should (eql (buffer-modified-tick) tick-before))  ; no change
          (should (eq (buffer-size) 0)))))))

(ert-deftest bazel-insert-http-archive/no-unique-prefix ()
  (bazel-test--with-temp-directory dir "http-archive-no-unique-prefix.org"
    (let* ((archive (file-name-unquote
                     (expand-file-name "archive.tar.gz" dir)))
           (url-unreserved-chars (cons ?/ url-unreserved-chars))
           (url (concat "file://" (url-hexify-string archive)))
           (tar (executable-find "tar"))
           (default-directory dir))
      (skip-unless tar)
      (process-lines tar "-c" "-z" "-f" archive "--" "prefix-1" "prefix-2")
      (with-temp-buffer
        (let ((tick-before (buffer-modified-tick)))
          (bazel-workspace-mode)
          (when (version< emacs-version "26.2")
            ;; Work around Bug#31950.
            (set-window-buffer nil (current-buffer)))
          (should-error (bazel-insert-http-archive url) :type 'user-error)
          (should (eql (buffer-modified-tick) tick-before))  ; no change
          (should (eq (buffer-size) 0)))))))

(ert-deftest bazel-insert-http-archive/invalid-archive ()
  ;; Don’t let ‘jka-compr’ interfere with writing the invalid archive file.
  (let ((jka-compr-inhibit t))
    (bazel-test--with-temp-directory dir "http-archive-invalid.org"
      (let* ((archive (file-name-unquote
                       (expand-file-name "archive.tar.gz" dir)))
             (url-unreserved-chars (cons ?/ url-unreserved-chars))
             (url (concat "file://" (url-hexify-string archive))))
        (with-temp-buffer
          (let ((tick-before (buffer-modified-tick)))
            (bazel-workspace-mode)
            (when (version< emacs-version "26.2")
              ;; Work around Bug#31950.
              (set-window-buffer nil (current-buffer)))
            (should-error (bazel-insert-http-archive url))
            (should (eql (buffer-modified-tick) tick-before))  ; no change
            (should (eq (buffer-size) 0))))))))

(ert-deftest bazelrc-mode ()
  (let ((text (ert-propertized-string
               '(face font-lock-keyword-face) "import"
               nil " %workspace%/other.bazelrc\n"
               '(face font-lock-comment-delimiter-face) "# "
               '(face font-lock-comment-face) "Comment\n"
               '(face font-lock-variable-name-face) "build"
               nil " --verbose_failures\n")))
    (with-temp-buffer
      (bazelrc-mode)
      (insert (substring-no-properties text))
      (font-lock-flush)
      (font-lock-ensure)
      (should (ert-equal-including-properties (buffer-string) text)))))

(ert-deftest bazelrc-ffap ()
  (bazel-test--with-temp-directory dir "bazelrc.org"
    (bazel-test--with-file-buffer (expand-file-name ".bazelrc" dir)
      (let ((case-fold-search nil)
            (search-spaces-regexp nil))
        (should (derived-mode-p 'bazelrc-mode))
        (search-forward "import %workspace%/")
        (let ((file-at-point (ffap-file-at-point)))
          (should (stringp file-at-point))
          (should (bazel-test--file-equal-p
                   file-at-point (expand-file-name "other.bazelrc" dir))))))))

(ert-deftest bazel-find-build-file ()
  (bazel-test--with-temp-directory dir nil
    (dolist (file '("WORKSPACE"
                    "BUILD"
                    "a/WORKSPACE"
                    "a/b/BUILD.bazel"
                    "a/b/BUILD"
                    "a/b/c/BUILD"
                    "a/b/c/d/BUILD.bazel"
                    "a/b/c/d/e/BUILD"))
      (cl-callf expand-file-name file dir)
      (make-directory (file-name-directory file) :parents)
      (write-region "" nil file nil nil nil 'excl))
    ;; CASE is a pair (FILE EXPECTED), where FILE is the current file and
    ;; EXPECTED is either the name of the BUILD file to find or a symbol
    ;; denoting an expected error.
    (pcase-dolist (`(,parent ,expected)
                   '(("" "BUILD")
                     ("a" user-error)
                     ("a/b" "a/b/BUILD.bazel")
                     ("a/b/c" "a/b/c/BUILD")
                     ("a/b/c/d" "a/b/c/d/BUILD.bazel")
                     ("a/b/c/d/e" "a/b/c/d/e/BUILD")
                     ("a/b/c/d/e/f" "a/b/c/d/e/BUILD")
                     ("z" "BUILD")
                     ("a/b/z" "a/b/BUILD.bazel")))
      (ert-info (parent :prefix "Parent directory: ")
        (with-temp-buffer
          (let ((buffer-file-name
                 (expand-file-name "test.cc" (expand-file-name parent dir))))
            (bazel-test--with-buffers
              (cl-etypecase expected
                (string
                 (bazel-find-build-file)
                 (should buffer-file-name)
                 (should (bazel-test--file-equal-p
                          buffer-file-name (expand-file-name expected dir))))
                (symbol
                 (should-error (bazel-find-build-file)
                               :type expected))))))))))

(ert-deftest bazel-find-workspace-file ()
  (bazel-test--with-temp-directory dir nil
    (dolist (file '("WORKSPACE"
                    "a/b/WORKSPACE.bazel"
                    "a/b/WORKSPACE"
                    "a/b/c/WORKSPACE"
                    "a/b/c/d/WORKSPACE.bazel"
                    "a/b/c/d/e/WORKSPACE"))
      (cl-callf expand-file-name file dir)
      (make-directory (file-name-directory file) :parents)
      (write-region "" nil file nil nil nil 'excl))
    ;; CASE is a pair (FILE EXPECTED), where FILE is the current file and
    ;; EXPECTED is the name of the WORKSPACE file to find.
    (pcase-dolist (`(,parent ,expected)
                   '(("" "WORKSPACE")
                     ("a" "WORKSPACE")
                     ("a/b" "a/b/WORKSPACE.bazel")
                     ("a/b/c" "a/b/c/WORKSPACE")
                     ("a/b/c/d" "a/b/c/d/WORKSPACE.bazel")
                     ("a/b/c/d/e" "a/b/c/d/e/WORKSPACE")
                     ("a/b/c/d/e/f" "a/b/c/d/e/WORKSPACE")
                     ("z" "WORKSPACE")
                     ("a/b/z" "a/b/WORKSPACE.bazel")))
      (ert-info (parent :prefix "Parent directory: ")
        (with-temp-buffer
          (let ((buffer-file-name
                 (expand-file-name "test.cc" (expand-file-name parent dir))))
            (bazel-test--with-buffers
              (bazel-find-workspace-file)
              (should buffer-file-name)
              (should (bazel-test--file-equal-p
                       buffer-file-name
                       (expand-file-name expected dir))))))))))

(ert-deftest bazelignore-mode/font-lock ()
  "Test Font Locking in ‘bazelignore-mode’."
  (let ((text (ert-propertized-string
               '(face font-lock-comment-delimiter-face) "# "
               '(face font-lock-comment-face) "comment\n" nil "\n"
               nil "directory\n\n"
               nil "directory/subdirectory\n"
               ;; Comments in .bazelignore files must cover an entire line,
               ;; cf. https://github.com/bazelbuild/bazel/blob/09c621e4cf5b968f4c6cdf905ab142d5961f9ddc/src/main/java/com/google/devtools/build/lib/skyframe/IgnoredPackagePrefixesFunction.java#L123.
               nil "directory # with number sign\n"
               '(face font-lock-comment-delimiter-face) "# "
               '(face font-lock-comment-face) "comment at end of buffer")))
    (with-temp-buffer
      (bazelignore-mode)
      (insert (substring-no-properties text))
      (font-lock-flush)
      (font-lock-ensure)
      ;; Remove the ‘syntax-table’ properties that
      ;; ‘bazelignore--syntax-propertize’ added; they are implementation
      ;; details.
      (remove-list-of-text-properties (point-min) (point-max) '(syntax-table))
      (should (ert-equal-including-properties (buffer-string) text)))))

(ert-deftest bazel-test/completion ()
  "Test completion for ‘bazel-test’."
  (bazel-test--with-temp-directory dir "target-completion-root.org"
    (bazel-test--with-file-buffer (expand-file-name "foo_test.py" dir)
      (cl-letf* ((completing-read-args ())
                 (completing-read-function
                  (lambda (&rest args)
                    (push args completing-read-args)
                    ":test"))
                 (compile-commands ())
                 ((symbol-function #'compile)
                  (lambda (command &optional _comint)
                    (push command compile-commands))))
        (call-interactively #'bazel-test)
        (pcase completing-read-args
          (`(("bazel -- test " ,_ nil nil nil bazel-target-history
              "//:foo_test" nil)))
          (_ (ert-fail (list "Invalid arguments to ‘completing-read’"
                             completing-read-args))))
        (should (equal compile-commands '("bazel test -- \\:test")))))))

(ert-deftest bazel-fix-visibility ()
  (bazel-test--with-temp-directory workspace "fix-visibility.org"
    (cl-letf* ((commands ())
               ((symbol-function #'process-file)
                (lambda (program &optional _infile _buffer _display &rest args)
                  (push (cons program args) commands)
                  0)))
      (with-temp-buffer
        (insert-file-contents (expand-file-name "bazel.out" workspace))
        (let ((bazel-fix-visibility t)
              (default-directory (expand-file-name "lib/" workspace)))
          (compilation-handle-exit 'exit 1 "exited abnormally with code 1\n")))
      (should
       (equal commands
              '(("buildozer" "--" "add visibility //:__pkg__" "//lib:lib")))))))

(ert-deftest bazel/set-auto-mode ()
  "Test that ‘set-auto-mode’ finds the expected modes."
  (with-temp-buffer
    (pcase-dolist (`(,file ,mode)
                   '(("BUILD" bazel-build-mode)
                     ("BUILD.bazel" bazel-build-mode)
                     ("WORKSPACE" bazel-workspace-mode)
                     ("WORKSPACE.bazel" bazel-workspace-mode)
                     ("def.bzl" bazel-starlark-mode)
                     (".bazelrc" bazelrc-mode)
                     (".bazelignore" bazelignore-mode)))
      (ert-info (file :prefix "File name: ")
        (let ((enable-local-variables nil)
              (auto-mode-case-fold nil)
              (buffer-file-name (expand-file-name file)))
          (set-auto-mode)
          (should (eq major-mode mode)))))))

;;;; Test helpers

(put #'looking-at-p 'ert-explainer #'bazel-test--explain-looking-at-p)

(defun bazel-test--explain-looking-at-p (regexp)
  "ERT explainer for ‘looking-at-p’.
See Info node ‘(ert) Defining Explanation Functions’.  REGEXP is
the expected regular expression."
  (cl-check-type regexp string)
  (unless (looking-at-p regexp)
    `(rest-of-line ,(buffer-substring-no-properties
                     (point) (line-end-position)))))

(defun bazel-test--tangle (directory org-file)
  "Expand code blocks in ORG-FILE into DIRECTORY.
ORG-FILE is a filename within the “testdata” directory.  The code
blocks should have a ‘:tangle’ header argument specifying the
filename within DIRECTORY.
See Info node ‘(org) Extracting Source Code’."
  (cl-check-type directory string)
  (cl-check-type org-file string)
  ;; Tangling requires a file-visiting Org buffer.
  (bazel-test--with-file-buffer (expand-file-name (concat "testdata/" org-file)
                                                  bazel-test--directory)
    (let ((default-directory directory)
          (org-babel-tangle-body-hook
           (list (lambda ()
                   ;; Replace the %ROOTDIR% placeholder added by
                   ;; testdata/make_*_out by our temporary root.
                   (save-excursion
                     (let ((case-fold-search nil)
                           (search-spaces-regexp nil))
                       (goto-char (point-min))
                       (while (search-forward "%ROOTDIR%" nil t)
                         (replace-match
                          (file-name-unquote (directory-file-name directory))
                          :fixedcase :literal))))))))
      (org-babel-tangle))))

(defun bazel-test--buildifier-running-p ()
  "Return whether we have a running Buildifier process.
This relies on the variable ‘bazel-buildifier-command’"
  (cl-some (lambda (process)
             (and (eq (process-type process) 'real)
                  (bazel-test--file-equal-p (car (process-command process))
                                            bazel-buildifier-command)))
           (process-list)))

;; In Emacs 26, ‘file-equal-p’ is buggy and doesn’t work correctly on quoted
;; filenames.  We can drop this hack once we stop supporting Emacs 26.
(defalias 'bazel-test--file-equal-p
  (if (>= emacs-major-version 27) #'file-equal-p
    (lambda (a b)
      (file-equal-p (file-name-unquote a) (file-name-unquote b)))))

;;; test.el ends here
