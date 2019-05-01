;;; bazel-build.el --- Emacs utilities for using Bazel -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2018 Google LLC
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;      http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;
;;; Commentary:
;;
;; This package provides commands to build and run code using Bazel.
;; It defines interactive commands which perform completion of available Bazel
;; targets:
;;   - `bazel-build'
;;   - `bazel-run'
;;   - `bazel-test'
;;
;;; Code:

(defun bazel-build (target)
  "Build a Bazel TARGET."
  (interactive (list (bazel-build--read-target "bazel build ")))
  (bazel-build--run-bazel-command "build" target))

(defun bazel-run (target)
  "Build and run a Bazel TARGET."
  (interactive (list (bazel-build--read-target "bazel run ")))
  (bazel-build--run-bazel-command "run" target))

(defun bazel-test (target)
  "Build and run a Bazel test TARGET."
  (interactive (list (bazel-build--read-target "bazel test ")))
  (bazel-build--run-bazel-command "test" target))

(defun bazel-build--run-bazel-command (command target)
  "Run Bazel tool with given COMMAND, e.g. build or run, on the given TARGET."
  (compile
   (mapconcat #'shell-quote-argument (list "bazel" command target) " ")))

(defun bazel-build--read-target (prompt)
  "Read a Bazel build target from the minibuffer.  PROMPT is a read-only prompt."
  (let* ((file-name (buffer-file-name))
         (workspace-root
          (or (bazel-build--find-workspace-root file-name)
              (user-error "Not in a Bazel workspace. No WORKSPACE file found.")))
         (package-name
          (or (bazel-build--extract-package-name file-name workspace-root)
              (user-error "Not in a Bazel package. No BUILD file found.")))
         (initial-input (concat "//" package-name)))
    (read-string prompt initial-input)))

(defun bazel-build--find-workspace-root (file-name)
  "Find the root of the Bazel workspace containing FILE-NAME.
If FILE-NAME is not in a Bazel workspace, return nil."
  (let ((workspace-root (locate-dominating-file file-name "WORKSPACE")))
    (when workspace-root workspace-root)))

(defun bazel-build--extract-package-name (file-name workspace-root)
  "Return the nearest Bazel package for FILE-NAME under WORKSPACE-ROOT.
If FILE-NAME is not in a Bazel package, return nil."
  (let* ((build-file-directory
          (cl-some (lambda (build-name)
                     (locate-dominating-file file-name build-name))
                   '("BUILD.bazel" "BUILD")))
         (package-name
          (when build-file-directory
            (cond ((string= workspace-root build-file-directory) "")
                  ((string-prefix-p workspace-root build-file-directory)
                   (file-relative-name build-file-directory workspace-root))
                  (t nil)))))
    ;; Only return package-name if we can confirm it is the local relative
    ;; file name of a BUILD file.
    (and package-name
         (not (file-remote-p package-name))
         (not (file-name-absolute-p package-name))
         (directory-file-name package-name))))

(provide 'bazel-build)

;;; bazel-build.el ends here
