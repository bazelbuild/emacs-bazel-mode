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

(defun bazel-build (label)
  "Build a Bazel LABEL."
  (interactive "sbazel build //")
  (bazel-build--run-bazel-command "build" label))

(defun bazel-run (label)
  "Run a Bazel LABEL."
  (interactive "sbazel run //")
  (bazel-build--run-bazel-command "run" label))

(defun bazel-test (label)
  "Run a Bazel test LABEL."
  (interactive "sbazel test //")
  (bazel-build--run-bazel-command "test" label))

(defun bazel-build--run-bazel-command (command label)
  "Run Bazel tool with given COMMAND, e.g. build or run, on the given LABEL."
  (compile
   (mapconcat #'shell-quote-argument (list "bazel" command label) " ")))

(provide 'bazel-build)

;;; bazel-build.el ends here
