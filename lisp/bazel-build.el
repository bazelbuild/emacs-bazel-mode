;;; bazel-build.el --- Emacs utilities for using Bazel -*- lexical-binding: t; -*-

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

;;; Commentary:

;; This package provides commands to build and run code using Bazel.
;; It defines interactive commands which perform completion of available Bazel
;; targets:
;;   - `bazel-build'
;;   - `bazel-run'
;;   - `bazel-test'

;;; Code:

(require 'bazel-util)
(require 'subr-x)

(defun bazel-build (target)
  "Build a Bazel TARGET."
  (interactive (list (bazel-build--read-target "bazel build")))
  (bazel-build--run-bazel-command "build" target))

(defun bazel-run (target)
  "Build and run a Bazel TARGET."
  (interactive (list (bazel-build--read-target "bazel run")))
  (bazel-build--run-bazel-command "run" target))

(defun bazel-test (target)
  "Build and run a Bazel test TARGET."
  (interactive (list (bazel-build--read-target "bazel test")))
  (bazel-build--run-bazel-command "test" target))

(defvar bazel-command "bazel"
  "Bazel executable to run for subprocesses.")

(defvar bazel-query-args '("query" "--noblock_for_lock")
  "Bazel query subcommand and arguments.")

(defvar bazel-run-commands-from-root nil
  "A flag indicating to run commands from the workspace root.")

(defun bazel-build--run-bazel-command (command target)
  "Run Bazel tool with given COMMAND, e.g. build or run, on the given TARGET."
  (let ((command
         (if bazel-run-commands-from-root
             (let ((root-dir (or (bazel-util-workspace-root
                                  (or (buffer-file-name)
                                      (and dired-directory (directory-file-name dired-directory))))
                                 (user-error "Could not find workspace"))))
               (format "cd %s && %s %s %s" root-dir
                       bazel-command command target))
           (mapconcat #'shell-quote-argument (list bazel-command command target) " "))))
    (compile command)))

(defun bazel-build--process (&rest args)
  "Run a bazel subcommand with arguments ARGS. Return stdout as string."
  ;; Note: The current working directory of the subprocess is set to the current
  ;; buffer's value of default-directory.
  (let ((stderr-file (make-temp-file "bazel-stderr")))
    (unwind-protect
        (string-trim-right
         (with-output-to-string
           (let ((status
                  (apply #'process-file bazel-command
                         nil (list standard-output stderr-file) nil args)))
             (if (/= status 0)
                 (with-temp-buffer
                   (insert-file-contents stderr-file)
                   (error "Error running command: %s" (buffer-string)))))))
      (if (file-exists-p stderr-file)
	  (delete-file stderr-file)))))

(defun bazel-query (&rest args)
  "Run a bazel query subcommand with argument ARGS. Return stdout as string."
  (apply #'bazel-build--process (append bazel-query-args args)))

(defun bazel-build--target-for-file (filename)
  "Get the list of targets which includes FILENAME."
  (let* ((default-directory (file-name-directory filename))
         ;; Resolve label for file with bazel query.
         (fullname (bazel-query (file-name-nondirectory filename))))
    ;; Produce target the file-label is in using bazel query.
    (let ((cmd (format "attr('srcs', %s, %s:*)" fullname
                       (car (split-string fullname ":")))))
      (split-string (bazel-query cmd)))))

(defun bazel-build--target-for-directory (dirname)
  "Get the list of targets under DIRNAME."
  (let* ((default-directory dirname)
         (results (split-string
                   (bazel-query "kind('.*rule', ':all')")))
         (package (car (split-string (car results) ":"))))
    (cons (concat package ":all") results)))

(defun bazel-build--target-for-directory-or-filename (file-or-dir)
  "Get the list of targets under the FILE-OR-DIR, a filename or a directory name."
  (if (file-directory-p file-or-dir)
      (bazel-build--target-for-directory file-or-dir)
    (bazel-build--target-for-file file-or-dir)))

(defun bazel-build--read-target (prompt &optional filename)
  "Read a target name for current buffer or dired directory name FILENAME.
Prompt with PROMPT."
  ;; Bazel query invocation can be slow, issue a message.
  (message "Generating completions...")
  (let* ((targets (bazel-build--target-for-directory-or-filename
                   (or
                    ;; A given filename.
                    filename
                    ;; Open on a BUILD file.
                    (let* ((file-name (buffer-file-name)))
                      (when (and file-name
                                 (let ((basename (file-name-nondirectory file-name)))
                                   (or (string= basename "BUILD")
                                       (string= basename "BUILD.bazel"))))
                        (directory-file-name (file-name-directory file-name))))
                    ;; The buffer filename.
                    (buffer-file-name)
                    ;; Open on a dired directory.
                    (and dired-directory (directory-file-name dired-directory))))))
    (completing-read (format "Target for %s: " prompt) targets nil nil (car targets))))

(provide 'bazel-build)

;;; bazel-build.el ends here
