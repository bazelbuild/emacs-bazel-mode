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

(defvar bazel-command "bazel"
  "Bazel executable to run for subprocesses.")

(defvar bazel-query-args '("query" "--noblock_for_lock")
  "Bazel query subcommand and arguments.")

(defun bazel-build--run-bazel-command (command target)
  "Run Bazel tool with given COMMAND, e.g. build or run, on the given TARGET."
  (compile
   (mapconcat #'shell-quote-argument (list bazel-command command target) " ")))

(defun bazel-build--run-bazel-command-from-root (command target)
  "Launch an interactive compilation on the target of the current buffer."
  (let ((root-dir (or (bazel-util-workspace-root
                       (or (buffer-file-name)
                           (and dired-directory (directory-file-name dired-directory))))
                      (user-error "Could not find workspace."))))
    (compile (format "cd %s && %s %s %s" root-dir
                     bazel-command command target))))

(defun bazel-build--call-process (&rest args)
  "Run a bazel subcommand. Return stdout as string."
  ;; Note: The current working directory of the subprocess is set to the current
  ;; buffer's value of default-directory.
  (let ((stderr-file (make-temp-file "bazel-stderr")))
    (unwind-protect
        (string-rstrip
         (with-output-to-string
           (let ((status
                  (apply #'call-process bazel-command
                         nil (list standard-output stderr-file) nil args)))
             (if (/= status 0)
                 (with-temp-buffer
                   (insert-file-contents stderr-file)
                   (error "Error running command: %s" (buffer-string)))))))
      (if (file-exists-p stderr-file)
	  (delete-file stderr-file)))))

(defun bazel-query (&rest args)
  "Run a bazel query subcommand. Return stdout as string."
  (apply #'bazel-build--call-process (append bazel-query-args args)))

(defun bazel-build--target-for-file (filename)
  "Get the list of targets which includes the given filename."
  (let* ((default-directory (file-name-directory filename))
         ;; Resolve label for file with bazel query.
         (fullname (bazel-query (file-name-nondirectory filename))))
    ;; Produce target the file-label is in using bazel query.
    (let ((cmd (format "attr('srcs', %s, %s:*)" fullname
                       (car (split-string fullname ":")))))
      (split-string (bazel-query cmd)))))

(defun bazel-build--target-for-directory (dirname)
  "Get the list of targets under the given directory name."
  (let* ((default-directory dirname)
         (results (split-string
                   (bazel-query "kind('.*rule', ':*')")))
         (package (car (split-string (car results) ":"))))
    (append (list (concat package ":all")) results)))

(defun bazel-build--target-for-directory-or-filename (file-or-dir)
  "Get the list of targets under the given file or directory name."
  (if (file-directory-p file-or-dir)
      (bazel-build--target-for-directory file-or-dir)
    (bazel-build--target-for-file file-or-dir)))

(defun bazel-build--read-target (prompt &optional filename)
  "Read a target name for the given or current file or dired directory name."
  ;; Bazel query invocation can be slow, issue a message.
  (message "Generating completions...")
  (let* ((targets (bazel-build--target-for-directory-or-filename
                   (or
                    ;; A given filename.
                    filename
                    ;; Open on a BUILD file.
                    (let* ((file-name (buffer-file-name)))
                      (when (and file-name
                                 (string= (file-name-nondirectory file-name) "BUILD"))
                        (directory-file-name (file-name-directory file-name))))
                    ;; The buffer filename.
                    (buffer-file-name)
                    ;; Open on a dired directory.
                    (and dired-directory (directory-file-name dired-directory))))))
    (completing-read (format "Target for %s: " prompt) targets nil nil (car targets))))

;; TODO(blais): There may be a cl equivalent for this.
(defun string-rstrip (str)
  "Strips the whitespace at the end of string STR."
  (string-match "[ \t\n]*\\'" str)
  (substring str 0 (match-beginning 0)))

(provide 'bazel-build)

;;; bazel-build.el ends here
