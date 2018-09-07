;;; bazel-build.el --- Emacs utilities for using Bazel
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
;; It defines interactive commands bazel-build and bazel-run which
;; perform completion of available Bazel targets.

;;; -*- lexical-binding: t; -*-
;;; Code:

(require 'cl-lib)

(defun bazel-build ()
  "Find and build a Bazel target."
  (interactive)
  (funcall-interactively #'bazel-build--run-bazel-command "build"))

(defun bazel-run ()
  "Find and run a Bazel target."
  (interactive)
  (funcall-interactively #'bazel-build--run-bazel-command "run"))

(defun bazel-build--run-bazel-command (command)
  "Run Bazel tool with given COMMAND, e.g. build or run."
  (let* ((file-name (buffer-file-name))
         (workspace-root (bazel-build--find-workspace-root file-name))
         (initial-input (bazel-build--suggest-initial-input file-name workspace-root))
         (target (completing-read
                  (format "bazel %s " command) ; prompt
                  #'bazel-build--completions   ; collection
                  nil                          ; predicate
                  nil                          ; require-match
                  initial-input)))             ; initial-input
    (with-temp-buffer
      (compile
       (format "bazel %s %s" command target)))))

(defun bazel-build--find-workspace-root (file-name)
  "Find the root of the Bazel workspace containing FILE-NAME."
  (let ((workspace-root (locate-dominating-file file-name "WORKSPACE")))
    (if workspace-root (expand-file-name workspace-root)
      (error "Not in a Bazel workspace"))))

(defun bazel-build--suggest-initial-input (file-name workspace-root)
  "Suggest initial input to the interactive completion.
FILE-NAME is the file-name of the current buffer.
WORKSPACE-ROOT is the root of the Bazel workspace."
  (concat "//" (directory-file-name (bazel-build--extract-package-name file-name workspace-root))))

(defun bazel-build--extract-package-name (file-name workspace-root)
  "Extract the nearest Bazel package for FILE-NAME under WORKSPACE-ROOT."
  (let ((nearest-build-file (or (bazel-build--find-nearest-build-package file-name) workspace-root)))
    (file-relative-name nearest-build-file workspace-root)))

(defun bazel-build--find-nearest-build-package (file-name)
  "Return nearest Bazel build package to FILE-NAME.
This is the directory containing the first BUILD file up
the directory from FILE-NAME."
  (let ((nearest-build-package
         (cl-some (lambda (build-name) (locate-dominating-file file-name build-name)) bazel-build--recognized-build-file-names)))
    (if nearest-build-package (expand-file-name nearest-build-package))))

(defconst bazel-build--recognized-build-file-names
  '("BUILD.bazel" "BUILD")
  "Names files Bazel will look in to find build rules.")

(defun bazel-build--completions (string pred mode)
  "Programmed completion for arguments to 'bazel-build', 'bazel-run', etc.
STRING is the string to be completed by this rule.
PRED is a predicate which may be used to filter possible completions.
MODE specifies the completion mode."
  (let* ((string-split (bazel-build--extract-special-prefix string))
         (string-prefix (car string-split))
         (string-base (cadr string-split))
         (file-name (file-name-nondirectory string-base))
         (dirname (or (file-name-directory string-base) ""))
         (slashpos (or (string-match "/.*$" string-base) 0))
         (workspace-root (bazel-build--find-workspace-root dirname))
         (subdirectories (bazel-build--subdirectory-completions dirname workspace-root))
         (rules (bazel-build--rule-completions dirname workspace-root))
         (candidate-completions (mapcar (lambda (d) (concat string-prefix d)) (append subdirectories rules)))
         (completions (all-completions string candidate-completions)))

      ;; return differently based on what mode we were called in
      ;; see http://www.gnu.org/software/emacs/manual/html_node/elisp/Programmed-Completion.html
      (cond
       ;; `try-completion' operation.
       ;;  - if the specified string is a unique and exact match, return t
       ;;  - if there is more than one match, return the common substring of all
       ;;    matches. if the string is an exact match for one completion
       ;;    alternative but also matches other longer alternatives, the return
       ;;    value is the string.
       ;;  - if there are no matches, it should return nil.
       ((eq mode nil)
        (cond
         ((eq 0 (length completions)) nil)

         ((and (eq 1 (length completions)) (string= string (car completions))) 't)

         ('t (cl-reduce #'fill-common-string-prefix completions))))

       ;; `all-completions' operation. returns a list of all possible completions.
       ((eq mode 't) completions)

       ;; `test-completion' operation.
       ;;  - if the specified string is an exact match for some completion
       ;;    alternative, returns t.
       ;;  - otherwise, returns nil.
       ((eq mode 'lambda) (member (file-name-as-directory file-name) completions))

       ;; `completion-boundaries' operation.
       ;; let completion know our completions only apply after the last /
       ('t (cons (list 'boundaries slashpos) (length file-name))))))

(defun bazel-build--extract-special-prefix (str)
  "Extract any prefix from STR with special meaning to Bazel."
  (let ((prefixes '("//" ""))) ;; empty string must come last to provide default behavior
    (cl-some (lambda (pref) (if (string-prefix-p pref str) (list pref (substring str (length pref)))))
          prefixes)))

(defun bazel-build--rule-completions (dirname workspace-root)
  "Return Bazel rules which complete DIRNAME.
WORKSPACE-ROOT is the root of the Bazel workspace."
  (let* ((subdirectories (bazel-build--subdirectory-completions dirname workspace-root))
         (directories (if (string= dirname "") (append '("") subdirectories) subdirectories)))
    (cl-mapcan
     (lambda (subdir)
       (let* ((fulldir (bazel-build--concat-file-name-components workspace-root subdir))
              (build-file-name (bazel-build--build-file-in-directory fulldir))
              (prefix (concat (bazel-build--concat-file-name-components dirname (file-name-nondirectory (directory-file-name subdir))) ":")))
         (if build-file-name
           (mapcar (lambda (rule-name) (concat prefix rule-name))
                   (bazel-build--rules-in-file (expand-file-name build-file-name fulldir))))))
     directories)))

(defun bazel-build--subdirectory-completions (dirname workspace-root)
  "Return subdirectories of DIRNAME which are not generated by Bazel.
WORKSPACE-ROOT is the root of the Bazel workspace."
  (let ((fulldir (bazel-build--concat-file-name-components workspace-root dirname)))
    (mapcar (lambda (d) (file-name-as-directory (file-relative-name d workspace-root))) (bazel-build--visible-subdirectories fulldir))))

(defun bazel-build--concat-file-name-components-list (components)
  "Concatenate COMPONENTS to form a file name."
  (let ((nonempty-components (seq-filter (lambda (c) (and c (> (length c) 0))) components)))
    (if (cdr nonempty-components)
      (concat (file-name-as-directory (car nonempty-components)) (bazel-build--concat-file-name-components-list (cdr nonempty-components)))
    (car nonempty-components))))

(defun bazel-build--concat-file-name-components (&rest components)
  "Concatenate COMPONENTS to form a file-name."
  (bazel-build--concat-file-name-components-list components))

(defun bazel-build--build-file-in-directory (dirname)
  "Return the name of the Bazel build file in DIRNAME, if present."
  (let ((files-in-directory (directory-files dirname nil)))
    (cl-some (lambda (build-file-name) (if (member build-file-name files-in-directory) build-file-name))
          bazel-build--recognized-build-file-names)))

(defun bazel-build--rules-in-file (file-name)
  "Return the names of all Bazel rules in FILE-NAME."
  (let ((matches)
        ;; matches, e.g., 'foo' from 'name = "foo",'
        (rule-name-regex "name[[:space:]]*=[[:space:]]*[\"\']\\([^\'\"]+\\)[\'\"]"))
    (with-temp-buffer
      (insert-file-contents file-name)
      (while (search-forward-regexp rule-name-regex nil t 1)
        (push (match-string 1) matches)))
    matches))

;;(defun bazel-build--rules-in-file (file-name)
;;  "Return the names of all Bazel rules in FILE-NAME."
;;  (let ((matches)
;;        ;; matches, e.g., 'foo' from 'name = "foo",'
;;        (rule-name-regex "name[[:space:]]*=[[:space:]]*[\"\']\\([^\'\"]+\\)[\'\"]"))
;;    (save-match-data
;;      (with-temp-buffer
;;        (insert-file-contents file-name)
;;        (while (search-forward-regexp rule-name-regex nil t 1)
;;          (push (match-string 1) matches)))
;;      matches)))

(defun bazel-build--bazel-generated-directory-p (dirname)
  "Return whether DIRNAME is a Bazel generated directory."
  (let ((bazel-generated-directories '("bazel-bin" "bazel-genfiles" "bazel-out" "bazel-testlogs" "bazel-tmp")))
    (cl-some (lambda (dir) (string-match-p (regexp-quote dir) dirname)) bazel-generated-directories)))

(defun bazel-build--visible-subdirectories (dirname)
  "Return visible subdirectories of a given directory.
DIRNAME is the directory whose visible subdirectories are returned."
  (let ((no-leading-dot-regex "^[^\\.]")) ;; TODO: is there a more generic way to recognize hidden files?
    (seq-filter (lambda (x) (and (file-directory-p x) (not (bazel-build--bazel-generated-directory-p x))))
                (directory-files dirname t no-leading-dot-regex))))

(provide 'bazel-build)

;;; bazel-build.el ends here
