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
;;; Code:

(require 'cl-lib)

(defun bazel-build--find-workspace-root (filename)
  "Find the root of the Bazel workspace, i.e. the path to the WORKSPACE file.
FILENAME is the file whose parent directories are crawled recursively until one finding a file named WORKSPACE file is found."
  (let ((workspace-root (locate-dominating-file filename "WORKSPACE")))
	(if workspace-root (expand-file-name workspace-root)
	  (error "Not in a Bazel workspace"))))

(defconst bazel-build--recognized-build-filenames
  '("BUILD.bazel" "BUILD")
  "Names files Bazel will look in to find build rules.")

(defun bazel-build--find-nearest-build-file (filename)
  "Return first BUILD file up the directory hierarchy from input filename.
FILENAME is the file whose parent directories are crawled recursively until one finding a file with a named recognized as a Bazel BUILD file is found."
  (let ((nearest-build-file
		 (some (lambda (build-name) (locate-dominating-file filename build-name)) bazel-build--recognized-build-filenames)))
	(if nearest-build-file (expand-file-name nearest-build-file))))

(defun bazel-build--extract-build-path (filename workspace-root)
  "Extract the path from the Bazel workspace root to the argument.
FILENAME names the file whose path relative to the workspace root is extracted.
WORKSPACE-ROOT is the root of the Bazel workspace."
  (let* ((nearest-build-file (or (bazel-build--find-nearest-build-file filename) workspace-root)))
	(file-relative-name nearest-build-file workspace-root)))

(defun bazel-build--concat-path-components-list (components)
  "Concatenate a list of path components to form a path.
COMPONENTS is the list of path components to concatenate."
  (let ((nonempty-components (seq-filter (lambda (c) (and c (> (length c) 0))) components)))
    (if (cdr nonempty-components)
      (concat (file-name-as-directory (car nonempty-components)) (bazel-build--concat-path-components-list (cdr nonempty-components)))
    (car nonempty-components))))

(defun bazel-build--concat-path-components (&rest components)
  "Concatenate a path components to form a path.
COMPONENTS are the path components to concatenate."
  (bazel-build--concat-path-components-list components))

(defun bazel-build--build-file-in-directory (dirname)
  "Return the name of the Bazel build file, if present, in a directory.
DIRNAME is the name of the directory possibly containing a Bazel build file."
  (let ((files-in-directory (directory-files dirname nil)))
    (some (lambda (build-filename) (if (member build-filename files-in-directory) build-filename))
          bazel-build--recognized-build-filenames)))

(defun bazel-build--rules-in-file (filename)
  "Return the list of names of all Bazel rules in a BUILD or WORKSPACE file.
FILENAME is the name of a Bazel BUILD or WORKSPACE file."
  (let ((matches)
        ;; matches, e.g., 'foo' from 'name = "foo",'
        (rule-name-regex "name[[:space:]]*=[[:space:]]*[\"\']\\([^\'\"]+\\)[\'\"]"))
    (save-match-data
      (with-temp-buffer
        (insert-file-contents filename)
        (while (search-forward-regexp rule-name-regex nil t 1)
          (push (match-string 1) matches)))
      matches)))

(defun bazel-build--rule-completions (dirname workspace-root)
  "Return Bazel rules which complete an input path.
DIRNAME is the path to compute.
WORKSPACE-ROOT is the root of the Bazel workspace."
  (let* ((subdirectories (bazel-build--subdirectory-completions dirname workspace-root))
         (directories (if (string= dirname "") (append '("") subdirectories) subdirectories)))
    (mapcan
     (lambda (subdir)
       (let* ((fulldir (bazel-build--concat-path-components workspace-root subdir))
              (build-file-name (bazel-build--build-file-in-directory fulldir))
              (prefix (concat (bazel-build--concat-path-components dirname (file-name-nondirectory (directory-file-name subdir))) ":")))
         (if build-file-name
           (mapcar (lambda (rule-name) (concat prefix rule-name))
                   (bazel-build--rules-in-file (expand-file-name build-file-name fulldir))))))
     directories)))

(defun bazel-build--bazel-generated-directory-p (dirname)
  "Return whether the input directory is a Bazel generated directory.
DIRNAME is the directory that is tested whether it is a Bazel generated directory."
  (let ((bazel-generated-directories '("bazel-bin" "bazel-genfiles" "bazel-out" "bazel-testlogs" "bazel-tmp")))
    (some (lambda (dir) (string-match-p (regexp-quote dir) dirname)) bazel-generated-directories)))

(defun bazel-build--visible-subdirectories (dirname)
  "Return visible subdirectories of a given directory.
DIRNAME is the directory whose visible subdirectories are returned."
  (let ((no-leading-dot-regex "^[^\\.]")) ;; TODO: is there a more generic way to recognize hidden files?
    (seq-filter (lambda (x) (and (file-directory-p x) (not (bazel-build--bazel-generated-directory-p x))))
                (directory-files dirname t no-leading-dot-regex))))

(defun bazel-build--subdirectory-completions (dirname workspace-root)
  "Return subdirectories that complete the input path relative.
Argument DIRNAME is the directory whose Bazel build rule completions are
returned.  It is a path relative to the workspace root.
WORKSPACE-ROOT is the root of the Bazel workspace."
  (let* ((fulldir (bazel-build--concat-path-components workspace-root dirname)))
    (mapcar (lambda (d) (file-name-as-directory (file-relative-name d workspace-root))) (bazel-build--visible-subdirectories fulldir))))

(defun bazel-build--extract-special-prefix (str)
  "Extract from the input a prefix with special meaning to Bazel, if present.
Argument STR is the string from which special prefixed might be extracted."
  (let ((prefixes '("//" "@" ""))) ;; empty string must come last to provide default behavior
    (some (lambda (pref) (if (string-prefix-p pref str) (list pref (substring str (length pref)))))
          prefixes)))

(defun bazel-build--completions (string pred mode)
  "Programmed completion for arguments to 'bazel-build', 'bazel-run', etc.
STRING is the string to be completed by this rule.
PRED is a predicate which may be used to filter possible completions.
MODE specifies the completion mode."
  (let* ((string-split (bazel-build--extract-special-prefix string))
         (string-prefix (car string-split))
         (string-base (cadr string-split))
         (filename (file-name-nondirectory string-base))
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

         ('t (reduce #'fill-common-string-prefix completions))))

       ;; `all-completions' operation. returns a list of all possible completions.
       ((eq mode 't) completions)

       ;; `test-completion' operation.
       ;;  - if the specified string is an exact match for some completion
       ;;    alternative, returns t.
       ;;  - otherwise, returns nil.
       ((eq mode 'lambda) (member (file-name-as-directory filename) completions))

       ;; `completion-boundaries' operation.
       ;; let completion know our completions only apply after the last /
       ('t (cons (list 'boundaries slashpos) (length filename))))))

(defun bazel-build--suggest-initial-input (filename workspace-root)
  "Suggest initial input to the interactive completion.
FILENAME is the filename of the current buffer.
WORKSPACE-ROOT is the root of the Bazel workspace."
  (concat "//" (directory-file-name (bazel-build--extract-build-path filename workspace-root))))

(defun bazel-build ()
  "Find an run a Bazel build rule."
  (interactive)
  (let* ((filename (buffer-file-name))
         (workspace-root (bazel-build--find-workspace-root filename))
         (initial-input (bazel-build--suggest-initial-input filename workspace-root))
		 (target (completing-read
				"bazel build "             ; prompt
				#'bazel-build--completions ; collection
				nil                        ; predicate
				't                         ; require-match
				initial-input)))           ; initial-input
	(with-temp-buffer
	  (compile
	   (format "bazel build %s" target)))))

(provide 'bazel-build)

;;; bazel-build.el ends here
