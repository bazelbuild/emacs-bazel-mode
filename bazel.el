;;; bazel.el --- Emacs major mode for editing Bazel BUILD and WORKSPACE files -*- lexical-binding: t; -*-

;; URL: https://github.com/bazelbuild/emacs-bazel-mode
;; Keywords: build tools, languages
;; Package-Requires: ((emacs "26.1"))
;; Version: 0

;; Copyright (C) 2018, 2021 Google LLC
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

;; This package provides Emacs bazel-mode, a major mode for editing Bazel
;; BUILD and WORKSPACE files.
;;
;; This package provides ‘bazelrc-mode’, a major mode to edit .bazelrc files.
;; See https://docs.bazel.build/versions/3.0.0/guide.html#bazelrc.
;;
;; This package provides commands to build and run code using Bazel.
;; It defines interactive commands which perform completion of available Bazel
;; targets:
;;   - `bazel-build'
;;   - `bazel-run'
;;   - `bazel-test'

;;; Code:

(require 'cl-lib)
(require 'compile)
(require 'conf-mode)
(require 'derived)
(require 'ffap)
(require 'flymake)
(require 'font-lock)
(require 'imenu)
(require 'json)
(require 'pcase)
(require 'project)
(require 'python)
(require 'rx)
(require 'subr-x)
(require 'syntax)
(require 'xref)

(defgroup bazel nil
  "Package for editing, building, and running code using Bazel."
  :link '(url-link "https://bazel.build")
  :link '(url-link "https://github.com/bazelbuild/emacs-bazel-mode")
  :group 'languages)

(define-obsolete-variable-alias 'bazel-build-bazel-command
  'bazel-command "2021-04-13")

(defcustom bazel-command '("bazel")
  "Command and arguments that should be used to invoke Bazel."
  :type '(repeat string)
  :risky t
  :group 'bazel)

(define-obsolete-variable-alias 'bazel-mode-buildifier-command
  'bazel-buildifier-command "2021-04-13")

(defcustom bazel-buildifier-command "buildifier"
  "Filename of buildifier executable."
  :type 'file
  :group 'bazel
  :link '(url-link
          "https://github.com/bazelbuild/buildtools/tree/master/buildifier"))

(define-obsolete-variable-alias 'bazel-mode-buildifier-before-save
  'bazel-buildififer-before-save "2021-04-13")

(defcustom bazel-buildifier-before-save nil
  "Specifies whether to run buildifer in `before-save-hook'."
  :type 'boolean
  :group 'bazel
  :link '(url-link
          "https://github.com/bazelbuild/buildtools/tree/master/buildifier")
  :risky t)

(defconst bazel--magic-comment-regexp
  (rx (or "keep sorted"
          "do not sort"
          "@unused"
          "@unsorted-dict-items"
          "buildifier: leave-alone"
          (seq (or "buildifier" "buildozer") ": "
               (seq "disable=" (+ (any "A-Za-z-"))))))
  "Regular expression identifying magic comments known to Buildifier.

Many of these are documented at
URL `https://github.com/bazelbuild/buildtools/blob/master/WARNINGS.md'.

The magic comments \"keep sorted\", \"do not sort\", and
\"buildifier: leave-alone\" don't look to be documented, but are
mentioned in the Buildifer source code at URL
`https://git.io/JOuVL' and have tests.")

;;;; Commands to run Buildifier.
(defvar-local bazel--buildifier-type nil
  "Type of the file that the current buffer visits.
This must be a symbol and a valid value for the Buildifier -type
flag.  See
https://github.com/bazelbuild/buildtools/blob/2.2.0/buildifier/utils/flags.go#L11.
If nil, don’t pass a -type flag to Buildifier.")

(defun bazel-buildifier ()
  "Format current buffer using buildifier."
  (interactive "*")
  (let ((input-buffer (current-buffer))
        (input-file buffer-file-name)
        (buildifier-buffer (get-buffer-create "*buildifier*"))
        ;; Run buildifier on a file to support remote BUILD files.
        (buildifier-input-file (make-nearby-temp-file "buildifier"))
        (buildifier-error-file (make-nearby-temp-file "buildifier"))
        (type bazel--buildifier-type))
    (unwind-protect
      (write-region (point-min) (point-max) buildifier-input-file nil :silent)
      (with-current-buffer buildifier-buffer
        (setq-local inhibit-read-only t)
        (erase-buffer)
        (let ((return-code
               (apply #'process-file
                      bazel-mode-buildifier-command buildifier-input-file
                      `(t ,buildifier-error-file) nil
                      (bazel--buildifier-file-flags type input-file))))
          (if (eq return-code 0)
              (progn
                (set-buffer input-buffer)
                (replace-buffer-contents buildifier-buffer)
                (kill-buffer buildifier-buffer))
            (with-temp-buffer-window
             buildifier-buffer nil nil
             (insert-file-contents buildifier-error-file)
             (compilation-minor-mode)))))
      (delete-file buildifier-input-file)
      (delete-file buildifier-error-file))))

(define-obsolete-function-alias 'bazel-mode-buildifier
  #'bazel-buildifier "2021-04-13")

(defun bazel--buildifier-before-save-hook ()
  "Run buildifer in `before-save-hook'."
  (when bazel-mode-buildifier-before-save
    (bazel-buildifier)))

;;;; ‘bazel-mode’ and child modes

(defconst bazel--font-lock-keywords
  `(
    ;; Some Starlark functions are exposed to BUILD files as builtins. For
    ;; details see https://github.com/bazelbuild/starlark/blob/master/spec.md.
    (,(regexp-opt '("exports_files" "glob" "licenses" "package"
                    "package_group" "select" "workspace")
                  'symbols)
     . 'font-lock-builtin-face)
    ;; Keywords for BUILD files are the same as bzl files. Even if some of them
    ;; are forbidden in BUILD files, they should be highlighted.
    ;; See spec link above.
    (,(regexp-opt '("and" "else" "for" "if" "in" "not" "or" "load"
                    "break" "continue" "def" "pass" "elif" "return")
                  'symbols)
     . 'font-lock-keyword-face)
    ;; Reserved keywords
    (,(regexp-opt '("as" "is" "assert" "lambda" "class" "nonlocal" "del" "raise"
                    "except" "try" "finally" "while" "from" "with" "global"
                    "yield" "import")
                  'symbols)
     . 'font-lock-keyword-face)
    ;; Constants
    (,(regexp-opt '("True" "False" "None")
                  'symbols)
     . 'font-lock-constant-face)
    ;; Magic comments
    (bazel--find-magic-comment 0 'font-lock-preprocessor-face prepend)))

(defconst bazel-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; single line comment start
    (modify-syntax-entry ?# "<" table)
    ;; single line comment end
    (modify-syntax-entry ?\n ">" table)
    ;; strings using single quotes
    (modify-syntax-entry ?' "\"" table)
    table)
  "Syntax table for `bazel-mode'.")

(define-derived-mode bazel-mode prog-mode "Bazel"
  "Major mode for editing Bazel BUILD and WORKSPACE files.
This is the parent mode for the more specific modes
‘bazel-build-mode’, ‘bazel-workspace-mode’, and
‘bazel-starlark-mode’."
  ;; Almost all Starlark code in existence uses 4 spaces for indentation.
  ;; Buildifier also enforces this style.
  (setq-local tab-width 4)
  (setq-local indent-tabs-mode nil)
  ;; Take over some syntactic constructs from ‘python-mode’.
  (setq-local comment-start "# ")
  (setq-local comment-start-skip (rx (+ ?#) (* (syntax whitespace))))
  (setq-local comment-end "")
  (setq-local comment-use-syntax t)
  (setq-local parse-sexp-ignore-comments t)
  (setq-local forward-sexp-function #'python-nav-forward-sexp)
  (setq-local font-lock-defaults (list bazel--font-lock-keywords))
  (setq-local syntax-propertize-function python-syntax-propertize-function)
  (setq-local indent-line-function #'python-indent-line-function)
  (setq-local indent-region-function #'python-indent-region)
  (setq-local electric-indent-inhibit t)
  ;; Treat magic comments as being separate paragraphs for filling.
  (setq-local paragraph-start
              (rx-to-string
               `(or (seq (* (syntax whitespace)) (regexp ,comment-start-skip)
                         (regexp ,bazel--magic-comment-regexp))
                    (regexp ,paragraph-start))
               :no-group))
  (add-hook 'before-save-hook #'bazel--buildifier-before-save-hook nil :local)
  (add-hook 'flymake-diagnostic-functions #'bazel-mode-flymake nil :local)
  (add-hook 'xref-backend-functions #'bazel-mode-xref-backend nil :local))

;;;###autoload
(define-derived-mode bazel-build-mode bazel-mode "Bazel BUILD"
  "Major mode for editing Bazel BUILD files."
  (setq bazel--buildifier-type 'build)
  ;; In BUILD files, we don’t have function definitions.  Instead, treat rules
  ;; (= Python statements) as functions.
  (setq-local beginning-of-defun-function #'python-nav-beginning-of-statement)
  (setq-local end-of-defun-function #'python-nav-end-of-statement)
  (setq-local imenu-create-index-function #'bazel-mode-create-index))

;;;###autoload
(add-to-list 'auto-mode-alist
             ;; https://docs.bazel.build/versions/3.0.0/build-ref.html#packages
             (cons (rx ?/ (or "BUILD" "BUILD.bazel") eos) #'bazel-build-mode))

;;;###autoload
(define-derived-mode bazel-workspace-mode bazel-mode "Bazel WORKSPACE"
  "Major mode for editing Bazel WORKSPACE files."
  (setq bazel--buildifier-type 'workspace)
  ;; In WORKSPACE files, we don’t have function definitions.  Instead, treat
  ;; rules (= Python statements) as functions.
  (setq-local beginning-of-defun-function #'python-nav-beginning-of-statement)
  (setq-local end-of-defun-function #'python-nav-end-of-statement)
  (setq-local imenu-create-index-function #'bazel-mode-create-index))

;;;###autoload
(add-to-list 'auto-mode-alist
             ;; https://docs.bazel.build/versions/3.0.0/build-ref.html#workspace
             (cons (rx ?/ (or "WORKSPACE" "WORKSPACE.bazel") eos)
                   #'bazel-workspace-mode))

;;;###autoload
(define-derived-mode bazel-starlark-mode bazel-mode "Starlark"
  "Major mode for editing Bazel Starlark files."
  (setq bazel--buildifier-type 'bzl)
  ;; In Starlark files, we do have Python-like function definitions, so use the
  ;; Python commands to navigate.
  (setq-local beginning-of-defun-function #'python-nav-beginning-of-defun)
  (setq-local end-of-defun-function #'python-nav-end-of-defun)
  (setq-local imenu-extract-index-name-function
              #'bazel-mode-extract-function-name))

;;;###autoload
(add-to-list 'auto-mode-alist
             ;; https://docs.bazel.build/versions/3.0.0/skylark/concepts.html#getting-started
             (cons (rx ?/ (+ nonl) ".bzl" eos) #'bazel-starlark-mode))

;;;; ‘bazelrc-mode’

;;;###autoload
(define-derived-mode bazelrc-mode conf-space-mode "bazelrc"
  "Major mode for editing .bazelrc files.")

;;;###autoload
(add-to-list 'auto-mode-alist
             ;; https://docs.bazel.build/versions/3.0.0/guide.html#where-are-the-bazelrc-files
             (cons (rx ?/ (or "bazel.bazelrc" ".bazelrc") eos) #'bazelrc-mode))

(font-lock-add-keywords
 #'bazelrc-mode
 ;; https://docs.bazel.build/versions/3.0.0/guide.html#imports
 (list (rx symbol-start (or "import" "try-import") symbol-end)))

;;;; Flymake support using Buildifier

(defvar-local bazel--flymake-process nil
  "Current Buildifier process for this buffer.
‘bazel-mode-flymake’ sets this variable to the most recent
Buildifier process for the current buffer.")

(defun bazel-mode-flymake (report-fn &rest _keys)
  "Flymake backend function for ‘bazel-mode’.
Use REPORT-FN to report linter warnings found by Buildifier.  See
https://github.com/bazelbuild/buildtools/blob/master/buildifier/README.md
for how to install Buildifier.  The function ‘bazel-mode’ adds
this function to ‘flymake-diagnostic-functions’.  See Info node
‘(Flymake) Backend functions’ for details about Flymake
backends."
  (let ((process bazel--flymake-process))
    (when process
      ;; The order here is important: ‘delete-process’ will trigger the
      ;; sentinel, and then ‘bazel--flymake-process’ already has to be nil to
      ;; avoid an obsolete report.
      (setq bazel--flymake-process nil)
      (delete-process process)))
  (let* ((non-essential t)
         (command `(,bazel-mode-buildifier-command
                    ,@(bazel--buildifier-file-flags bazel--buildifier-type
                                                    buffer-file-name)
                    "-mode=check" "-format=json" "-lint=warn"))
         (input-buffer (current-buffer))
         (output-buffer (generate-new-buffer
                         (format " *Buildifier output for %s*" (buffer-name))))
         (error-buffer (generate-new-buffer
                        (format " *Buildifier errors for %s*" (buffer-name))))
         (sentinel
          (lambda (process event)
            (let ((success (string-equal event "finished\n")))
              ;; First, make sure that the buffer to be linted is still alive.
              (when (buffer-live-p input-buffer)
                (with-current-buffer input-buffer
                  ;; Don’t report anything if a new process has already been
                  ;; started.
                  (when (eq bazel--flymake-process process)
                    ;; Report even in case of failure so that Flymake doesn’t
                    ;; treat the backend as continuously running.
                    (funcall report-fn
                             (and success
                                  (bazel--make-diagnostics output-buffer)))
                    (setq bazel--flymake-process nil))))
              (if success
                  (progn (kill-buffer output-buffer)
                         (kill-buffer error-buffer))
                (flymake-log :warning
                             (concat "Buildifier process failed; "
                                     "see buffers ‘%s’ and ‘%s’ for details")
                             output-buffer error-buffer)))))
         ;; We always start a local process, even if visiting a remote
         ;; filename.  If Buildifier is installed locally, the reports should
         ;; match well enough.  Starting a remote process via Tramp tends to be
         ;; way too slow to run interactively.
         (process (make-process :name (format "Buildifier for %s" (buffer-name))
                                :buffer output-buffer
                                :command command
                                :coding '(utf-8-unix . utf-8-unix)
                                :connection-type 'pipe
                                :noquery t
                                :sentinel sentinel
                                :stderr error-buffer)))
    (setq bazel--flymake-process process)
    (save-restriction
      (widen)
      (process-send-region process (point-min) (point-max)))
    (process-send-eof process)))

(defun bazel--buildifier-file-flags (type filename)
  "Return a list of -path and -type flags for Buildifier.
TYPE should be one of the possible values of
‘bazel--buildifier-type’.  Use TYPE and FILENAME to derive
appropriate flags, if possible.  Otherwise, return an empty
list."
  (append
   (and filename
        (when-let ((workspace (bazel--workspace-root filename)))
          (list (concat "-path=" (file-relative-name filename workspace)))))
   (and type (list (concat "-type=" (symbol-name type))))))

(defun bazel--make-diagnostics (output-buffer)
  "Return Flymake diagnostics for the Buildifier report in OUTPUT-BUFFER.
OUTPUT-BUFFER should contain a JSON report for the file visited
by the current buffer as described in
https://github.com/bazelbuild/buildtools/blob/master/buildifier/README.md#file-diagnostics-in-json.
All filenames in OUTPUT-BUFFER are ignored; all messages are
attached to the current buffer.  Return a list of Flymake
diagnostics; see Info node ‘(Flymake) Backend functions’ for
details."
  (cl-loop with report = (with-current-buffer output-buffer
                           (save-excursion
                             (save-restriction
                               (widen)
                               (goto-char (point-min))
                               ;; Skip over standard error messages if possible.
                               (when (re-search-forward (rx bol ?{) nil t)
                                 (backward-char)
                                 (bazel--json-parse-buffer)))))
           for file across (gethash "files" report)
           nconc (cl-loop for warning across (gethash "warnings" file)
                          collect (bazel--diagnostic-for-warning warning))))

(defun bazel--diagnostic-for-warning (warning)
  "Return a Flymake diagnostic for the Buildifier WARNING.
WARNING should be a hashtable containing a single warning, as
described in
https://github.com/bazelbuild/buildtools/blob/master/buildifier/README.md#file-diagnostics-in-json."
  (let* ((case-fold-search nil)
         (start (gethash "start" warning))
         (end (gethash "end" warning))
         ;; Note: Both line and column are one-based in the Buildifier output.
         (start-line (gethash "line" start))
         (start-column (1- (gethash "column" start)))
         (end-line (gethash "line" end))
         (end-column (1- (gethash "column" end)))
         (message (gethash "message" warning)))
    (when (and (= start-line end-line) (= start-column 0) (= end-column 1))
      ;; Heuristic: assume this means the entire line.  Buildifier emits this
      ;; e.g. for the module-docstring warning.  ‘bazel--line-column-pos’
      ;; ensures that we don’t cross line boundaries.
      (setq end-column (buffer-size)))
    (flymake-make-diagnostic
     (current-buffer)
     (bazel--line-column-pos start-line start-column)
     (bazel--line-column-pos end-line end-column)
     :warning
     (format-message "%s [%s] (%s)"
                     ;; We only take the first line of the message, otherwise
                     ;; it becomes too long and looks ugly.
                     (substring-no-properties message nil
                                              (string-match-p (rx ?\n) message))
                     (gethash "category" warning)
                     (gethash "url" warning)))))

;;;; XRef backend

;; This backend is optimized for speed, not correctness.  This means that we
;; use heuristics to find BUILD files and targets.  In particular, we assume
;; that if a target looks like it refers to a file and the file exists, it
;; actually refers to that file.

(defun bazel-mode-xref-backend ()
  "XRef backend function for ‘bazel-mode’.
This gets added to ‘xref-backend-functions’."
  (and (derived-mode-p 'bazel-mode)
       buffer-file-name
       ;; It only makes sense to find targets if we are in a workspace,
       ;; otherwise we don’t know how to resolve absolute labels.
       (bazel--workspace-root buffer-file-name)
       'bazel-mode))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql bazel-mode)))
  ;; This only detects string literals representing labels.
  (let ((identifier (bazel--string-at-point)))
    (when identifier
      (cl-destructuring-bind (&whole valid-p &optional workspace package target)
          (bazel--parse-label identifier)
        (when valid-p
          ;; Save the current workspace directory as text property in case the
          ;; user switches directories between this call and selecting a
          ;; reference.  ‘xref-backend-definitions’ falls back to the current
          ;; workspace if the property isn’t present so that users can still
          ;; invoke ‘xref-find-definitions’ and enter a label manually.
          ;; Likewise, save the current package if possible by canonicalizing
          ;; the label.  We don’t care about valid but exotic labels here,
          ;; e.g., labels containing newlines or backslashes.
          (let* ((this-workspace
                  (and buffer-file-name
                       (bazel--workspace-root buffer-file-name)))
                 (package
                  (or package
                      (and buffer-file-name this-workspace
                           (bazel--package-name buffer-file-name
                                                this-workspace)))))
            (propertize (bazel--canonical workspace package target)
                        'bazel-mode-workspace this-workspace)))))))

(cl-defmethod xref-backend-definitions ((_backend (eql bazel-mode)) identifier)
  ;; Reparse the identifier so that users can invoke ‘xref-find-definitions’
  ;; and enter a label directly.
  (cl-destructuring-bind (&whole valid-p &optional workspace package target)
      (bazel--parse-label identifier)
    (when valid-p
      (let* ((this-workspace
              (or (get-text-property 0 'bazel-mode-workspace identifier)
                  (and buffer-file-name
                       (bazel--workspace-root buffer-file-name))))
             (package
              (or package
                  (and buffer-file-name this-workspace
                       (bazel--package-name buffer-file-name this-workspace)))))
        (when (and this-workspace package)
          (let ((location
                 (bazel--target-location
                  (bazel--external-workspace workspace this-workspace)
                  package target)))
            (when location
              (list (xref-make (bazel--canonical workspace package target)
                               location)))))))))

(cl-defmethod xref-backend-identifier-completion-table
  ((_backend (eql bazel-mode)))
  (completion-table-with-cache #'bazel--completion-table))

(defun bazel--target-location (workspace package target)
  "Return an ‘xref-location’ for a Bazel target.
The target is in the workspace with root directory WORKSPACE and
the package PACKAGE.  Its local name is TARGET.  If no target was
found, return nil.  This function uses heuristics to find the
target; in particular, it assumes that a target that looks like a
valid file target is indeed a file target."
  (cl-check-type workspace string)
  (cl-check-type package string)
  (cl-check-type target string)
  (let* ((directory (expand-file-name package workspace))
         (filename (expand-file-name target directory)))
    (if (file-exists-p filename)
        ;; A label that likely refers to a source file.
        (bazel--file-location filename)
      ;; A label that likely refers to a rule.  Try to find the rule in the
      ;; BUILD file of the package.
      (let ((build-file (locate-file "BUILD" (list directory) '("" ".bazel"))))
        (when build-file
          (bazel--rule-location build-file target))))))

(defun bazel--completion-table (prefix)
  "Return target completions for the given PREFIX.
Right now, only supports targets in the current package."
  (cl-check-type prefix string)
  (let ((case-fold-search nil))
    (pcase (substring-no-properties prefix)
      ((rx bos (let colon (? ?:)) (let prefix (* (not (any ?: ?/)))) eos)
       (mapcar (lambda (elem) (concat colon elem))
               (append (bazel--complete-rules prefix)
                       (bazel--complete-files prefix)))))))

(defun bazel--file-location (filename)
  "Return an ‘xref-location’ for the source file FILENAME."
  (cl-check-type filename string)
  ;; The location actually refers to the whole file.  Avoid jumping around
  ;; within the already-visited file by pretending the reference isn’t found so
  ;; that we return point.
  (bazel--xref-location filename (lambda ())))

(defun bazel--complete-files (prefix)
  "Return file names in the current directory starting with PREFIX.
Exclude files that are normally not Bazel targets, such as
directories and BUILD files."
  (let ((case-fold-search nil)
        (files ()))
    (dolist (data (directory-files-and-attributes
                   default-directory nil
                   (rx-to-string `(seq bos ,prefix) :no-group)))
      (cl-destructuring-bind (filename &rest attributes) data
        (unless (or (file-attribute-type attributes)  ; not a regular file
                    (member filename '("BUILD" "BUILD.bazel" "WORKSPACE"))
                    (equal (file-name-extension filename) "BUILD")
                    (string-prefix-p "bazel-" filename))
          (push filename files))))
    (nreverse files)))

(defun bazel--rule-location (build-file name)
  "Return an ‘xref-location’ for a rule within a BUILD file.
The name of the BUILD file is BUILD-FILE, and NAME is the local
name of the rule.  If NAME doesn’t seem to exist in BUILD-FILE,
return a location referring to an arbitrary position within the
BUILD file."
  (cl-check-type build-file string)
  (cl-check-type name string)
  (bazel--xref-location build-file (lambda () (bazel--find-rule name))))

(defun bazel--find-rule (name)
  "Find the rule with the given NAME within the current buffer.
The current buffer should visit a BUILD file.  If there’s a rule
with the given NAME, return the location of the rule.  Otherwise,
return nil."
  (cl-check-type name string)
  (let ((case-fold-search nil))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        ;; Heuristic: we search for a “name” attribute as it would show up
        ;; in typical BUILD files.  That’s not 100% correct, but doesn’t
        ;; rely on external processes and should work fine in common cases.
        (when (re-search-forward
               (rx-to-string
                `(seq bol (* blank) "name" (* blank) ?= (* blank)
                      (group (any ?\" ?')) (group ,name) (backref 1))
                :no-group)
               nil t)
          (match-beginning 2))))))

(defun bazel--xref-location (filename find-function)
  "Return an ‘xref-location’ for some entity within FILENAME.
FIND-FUNCTION should be a function taking zero arguments.  This
function calls FIND-FUNCTION in some buffer containing the
contents of FILENAME.  FIND-FUNCTION should then either return
nil (if the entity isn’t found) or the position of the entity
within the current buffer.  FIND-FUNCTION should not move point
or change the buffer state permanently."
  (cl-check-type filename string)
  (cl-check-type find-function function)
  ;; Prefer a buffer that already visits FILENAME.
  (let ((buffer (find-buffer-visiting filename)))
    (if buffer
        ;; If not found, use point to avoid jumping around in the buffer.
        (xref-make-buffer-location buffer
                                   (with-current-buffer buffer
                                     (or (funcall find-function) (point))))
      (with-temp-buffer
        (insert-file-contents filename)
        (goto-char (or (funcall find-function) (point-min)))
        (xref-make-file-location filename
                                 (line-number-at-pos)
                                 (- (point) (line-beginning-position)))))))

(defun bazel--complete-rules (prefix)
  "Find all rules starting with the given PREFIX in the current buffer.
The current buffer should visit a BUILD file.  Return a list of
rule names that start with PREFIX."
  (cl-check-type prefix string)
  (when (derived-mode-p 'bazel-mode)
    (let ((case-fold-search nil)
          (rules ()))
      (save-excursion
        ;; We don’t widen here.  If the user has narrowed the buffer, it’s fair
        ;; to assume they only want completions within the narrowed portion.
        (goto-char (point-min))
        (while (re-search-forward
                (rx-to-string
                 `(seq bol (* blank) "name" (* blank) ?= (* blank)
                       (group (any ?\" ?'))
                       (group ,prefix (* (not (any ?\" ?' ?\n))))
                       (backref 1))
                 :no-group)
                nil t)
          (push (match-string-no-properties 2) rules)))
      (nreverse rules))))

;;;; ‘find-file-at-point’ support for ‘bazel-mode’

;; Since we match every filename, we want to come last.
(add-to-list 'ffap-alist (cons (rx anything) #'bazel-mode-ffap) :append)

(defun bazel-mode-ffap (filename)
  "Attempt to find FILENAME in all workspaces.
This gets added to ‘ffap-alist’."
  (when-let ((main-root (bazel--workspace-root filename)))
    (let ((external-roots
           (condition-case nil
               (directory-files
                (bazel--external-workspace-dir main-root)
                :full
                ;; Assume that workspace names follow similar patters as
                ;; package names,
                ;; https://docs.bazel.build/versions/3.0.0/build-ref.html#package-names-package-name.
                (rx bos (+ (any alnum "-._")) eos))
             ;; If there’s no external workspace directory, don’t signal an
             ;; error.
             (file-missing nil))))
      (locate-file filename (cons main-root external-roots)))))

;;;; ‘find-file-at-point’ support for ‘bazelrc-mode’

(add-to-list 'ffap-string-at-point-mode-alist
             ;; By default the percent sign isn’t included in the list of
             ;; allowed filename characters, so this would miss %workspace%
             ;; names.
             (list #'bazelrc-mode "-_/.%[:alnum:]" "" ""))

(add-to-list 'ffap-alist (cons #'bazelrc-mode #'bazelrc-ffap))

(defun bazelrc-ffap (name)
  "Function for ‘ffap-alist’ in ‘bazelrc-mode’.
Look for an imported file with the given NAME."
  ;; https://docs.bazel.build/versions/3.0.0/guide.html#imports
  (pcase name
    ((rx bos "%workspace%" (+ ?/) (let rest (+ nonl)))
     (when buffer-file-name
       (when-let ((workspace (bazel--workspace-root buffer-file-name)))
         (let ((file-name (expand-file-name rest workspace)))
           (and (file-exists-p file-name) file-name)))))))

;;;; Compilation support

;; Add entries to ‘compilation-error-regexp-alist’.  Bazel errors are of the
;; form “SEVERITY: FILE:LINE:COLUMN: MESSAGE.”  We restrict the filename to the
;; most common characters to avoid matching too much.  In particular, Bazel
;; doesn’t allow spaces in filenames
;; (https://github.com/bazelbuild/bazel/issues/167), so we don’t have to look
;; for spaces here.

(eval-when-compile
  (defmacro bazel--add-compilation-error-regexp (name prefix type)
    "Add a new entry NAME to ‘compilation-error-regexp-alist’.
PREFIX is a literal string specifying the line prefix.  TYPE is
the message type, as in ‘compilation-error-regexp-alist’."
    (declare (debug nil) (indent 0))
    (cl-check-type name symbol)
    (cl-check-type prefix string)
    (cl-check-type type natnum)
    (let ((regexp (rx-to-string
                   `(seq bol ,prefix ": "
                         (group (+ (any "/._-" alnum)) "/BUILD" (? ".bazel")) ?:
                         (group (+ digit)) ?: (group (+ digit)) ": ")
                   :no-group)))
      `(progn
         (add-to-list 'compilation-error-regexp-alist ',name)
         (add-to-list 'compilation-error-regexp-alist-alist
                      (list ',name ,regexp 1 2 3 ,type))))))

(bazel--add-compilation-error-regexp bazel-mode-info "INFO" 0)
(bazel--add-compilation-error-regexp bazel-mode-warning "WARNING" 1)
(bazel--add-compilation-error-regexp bazel-mode-error "ERROR" 2)

;;;; Imenu support

(defun bazel-mode-create-index ()
  "Return an Imenu index for the rules in the current buffer.
This function is useful as ‘imenu-create-index-function’ for
‘bazel-build-mode’ and ‘bazel-workspace-mode’.  See Info node
‘(elisp) Imenu’ for details."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((index ()))
        ;; Heuristic: We search for “name” attributes as they would show in
        ;; typical BUILD files.  That’s not 100% correct, but doesn’t rely on
        ;; external processes and should work fine in common cases.
        (while (re-search-forward
                ;; The target pattern isn’t the same as
                ;; https://docs.bazel.build/versions/3.1.0/build-ref.html#name
                ;; (we don’t allow quotation marks in target names), but should
                ;; be good enough here.
                (rx bol (* blank) "name" (* blank) ?= (* blank)
                    (group (any ?\" ?'))
                    (group (+ (any "a-z" "A-Z" "0-9"
                                   ?- "!%@^_` #$&()*+,;<=>?[]{|}~/.")))
                    (backref 1))
                nil t)
          (let ((name (match-string-no-properties 2))
                (pos (save-excursion
                       (python-nav-beginning-of-statement)
                       (if imenu-use-markers (point-marker) (point)))))
            (push (cons name pos) index)))
        (nreverse index)))))

(defun bazel-mode-extract-function-name ()
  "Return the name of the Starlark function at point.
Return nil if no name was found.  This function is useful as
‘imenu-extract-index-name-function’ for ‘bazel-starlark-mode’."
  (and (looking-at python-nav-beginning-of-defun-regexp)
       (match-string-no-properties 1)))

;;;; Speedbar

(with-eval-after-load 'speedbar
  (speedbar-add-supported-extension (rx "BUILD" (? ".bazel"))))

(declare-function speedbar-add-supported-extension "speedbar" (extension))

;;;; Project integration

;; We add ourselves with relatively low priority because we don’t have an
;; optimized ‘project-files’ implementation.  Other project functions with
;; potentially optimized implementations should get a chance to run first.
(add-hook 'project-find-functions #'bazel-find-project 20)

(cl-defstruct (bazel-workspace (:noinline t))
  "Represents a Bazel workspace."
  (root nil
        :read-only t
        :type string
        :documentation "The workspace root directory."))

(defun bazel-find-project (directory)
  "Find a Bazel workspace for the given DIRECTORY.
Return nil if outside a Bazel workspace or a project instance for
the containing workspace.  This function is suitable for
‘project-find-functions’."
  (cl-check-type directory string)
  (when-let ((root (bazel--workspace-root directory)))
    (make-bazel-workspace :root root)))

(cl-defmethod project-roots ((project bazel-workspace))
  "Return the primary root directory of the Bazel workspace PROJECT."
  (list (bazel-workspace-root project)))

(cl-defmethod project-external-roots ((project bazel-workspace))
  "Return the external workspace roots of the Bazel workspace PROJECT."
  (let ((main-root (bazel-workspace-root project)))
    (condition-case nil
        (directory-files (bazel--external-workspace-dir main-root)
                         :full
                         ;; Assume that workspace names follow similar patters
                         ;; as package names,
                         ;; https://docs.bazel.build/versions/3.0.0/build-ref.html#package-names-package-name.
                         (rx bos (+ (any alnum "-._")) eos))
      ;; If there’s no external workspace directory, don’t signal an error.
      (file-missing nil))))

;;;; Commands to build and run code using Bazel

(defun bazel-build (target)
  "Build a Bazel TARGET."
  (interactive (list (bazel--read-target "build")))
  (bazel--run-bazel-command "build" target))

(defun bazel-run (target)
  "Build and run a Bazel TARGET."
  (interactive (list (bazel--read-target "run")))
  (bazel--run-bazel-command "run" target))

(defun bazel-test (target)
  "Build and run a Bazel test TARGET."
  (interactive (list (bazel--read-target "test")))
  (bazel--run-bazel-command "test" target))

(defun bazel-coverage (target)
  "Run Bazel test TARGET with coverage instrumentation enabled."
  (interactive (list (bazel--read-target "coverage")))
  (bazel--run-bazel-command "coverage" target))

(defun bazel--run-bazel-command (command target)
  "Run Bazel tool with given COMMAND, e.g. build or run, on the given TARGET."
  (compile
   (mapconcat #'shell-quote-argument
              (append bazel-build-bazel-command (list command target)) " ")))

(defun bazel--read-target (command)
  "Read a Bazel build target from the minibuffer.
COMMAND is a Bazel command to be included in the minibuffer prompt."
  (let* ((file-name (buffer-file-name))
         (workspace-root
          (or (bazel--workspace-root file-name)
              (user-error "Not in a Bazel workspace.  No WORKSPACE file found")))
         (package-name
          (or (bazel--package-name file-name workspace-root)
              (user-error "Not in a Bazel package.  No BUILD file found")))
         (initial-input (concat "//" package-name))
         (prompt (combine-and-quote-strings
                  (append bazel-build-bazel-command (list command "")))))
    (read-string prompt initial-input)))

;;;; Utility functions to work with Bazel workspaces

(defun bazel-util-workspace-root (file-name)
  "Find the root of the Bazel workspace containing FILE-NAME.
If FILE-NAME is not in a Bazel workspace, return nil.  Otherwise,
the return value is a directory name."
  (declare (obsolete "don’t use it, as it’s an internal function."
                     "2021-04-13"))
  (bazel--workspace-root file-name))

(defun bazel--workspace-root (file-name)
  "Find the root of the Bazel workspace containing FILE-NAME.
If FILE-NAME is not in a Bazel workspace, return nil.  Otherwise,
the return value is a directory name."
  (cl-check-type file-name string)
  (let ((result (or (locate-dominating-file file-name "WORKSPACE")
                    (locate-dominating-file file-name "WORKSPACE.bazel"))))
    (and result (file-name-as-directory result))))

(defun bazel-util-package-name (file-name workspace-root)
  "Return the nearest Bazel package for FILE-NAME under WORKSPACE-ROOT.
If FILE-NAME is not in a Bazel package, return nil."
  (declare (obsolete "don’t use it, as it’s an internal function."
                     "2021-04-13"))
  (bazel--package-name file-name workspace-root))

(defun bazel--package-name (file-name workspace-root)
  "Return the nearest Bazel package for FILE-NAME under WORKSPACE-ROOT.
If FILE-NAME is not in a Bazel package, return nil."
  (cl-check-type file-name string)
  (cl-check-type workspace-root string)
  (when (< emacs-major-version 27)
    ;; Work around https://debbugs.gnu.org/cgi/bugreport.cgi?bug=29579.
    (cl-callf file-name-unquote file-name)
    (cl-callf file-name-unquote workspace-root))
  (let ((build-file-directory
         (cl-some (lambda (build-name)
                    (locate-dominating-file file-name build-name))
                  '("BUILD.bazel" "BUILD"))))
    (cond ((not build-file-directory) nil)
          ((file-equal-p workspace-root build-file-directory) "")
          ((file-in-directory-p build-file-directory workspace-root)
           (let ((package-name
                  (file-relative-name build-file-directory workspace-root)))
             ;; Only return package-name if we can confirm it is the local
             ;; relative file name of a BUILD file.
             (and package-name
                  (not (file-remote-p package-name))
                  (not (file-name-absolute-p package-name))
                  (not (string-prefix-p "." package-name))
                  (directory-file-name package-name)))))))

(defun bazel--external-workspace (workspace-name this-workspace-root)
  "Return the workspace root of an external workspace.
WORKSPACE-NAME should be either a string naming an external
workspace, or nil to refer to the current workspace.
THIS-WORKSPACE-ROOT should be the name of the current workspace
root directory, as returned by ‘bazel--workspace-root’.  The
return value is a directory name."
  (cl-check-type workspace-name (or null string))
  (cl-check-type this-workspace-root string)
  (file-name-as-directory
   (if workspace-name
       ;; See https://docs.bazel.build/versions/2.0.0/output_directories.html
       ;; for some overview of the Bazel directory layout.  Empirically, the
       ;; directory ROOT/bazel-ROOT/external/WORKSPACE is a symlink to the
       ;; workspace root of the external workspace WORKSPACE.  Again, this is a
       ;; heuristic, and should work for the common case.  In particular, we
       ;; don’t want to shell out to “bazel info workspace” here, because that
       ;; might block indefinitely if another Bazel instance holds the lock.
       ;; We don’t check whether the directory exists, because it’s generated
       ;; and cached when needed.
       (expand-file-name workspace-name
                         (bazel--external-workspace-dir this-workspace-root))
     this-workspace-root)))

(defun bazel--external-workspace-dir (root)
  "Return a directory name for the parent directory of the external workspaces.
ROOT should be the main workspace root as returned by
‘bazel--workspace-root’."
  (cl-check-type root string)
  ;; See the commentary in ‘bazel--external-workspace’ for how to find external
  ;; workspaces.
  (expand-file-name
   (concat "bazel-" (file-name-nondirectory (directory-file-name root))
           "/external/" )
   root))

(defun bazel--parse-label (label)
  "Parse Bazel label LABEL.
If LABEL isn’t syntactically valid, return nil.  Otherwise,
return a triple (WORKSPACE PACKAGE TARGET).  WORKSPACE is
nil (for the current workspace) or a string referring to some
external workspace.  PACKAGE is nil (for the current package) or
a package name string.  TARGET is a string referring to the local
name of LABEL.  See
https://docs.bazel.build/versions/2.0.0/build-ref.html#lexi for
the lexical syntax of labels."
  (cl-check-type label string)
  (pcase (substring-no-properties label)
    ((rx bos
         (or
          ;; @workspace//package:target
          (seq "@" (let workspace (+ (not (any ?: ?/))))
               "//" (let package (* (not (any ?:))))
               ?: (let target (+ (not (any ?:)))))
          ;; @workspace//package
          (seq "@" (let workspace (+ (not (any ?: ?/))))
               "//" (let package (+ (not (any ?:)))))
          ;; //package:target
          (seq "//" (let package (* (not (any ?:))))
               ?: (let target (+ (not (any ?:)))))
          ;; //package
          (seq "//" (let package (+ (not (any ?:)))))
          ;; :target
          (seq ?: (let target (+ (not (any ?:)))))
          ;; target
          (seq (let target (not (any ?: ?/ ?@)) (* (not (any ?:))))))
         eos)
     (unless target (setq target (bazel--default-target package)))
     (and (or (null workspace)
              (string-match-p (rx bos (+ (any ?- "A-Za-z0-9.")) eos) workspace))
          (or (null package)
              ;; https://docs.bazel.build/versions/2.0.0/build-ref.html#package-names-package-name
              (string-match-p (rx bos (* (any ?- "A-Za-z0-9/.")) eos) package))
          ;; https://docs.bazel.build/versions/2.0.0/build-ref.html#name
          (string-match-p (rx bos
                              (+ (any ?- "a-zA-Z0-9!%-@^_` \"#$&'()*+,;<=>"
                                      "?[]{|}~/."))
                              eos)
                          target)
          (list workspace package target)))))

(defun bazel--default-target (package)
  "Return the default target name for PACKAGE.
For a package “foo/bar”, “bar” is the default target."
  (cl-check-type package string)
  ;; There’s no function to search backwards within a string, so we reverse the
  ;; string twice.
  (let ((reversed (reverse package)))
    (nreverse (substring-no-properties reversed nil
                                       (string-match-p (rx ?/) reversed)))))

(defun bazel--canonical (workspace package target)
  "Return a canonical label.
WORKSPACE is either nil (referring to the current workspace) or
an external workspace name.  PACKAGE and TARGET should both be
strings.  Return either @WORKSPACE//PACKAGE:TARGET or
//PACKAGE:TARGET."
  (cl-check-type workspace (or null string))
  (cl-check-type package string)
  (cl-check-type target string)
  (concat (and workspace (concat "@" workspace)) "//" package ":" target))

(defun bazel--string-at-point ()
  "Return the string literal at point, or nil if not inside a string literal."
  (let ((state (syntax-ppss)))
    (when (nth 3 state)  ; in string
      (let ((start (1+ (nth 8 state))))  ; (nth 8 state) is the opening quote
        (save-excursion
          ;; Jump to the closing quotation mark.
          (parse-partial-sexp (point) (point-max) nil nil state 'syntax-table)
          (buffer-substring-no-properties start (1- (point))))))))

(defun bazel--find-magic-comment (bound)
  "Search for a magic comment from point to BOUND.
If a magic comment was found, return non-nil and set the match to
the comment text."
  (let ((case-fold-search t))
    (and (re-search-forward bazel--magic-comment-regexp
                            bound t)
         (nth 4 (syntax-ppss)))))

(defun bazel--line-column-pos (line column)
  "Return buffer position in the current buffer for LINE and COLUMN.
Restrict LINE to the buffer size and COLUMN to the number of
characters in LINE.  COLUMN is measured in characters, not visual
columns."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (forward-line (1- line))
      (min (line-end-position) (+ (point) column)))))

(defalias 'bazel--json-parse-buffer
  (if (fboundp 'json-parse-buffer) #'json-parse-buffer
    (lambda ()
      "Polyfill for ‘json-parse-buffer’."
      (let ((json-object-type 'hash-table)
            (json-array-type 'vector)
            (json-key-type 'string)
            (json-null :null)
            (json-false :false)
            (json-pre-element-read-function nil)
            (json-post-element-read-function nil))
        (json-read)))))

(provide 'bazel)
;;; bazel.el ends here
