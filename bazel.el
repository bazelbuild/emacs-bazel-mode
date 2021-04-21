;;; bazel.el --- Bazel support for Emacs -*- lexical-binding: t; -*-

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

;; This package provides support for the Bazel build system.  See
;; https://bazel.build/ for background on Bazel.
;;
;; The package provides four major modes for editing Bazel-related files:
;; ‘bazel-build-mode’ for BUILD files, ‘bazel-workspace-mode’ for WORKSPACE
;; files, ‘bazelrc-mode’ for .bazelrc configuration files, and
;; ‘bazel-starlark-mode’ for extension files written in the Starlark language.
;; These modes also extend Imenu and ‘find-file-at-point’ to support
;; Bazel-specific syntax.
;;
;; If Buildifier is available, the ‘bazel-mode-flymake’ backend for Flymake
;; provides on-the-fly syntax checking for Bazel files.  You can also run
;; Buildifier manually using the ‘bazel-buildifier’ command to reformat a Bazel
;; file buffer.
;;
;; The Bazel modes integrate with Xref to provide basic functionality to jump to
;; the definition of Bazel targets.
;;
;; To simplify running Bazel commands, the package provides the commands
;; ‘bazel-build’, ‘bazel-test’, ‘bazel-coverage’ and ‘bazel-run’, which execute
;; the corresponding Bazel commands in a compilation mode buffer.
;;
;; You can customize some aspects of this package using the ‘bazel’
;; customization group.  If you set the user option ‘bazel-display-coverage’ to
;; a non-nil value and then run ‘bazel coverage’ (either using the ‘compile’ or
;; ‘bazel-coverage’ command), the package will display code coverage status in
;; buffers that visit instrumented files, using the faces ‘bazel-covered-line’
;; and ‘bazel-uncovered-line’ for covered and uncovered lines, respectively.

;;; Code:

(require 'add-log)
(require 'cl-lib)
(require 'compile)
(require 'conf-mode)
(require 'derived)
(require 'easymenu)
(require 'ffap)
(require 'flymake)
(require 'font-lock)
(require 'imenu)
(require 'json)
(require 'menu-bar)
(require 'pcase)
(require 'project)
(require 'python)
(require 'rx)
(require 'subr-x)
(require 'syntax)
(require 'testcover)
(require 'xref)

;;;; Customization options

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
  :risky t
  :group 'bazel
  :link '(url-link
          "https://github.com/bazelbuild/buildtools/tree/master/buildifier"))

(define-obsolete-variable-alias 'bazel-mode-buildifier-before-save
  'bazel-buildifier-before-save "2021-04-13")

(defcustom bazel-buildifier-before-save nil
  "Specifies whether to run buildifer in `before-save-hook'."
  :type 'boolean
  :group 'bazel
  :link '(url-link
          "https://github.com/bazelbuild/buildtools/tree/master/buildifier")
  :risky t)

(defcustom bazel-display-coverage nil
  "Specifies whether to parse compilation buffers for coverage information.
If nil, don’t attempt to find coverage information in compilation
buffers.  If t, always search for coverage information, and
display the coverage status in buffers that visit covered files.
If ‘local’, only do so for local files."
  :type '(radio (const :tag "Never" nil)
                (const :tag "Always" t)
                (const :tag "Only for local files" local))
  :group 'bazel)

(defface bazel-covered-line '((default :extend t)
                              ;; Various shades of green.
                              (((background dark)) :background "#3D5C3A")
                              (t :background "#BEFFB8"))
  "Face for lines covered by unit tests."
  :group 'bazel)

(defface bazel-uncovered-line '((t :inherit testcover-nohits :extend t))
  "Face for lines not covered by unit tests."
  :group 'bazel)

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
                      bazel-buildifier-command buildifier-input-file
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
  (when bazel-buildifier-before-save
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
  ;; “keep sorted” is a magic comment that tells Buildifier to keep a list
  ;; sorted.  We treat it as a separate paragraph for filling.
  (setq-local paragraph-start
              (rx-to-string
               `(or (seq (* (syntax whitespace)) (regexp ,comment-start-skip)
                         "keep sorted")
                    (regexp ,paragraph-start))
               :no-group))
  (setq-local add-log-current-defun-function #'bazel-mode-current-rule-name)
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

;;;; Menu item

(easy-menu-add-item menu-bar-tools-menu nil
                    '("Bazel"
                      ;; We enable the workspace commands unconditionally
                      ;; because checking whether we’re in a Bazel workspace
                      ;; hits the filesystem and might be too slow.
                      ["Build..." bazel-build]
                      ["Test..." bazel-test]
                      ["Collect code coverage..." bazel-coverage]
                      ["Run target..." bazel-run]
                      ["Format buffer with Buildifier" bazel-buildifier
                       (derived-mode-p 'bazel-mode)])
                    "Debugger (GDB)...")

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
  (cl-check-type report-fn function)
  (let ((process bazel--flymake-process))
    (when process
      ;; The order here is important: ‘delete-process’ will trigger the
      ;; sentinel, and then ‘bazel--flymake-process’ already has to be nil to
      ;; avoid an obsolete report.
      (setq bazel--flymake-process nil)
      (delete-process process)))
  (let* ((non-essential t)
         (command `(,bazel-buildifier-command
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
  (cl-check-type type symbol)
  (cl-check-type filename (or string null))
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
  (cl-check-type output-buffer buffer-live)
  (cl-loop with case-fold-search = nil
           with report = (with-current-buffer output-buffer
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
  (cl-check-type warning hash-table)
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
  "Return the Bazel label at point as an XRef identifier."
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
  "Return locations where the Blaze target IDENTIFIER might be defined.
IDENTIFIER should be an XRef identifier returned by
‘xref-backend-identifier-at-point’ with the same backend."
  (cl-check-type identifier string)
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
  "Return a completion table for Bazel targets."
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
  (cl-check-type prefix string)
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
  (if-let ((buffer (find-buffer-visiting filename)))
      ;; If not found, use point to avoid jumping around in the buffer.
      (xref-make-buffer-location buffer
                                 (with-current-buffer buffer
                                   (or (funcall find-function) (point))))
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char (or (funcall find-function) (point-min)))
      (xref-make-file-location filename
                               (line-number-at-pos)
                               (- (point) (line-beginning-position))))))

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
  (cl-check-type filename string)
  (when-let ((main-root (bazel--workspace-root filename)))
    (let ((external-roots (bazel--external-workspace-roots main-root)))
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
  (cl-check-type name string)
  ;; https://docs.bazel.build/versions/3.0.0/guide.html#imports
  (let ((case-fold-search nil))
    (pcase name
      ((rx bos "%workspace%" (+ ?/) (let rest (+ nonl)))
       (when buffer-file-name
         (when-let ((workspace (bazel--workspace-root buffer-file-name)))
           (let ((file-name (expand-file-name rest workspace)))
             (and (file-exists-p file-name) file-name))))))))

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
                      '(,name ,regexp 1 2 3 ,type))))))

(bazel--add-compilation-error-regexp bazel-mode-info "INFO" 0)
(bazel--add-compilation-error-regexp bazel-mode-warning "WARNING" 1)
(bazel--add-compilation-error-regexp bazel-mode-error "ERROR" 2)

(add-hook 'compilation-finish-functions #'bazel-finish-compilation)

(defun bazel-finish-compilation (buffer message)
  "Parse Bazel build output in BUFFER.
If the option ‘bazel-display-coverage’ is non-nil and there are
references to coverage results in BUFFER, attempt to display the
line coverage status in buffers that are visiting files with
coverage information.  MESSAGE is the status message for the
Bazel process exit; \"finished\\n\" if Bazel completed
successfully.  This function is suitable for
‘compilation-finish-functions’."
  (cl-check-type buffer buffer)
  (cl-check-type message string)
  (when (and bazel-display-coverage (buffer-live-p buffer)
             (string-equal message "finished\n"))
    (with-current-buffer buffer
      (let ((remote (file-remote-p default-directory))
            (files ()))
        (when (or (not remote) (eq bazel-display-coverage t))
          ;; First collect potential coverage files.  If there are none (typical
          ;; case), we don’t have to hit the filesystem.
          (save-excursion
            (save-restriction
              (goto-char (point-min))
              ;; Parse Bazel output.  It’s supposed to be stable and easy to
              ;; parse,
              ;; cf. https://docs.bazel.build/versions/4.0.0/guide.html#parsing-output.
              ;; We assume that coverage instrumentation files are always called
              ;; “coverage.dat”.
              (while (re-search-forward
                      (rx bol "  " (group ?/ (+ nonl) "/coverage.dat") eol)
                      nil t)
                (let ((file (match-string-no-properties 1)))
                  ;; The coverage files are external filenames, so quote them
                  ;; (to avoid clashes with filename handlers) and make them
                  ;; remote if necessary.
                  (push (concat remote (file-name-quote file)) files))))))
        (when files
          ;; Only continue if we’re in a Bazel workspace.
          (when-let ((root (bazel--workspace-root default-directory)))
            ;; COVERAGE maps buffers to hashtables that in turn map line numbers
            ;; to hit counts.  We first collect coverage information into this
            ;; hashtable to correctly deal with duplicate file sections.
            (let ((coverage (make-hash-table :test #'eq)))
              (dolist (file files)
                (with-temp-buffer
                  ;; Don’t bail out if the coverage file can’t be read.  Maybe
                  ;; it has already been garbage-collected.
                  (when (condition-case nil
                            (insert-file-contents file)
                          (file-missing nil))
                    (bazel--parse-coverage root coverage))))
              (maphash #'bazel--display-coverage coverage))))))))

(defun bazel--parse-coverage (root coverage)
  "Parse coverage information in the current buffer.
ROOT is the Bazel workspace root directory.  COVERAGE is a
hashtable that maps buffers to hashtables that in turn map line
numbers to hit counts.  The function walks over the coverage
information in the current buffer and fills in COVERAGE."
  ;; See the manual page of ‘geninfo’ for a description of the coverage format.
  (while (re-search-forward (rx bol "SF:" (group (+ nonl)) eol) nil t)
    (let ((begin (line-beginning-position 2))
          (file (expand-file-name (match-string-no-properties 1) root)))
      (when-let ((end (re-search-forward (rx bol "end_of_record" eol) nil t))
                 ;; Only collect coverage for files that are visited in some
                 ;; buffer.
                 (buffer (find-buffer-visiting file)))
        (goto-char begin)
        (while (re-search-forward (rx bol "DA:" (group (+ digit)) ?,
                                      (group (+ digit)) (? ?, (+ nonl)) eol)
                                  end t)
          (let ((line (cl-the natnum (string-to-number
                                      (match-string-no-properties 1))))
                (hits (cl-the natnum (string-to-number
                                      (match-string-no-properties 2))))
                ;; DATA maps line numbers to hit counts for the current file.
                (data (or (gethash buffer coverage)
                          (puthash buffer (make-hash-table :test #'eql)
                                   coverage))))
            (cl-incf (gethash line data 0) hits)))))))

(defun bazel--display-coverage (buffer coverage)
  "Add overlays for coverage information in BUFFER.
COVERAGE is a hashtable mapping line numbers to hit counts.
Remove existing coverage overlays first."
  (let ((pairs ())
        (runs ()))
    (maphash
     (lambda (line hits)
       (let ((face
              (if (cl-plusp hits) 'bazel-covered-line 'bazel-uncovered-line)))
         (push (cons line face) pairs)))
     coverage)
    (cl-callf sort pairs #'car-less-than-car)
    ;; The Emacs Lisp manual cautions that overlays don’t scale well; see Info
    ;; node ‘(elisp) Overlays’.  Therefore, we create as few overlays as
    ;; possible by grouping consecutive lines with identical coverage status
    ;; into runs.
    (while pairs
      (cl-loop with (i . f) = (car pairs)
               for tail on (cdr pairs)
               for (j . g) = (car tail)
               for k from (1+ i)  ; expected line number for the next element
               while (and (eq f g) (eql j k))  ; stop as soon as run ends
               finally (push (list f i (1- k)) runs) (setq pairs tail)))
    (cl-callf nreverse runs)
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          ;; See remark at the bottom of Info node ‘(elisp) Managing Overlays’.
          (overlay-recenter (point-max))
          ;; We first remove existing coverage overlays because they are likely
          ;; stale.
          (bazel-remove-coverage-display)
          (cl-loop for (face i j) in runs
                   ;; Choose overlay positions and stickiness so that inserting
                   ;; text before or after a run doesn’t appear to extend the
                   ;; covered region.
                   for o = (make-overlay (line-beginning-position i)
                                         (line-end-position j)
                                         buffer :front-advance)
                   do
                   ;; Add ‘category’ property to find the overlays later (see
                   ;; ‘bazel-remove-coverage-display’).
                   (overlay-put o 'category 'bazel-coverage)
                   (overlay-put o 'face face)))))))

(defun bazel-remove-coverage-display ()
  "Remove all Bazel coverage display in the current buffer.
If the current buffer is narrowed, only act on the accessible
portion."
  (interactive)
  (remove-overlays (point-min) (point-max) 'category 'bazel-coverage))

;; Default overlay properties.
(put 'bazel-coverage 'priority 10)
(put 'bazel-coverage 'evaporate t)

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
      (let ((case-fold-search nil)
            (index ()))
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

(defun bazel-mode-current-rule-name ()
  "Return the name of the Bazel rule at point.
Return nil if not inside a Bazel rule."
  (let ((case-fold-search nil)
        (bound (save-excursion (python-nav-end-of-statement) (point))))
    (save-excursion
      (python-nav-beginning-of-statement)
      (when (re-search-forward
             ;; The target pattern isn’t the same as
             ;; https://docs.bazel.build/versions/3.1.0/build-ref.html#name (we
             ;; don’t allow quotation marks in target names), but should be good
             ;; enough here.
             (rx bol (* blank) "name" (* blank) ?= (* blank)
                 (group (any ?\" ?'))
                 (group (+ (any "a-z" "A-Z" "0-9"
                                ?- "!%@^_` #$&()*+,;<=>?[]{|}~/.")))
                 (backref 1))
             bound t)
        (match-string-no-properties 2)))))

(defun bazel-mode-extract-function-name ()
  "Return the name of the Starlark function at point.
Return nil if no name was found.  This function is useful as
‘imenu-extract-index-name-function’ for ‘bazel-starlark-mode’."
  (let ((case-fold-search nil))
    (and (looking-at python-nav-beginning-of-defun-regexp)
         (match-string-no-properties 1))))

;;;; Speedbar

(with-eval-after-load 'speedbar
  (speedbar-add-supported-extension (rx "BUILD" (? ".bazel"))))

(declare-function speedbar-add-supported-extension "speedbar" (extension))

;;;; Project integration

;; We add ourselves with relatively low priority because we don’t have an
;; optimized ‘project-files’ implementation.  Other project functions with
;; potentially optimized implementations should get a chance to run first.
(add-hook 'project-find-functions #'bazel-find-project 20)

(cl-defstruct bazel-workspace
  "Represents a Bazel workspace."
  (root nil
        :read-only t
        :type string
        :documentation "The workspace root directory."))

;; Inlining accessors would break clients that are compiled against a version
;; with an incompatible layout.  Normally we’d use the ‘:noinline’ structure
;; keyword in ‘cl-defstruct’, but we still support Emacs 26, which doesn’t know
;; about that keyword.  So we remove the compiler macros by hand for now.  Once
;; we drop support for Emacs 26, we should remove this hack in favor of
;; ‘:noinline’.
(put #'make-bazel-workspace 'compiler-macro nil)
(put #'copy-bazel-workspace 'compiler-macro nil)
(put #'bazel-workspace-p 'compiler-macro nil)
(put #'bazel-workspace-root 'compiler-macro nil)

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
  (bazel--external-workspace-roots (bazel-workspace-root project)))

;;;; Commands to build and run code using Bazel

(defun bazel-build (target)
  "Build a Bazel TARGET."
  (interactive (list (bazel--read-target "build")))
  (cl-check-type target string)
  (bazel--run-bazel-command "build" target))

(defun bazel-run (target)
  "Build and run a Bazel TARGET."
  (interactive (list (bazel--read-target "run")))
  (cl-check-type target string)
  (bazel--run-bazel-command "run" target))

(defun bazel-test (target)
  "Build and run a Bazel test TARGET."
  (interactive (list (bazel--read-target "test")))
  (cl-check-type target string)
  (bazel--run-bazel-command "test" target))

(defun bazel-coverage (target)
  "Run Bazel test TARGET with coverage instrumentation enabled."
  (interactive (list (bazel--read-target "coverage")))
  (cl-check-type target string)
  (bazel--run-bazel-command "coverage" target))

(defun bazel--run-bazel-command (command target)
  "Run Bazel tool with given COMMAND, e.g. build or run, on the given TARGET."
  (cl-check-type command string)
  (cl-check-type target string)
  (compile
   (mapconcat #'shell-quote-argument
              (append bazel-command (list command target)) " ")))

(defun bazel--read-target (command)
  "Read a Bazel build target from the minibuffer.
COMMAND is a Bazel command to be included in the minibuffer prompt."
  (cl-check-type command string)
  (let* ((file-name (buffer-file-name))
         (workspace-root
          (or (bazel--workspace-root file-name)
              (user-error "Not in a Bazel workspace.  No WORKSPACE file found")))
         (package-name
          (or (bazel--package-name file-name workspace-root)
              (user-error "Not in a Bazel package.  No BUILD file found")))
         (initial-input (concat "//" package-name))
         (prompt (combine-and-quote-strings
                  (append bazel-command (list command ""))))
         (candidates (bazel--rules-in-package workspace-root package-name)))
    (completing-read prompt candidates nil nil initial-input)))

;;;; Utility functions to work with Bazel workspaces

(defun bazel-util-workspace-root (file-name)
  "Find the root of the Bazel workspace containing FILE-NAME.
If FILE-NAME is not in a Bazel workspace, return nil.  Otherwise,
the return value is a directory name."
  (declare (obsolete "don’t use it, as it’s an internal function."
                     "2021-04-13"))
  (cl-check-type file-name string)
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
  (cl-check-type file-name string)
  (cl-check-type workspace-root string)
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

(defun bazel--external-workspace-roots (main-root)
  "Return the directory names of the external workspace roots.
MAIN-ROOT should be the main workspace root as returned by
‘bazel--workspace-root’."
  (let ((case-fold-search nil))
    (condition-case nil
        (directory-files (bazel--external-workspace-dir main-root)
                         :full
                         ;; Assume that workspace names follow similar patters
                         ;; as package names,
                         ;; https://docs.bazel.build/versions/3.0.0/build-ref.html#package-names-package-name.
                         (rx bos (+ (any alnum "-._")) eos))
      ;; If there’s no external workspace directory, don’t signal an error.
      (file-missing nil))))

(defun bazel--rules-in-package (root package)
  "Return the canonical labels for all rules in PACKAGE.
ROOT is the workspace root directory returned by
‘bazel--workspace-root’.  Also return the “//PACKAGE:all” and
“//PACKAGE/...” pseudo-targets."
  (cl-check-type root string)
  (cl-check-type package string)
  (append
   (when-let ((filename
               (locate-file "BUILD" (list (expand-file-name package root))
                            '(".bazel" ""))))
     (mapcar
      (lambda (name) (format "//%s:%s" package name))
      (if-let ((buffer (find-buffer-visiting filename)))
          (with-current-buffer buffer
            (bazel--complete-rules ""))
        (with-temp-buffer
          (insert-file-contents filename)
          (bazel-build-mode)
          (bazel--complete-rules "")))))
   (list (format "//%s:all" package)
         (if (string-empty-p package) "//..."
           (format "//%s/..." package)))))

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
  (let ((case-fold-search nil))
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
                (string-match-p (rx bos (+ (any ?- "A-Za-z0-9.")) eos)
                                workspace))
            (or (null package)
                ;; https://docs.bazel.build/versions/2.0.0/build-ref.html#package-names-package-name
                (string-match-p (rx bos (* (any ?- "A-Za-z0-9/.")) eos)
                                package))
            ;; https://docs.bazel.build/versions/2.0.0/build-ref.html#name
            (string-match-p (rx bos
                                (+ (any ?- "a-zA-Z0-9!%-@^_` \"#$&'()*+,;<=>"
                                        "?[]{|}~/."))
                                eos)
                            target)
            (list workspace package target))))))

(defun bazel--default-target (package)
  "Return the default target name for PACKAGE.
For a package “foo/bar”, “bar” is the default target."
  (cl-check-type package string)
  ;; There’s no function to search backwards within a string, so we reverse the
  ;; string twice.
  (let ((case-fold-search nil)
        (reversed (reverse package)))
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
  (cl-check-type bound natnum)
  (let ((case-fold-search nil))
    (and (search-forward "keep sorted" bound t)
         (nth 4 (syntax-ppss)))))

(defun bazel--line-column-pos (line column)
  "Return buffer position in the current buffer for LINE and COLUMN.
Restrict LINE to the buffer size and COLUMN to the number of
characters in LINE.  COLUMN is measured in characters, not visual
columns."
  (cl-check-type line natnum)
  (cl-check-type column natnum)
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
