;;; bazel-mode.el --- Emacs major mode for editing Bazel BUILD and WORKSPACE files -*- lexical-binding: t; -*-


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
;; This package provides Emacs bazel-mode, a major mode for editing Bazel
;; BUILD and WORKSPACE files.
;;
;;; Code:

(require 'cl-lib)
(require 'flymake)
(require 'json)
(require 'pcase)
(require 'python)
(require 'rx)
(require 'xref)

(require 'bazel-util)

(defgroup bazel-mode nil
  "Major mode for Bazel BUILD files."
  :link '(url-link "https://github.com/bazelbuild/emacs-bazel-mode")
  :group 'languages)

(defcustom bazel-mode-buildifier-command "buildifier"
  "Filename of buildifier executable."
  :type 'file
  :group 'bazel-mode
  :link '(url-link
          "https://github.com/bazelbuild/buildtools/tree/master/buildifier"))

(defcustom bazel-mode-buildifier-before-save nil
  "Specifies whether to run buildifer in `before-save-hook'."
  :type 'boolean
  :group 'bazel-mode
  :link '(url-link
          "https://github.com/bazelbuild/buildtools/tree/master/buildifier")
  :risky t)

(defun bazel-mode-buildifier ()
  "Format current buffer using buildifier."
  (interactive "*")
  (let ((input-buffer (current-buffer))
        (input-file buffer-file-name)
        (buildifier-buffer (get-buffer-create "*buildifier*"))
        ;; Run buildifier on a file to support remote BUILD files.
        (buildifier-input-file (make-nearby-temp-file "buildifier"))
        (buildifier-error-file (make-nearby-temp-file "buildifier")))
    (unwind-protect
      (write-region (point-min) (point-max) buildifier-input-file nil :silent)
      (with-current-buffer buildifier-buffer
        (setq-local inhibit-read-only t)
        (erase-buffer)
        (let ((return-code
               (apply #'process-file
                      bazel-mode-buildifier-command buildifier-input-file
                      `(t ,buildifier-error-file) nil
                      (bazel-mode--buildifier-file-flags input-file))))
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

(defun bazel-mode--buildifier-before-save-hook ()
  "Run buildifer in `before-save-hook'."
  (when bazel-mode-buildifier-before-save
    (bazel-mode-buildifier)))

(defconst bazel-mode--font-lock-keywords
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
    ))

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

;;;###autoload
(define-derived-mode bazel-mode prog-mode "Bazel"
  "Major mode for editing Bazel BUILD and WORKSPACE files."
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+ *")
  (setq-local comment-end "")
  (setq-local comment-use-syntax t)
  (setq-local font-lock-defaults (list bazel-mode--font-lock-keywords))
  (setq-local indent-line-function #'python-indent-line-function)
  (setq-local indent-region-function #'python-indent-region)
  (add-hook 'before-save-hook #'bazel-mode--buildifier-before-save-hook
            nil :local)
  (add-hook 'flymake-diagnostic-functions #'bazel-mode-flymake nil :local)
  (add-hook 'xref-backend-functions #'bazel-mode-xref-backend nil :local))

;;;###autoload
(add-to-list 'auto-mode-alist
             (cons (rx (or "/BUILD" "/WORKSPACE" ".bazel" ".bzl" ".BUILD") eos)
                   #'bazel-mode))

;;; Flymake support using Buildifier

(defvar-local bazel-mode--flymake-process nil
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
  (when bazel-mode--flymake-process
    (delete-process bazel-mode--flymake-process))
  (let* ((coding-system-for-read 'utf-8-unix)
         (coding-system-for-write 'utf-8-unix)
         (process-connection-type nil)
         (input-buffer (current-buffer))
         (output-buffer (generate-new-buffer
                         (format " *Buildifier output for %s*" (buffer-name))))
         (process
          (apply #'start-file-process
                 (format "Buildifier for %s" (buffer-name))
                 output-buffer
                 bazel-mode-buildifier-command
                 `(,@(bazel-mode--buildifier-file-flags buffer-file-name)
                   "-mode=check" "-format=json" "-lint=warn"))))
    (set-process-sentinel
     process
     (lambda (process event)
       ;; First, make sure that the buffer to be linted is still alive.
       (when (and (buffer-live-p input-buffer)
                  (string-equal event "finished\n"))
         (with-current-buffer input-buffer
           ;; Don’t report anything if a new process has already been started.
           (when (eq bazel-mode--flymake-process process)
             (funcall report-fn (bazel-mode--make-diagnostics output-buffer)))
           (setq bazel-mode--flymake-process nil))
         (kill-buffer output-buffer))))
    (set-process-query-on-exit-flag process nil)
    (setq bazel-mode--flymake-process process)
    (save-restriction
      (widen)
      (process-send-region process (point-min) (point-max)))
    (process-send-eof process)))

(defun bazel-mode--buildifier-file-flags (filename)
  "Return a list of -path and -type flags for Buildifier.
Use FILENAME to derive appropriate flags, if possible.
Otherwise, return an empty list."
  (when filename
    (delq nil
          (let ((workspace (bazel-util-workspace-root filename))
                (base (file-name-base filename))
                (extension (file-name-extension filename :period)))
            (list (and workspace
                       (concat "-path="
                               (file-relative-name filename workspace)))
                  (cond ((or (string-equal base "BUILD")
                             (string-equal extension ".BUILD"))
                         "-type=build")
                        ((string-equal extension ".bzl")
                         "-type=bzl")
                        ((string-equal base "WORKSPACE")
                         "-type=workspace")))))))

(defun bazel-mode--make-diagnostics (output-buffer)
  "Return Flymake diagnostics for the Buildifier report in OUTPUT-BUFFER.
OUTPUT-BUFFER should contain a JSON report for the file visited
by the current buffer as described in
https://github.com/bazelbuild/buildtools/blob/master/buildifier/README.md#file-diagnostics-in-json.
All filenames in OUTPUT-BUFFER are ignored; all messages are
attached to the current buffer.  Return a list of Flymake
diagnostics; see Info node ‘(Flymake) Backend functions’ for
details."
  (let ((report (bazel-mode--parse-report output-buffer)))
    (mapcan #'bazel-mode--diagnostics-for-file (gethash "files" report))))

(defun bazel-mode--parse-report (buffer)
  "Parse Buildifier JSON report in BUFFER.
Return the JSON report as a hashtable.  The format of the report
is described in
https://github.com/bazelbuild/buildtools/blob/master/buildifier/README.md#file-diagnostics-in-json."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        ;; Skip over standard error messages if possible.
        (when (re-search-forward (rx bol ?{) nil t)
          (backward-char)
          (bazel-mode--json-parse-buffer))))))

(defun bazel-mode--diagnostics-for-file (file)
  "Return list of Flymake diagnostics for FILE.
FILE should be a hashtable containing diagnostics for a single
file, as described in
https://github.com/bazelbuild/buildtools/blob/master/buildifier/README.md#file-diagnostics-in-json."
  (mapcar #'bazel-mode--diagnostic-for-warning (gethash "warnings" file)))

(defun bazel-mode--diagnostic-for-warning (warning)
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
      ;; e.g. for the module-docstring warning.  ‘bazel-mode--line-column-pos’
      ;; ensures that we don’t cross line boundaries.
      (setq end-column (buffer-size)))
    (flymake-make-diagnostic
     (current-buffer)
     (bazel-mode--line-column-pos start-line start-column)
     (bazel-mode--line-column-pos end-line end-column)
     :warning
     (format-message "%s [%s] (%s)"
                     ;; We only take the first line of the message, otherwise
                     ;; it becomes too long and looks ugly.
                     (substring-no-properties message nil
                                              (string-match-p (rx ?\n) message))
                     (gethash "category" warning)
                     (gethash "url" warning)))))

;;; XRef backend

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
       (bazel-util-workspace-root buffer-file-name)
       'bazel-mode))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql bazel-mode)))
  ;; This only detects string literals representing labels.
  (let ((identifier (bazel-mode--string-at-point)))
    (when identifier
      ;; Save the current workspace directory and package as text properties
      ;; in case the user switches directories between this call and
      ;; selecting a reference.  ‘xref-backend-definitions’ falls back to the
      ;; current workspace and package if these properties aren’t present so
      ;; that users can still invoke ‘xref-find-definitions’ and enter a
      ;; label manually.  We don’t care about valid but exotic labels here,
      ;; e.g., labels containing newlines or backslashes.
      (let* ((workspace
              (and buffer-file-name
                   (bazel-util-workspace-root buffer-file-name)))
             (package
              (and workspace
                   (bazel-util-package-name buffer-file-name workspace))))
        (propertize identifier
                    'bazel-mode-workspace workspace
                    'bazel-mode-package package)))))

(cl-defmethod xref-backend-definitions ((_backend (eql bazel-mode)) identifier)
  (cl-destructuring-bind (&whole valid-p &optional workspace package target)
      (bazel-mode--parse-label identifier)
    (when valid-p
      (let* ((this-workspace
              (or (get-text-property 0 'bazel-workspace-mode identifier)
                  (and buffer-file-name
                       (bazel-util-workspace-root buffer-file-name))))
             (package
              (or package
                  (get-text-property 0 'bazel-mode-package identifier)
                  (and buffer-file-name this-workspace
                       (bazel-util-package-name buffer-file-name
                                                this-workspace)))))
        (when (and this-workspace package)
          (let* ((workspace-root
                  (bazel-mode--external-workspace workspace this-workspace))
                 (directory (expand-file-name package workspace-root))
                 (filename (expand-file-name target directory))
                 (location
                  (if (file-exists-p filename)
                      ;; A label that likely refers to a source file.
                      (bazel-mode--file-location filename)
                    ;; A label that likely refers to a rule.  Try to find the
                    ;; rule in the BUILD file of the package.
                    (let ((build-file
                           (locate-file "BUILD" (list directory) '("" ".bazel"))))
                      (when build-file
                        (bazel-mode--rule-location build-file target))))))
            (when location
              (list (xref-make (bazel-mode--canonical workspace package target)
                               location)))))))))

(defun bazel-mode--file-location (filename)
  "Return an ‘xref-location’ for the source file FILENAME."
  (cl-check-type filename string)
  (let ((buffer (find-buffer-visiting filename)))
    (if buffer
        ;; The location actually refers to the whole file.  Avoid jumping
        ;; around within the already-visited file by pretending the reference
        ;; is at point.
        (xref-make-buffer-location buffer (with-current-buffer buffer (point)))
      ;; File not yet visited; beginning of the file is fine.
      (xref-make-file-location filename 1 0))))

(defun bazel-mode--rule-location (build-file name)
  "Return an ‘xref-location’ for a rule within a BUILD file.
The name of the BUILD file is BUILD-FILE, and NAME is the local
name of the rule.  If NAME doesn’t seem to exist in BUILD-FILE,
return a location referring to an arbitrary position within the
BUILD file."
  (cl-check-type build-file string)
  (cl-check-type name string)
  (let ((buffer (find-buffer-visiting build-file)))
    ;; Reuse a buffer that already visits the BUILD file if possible.
    (if buffer
        (with-current-buffer buffer
          (xref-make-buffer-location buffer (bazel-mode--find-rule name)))
      (with-temp-buffer
        (insert-file-contents build-file)
        (goto-char (bazel-mode--find-rule name))
        (xref-make-file-location build-file
                                 (line-number-at-pos)
                                 (- (point) (line-beginning-position)))))))

(defun bazel-mode--find-rule (name)
  "Find the rule with the given NAME within the current buffer.
The current buffer should visit a BUILD file.  If there’s a rule
with the given NAME, return the location of the rule.  Otherwise,
return point."
  (cl-check-type name string)
  (let ((case-fold-search nil))
    (or (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            ;; Heuristic: we search for a “name” attribute as it would show up
            ;; in typical BUILD files.  That’s not 100% correct, but doesn’t
            ;; rely on external processes and should work fine in common cases.
            (when (re-search-forward
                   (rx-to-string
                    `(seq bol (* blank) "name" (* blank) ?= (* blank)
                          (group (any ?\" ?')) (group ,name) (backref 1)))
                   nil t)
              (match-beginning 2))))
        (point))))

;;; Utilities

(defun bazel-mode--external-workspace (workspace-name this-workspace-root)
  "Return the workspace root of an external workspace.
WORKSPACE-NAME should be either a string naming an external
workspace, or nil to refer to the current workspace.
THIS-WORKSPACE-ROOT should be the name of the current workspace
root directory, as returned by ‘bazel-util-workspace-root’.  The
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
       (expand-file-name (concat "bazel-"
                                 (file-name-nondirectory
                                  (directory-file-name this-workspace-root))
                                 "/external/" workspace-name)
                         this-workspace-root)
     this-workspace-root)))

(defun bazel-mode--parse-label (label)
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
          (seq (let target (+ (not (any ?:))))))
         eos)
     (unless target (setq target (bazel-mode--default-target package)))
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

(defun bazel-mode--default-target (package)
  "Return the default target name for PACKAGE.
For a package “foo/bar”, “bar” is the default target."
  (cl-check-type package string)
  ;; There’s no function to search backwards within a string, so we reverse the
  ;; string twice.
  (let ((reversed (reverse package)))
    (nreverse (substring-no-properties reversed nil
                                       (string-match-p reversed (rx ?/))))))

(defun bazel-mode--canonical (workspace package target)
  "Return a canonical label.
WORKSPACE is either nil (referring to the current workspace) or
an external workspace name.  PACKAGE and TARGET should both be
strings.  Return either @WORKSPACE//PACKAGE:TARGET or
//PACKAGE:TARGET."
  (cl-check-type workspace (or null string))
  (cl-check-type package string)
  (cl-check-type target string)
  (concat (and workspace (concat "@" workspace)) "//" package ":" target))

(defun bazel-mode--string-at-point ()
  "Return the string literal at point, or nil if not inside a string literal."
  (let ((state (syntax-ppss)))
    (when (nth 3 state)  ; in string
      (let ((start (1+ (nth 8 state))))  ; (nth 8 state) is the opening quote
        (save-excursion
          ;; Jump to the closing quotation mark.
          (parse-partial-sexp (point) (point-max) nil nil state 'syntax-table)
          (buffer-substring-no-properties start (1- (point))))))))

(defun bazel-mode--line-column-pos (line column)
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

(defalias 'bazel-mode--json-parse-buffer
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

(provide 'bazel-mode)

;;; bazel-mode.el ends here
