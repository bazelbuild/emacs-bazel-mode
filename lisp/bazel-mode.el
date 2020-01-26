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

(require 'flymake)
(require 'json)
(require 'python)
(require 'rx)

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
                      `(t ,buildifier-error-file) nil "-type=build"
                      (and input-file (list (concat "-path=" input-file))))))
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
  (add-hook 'flymake-diagnostic-functions #'bazel-mode-flymake nil :local))

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
         (process (apply #'start-file-process
                         (format "Buildifier for %s" (buffer-name))
                         output-buffer
                         bazel-mode-buildifier-command
                         `(,@(bazel-mode--buildifier-file-flags)
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

(defun bazel-mode--buildifier-file-flags ()
  "Return a list of -path and -type flags for Buildifier.
Use the name of the file that the current buffer visits to derive
appropriate flags, if possible.  Otherwise, return an empty
list."
  (when buffer-file-name
    (delq nil
          (let ((workspace (locate-dominating-file buffer-file-name "WORKSPACE"))
                (base (file-name-base))
                (extension (file-name-extension buffer-file-name :period)))
            (list (and workspace
                       (concat "-path="
                               (file-relative-name buffer-file-name workspace)))
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
  (flymake-make-diagnostic
   (current-buffer)
   (bazel-mode--buildifier-pos (gethash "start" warning))
   (bazel-mode--buildifier-pos (gethash "end" warning))
   :warning
   (format-message "%s [%s] (%s)"
                   (gethash "message" warning)
                   (gethash "category" warning)
                   (gethash "url" warning))))

(defun bazel-mode--buildifier-pos (location)
  "Return buffer position for LOCATION returned by Buildifier.
LOCATION should be a hashtable with keys “line” and “column.”
Return the buffer position in the current buffer corresponding to
that line and column."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (min (point-max)
           (max (point-min)
                (+ (line-beginning-position (gethash "line" location 1))
                   (gethash "column" location 0)))))))

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
