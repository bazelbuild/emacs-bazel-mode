;;; bazel-mode.el       -*- lexical-binding:t -*-

;; Copyright (C) 2018 Robert E. Brown.

;; Bazel Mode is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; Bazel Mode is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Bazel Mode.  If not, see <http://www.gnu.org/licenses/>.

(require 'cl)
(require 'python)

(defgroup bazel nil
  "Major mode for editing Bazel code."
  :group 'languages
  :link '(url-link "https://github.com/brown/bazel-mode"))

(defcustom bazel-mode-hook nil
  "Hook called by `bazel-mode'."
  :type 'hook
  :group 'bazel)

(defcustom bazel-format-command "buildifier"
  "The executable used to format Bazel BUILD files."
  :type 'string
  :group 'bazel)

(defvar bazel-font-lock-keywords
  `(;; keywords
    ,(rx symbol-start
         (or "and" "break" "continue" "ctx" "def" "elif" "else" "fail" "for" "if" "in" "not" "or"
             "pass" "return" "self")
         symbol-end)
    ;; function definitions
    (,(rx symbol-start "def" (1+ space) (group (1+ (or word ?_))))
     (1 font-lock-function-name-face))
    ;; constants from Runtime.java
    (,(rx symbol-start (or "False" "None" "True") symbol-end)
     . font-lock-constant-face)
    ;; built-in functions
    (,(rx symbol-start
          (or
           ;; from MethodLibrary.java
           "all" "any" "bool" "capitalize" "count" "dict" "dir" "elems" "endswith" "enumerate"
           "fail" "find" "format" "getattr" "hasattr" "hash" "index" "int" "isalnum" "isalpha"
           "isdigit" "islower" "isspace" "istitle" "isupper" "join" "len" "list" "lower" "lstrip"
           "max" "min" "partition" "print" "range" "replace" "repr" "reversed" "rfind" "rindex"
           "rpartition" "rsplit" "rstrip" "sorted" "split" "splitlines" "startswith" "str" "strip"
           "title" "tuple" "upper" "zip"
           ;; from BazelLibrary.java
           "depset" "select" "to_list" "type" "union"
           ;; from SkylarkRepositoryModule.java
           "repository_rule"
           ;; from SkylarkAttr.java
           "configuration_field"
           ;; from SkylarkRuleClassFunctions.java
           "struct" "DefaultInfo" "OutputGroupInfo" "Actions" "provider" "rule" "aspect" "Label"
           "to_proto" "to_json"
           ;; from PackageFactory.java
           "distribs" "environment_group" "exports_files" "glob" "licenses" "native" "package"
           "package_group" "package_name" "repository_name"
           ;; from WorkspaceFactory.java
           "workspace" "register_execution_platforms" "register_toolchains"
           ;; from SkylarkNativeModule.java but not also in PackageFactory.java
           "existing_rule" "existing_rules"
           ;; from searching Bazel's Java code for "BLAZE_RULES".
           "aar_import" "action_listener" "alias" "android_binary" "android_device"
           "android_instrumentation_test" "android_library" "android_local_test"
           "android_ndk_repository" "android_sdk_repository" "apple_binary" "apple_static_library"
           "apple_stub_binary" "bind" "cc_binary" "cc_import" "cc_library" "cc_proto_library"
           "cc_test" "config_setting" "constraint_setting" "constraint_value" "extra_action"
           "filegroup" "genquery" "genrule" "git_repository" "http_archive" "http_file" "http_jar"
           "j2objc_library" "java_binary" "java_import" "java_library" "java_lite_proto_library"
           "java_package_configuration" "java_plugin" "java_proto_library" "java_runtime"
           "java_runtime_suite" "java_test" "java_toolchain" "local_repository" "maven_jar"
           "maven_server" "new_git_repository" "new_http_archive" "new_local_repository"
           "objc_bundle" "objc_bundle_library" "objc_framework" "objc_import" "objc_library"
           "objc_proto_library" "platform" "proto_lang_toolchain" "proto_library" "py_binary"
           "py_library" "py_runtime" "py_test" "sh_binary" "sh_library" "sh_test" "test_suite"
           "toolchain" "xcode_config" "xcode_version")
          symbol-end)
     . font-lock-builtin-face)
    ;; TODO:  Handle assignments better.  The code below fontifies a[b] = 1 and a = b = 2.
    ,(nth 7 python-font-lock-keywords)
    ,(nth 8 python-font-lock-keywords)
    ))

(defvar bazel-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-f" 'bazel-format)
    map))

(define-derived-mode bazel-mode python-mode "Bazel"
  "Major mode for editing Bazel files.

\\{bazel-mode-map}"
  :group 'bazel

  ;; Replace Python keyword fontification with Bazel keyword fontification.
  (setq font-lock-defaults
        '(bazel-font-lock-keywords
          nil nil nil nil
          (font-lock-syntactic-face-function . python-font-lock-syntactic-face-function))))

(defun bazel-parse-diff-action ()
  (unless (looking-at (rx line-start
                          (group (+ digit)) (? ?, (group (+ digit)))
                          (group (| ?a ?d ?c))
                          (group (+ digit)) (? ?, (group (+ digit)))
                          line-end))
    (error "bad diff output"))
  (let* ((orig-start (string-to-number (match-string 1)))
         (orig-count (if (null (match-string 2))
                         1
                       (1+ (- (string-to-number (match-string 2)) orig-start))))
         (command (match-string 3))
         (formatted-count (if (null (match-string 5))
                              1
                            (1+ (- (string-to-number (match-string 5))
                                   (string-to-number (match-string 4)))))))
    (list command orig-start orig-count formatted-count)))

(defun bazel-patch-buffer (buffer diff-buffer)
  "Applies the diff editing actions contained in DIFF-BUFFER to BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((orig-offset 0)
          (current-line 1))
      (cl-flet ((goto-orig-line (orig-line)
                  (let ((desired-line (+ orig-line orig-offset)))
                    (forward-line (- desired-line current-line))
                    (setq current-line desired-line)))
                (insert-lines (lines)
                  (dolist (line lines) (insert line))
                  (cl-incf current-line (length lines))
                  (cl-incf orig-offset (length lines)))
                (delete-lines (count)
                  (let ((start (point)))
                    (forward-line count)
                    (delete-region start (point)))
                  (cl-decf orig-offset count)))
        (save-excursion
          (with-current-buffer diff-buffer
            (goto-char (point-min))
            (while (not (eobp))
              (cl-multiple-value-bind (command orig-start orig-count formatted-count)
                  (bazel-parse-diff-action)
                (forward-line)
                (cl-flet ((fetch-lines ()
                            (cl-loop repeat formatted-count
                                     collect (let ((start (point)))
                                               (forward-line 1)
                                               ;; Return only the text after "< " or "> ".
                                               (substring (buffer-substring start (point)) 2)))))

                  (cond ((equal command "a")
                         (let ((lines (fetch-lines)))
                           (with-current-buffer buffer
                             (goto-orig-line (1+ orig-start))
                             (insert-lines lines))))
                        ((equal command "d")
                         (forward-line orig-count)
                         (with-current-buffer buffer
                           (goto-orig-line orig-start)
                           (delete-lines orig-count)))
                        ((equal command "c")
                         (forward-line (+ orig-count 1))
                         (let ((lines (fetch-lines)))
                           (with-current-buffer buffer
                             (goto-orig-line orig-start)
                             (delete-lines orig-count)
                             (insert-lines lines))))))))))))))

(defun bazel-format ()
  "Format the current buffer using buildifier."
  (interactive)
  (let ((input nil)
        (output nil)
        (errors nil))
    (unwind-protect
        (progn
          (setf input (make-temp-file "bazel-format-input-")
                output (get-buffer-create "*bazel-format-output*")
                errors (make-temp-file "bazel-format-errors-"))
          (write-region nil nil input nil 'no-write-message)
          (with-current-buffer output (erase-buffer))
          (let ((status
                 (call-process bazel-format-command nil `(,output ,errors) nil "-mode=diff" input)))
            (if (zerop status)
                (save-excursion (bazel-patch-buffer (current-buffer) output))
              (let ((errors-buffer (get-buffer-create "*BazelFormatErrors*"))
                    (coding-system-for-read "utf-8"))
                (message "Bazel format errors")
                (let ((file-name (file-name-nondirectory (buffer-file-name))))
                  (with-current-buffer errors-buffer
                    ;; A previously created compilation buffer will be read only.
                    (setq buffer-read-only nil)
                    (erase-buffer)
                    (insert-file-contents-literally errors)
                    (let ((regexp (rx-to-string `(sequence line-start (group ,input) ":"))))
                      (while (search-forward-regexp regexp nil t)
                        (replace-match file-name t t nil 1)))
                    (goto-char (point-min))
                    (compilation-mode))
                  (display-buffer errors-buffer))))))
      (when input (delete-file input))
      (when output (kill-buffer output))
      (when errors (delete-file errors)))))

(provide 'bazel-mode)
