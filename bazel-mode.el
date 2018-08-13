;;; bazel-mode.el --- Emacs major mode for editing Bazel BUILD and WORKSPACE files

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

(defun bazel-mode--sort-strings-longest-first (l)
  "Sort strings by length and put the longest strings first.
L is the list of strings to sort."
  (sort l #'(lambda (s1 s2) (> (length s1) (length s2)))))

;; define several category of keywords
(defconst bazel-keywords
  '("for"))

(defconst cc-rules
  '("cc_binary" "cc_inc_library" "cc_library" "cc_proto_library" "cc_test"))

(defconst extra-actions-rules
  '("action_listener" "extra_action"))

;; See https://docs.bazel.build/versions/master/be/functions.html.
(defconst function-rules
  '("exports_files" "glob" "licenses" "load" "package" "package_group" "select"
	"workspace"))

(defconst general-rules
  '("alias" "config_setting" "filegroup" "genrule" "genquery" "test_suite"))

(defconst java-rules
  '("java_binary" "java_import" "java_library" "java_lite_proto_library"
	"java_plugin" "java_proto_library" "java_test" "java_runtime"
	"java_runtime_suite" "java_toolchain"))

(defconst platform-rules
  '("constraint_setting" "constraint_value" "platform"))

(defconst protobuf-rules
  '("proto_lang_toolchain" "proto_library"))

(defconst python-rules
  '("py_binary" "py_library" "py_proto_library" "py_test"))

(defconst workspace-rules
  '("bind" "git_repository" "http_archive" "http_file" "http_jar"
	"local_repository" "maven_jar" "maven_server" "new_git_repository"
	"new_http_archive" "new_local_repository" "xcode_config" "xcode_version"))

(defconst bazel-functions
  (bazel-mode--sort-strings-longest-first
   (append cc-rules extra-actions-rules function-rules general-rules java-rules
		   platform-rules protobuf-rules python-rules workspace-rules)))

;; arguments to functions
(defconst bind-args
  '("actual"))

(defconst cc-buildrule-args
  '("copts" "defines" "hdrs" "includes" "include_prefix" "linkopts" "linkshared"
	"linkstatic" "malloc" "nocopts" "strip_include_prefix" "textual_headers"))

(defconst common-buildrule-args
  '("deps" "data" "args" "defines" "stamp" "toolchains"))

(defconst common-testrule-args
  '("local" "flaky" "shard_count" "size" "timeout"))

(defconst config-setting-args
  '("values"))

(defconst genrule-args
  '("cmd" "compatible_with" "deprecation" "distribs" "executable" "features"
	"licenses" "message" "name" "output_licenses" "output_to_bindir" "outs"
	"restricted_to" "srcs" "tags" "testonly" "tools" "visibility"))

(defconst genquery-args
  '("deps" "data" "expression" "opts" "scope" "strict"))

(defconst git-repository-args
  '("commit" "init_submodules" "remote" "sha256"))

(defconst glob-args
  '("exclude" "exclude_directories" "include"))

(defconst http-args
  '("executable" "strip_prefix" "type" "url" "urls"))

(defconst java-buildrule-args
  '("classpath_resources" "create_executable" "deploy_manifest_lines"
	"javacopts" "jvm_flags" "launcher" "main_class" "output_licenses" "plugins"
	"resources" "resource_jars" "resource_strip_prefix" "restricted_to"
	"runtime_deps" "use_testrunner"))

(defconst local-repository-args
  '("path"))

(defconst maven-args
  '("artifact" "repository" "server" "sha1"))

(defconst new-repository-args
  '("build_file" "build_file_content" "workspace_file" "workspace_file_content"))

(defconst package-args
  '("default_deprecation" "default_testonly" "default_visibility"))

(defconst py-buildrule-args
  '("default_python_version" "default_runtime" "files" "imports" "interpreter"
    "interpreter_path" "main" "protoc" "srcs_version"))

(defconst bazel-types
  (bazel-mode--sort-strings-longest-first
    (append bind-args cc-buildrule-args common-buildrule-args common-testrule-args
	  config-setting-args genrule-args genquery-args git-repository-args glob-args
	  http-args java-buildrule-args local-repository-args maven-args
	  new-repository-args package-args py-buildrule-args)))

(defconst bazel-constants
  '("true"))

(defconst bazel-events
  '())

;; generate regex string for each category of keywords
(setq bazel-keywords-regexp (regexp-opt bazel-keywords 'words))
(setq bazel-functions-regexp (regexp-opt bazel-functions 'words))
(setq bazel-type-regexp (regexp-opt bazel-types 'words))
(setq bazel-constant-regexp (regexp-opt bazel-constants 'words))
(setq bazel-event-regexp (regexp-opt bazel-events 'words))

(defvar bazel-font-lock-keywords
      `(
        (,bazel-keywords-regexp . font-lock-keyword-face)
        (,bazel-functions-regexp . font-lock-function-name-face)
        (,bazel-type-regexp . font-lock-type-face)
        (,bazel-constant-regexp . font-lock-constant-face)
        (,bazel-event-regexp . font-lock-builtin-face)
        ;; note: order above matters. in general, longer words first.
        ))

(defvar bazel-mode-syntax-table
  (let ((table (make-syntax-table)))
	;; comments start with '#' and end with line break
	(modify-syntax-entry ?# "<" table)
	(modify-syntax-entry ?\n ">" table)
	table))

;;;###autoload
(define-derived-mode bazel-mode prog-mode "Bazel"
  "Major mode for editing Bazel BUILD and WORKSPACE files"
  (setq font-lock-defaults '((bazel-font-lock-keywords))))

(provide 'bazel-mode)

;;; bazel-mode.el ends here
