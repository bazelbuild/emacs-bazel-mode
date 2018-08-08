(defun sort-strings-longest-first (l)
  (sort l #'(lambda (s1 s2) (> (length s1) (length s2)))))

;; define several category of keywords
(setq bazel-keywords
  '("for"))

(setq android-rules
  '("android_binary" "aar_import" "android_library" "android_device"
	"android_ndk_repository" "android_sdk_repository"))

(setq apple-rules
  '("apple_genrule" "swift_library"))

(setq cc-rules
  '("cc_binary" "cc_inc_library" "cc_library" "cc_proto_library" "cc_test"))

(setq closure-rules
  '("closure_css_binary" "closure_css_library" "closure_java_template_library"
	"closure_js_binary" "closure_js_deps" "closure_js_library"
	"closure_js_proto_library" "closure_js_template_library" "closure_js_test"
	"closure_py_template_library" "phantomjs_test"))

(setq d-rules
  '("d_library" "d_source_library" "d_binary" "d_test" "d_docs"))

(setq docker-rules
  '("docker_build" "docker_bundle" "docker_pull" "docker_push"))

(setq dotnet-rules
  '("csharp_binary" "csharp_library" "csharp_nunit_test" "dll_import"
	"new_nuget_package" "nuget_package"))

(setq extra-actions-rules
  '("action_listener" "extra_action"))

(setq function-rules
  '("exports_files" "glob" "licenses" "load" "package" "package_group" "select"
	"workspace"))

(setq general-rules
  '("alias" "config_setting" "filegroup" "genrule" "genquery" "test_suite"))

(setq go-rules
  '("cgo_library" "go_binary" "go_embed_data" "go_library" "go_prefix"
	"go_proto_library" "go_repositories" "go_repository" "go_test"))

(setq groovy-rules
  '("groovy_and_java_library" "groovy_binary" "groovy_junit_test"
	"groovy_library" "spock_test"))

(setq java-appengine-rules
  '("appengine_war" "java_war" "py_appengine_binary" "py_appengine_test"))

(setq java-rules
  '("java_binary" "java_import" "java_library" "java_lite_proto_library"
	"java_plugin" "java_proto_library" "java_test" "java_runtime"
	"java_runtime_suite" "java_toolchain"))

(setq jsonnet-rules
  '("jsonnet_library" "jsonnet_repositories" "jsonnet_to_json"
	"jsonnet_to_json_test"))

(setq package-rules
  '("pkg_deb" "pkg_rpm" "pkg_tar"))

(setq objc-rules
  '("apple_binary" "apple_static_library" "apple_watch1_extension"
	"apple_watch2_extension" "apple_watch_extension_binary" "ios_application"
	"ios_device" "ios_extension" "ios_extension_binary" "ios_test"
	"j2objc_library" "objc_binary" "objc_bundle" "objc_bundle_library"
	"objc_framework" "objc_import" "objc_library" "objc_proto_library"))

;; github.com/google/subpar
(setq subpar-rules
  '("par_binary" "parfile"))

(setq perl-rules
  '("perl_binary" "perl_library" "perl_test"))

(setq platform-rules
  '("constraint_setting" "constraint_value" "platform"))

(setq protobuf-rules
  '("proto_lang_toolchain" "proto_library"))

(setq python-rules
  '("py_binary" "py_library" "py_proto_library" "py_test"))

(setq rust-rules
  '("rust_bench_test" "rust_binary"  "rust_doc" "rust_doc_test" "rust_library"
	"rust_repositories" "rust_test"))

(setq sass-rules
  '("sass_binary" "sass_library"))

(setq scala-rules
  '("scala_binary" "scala_library" "scala_macro_library" "scala_test"))

(setq shell-rules
  '("sh_binary" "sh_library" "sh_test"))

(setq typescript-rules
  '("ts_library"))

(setq webtesting-rules
  '("go_web_test_suite" "java_web_test_suite" "py_web_test_suite" "web_test"
	"web_test_repositories" "web_test_suite"))

(setq workspace-rules
  '("bind" "git_repository" "http_archive" "http_file" "http_jar"
	"local_repository" "maven_jar" "maven_server" "new_git_repository"
	"new_http_archive" "new_local_repository" "xcode_config" "xcode_version"))

(setq bazel-functions
  (sort-strings-longest-first
   (append apple-rules android-rules cc-rules closure-rules d-rules docker-rules
		   dotnet-rules extra-actions-rules function-rules general-rules go-rules
		   groovy-rules java-appengine-rules java-rules jsonnet-rules objc-rules
		   perl-rules platform-rules protobuf-rules python-rules rust-rules sass-rules
		   shell-rules subpar-rules typescript-rules webtesting-rules
		   workspace-rules)))

;; arguments to functions
(setq bind-args
  '("actual"))

(setq cc-buildrule-args
  '("copts" "defines" "hdrs" "includes" "include_prefix" "linkopts" "linkshared"
	"linkstatic" "malloc" "nocopts" "strip_include_prefix" "textual_headers"))

(setq common-buildrule-args
  '("deps" "data" "args" "defines" "stamp" "toolchains"))

(setq common-testrule-args
  '("local" "flaky" "shard_count" "size" "timeout"))

(setq config-setting-args
  '("values"))

(setq genrule-args
  '("cmd" "compatible_with" "deprecation" "distribs" "executable" "features"
	"licenses" "message" "name" "output_licenses" "output_to_bindir" "outs"
	"restricted_to" "srcs" "tags" "testonly" "tools" "visibility"))

(setq genquery-args
  '("deps" "data" "expression" "opts" "scope" "strict"))

(setq git-repository-args
  '("commit" "init_submodules" "remote" "sha256"))

(setq glob-args
  '("exclude" "exclude_directories" "include"))

(setq go-repositories-args
  '("go_version"))

(setq go-repository-args
  '("build_file_generation" "build_file_name" "build_tags" "importpath"
	"go-prefix" "tag" "vcs"))

(setq http-args
  '("executable" "strip_prefix" "type" "url" "urls"))

(setq java-buildrule-args
  '("classpath_resources" "create_executable" "deploy_manifest_lines"
	"javacopts" "jvm_flags" "launcher" "main_class" "output_licenses" "plugins"
	"resources" "resource_jars" "resource_strip_prefix" "restricted_to"
	"runtime_deps" "use_testrunner"))

(setq local-repository-args
  '("path"))

(setq maven-args
  '("artifact" "repository" "server" "sha1"))

(setq new-repository-args
  '("build_file" "build_file_content" "workspace_file" "workspace_file_content"))

(setq package-args
  '("default_deprecation" "default_testonly" "default_visibility"))

(setq py-buildrule-args
  '("default_python_version" "default_runtime" "files" "imports" "interpreter"
    "interpreter_path" "main" "protoc" "srcs_version"))

(setq bazel-types
  (sort-strings-longest-first
    (append bind-args cc-buildrule-args common-buildrule-args common-testrule-args
	  config-setting-args genrule-args genquery-args git-repository-args glob-args
	  go-repositories-args go-repository-args http-args java-buildrule-args
	  local-repository-args maven-args new-repository-args package-args
	  py-buildrule-args)))

(setq bazel-constants
  '("true"))

(setq bazel-events
  '())

;; generate regex string for each category of keywords
(setq bazel-keywords-regexp (regexp-opt bazel-keywords 'words))
(setq bazel-functions-regexp (regexp-opt bazel-functions 'words))
(setq bazel-type-regexp (regexp-opt bazel-types 'words))
(setq bazel-constant-regexp (regexp-opt bazel-constants 'words))
(setq bazel-event-regexp (regexp-opt bazel-events 'words))

;; create the list for font-lock.
;; each category of keyword is given a particular face
(setq bazel-font-lock-keywords
      `(
        (,bazel-keywords-regexp . font-lock-keyword-face)
        (,bazel-functions-regexp . font-lock-function-name-face)
        (,bazel-type-regexp . font-lock-type-face)
        (,bazel-constant-regexp . font-lock-constant-face)
        (,bazel-event-regexp . font-lock-builtin-face)
        ;; note: order above matters, because once colored, that part won't change.
        ;; in general, longer words first
        ))

(setq bazel-mode-syntax-table
  (let ((table (make-syntax-table)))
	;; comments start with '#' and end with line break
	(modify-syntax-entry ?# "<" table)
	(modify-syntax-entry ?\n ">" table)
	table))

;;;###autoload
(define-derived-mode bazel-mode prog-mode "Bazel"
  "Major mode for editing Bazel BUILD and WORKSPACE files"

  ;; code for syntax highlighting
  (setq font-lock-defaults '((bazel-font-lock-keywords)))

  (set-syntax-table bazel-mode-syntax-table))

;; add the mode to the `features' list
(provide 'bazel-mode)

;;; bazel-mode.el ends here
