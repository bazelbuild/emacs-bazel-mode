# Copyright 2020, 2021, 2023 Google LLC
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     https://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Note: all files starting with "bazel-" have to be in a subdirectory due to
# https://github.com/bazelbuild/bazel/issues/10560.

load("@phst_rules_elisp//elisp:defs.bzl", "elisp_library", "elisp_test")

package(
    default_applicable_licenses = ["//:license"],
    default_visibility = ["//visibility:private"],
)

licenses(["notice"])

elisp_library(
    name = "bazel_build",
    srcs = ["bazel-build.el"],
    load_path = ["."],
    deps = [
        ":bazel_util",
        "//:bazel",
    ],
)

elisp_library(
    name = "bazel_mode",
    srcs = ["bazel-mode.el"],
    load_path = ["."],
    deps = [
        ":bazel_util",
        "//:bazel",
    ],
)

elisp_library(
    name = "bazelrc_mode",
    srcs = ["bazelrc-mode.el"],
    load_path = ["."],
    deps = [
        ":bazel_util",
        "//:bazel",
    ],
)

elisp_library(
    name = "bazel_util",
    srcs = ["bazel-util.el"],
    load_path = ["."],
    deps = ["//:bazel"],
)
