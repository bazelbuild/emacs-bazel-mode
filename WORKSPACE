# Copyright 2020, 2021, 2022, 2023 Google LLC
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

workspace(name = "bazelbuild_emacs_bazel_mode")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

http_archive(
    name = "phst_rules_elisp",
    sha256 = "780aaf8dd02f8eaf918145a5eaa80a3c841b04017eafd14ce948e4e91cce4208",
    strip_prefix = "rules_elisp-d6eb0adc9f4f16546e7e57818f4d0bea422d2b4b",
    urls = [
        "https://github.com/phst/rules_elisp/archive/d6eb0adc9f4f16546e7e57818f4d0bea422d2b4b.zip",  # 2023-08-04
    ],
)

load(
    "@phst_rules_elisp//elisp:repositories.bzl",
    "rules_elisp_dependencies",
    "rules_elisp_toolchains",
)

rules_elisp_dependencies()

rules_elisp_toolchains()

http_archive(
    name = "rules_license",
    sha256 = "6157e1e68378532d0241ecd15d3c45f6e5cfd98fc10846045509fb2a7cc9e381",
    urls = ["https://github.com/bazelbuild/rules_license/releases/download/0.0.4/rules_license-0.0.4.tar.gz"],
)

load("@rules_license//:deps.bzl", "rules_license_dependencies")

rules_license_dependencies()
