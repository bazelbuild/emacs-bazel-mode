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
    sha256 = "ca04b2dc7dc345d9746c6dc723879fb4a110e62dc17ebe7a4bbc982d76e60bc2",
    strip_prefix = "rules_elisp-4482e8e3fe4dc5b93aad597342d70ef4762f93f0",
    urls = [
        "https://github.com/phst/rules_elisp/archive/4482e8e3fe4dc5b93aad597342d70ef4762f93f0.zip",  # 2022-11-28
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
