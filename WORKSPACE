# Copyright 2020, 2021 Google LLC
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
    sha256 = "5fb9675d27ffa99dfba759c0bf646a27bbee7c69845e2efa364e68dfe8fbb576",
    strip_prefix = "rules_elisp-3573c8a02da33284c36b4388cbc2d2d3feac51d5",
    urls = [
        "https://github.com/phst/rules_elisp/archive/3573c8a02da33284c36b4388cbc2d2d3feac51d5.zip",  # 2021-05-15
    ],
)

load(
    "@phst_rules_elisp//elisp:repositories.bzl",
    "rules_elisp_dependencies",
    "rules_elisp_toolchains",
)

rules_elisp_dependencies()

rules_elisp_toolchains()
