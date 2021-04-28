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
    sha256 = "94724c276d49f99fefcd87ebad797ddcd004e92a64097f7d445e5a640bcb770f",
    strip_prefix = "rules_elisp-c6453f05328f8938c49fc49b17fbb8a17b0e64a6",
    urls = [
        "https://github.com/phst/rules_elisp/archive/c6453f05328f8938c49fc49b17fbb8a17b0e64a6.zip",  # 2021-04-24
    ],
)

load(
    "@phst_rules_elisp//elisp:repositories.bzl",
    "rules_elisp_dependencies",
    "rules_elisp_toolchains",
)

rules_elisp_dependencies()

rules_elisp_toolchains()
