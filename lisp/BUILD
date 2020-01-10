# Copyright 2020 Google LLC
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

sh_test(
    name = "bazel_mode_test",
    srcs = ["bazel-mode-test.sh"],
    data = [
        "BUILD",
        "bazel-mode.el",
        "bazel-mode-test.el",
    ],
)
