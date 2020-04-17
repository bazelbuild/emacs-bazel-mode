#!/bin/bash

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


# https://docs.bazel.build/versions/2.2.0/test-encyclopedia.html#initial-conditions.
declare -r dir="${TEST_SRCDIR:?}/${TEST_WORKSPACE:?}/lisp"

"${EMACS:-emacs}" --quick --batch \
  --directory="${dir:?}/" \
  --load="${dir:?}/bazel-mode-test.elc" \
  --funcall=ert-run-tests-batch-and-exit
