# Copyright 2021 Google LLC
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

SHELL := /bin/sh

.DEFAULT: all
.SUFFIXES:

BAZEL := bazel
BAZELFLAGS :=

# All potentially supported Emacs versions.
versions := 26.1 26.2 26.3 27.1 27.2

kernel := $(shell uname -s)
ifeq ($(kernel),Linux)
  # GNU/Linux supports all Emacs versions.
else ifeq ($(kernel),Darwin)
  # macOS only supports Emacs 27.
  versions := $(filter-out 26.%,$(versions))
  ifneq ($(shell uname -m),x86_64)
    # Apple Silicon doesn’t support Emacs 27.1.
    versions := $(filter-out 27.1,$(versions))
  endif
else
  $(error Unsupported kernel $(kernel))
endif

# Test both default toolchain and versioned toolchains.
all: check $(versions)

check:
	$(BAZEL) test --test_output=errors $(BAZELFLAGS) -- //...

$(versions):
	$(MAKE) check BAZELFLAGS='$(BAZELFLAGS) --extra_toolchains=@phst_rules_elisp//elisp:emacs_$@_toolchain'

.PHONY: all check $(versions)
