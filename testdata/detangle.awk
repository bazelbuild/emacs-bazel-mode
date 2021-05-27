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

# Replace an Org Babel code block with new code.  Requires the variables ‘out’
# and ‘src’ to be set.

BEGIN {
  in_block = 0
}

/^#\+begin_src .+ :tangle / && $4 == out {
  print
  while ((getline < src) == 1) print
  close(src)
  in_block = 1
  next
}

/^#\+end_src$/ {
  print
  in_block = 0
  next
}

in_block == 0 {
  print
}
