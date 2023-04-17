# Bazel support for GNU Emacs

This repository provides support for [Bazel][] in [GNU Emacs][].  It consists of
a single file, `bazel.el`, which only depends on GNU Emacs and not on other
libraries.  You can install it by dropping the file somewhere in your
`load-path` and byte-compiling it.

The library provides major modes for editing Bazel [BUILD files][], [WORKSPACE
files][], [.bazelrc files][], as well as [Starlark files][].  It also provides
commands to [run Bazel commands][] and integration with core GNU Emacs
infrastructure like [compilation][] and [xref][].  See the [manual][] for
details.

[Bazel]: https://bazel.build/
[GNU Emacs]: https://www.gnu.org/software/emacs/
[BUILD files]: https://bazel.build/concepts/build-files
[WORKSPACE files]: https://bazel.build/concepts/build-ref#workspace
[.bazelrc files]: https://bazel.build/run/bazelrc
[Starlark files]: https://bazel.build/rules/language
[run Bazel commands]: https://bazel.build/run/build
[compilation]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Compilation.html
[xref]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Xref.html
[manual]: manual.org
