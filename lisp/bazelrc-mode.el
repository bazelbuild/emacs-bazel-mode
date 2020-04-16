;;; bazelrc-mode.el --- major mode for .bazelrc files  -*- lexical-binding: t; -*-

;; URL: https://github.com/bazelbuild/emacs-bazel-mode
;; Keywords: build tools, languages
;; Package-Requires: ((emacs "26.1"))
;; Version: 0

;; Copyright 2020 Google LLC
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     https://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:

;; This package provides ‘bazelrc-mode’, a major mode to edit .bazelrc files.
;; See https://docs.bazel.build/versions/3.0.0/guide.html#bazelrc.

;;; Code:

(require 'conf-mode)
(require 'derived)
(require 'ffap)
(require 'font-lock)
(require 'pcase)
(require 'rx)
(require 'subr-x)

(require 'bazel-util)

;;;###autoload
(define-derived-mode bazelrc-mode conf-space-mode "bazelrc"
  "Major mode for editing .bazelrc files.")

;;;###autoload
(add-to-list 'auto-mode-alist
             ;; https://docs.bazel.build/versions/3.0.0/guide.html#where-are-the-bazelrc-files
             (cons (rx ?/ (or "bazel.bazelrc" ".bazelrc") eos) #'bazelrc-mode))

(add-to-list 'ffap-string-at-point-mode-alist
             ;; By default the percent sign isn’t included in the list of
             ;; allowed filename characters, so this would miss %workspace%
             ;; names.
             (list #'bazelrc-mode "-_/.%[:alnum:]" "" ""))

(add-to-list 'ffap-alist (cons #'bazelrc-mode #'bazelrc-ffap))

(defun bazelrc-ffap (name)
  "Function for ‘ffap-alist’ in ‘bazelrc-mode’.
Look for an imported file with the given NAME."
  ;; https://docs.bazel.build/versions/3.0.0/guide.html#imports
  (pcase name
    ((rx bos "%workspace%" (+ ?/) (let rest (+ nonl)))
     (when buffer-file-name
       (when-let ((workspace (bazel-util-workspace-root buffer-file-name)))
         (let ((file-name (expand-file-name rest workspace)))
           (and (file-exists-p file-name) file-name)))))))

(font-lock-add-keywords
 #'bazelrc-mode
 ;; https://docs.bazel.build/versions/3.0.0/guide.html#imports
 (list (rx symbol-start (or "import" "try-import") symbol-end)))

(provide 'bazelrc-mode)
;;; bazelrc-mode.el ends here
