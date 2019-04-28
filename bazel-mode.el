;;; bazel-mode.el --- Emacs major mode for editing Bazel BUILD and WORKSPACE files -*- lexical-binding: t; -*-


;; Copyright (C) 2018 Google LLC
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;      http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;
;;; Commentary:
;;
;; This package provides Emacs bazel-mode, a major mode for editing Bazel
;; BUILD and WORKSPACE files.
;;
;;; Code:

(defgroup bazel-mode nil
  "Major mode for Bazel BUILD files."
  :link '(url-link "https://github.com/bazelbuild/emacs-bazel-mode")
  :group 'languages)

(defcustom bazel-mode--buildifier-cmd "buildifier"
  "Filename of buildifier executable."
  :type 'file
  :group 'bazel-mode)

(defun buildifier ()
  "Format current buffer using buildifier."
  (interactive "*")
  (let ((build-file-contents (buffer-string))
        (input-buffer (current-buffer))
        (buildifier-buffer (get-buffer-create "*buildifier*")))
    (with-current-buffer buildifier-buffer
      (erase-buffer)
      (insert build-file-contents)
      (let ((return-code
             (call-process-region
              (point-min) (point-max) buildifier-cmd t t nil "--type=build")))
        (unwind-protect
          (if (eq return-code 0)
              (progn
                (set-buffer input-buffer)
                (replace-buffer-contents buildifier-buffer)
                (kill-buffer buildifier-buffer))
              (set-buffer-modified-p nil)
              (display-buffer buildifier-buffer)))))))

(defconst bazel-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; single line comment start
    (modify-syntax-entry ?# "<" table)
    ;; single line comment end
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for `bazel-mode'.")

;;;###autoload
(define-derived-mode bazel-mode prog-mode "Bazel"
  "Major mode for editing Bazel BUILD and WORKSPACE files."
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+")
  (setq-local comment-end "")
  (setq-local comment-use-syntax t)
  (setq-local font-lock-defaults '(nil)))

(provide 'bazel-mode)

;;; bazel-mode.el ends here
