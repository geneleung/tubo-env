;;; emr-cc.el --- Refactoring commands for C  -*- lexical-binding: t; -*-

;; Copyright (C) 2013 Chris Barrett

;; Author: Chris Barrett <chris.d.barrett@me.com>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Refactoring commands for C.

;;; Code:

(require 'emr)
(require 's)
(require 'dash)
(require 'thingatpt)
(autoload 'ido-completing-read "ido")
(autoload 'c-mode-map "cc-mode")

; ------------------

(defcustom emr-clang-format-style 'Google
  "Style used to format codes with clang.
Refer to http://clang.llvm.org/docs/ClangFormatStyleOptions.html for more
detailed descriptions."
  :type '(radio (const :tag "Format with style suggested by Google." Google)
                (const :tag "Format used by LLVM project." LLVM)
                (const :tag "Format used by Chromium project." Chromium)
                (const :tag "Format used by Mozilla project." Mozilla)
                (const :tag "Format used by Webkit project." WebKit)
                (const :tag "load style configuration from file." file))
  :group 'emr)


(defun emr-cc:bob-after-comments ()
  "Move to the first non-comment character in the buffer."
  (goto-char (point-min))
  (while (emr-looking-at-comment?)
    (forward-line 1))
  (point))

;;; EMR Declarations

(autoload 'clang-format-region "clang-format" ""  t)

(defun emr-cc-format-region (start end)
  "Foramt region using clang."
  (interactive "rp")
  (clang-format-region start end (symbol-name emr-clang-format-style)))

(emr-declare-command 'emr-c-tidy-includes
  :title "tidy"
  :description "includes"
  :modes 'c++-mode
  :predicate (lambda ()
               (emr-c:looking-at-include?)))

(emr-declare-command 'emr-cc-format-region
  :title "Format region"
  :description "using clang"
  :modes '(prog-mode)
  :predicate (lambda ()
               (and mark-active (not (equal (mark) (point)))
                    (executable-find "clang-format"))))

(provide 'emr-cc)

;;; emr-cc.el ends here
