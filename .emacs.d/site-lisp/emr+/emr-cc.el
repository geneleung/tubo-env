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

(defcustom emr-clang-format-style 'Google
  "Style used to format codes with clang.
Refer to http://clang.llvm.org/docs/ClangFormatStyleOptions.html for more
detailed descriptions."
  :type '(radio (const :tag "Format with style suggested by Google." Google)
                (const :tag "Format used by LLVM project." LLVM)
                (const :tag "Format used by Chromium project." Chromium)
                (const :tag "Format used by Mozilla project." Mozilla)
                (const :tag "Format used by Webkit project." WebKit)
                (const :tag "Load style configuration from file." file)
                (repeat :tag "Customized alist." (cons (regexp :tag "Tag")
                               (directory :tag "Format"))))

  :group 'emr)

;;; EMR Declarations

(autoload 'clang-format-region "clang-format" ""  t)
(autoload 'clang-format-buffer "clang-format" ""  t)

(defun emr-cc-get-style ()
  "Return style as a string."
  (cond
   ((stringp emr-clang-format-style) emr-clang-format-style)
   ((listp emr-clang-format-style)
    (concat "{"(mapconcat (lambda (x)
                            (format "%s: %s" (car x) (cdr x)))
                          emr-clang-format-style ", ") "}"))
   ((symbolp emr-clang-format-style) (symbol-name emr-clang-format-style))
   (t nil)))

(defun emr-cc-format-region (start end)
  "Format region (START/END) using clang."
  (interactive "rp")
  (clang-format-region start end (emr-cc-get-style)))

(defun emr-cc-format-buffer ()
  "Format region (START/END) using clang."
  (interactive)
  (clang-format-buffer (emr-cc-get-style)))

(defalias 'emr-cc-tidy-includes 'emr-c-tidy-includes)
(emr-declare-command 'emr-cc-tidy-includes
  :title "tidy"
  :description "includes"
  :modes '(c++-mode c-mode)
  :predicate (lambda ()
               (emr-c:looking-at-include?)))

(emr-declare-command 'emr-cc-format-region
  :title "format region"
  :description "with clang"
  :modes '(c-mode c++-mode)
  :predicate (lambda ()
               (and mark-active (not (equal (mark) (point)))
                    (executable-find "clang-format"))))

(emr-declare-command 'emr-cc-format-buffer
  :title "format buffer"
  :description "with clang"
  :modes '(c-mode c++-mode)
  :predicate (lambda ()
               (and (not mark-active)
                    (executable-find "clang-format"))))


(defvar emr-cc-surround-var nil "Nil.")

(defun emr-cc-surround-if-end (start end)
  "Format region (START/END) using clang."
  (interactive "rp")
  (let ((var (completing-read "Variable Name: " emr-cc-surround-var
                              nil nil nil 'emr-cc-surround-var)))
    (kill-region start end)
    (let ((s (point))
          pos e)
      (insert (format "#if %s\n" var))
      (yank)
      (insert (format "\n#endif /*%s*/" var))
      (setq pos (point))
      (setq e (point))
      (goto-char pos)
      (emr-cc-format-region s e))))

(emr-declare-command 'emr-cc-surround-if-end
  :title "surround"
  :description "with if-endif"
  :modes '(c++-mode)
  :predicate (lambda ()
               (and mark-active (not (equal (mark) (point))))))
(provide 'emr-cc)

;;; emr-cc.el ends here
