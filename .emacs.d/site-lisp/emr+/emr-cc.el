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

(defvar emr-cc-surround-var nil "Nil.")

(defun emr-cc-surround-if-end (start end)
  "Format region (START/END) using clang."
  (interactive "rp")
  (let ((var (completing-read "Variable Name: " emr-cc-surround-var
                              nil nil nil 'emr-cc-surround-var)))
    (kill-region start end)
    (let ((s (point))
          pos e)
      (insert (format "#ifdef %s\n" var))
      (yank)
      (insert (format "\n#endif /*%s*/" var))
      (setq pos (point))
      (setq e (point))
      (goto-char pos)
      (emr-cc-format-region s e))))

(emr-declare-command 'emr-cc-surround-if-end
  :title "surround"
  :description "with if-endif"
  :modes '(c++-mode c-mode)
  :predicate (lambda ()
               (and mark-active (not (equal (mark) (point))))))
(provide 'emr-cc)

;;; emr-cc.el ends here
