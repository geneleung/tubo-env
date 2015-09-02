;;; emr-c++.el -- Brief introduction here.

;; Author: YangYingchao <yangyingchao@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.

;;; Commentary:

;;  Refactoring commands for C++.

;;; Code:

(require 'emr-cc)

(defun emr-cpp-try-catch (start end)
  "Format region (START/END) using clang."
  (interactive "rp")
  (kill-region start end)
  (let ((s (point))
        pos  end)
    (insert "try {\n")
    (yank)
    (insert
     "}\ncatch (exception& e) {\n")
    (setq pos (point))
    (insert "throw ;\n}\n")
    (setq e (point))
    (goto-char pos)
    (emr-cc-format-region s e)))

(emr-declare-command 'emr-cpp-try-catch
  :title "surround"
  :description "with try-catch"
  :modes '(c++-mode)
  :predicate (lambda ()
               (and mark-active (not (equal (mark) (point))))))

(provide 'emr-c++)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; emr-c++.el ends here
