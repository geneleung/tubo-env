;;; emr-prog2.el -- Brief introduction here.

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

;; Refactoring commands common prog-mode.

;;; Code:
(require 'emr)
(require 's)
(require 'dash)


(defun emr-prog-fill-region (start end)
  "Fill region (START/END)."
  (interactive "rp")
  (fill-region start end))

(emr-declare-command 'emr-prog-fill-region
  :title "fill region"
  :description ""
  :modes 'prog-mode
  :predicate (lambda ()
               (and
                mark-active (not (equal (mark) (point)))
                (s-matches? (concat "^[[:space:]]*" comment-start)
                            (buffer-substring (region-beginning)
                                              (region-end))))))

(provide 'emr-prog2)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; emr-prog2.el ends here
