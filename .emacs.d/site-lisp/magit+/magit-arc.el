;;; magit-arc.el -- arc wrapper for magit.

;; Author: YangYingchao <yangyingchao@gmail.com>

;; Copyright (C) 2015 yangyingchao@gmail.com
;;
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
;;; magit-arc.el -- Brief introduction here.


;;; Commentary:
;; This package provides basic support for integrating arc with magit.
;;
;; When `magit-arc-mode' is turned on, the 'arc'
;;
;; To enable the mode globally without dropping to a shell:
;;
;;   (add-hook 'magit-mode-hook 'magit-arc-mode)

;;; Code:

(require 'magit)

(defcustom magit-arc-rev-db
  "Database of COMMIT-REVISION mapping.
We need a mapping between git commits and arc "
  :type 'string
  :group 'magit-arc
  )



(defvar magit-arc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "N") 'magit-arc-popup)
    map))

;;;###autoload
(define-minor-mode magit-arc-mode
  "Git-Svn support for Magit."
  :lighter magit-arc-mode-lighter
  :keymap  magit-arc-mode-map
  (unless (derived-mode-p 'magit-mode)
    (user-error "This mode only makes sense with Magit"))
  (cond
   (magit-arc-mode
    (magit-add-section-hook 'magit-status-sections-hook
                            'magit-insert-svn-unpulled
                            'magit-insert-unpulled-commits t t)
    (magit-add-section-hook 'magit-status-sections-hook
                            'magit-insert-svn-unpushed
                            'magit-insert-unpushed-commits t t)
    (magit-add-section-hook 'magit-status-headers-hook
                            'magit-insert-svn-remote nil t t))
   (t
    (remove-hook 'magit-status-sections-hook 'magit-insert-svn-unpulled t)
    (remove-hook 'magit-status-sections-hook 'magit-insert-svn-unpushed t)
    (remove-hook 'magit-status-headers-hook  'magit-insert-svn-remote t)))
  (when (called-interactively-p 'any)
    (magit-refresh)))

;;;###autoload
(custom-add-option 'magit-mode-hook #'magit-arc-mode)

(provide 'magit-arc)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; magit-arc.el ends here
