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


;;; Options.

(defgroup magit-arc nil
  "arc support for Magit."
  :group 'magit-extensions)

(defcustom magit-arc-rev-db (expand-file-name "~/.emacs.d/magit-arc-db")
  "Database of COMMIT-REVISION mapping.
We need a mapping between git commits and arc so we can amend commit message
or close revision.  For now, this mapping will be saved as alist.  There might
be a better solution, for example, to retrieve it from server..."
  :type 'string
  :group 'magit-arc
  )

(defcustom magit-arc-executable  "arc"
  "The Git executable used by Magit."
  :group 'magit-arc
  :type 'string)

(defcustom magit-arc-global-arguments nil
  "Global git arguments.

The arguments set here are used every time the arc executable is
run as a subprocess."
  :group 'magit-arc
  :type '(repeat string))

(defvar magit-arc-rev-alist nil
  "AList of commit-rev mapping.
It will be loaded from and store into `magit-arc-rev-db'.")

(setq magit-arc-rev-alist nil)
(setq-default magit-arc-rev-db "/tmp/test-arc.db") //XXX: test only

;;; Utilities
(defun magit-arc-run (&rest args)
  "Call arc synchronously in a separate process, and refresh.

Option `magit-arc-executable' specifies the Git executable and
option `magit-arc-global-arguments' specifies constant arguments.
The arguments ARGS specify arguments to Git, they are flattened
before use.

After Git returns, the current buffer (if it is a Magit buffer)
as well as the current repository's status buffer are refreshed.
Unmodified buffers visiting files that are tracked in the current
repository are reverted if `magit-revert-buffers' is non-nil.

Process output goes into a new section in a buffer specified by
variable `magit-process-buffer-name-format'."
  (print args)
  (magit-call-process magit-arc-executable args)
  (magit-refresh))

;;; Commands

(magit-define-popup magit-arc-popup
  "Popup console for arc commands."
  'magit
  :man-page "git-arc"
  :actions  '(
              (?a "Amend & Close"   magit-arc-amend-close)
              (?s "Send Review"     magit-arc-fetch)))

(defun magit-arc-commit-to-revision (commit)
  "Find proper revision based on COMMIT."
  (unless magit-arc-rev-alist
    (condition-case error
        (with-temp-buffer
          (insert-file-contents magit-arc-rev-db)
          (goto-char (point-min))
          (setq magit-arc-rev-alist (read (current-buffer))))
      (error (message "Failed to load database from: %s" magit-arc-rev-db))))
  (cdr (assoc commit magit-arc-rev-alist)))

;;;###autoload
(defun magit-arc-amend-close (&optional args)
  "Amend and close commit message."
  (interactive)
  (let* ((commit (magit-commit-at-point))
         (revision (magit-arc-commit-to-revision commit)))
    (if (not revision)
        (error "Can't find revision for commit: %s" commit))
    (magit-arc-run "amend" "--revision" revision)
    (magit-arc-run "close-revision" revision)
    (message "Amend finished.")
    ))

(defvar magit-arc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "X") 'magit-arc-popup)
    map))

;;;###autoload
(define-minor-mode magit-arc-mode
  "Git-Svn support for Magit."
  :lighter magit-arc-mode-lighter
  :keymap  magit-arc-mode-map
  (unless (derived-mode-p 'magit-mode)
    (user-error "This mode only makes sense with Magit"))
  ;; (cond
  ;;  (magit-arc-mode
  ;;   (magit-add-section-hook 'magit-status-sections-hook
  ;;                           'magit-insert-svn-unpulled
  ;;                           'magit-insert-unpulled-commits t t)
  ;;   (magit-add-section-hook 'magit-status-sections-hook
  ;;                           'magit-insert-svn-unpushed
  ;;                           'magit-insert-unpushed-commits t t)
  ;;   (magit-add-section-hook 'magit-status-headers-hook
  ;;                           'magit-insert-svn-remote nil t t))
  ;;  (t
  ;;   (remove-hook 'magit-status-sections-hook 'magit-insert-svn-unpulled t)
  ;;   (remove-hook 'magit-status-sections-hook 'magit-insert-svn-unpushed t)
  ;;   (remove-hook 'magit-status-headers-hook  'magit-insert-svn-remote t)))
  (when (called-interactively-p 'any)
    (magit-refresh)))

;;;###autoload
(custom-add-option 'magit-mode-hook #'magit-arc-mode)

(easy-menu-define magit-arc-mode-menu nil "Magit-Arc mode menu"
  '("Git-arc"
    :visible magit-arc-mode
    ;; :active (lambda () (magit-get "svn-remote" "svn" "fetch"))
    ["Amend"         magit-arc-amend-close]
    ["Send Review"   magit-arc-send-review]
    ["Close"         magit-arc-close]
    ))

(provide 'magit-arc)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; magit-arc.el ends here
