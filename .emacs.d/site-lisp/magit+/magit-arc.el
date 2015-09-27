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


(require 'pp)
(require 'magit-mode)
(require 'magit)


;;; Options.

(defgroup magit-arc nil
  "arc support for Magit."
  :group 'magit-extensions)

(defcustom magit-arc-db (expand-file-name "~/.emacs.d/magit-arc-db")
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
It will be loaded from and store into `magit-arc-db'.")

;;XXX: test only
(setq magit-arc-rev-alist nil)
(setq-default magit-arc-db "/tmp/test-arc.db")
(setq-default magit-arc-executable (or (executable-find "arc") (executable-find "echo")))

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
  (apply 'magit-call-process magit-arc-executable args)
  (magit-refresh))

;;; Commands

(magit-define-popup magit-arc-popup
  "Popup console for arc commands."
  'magit
  :man-page "git-arc"
  :actions  '(
              (?a "Amend & Close"   magit-arc-amend-close)
              (?s "Send Review"     magit-arc-send-review)))

(defvar magit-arc-send-arguments nil "Nil.")

(defvar magit-arc-send-popup
  '(:variable magit-arc-send-arguments
    ;; :switches ((?g "Show graph"              "--graph")
    ;;            (?c "Show graph in color"     "--color")
    ;;            (?d "Show refnames"           "--decorate")
    ;;            (?S "Show signatures"         "--show-signature")
    ;;            (?u "Show diffs"              "--patch")
    ;;            (?s "Show diffstats"          "--stat")
    ;;            (?D "Simplify by decoration"  "--simplify-by-decoration")
    ;;            (?f "Follow renames when showing single-file log" "--follow"))
    :options  ((?e "Set Encoding"          "--encoding" read-from-minibuffer)
               (?r "Set Reviewers"         "--reviewers="  read-from-minibuffer)
               (?m "Force Update"          "--update="    read-from-minibuffer)
               (?p "Search patches"          "-G"         read-from-minibuffer))
    :actions  ((?l "Log current"             magit-log-current)
               (?L "Log local branches"      magit-log-branches)
               (?r "Reflog current"          magit-reflog-current)
               (?o "Log other"               magit-log)
               (?b "Log all branches"        magit-log-all-branches)
               (?O "Reflog other"            magit-reflog)
               (?h "Log HEAD"                magit-log-head)
               (?a "Log all references"      magit-log-all)
               (?H "Reflog HEAD"             magit-reflog-head))
    :default-action magit-log-current
    :max-action-columns 3))

(defun magit-arc-commit-to-revision (commit)
  "Find proper revision based on COMMIT."
  (unless magit-arc-rev-alist
    (condition-case error
        (with-temp-buffer
          (insert-file-contents magit-arc-db)
          (goto-char (point-min))
          (setq magit-arc-rev-alist (read (current-buffer))))
      (error (message "Failed to load database from: %s" magit-arc-db))))
  (cdr (assoc commit magit-arc-rev-alist)))

(defun magit-arc-db-remove-commit (commit &optional without-io)
  "Remove COMMIT from db.
If WITHOUT-IO is specified, database will not be written back to disk."
  (setq magit-arc-rev-alist (assq-delete-all commit magit-arc-rev-alist))
  (unless without-io (magit-arc-db-write)))

(defun magit-arc-db-add-commit (commit revision &optional without-io)
  "Add mapping of COMMIT & REVISION.
Dump this mapping into database If WITHOUT-IO is not specified."
  (push (cons commit revision) magit-arc-rev-alist)
  (unless without-io (magit-arc-db-write)))

(defun magit-arc-db-write ()
  "Write `magit-arc-rev-alist' into database."
    (with-temp-file magit-arc-db
      (insert "(")
      (dolist (i magit-arc-rev-alist) (pp i (current-buffer)))
      (insert ")")))

(defun magit-arc-get-commit ()
  "Find COMMIT at cursor as symbol."
  (let ((commit (magit-commit-at-point)))
    (if commit (intern commit) nil)))

;;;###autoload
(defun magit-arc-amend-close (&optional args)
  "Amend and close commit message."
  (interactive)
  (let* ((commit (magit-arc-get-commit))
         (revision (magit-arc-commit-to-revision commit)))
    (print (cons "A" revision))
    (if (not revision)
        (error "Can't find revision for commit: %s" commit))
    (magit-arc-run "amend" "--revision" revision)
    (magit-arc-run "close-revision" revision)
    (magit-arc-db-remove-commit commit)
    (message "Amend finished.")
    ))

;;;###autoload
(defun magit-arc-send-review (&optional args)
  "Send a commit to review.."
  (interactive)
  (let* ((commit (magit-arc-get-commit))
         (revision (magit-arc-commit-to-revision commit)))
    (if (not revision)
        (error "Can't find revision for commit: %s" commit))
    ;; Show popup and send review, then return a revision id.

    ;; TODO: (magit-arc-db-add-commit)
    (message "Send finished.")))

(defvar magit-arc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "X") 'magit-arc-popup)
    map))

;;;###autoload
(define-derived-mode magit-arc-mode magit-mode
  "Git-Svn support for Magit."
  ;; :lighter magit-arc-mode-lighter
  ;; :keymap  magit-arc-mode-map
  :group 'magit-arc
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
  (define-key magit-mode-map (kbd "X") 'magit-arc-popup)
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
