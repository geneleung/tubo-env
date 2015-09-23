;;; magit-arc.el --- Git-Svn extension for Magit

;; Copyright (C) 2010-2015  The Magit Project Developers

;; Author: Phil Jackson <phil@shellarchive.co.uk>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>
;; Keywords: vc tools
;; Package-Version: 20150711.415
;; Package: magit-arc
;; Package-Requires: ((emacs "24.4") (magit "2.1.0"))

;; Magit is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; This package provides basic support for Git-Svn.
;;
;;   Git-Svn is a Git command that aims to provide bidirectional
;;   operation between a Subversion repository and Git.
;;
;; For information about Git-Svn see its manual page `git-svn(1)'.
;;
;; If you are looking for native SVN support in Emacs, then have a
;; look at `psvn.el' and info node `(emacs)Version Control'.

;; When `magit-arc-mode' is turned on then the unpushed and unpulled
;; commit relative to the Subversion repository are displayed in the
;; status buffer, and `N' is bound to a popup with commands that wrap
;; the `git svn' subcommands fetch, rebase, dcommit, branch and tag,
;; as well as a few extras.

;; To enable the mode in a particular repository use:
;;
;;   cd /path/to/repository
;;   git config --add magit.extension svn
;;
;; To enable the mode for all repositories use:
;;
;;   git config --global --add magit.extension svn
;;
;; To enable the mode globally without dropping to a shell:
;;
;;   (add-hook 'magit-mode-hook 'magit-arc-mode)

;;; Code:

(require 'cl-lib)
(require 'dash)

(require 'magit)

(declare-function find-lisp-find-files-internal 'find-lisp)

;;; Options

(defgroup magit-arc nil
  "Git-Svn support for Magit."
  :group 'magit-extensions)

(defcustom magit-arc-externals-dir ".git_externals"
  "Directory from repository root that stores cloned SVN externals."
  :group 'magit-arc
  :type 'string)

(defcustom magit-arc-mode-lighter " Msvn"
  "Mode-line lighter for Magit-Arc mode."
  :group 'magit-arc
  :type 'string)

;;; Utilities

(defun magit-arc-get-url ()
  (magit-git-string "svn" "info" "--url"))

(defun magit-arc-get-rev ()
  (--when-let (--first (string-match "^Revision: \\(.+\\)" it)
                       (magit-git-lines "svn" "info"))
    (match-string 1 it)))

(defun magit-arc-get-ref ()
  (--when-let (--first (string-match "^Remote Branch: \\(.+\\)" it)
                       (magit-git-lines "svn" "rebase" "--dry-run"))
    (match-string 1 it)))

;;; Commands

(magit-define-popup magit-arc-popup
  "Popup console for svn commands."
  'magit
  :man-page "git-svn"
  :switches '((?n "Dry run"         "--dry-run"))
  :actions  '((?c "DCommit"         magit-arc-dcommit)
              (?r "Rebase"          magit-arc-rebase)
              (?f "Fetch"           magit-arc-fetch)
              (?x "Fetch Externals" magit-arc-fetch-externals)
              (?s "Show commit"     magit-arc-show-commit)
              (?b "Create branch"   magit-arc-create-branch)
              (?t "Create tag"      magit-arc-create-tag)))

;;;###autoload
(defun magit-arc-show-commit (rev &optional branch)
  "Show the Git commit for a Svn revision read from the user.
With a prefix argument also read a branch to search in."
  (interactive (list (read-number "SVN revision: ")
                     (and current-prefix-arg
                          (magit-read-local-branch "In branch"))))
  (--if-let (magit-git-string "svn" "find-rev" (format "r%i" rev) branch)
      (magit-show-commit it)
    (user-error "Revision r%s could not be mapped to a commit" rev)))

;;;###autoload
(defun magit-arc-create-branch (name &optional args)
  "Create svn branch NAME.
\n(git svn branch [--dry-run] NAME)"
  (interactive (list (read-string "Branch name: ") magit-current-popup-args))
  (magit-run-git "svn" "branch" args name))

;;;###autoload
(defun magit-arc-create-tag (name &optional args)
  "Create svn tag NAME.
\n(git svn tag [--dry-run] NAME)"
  (interactive (list (read-string "Tag name: ") magit-current-popup-args))
  (magit-run-git "svn" "tag" args name))

;;;###autoload
(defun magit-arc-rebase (&optional args)
  "Fetch revisions from Svn and rebase the current Git commits.
\n(git svn rebase [--dry-run])"
  (interactive (list magit-current-popup-args))
  (magit-run-git-async "svn" "rebase" args))

;;;###autoload
(defun magit-arc-dcommit (&optional args)
  "Run git-svn dcommit.
\n(git svn dcommit [--dry-run])"
  (interactive (list magit-current-popup-args))
  (magit-run-git-async "svn" "dcommit" args))

;;;###autoload
(defun magit-arc-fetch ()
  "Fetch revisions from Svn updating the tracking branches.
\n(git svn fetch)"
  (interactive)
  (magit-run-git-async "svn" "fetch"))

;;;###autoload
(defun magit-arc-fetch-externals()
  "Fetch and rebase all external repositories.
Loops through all external repositories found
in `magit-arc-external-directories' and runs
`git svn rebase' on each of them."
  (interactive)
  (require 'find-lisp)
  (--if-let (find-lisp-find-files-internal
             (expand-file-name magit-arc-externals-dir)
             (lambda (file dir)
               (string-equal file ".git"))
             'find-lisp-default-directory-predicate)
      (dolist (external it)
        (let ((default-directory (file-name-directory external)))
          (magit-run-git "svn" "rebase")))
    (user-error "No SVN Externals found. Check magit-arc-externals-dir"))
  (magit-refresh))

;;; Mode

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

(easy-menu-define magit-arc-mode-menu nil "Magit-Arc mode menu"
  '("Git-Svn"
    :visible magit-arc-mode
    :active (lambda () (magit-get "svn-remote" "svn" "fetch"))
    ["Dcommit"         magit-arc-dcommit]
    ["Rebase"          magit-arc-rebase]
    ["Fetch"           magit-arc-fetch]
    ["Fetch Externals" magit-arc-fetch-externals]
    ["Show commit"     magit-arc-show-commit]
    ["Create branch"   magit-arc-create-branch]
    ["Create tag"      magit-arc-create-tag]))

(easy-menu-add-item 'magit-mode-menu '("Extensions") magit-arc-mode-menu)

;;; Sections

(defun magit-insert-svn-unpulled ()
  (--when-let (magit-arc-get-ref)
    (magit-insert-section (svn-unpulled)
      (magit-insert-heading "Unpulled svn commits:")
      (magit-insert-log (format "HEAD..%s" it)))))

(defun magit-insert-svn-unpushed ()
  (--when-let (magit-arc-get-ref)
    (magit-insert-section (svn-unpushed)
      (magit-insert-heading "Unpushed git commits:")
      (magit-insert-log (format "%s..HEAD" it)))))

(magit-define-section-jumper svn-unpulled "Unpulled svn commits")
(magit-define-section-jumper svn-unpushed "Unpushed git commits")

(defun magit-insert-svn-remote ()
  (--when-let (magit-arc-get-rev)
    (magit-insert-section (line)
      (magit-insert (format "%-10s%s from %s\n" "Remote:"
                            (propertize (concat "r" it) 'face 'magit-hash)
                            (magit-arc-get-url))))))

;;; magit-arc.el ends soon

(define-obsolete-function-alias 'turn-on-magit-arc 'magit-arc-mode)

(provide 'magit-arc)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; magit-arc.el ends here
