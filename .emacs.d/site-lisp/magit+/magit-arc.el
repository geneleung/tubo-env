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

(defconst arc-commit-filename-regexp (rx "edit." (+ ascii) "new-commit" eol))

(defun arc-commit-setup-check-buffer ()
  (and buffer-file-name
       (string-match-p arc-commit-filename-regexp buffer-file-name)
       (arc-commit-setup)))

(defcustom magit-arc-desc-keywords nil
  "Keywords to check for descriptions."
  :type '(repeat :tag "Keywords to check"
                 (regexp :tag "Keyword"))
  :group 'magit-arc)

(defun magit-arc--check-keywords ()
  "Check if user specified keywords are included in descritpion."
  (message "enter")
  (let ((r-match-brief (rx (group (+ anything)) "
Summary:"))
        (valid t)
        brief missing)
    (save-excursion
      (goto-char (point-min))
      (when (search-forward-regexp r-match-brief)
        (setq brief (match-string 1))
        (dolist (item magit-arc-desc-keywords)
          (unless (string-match (regexp-opt (list item)) brief)
            (setq valid nil
                  missing (cons item missing))))))
    (unless valid
      (message "Some fields are missing: %s, please fix them." (concat missing)))
    valid))

(defun magit-arc--check-test-plan ()
  "Check if test plan is set or not.
If it is not set, it will ask whether it is allowed to set plan to `None'.
If it is not allowed, it will return nil so user can continue input correct test plan."
  (save-excursion
    (goto-char (point-min))
    ;; (re-search-forward (git-commit-summary-regexp) nil t)
    ;; (if (equal (match-string 1) "")
    ;;     t ; Just try; we don't know whether --allow-empty-message was used.
    ;;   (and (or (equal (match-string 2) "")
    ;;            (y-or-n-p "Summary line is too long.  Commit anyway? "))
    ;;        (or (equal (match-string 3) "")
    ;;            (y-or-n-p "Second line is not empty.  Commit anyway? "))))
    ))

(defcustom magit-arc-finish-query-functions
  '(magit-arc--check-test-plan
    magit-arc--check-keywords)
  "List of functions called to query before performing commit.

The commit message buffer is current while the functions are
called.  If any of them returns nil, then the commit is not
performed and the buffer is not killed.  The user should then
fix the issue and try again.

The functions are called with one argument.  If it is non-nil
then that indicates that the user used a prefix argument to
force finishing the session despite issues.  Functions should
usually honor this wish and return non-nil."
  :options '(magit-arc--check-test-plan)
  :type 'hook
  :group 'magit-arc)

(defun arc-commit-setup ()
  (setq with-editor-show-usage nil)
  (with-editor-mode 1)
  (add-hook 'with-editor-finish-query-functions
            'magit-arc-finish-query-functions nil t)
  ;; (add-hook 'with-editor-pre-finish-hook
  ;;           'git-commit-save-message nil t)
  ;; (add-hook 'with-editor-pre-cancel-hook
  ;;           'git-commit-save-message nil t)
  ;; (setq with-editor-cancel-message
  ;;       'git-commit-cancel-message)
  (make-local-variable 'log-edit-comment-ring-index)
  ;; (arc-commit-edit-mode 1)
  (git-commit-setup-font-lock)
  (when (boundp 'save-place)
    (setq save-place nil))
  (save-excursion
    (goto-char (point-min))
    (when (= (line-beginning-position)
             (line-end-position))
      (open-line 1)))
  (run-hooks 'git-commit-setup-hook)
  (set-buffer-modified-p nil))

;;; Commands

(magit-define-popup magit-arc-popup
  "Popup console for arc commands."
  'magit
  :man-page "git-arc"
  :actions  '(
              (?a "Amend & Close"   magit-arc-amend-close)
              (?s "Send Review"     magit-arc-send-review)))

;;TODO: update popup based on arguments.
(defcustom magit-arc-send-arguments nil
  "Default arguments used when sending review (arc diff)."
  :group 'magit-arc
  :type '(repeat (string :tag "Argument")))

(defvar magit-arc--current-commit nil "Nil.")

(cdsq magit-arc-send-popup
  '(:variable magit-arc-send-arguments
    ;; :switches ((?g "Show graph"              "--graph")
    ;;            (?c "Show graph in color"     "--color")
    ;;            (?d "Show refnames"           "--decorate")
    ;;            (?S "Show signatures"         "--show-signature")
    ;;            (?u "Show diffs"              "--patch")
    ;;            (?s "Show diffstats"          "--stat")
    ;;            (?D "Simplify by decoration"  "--simplify-by-decoration")
    ;;            (?f "Follow renames when showing single-file log" "--follow"))
    :options  ((?e "Set Encoding"          "--encoding=" read-from-minibuffer)
               (?r "Set Reviewers"         "--reviewers="  read-from-minibuffer)
               (?f "Force Update"          "--update="    read-from-minibuffer)
               )
    :actions  ((?s "Send review"             magit-arc-do-send-review))
    :default-action magit-arc-do-send-review
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

(defun magit-arc-send-arguments (&optional refresh)
  (cond ((memq magit-current-popup
               '(magit-arc-send-popup))
         (magit-popup-export-file-args magit-current-popup-args))
        ((derived-mode-p 'magit-log-mode)
         (list (nth 1 magit-refresh-args)
               (nth 2 magit-refresh-args)))
        (refresh
         (list magit-log-section-arguments nil))
        (t
         (-if-let (buffer (magit-mode-get-buffer nil 'magit-log-mode))
             (with-current-buffer buffer
               (list (nth 1 magit-refresh-args)
                     (nth 2 magit-refresh-args)))
           (list (default-value 'magit-arc-send-arguments) nil)))))

;;;###autoload
(defun magit-arc-amend-close (&optional args)
  "Amend and close commit message."
  (interactive)
  (let* ((commit (magit-arc-get-commit))
         (revision (magit-arc-commit-to-revision commit)))
    (unless revision
      (setq revision (completing-read "Input revision number: " nil)))

    (if (not revision)
      (error "Can't find revision for commit: %s" commit))
    (magit-arc-run "amend" "--revision" revision)
    (magit-arc-run "close-revision" revision)
    (magit-arc-db-remove-commit commit)
    (message "Amend finished.")))

;;;###autoload
(defun magit-arc-send-review (&optional args)
  "Send a commit to review.."
  (interactive "P")
  (let* ((commit (magit-arc-get-commit))
         (revision (magit-arc-commit-to-revision commit)))
    (setq magit-arc--current-commit commit)
    ;; Show popup and send review, then return a revision id.
    (magit-invoke-popup 'magit-arc-send-popup nil args)

    ;; TODO: (magit-arc-db-add-commit)
    (message "Send finished.")))

;;;###autoload
(defun magit-arc-do-send-review (&rest args)
  "Invoke arc to send review.
When `HEAD' is detached or with a prefix argument show log for
one or more revs read from the minibuffer."
  (interactive)
  (let* ((send-args (magit-arc-send-arguments))
         (cl (symbol-name magit-arc--current-commit))
         (arc-args (list "--head" cl (concat cl "~"))))

    (mapc (lambda (x)
            (when x
              (push x arc-args)))
          (car send-args))

    (mapc (lambda (x)
            (when x
              (push x arc-args)))
          (cdr send-args))

    (push "diff" arc-args)
    ;; TODO: get process buffer, save point-max, then process arc commands, and parse
    ;;       result between previous (point-max) and current point-max, find URL and
    ;;       save it into db.
    (let ((find-file-hook (append find-file-hook '('with-editor-mode))))
      (with-editor
        (apply 'magit-start-process "arc" nil arc-args)))))

(defvar magit-arc-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "X") 'magit-arc-popup)
    map))

;;;###autoload
(define-minor-mode magit-arc-mode
  "Arc support for Magit."
  ;; :lighter magit-arc-mode-lighter
  ;; :keymap  magit-arc-mode-map
  :group 'magit-arc
  (unless (derived-mode-p 'magit-mode)
    (user-error "This mode only makes sense with Magit"))
  (cond
   (magit-arc-mode
    (add-hook  'find-file-hook 'arc-commit-setup-check-buffer))
   (t
    (remove-hook 'find-file-hook 'arc-commit-setup-check-buffer)))
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
