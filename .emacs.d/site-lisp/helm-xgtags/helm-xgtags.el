;;; helm-xgtags.el --- gtags facility for Emacs, integrated with helm.
;;
;; Copyright (c) 2015 Yang Yingchao
;;
;; Author: Yang Yingchao <yangyingchao@gmail.com>
;; Maintainer: Yang Yingchao <yangyingchao@gmail.com>
;; URL: https://github.com/yangyingchao/helm-xgtags
;; Version: 1.0
;; Created: 2015-05-20
;; Date: 2015-05-20
;;
;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; This file is based on helm-xgtags (http://home.tiscali.de/mgidde/source/xgtags.el),
;; written by Marco Gidde, and inspired by helm-gtags by Syohei YOSHIDA,
;; (https://github.com/syohex/emacs-helm-gtags).
;;
;;; Installation
;;
;; To use helm-xgtags copy this file to some place where emacs can find it,
;; if necessary add the path you chose to the load-path variable. In
;; your .emacs add the line
;;
;;	(require 'helm-xgtags)
;;
;; In any buffer you can now toggle helm-xgtags-mode by calling the
;; interactive command with same name. Since this is a bit annoying,
;; you might choose to turn it on for some specific modes. For c-mode
;; you can add something like the following snippet to your .emacs
;; file. Other modes provide similar hooks.
;;
;;         (add-hook 'c-mode-common-hook
;;                   (lambda ()
;; 		        (helm-xgtags-mode 1)))
;;
;; After that you can use the predefined keybindings to query your
;; GLOBAL database. Call 'describe-function RE'S helm-xgtags-mode' to get an
;; overview of those bindings.
;;
;; TODO: Hide similar parts of file path...
;;
;;; Code:

(require 'easymenu)
(require 'cl-lib)
(require 'helm)

(defgroup helm-xgtags nil
  "Using gtags and global for crossrefencing"
  :group 'tools)

(defvar helm-xgtags--completing-history nil)
;;; Faces

(defface helm-xgtags-match-face
  '((((class color) (background dark))
     (:foreground "cyan"))
    (((class color) (background light))
     (:foreground "magenta"))
    (t (:bold t)))
  "Face used to highlight function name in the *xgtags* buffer."
  :group 'helm-xgtags)

(defface helm-xgtags-match-selected-face
  '((((class color) (background dark))
     (:foreground "cyan" :bold t))
    (((class color) (background light))
     (:foreground "magenta" :bold t))
    (t (:bold t)))
  "Face used to highlight selected function name in the *xgtags* buffer."
  :group 'helm-xgtags)

(defface helm-xgtags-file-face
  '((((class color) (background dark))
     (:foreground "yellow"))
    (((class color) (background light))
     (:foreground "blue"))
    (t (:bold t)))
  "Face used to highlight file name in the *xgtags* buffer."
  :group 'helm-xgtags)

(defface helm-xgtags-file-selected-face
  '((((class color) (background dark))
     (:foreground "yellow" :bold t))
    (((class color) (background light))
     (:foreground "blue" :bold t))
    (t (:bold t)))
  "Face used to highlight selected file name in the *xgtags* buffer."
  :group 'helm-xgtags)

(defface helm-xgtags-line-number-face
  '((((class color) (background dark))
     (:foreground "red"))
    (((class color) (background light))
     (:foreground "red"))
    (t (:bold t)))
  "Face used to highlight line number in the *xgtags* buffer."
  :group 'helm-xgtags)

(defface helm-xgtags-line-number-selected-face
  '((((class color) (background dark))
     (:foreground "red" :bold t))
    (((class color) (background light))
     (:foreground "red" :bold t))
    (t (:bold t)))
  "Face used to highlight selected line number in the *xgtags* buffer."
  :group 'helm-xgtags)

(defface helm-xgtags-line-face
  '((((class color) (background dark))
     (:foreground "green"))
    (((class color) (background light))
     (:foreground "black"))
    (t (:bold nil)))
  "Face used to highlight the rest of line in the *xgtags* buffer."
  :group 'helm-xgtags)

(defface helm-xgtags-line-selected-face
  '((((class color) (background dark))
     (:foreground "green" :bold t))
    (((class color) (background light))
     (:foreground "black" :bold t))
    (t (:bold nil)))
  "Face used to highlight the rest of line in the *xgtags* buffer."
  :group 'helm-xgtags)


;;; customization

(defcustom helm-xgtags-read-only nil
  "*When set to true, new files are opened in read only mode."
  :type 'boolean
  :group 'helm-xgtags)

(defcustom helm-xgtags-ignore-case nil
  "Ignore case in each search."
  :type 'boolean
  :group 'helm-xgtags)

(defcustom helm-xgtags-path-style 'root
  "*How to show paths in the selection buffer."
  :type '(radio (const :tag "Root of the current project" root)
                (const :tag "Relative from the current directory" relative)
                (const :tag "Absolute Path" absolute))
  :group 'helm-xgtags)

(defcustom helm-xgtags-auto-update-db t
  "*Controls if auto update database or not."
  :type 'boolean
  :group 'helm-xgtags)

(defcustom helm-xgtags-find-multiple-db nil
  "*Function to return multiple dbs.
When bound to a function this function is called with one
argument, namely the current directory, and should return a list of
directories with GTAGS databases.
All databases are searched one after another."
  :type 'function
  :group 'helm-xgtags)

(defcustom helm-xgtags-kill-buffers t
  "*Whether to kill buffer after stack entry is popped.
Be careful: even buffers not opened by helm-xgtags itself will be killed!"
  :type 'boolean
  :group 'helm-xgtags)

(defcustom helm-xgtags-select-buffer-name "*xgtags*"
  "*Name to use as the select buffer."
  :type 'string
  :group 'helm-xgtags)

(defcustom helm-xgtags-rootdir nil
  "*Root directory of source tree."
  :type 'string
  :group 'helm-xgtags)

(defcustom helm-xgtags-update-interval-second 60
  "*Interval used to decide when to update db.
Tags are updated in `after-save-hook' if this seconds is passed from last
update.
Always update if value of this variable is nil."
  :type '(choice (integer :tag "Update interval seconds")
                 (boolean :tag "Update every time" nil))
  :group 'helm-xgtags)

(defcustom helm-xgtags--debug nil
  "Switch of debug/non-debug."
  :type 'boolean
  :group 'helm-xgtags)


(defconst helm-xgtags--symbol-regexp "[A-Za-z_][A-Za-z_0-9]*"
  "Regexp matching tag name.")
(defconst helm-xgtags--definition-regexp "#[ \t]*define[ \t]+\\|ENTRY(\\|ALTENTRY("
  "Regexp matching tag definition name.")
(defconst helm-xgtags--tag-regexp "\\([^ \t]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([^ \t]+\\)[ \t]+\\([^\n]+\\)"
  "Regex matching the current output line for a tag.")
(defconst helm-xgtags--file-regexp "\\([^ \t]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([^ \t\n]+\\)"
  "Regex matching the current output line for a tag.")

(defvar helm-xgtags-minor-mode-text " helm-xgtags"
  "Text to be shown in the mode line.")

(defvar helm-xgtags-select-mode-name "xgtags-select"
  "Text to be shown in the mode line.")

(defvar helm-xgtags-select-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-m" 'helm-xgtags-select-tag-near-point)
    (define-key keymap "\e." 'helm-xgtags-select-tag-near-point)
    (do ((k (string-to-char "a") (+ 1 k))) ((> k (string-to-char "z")))
      (define-key
    	keymap
    	(read-kbd-macro (char-to-string k))
    	'helm-xgtags--activate))
    (do ((k (string-to-char "A") (+ 1 k))) ((> k (string-to-char "Z")))
      (define-key
    	keymap
    	(read-kbd-macro (char-to-string k))
        'helm-xgtags--activate))
    (do ((k (string-to-char "0") (+ 1 k))) ((> k (string-to-char "9")))
      (define-key
    	keymap
    	(read-kbd-macro (char-to-string k))
        'helm-xgtags--activate))
    keymap)
  "Keymap used in helm-xgtags select mode.")

(defvar helm-xgtags--last-update-time 0 "Last update time.")

(defvar helm-xgtags--tags nil
  "List of current tags.")
(defvar helm-xgtags--selected-tag nil
  "The currently selected tag.")
(defvar helm-xgtags--stack nil
  "Stack for tag browsing.")


;;; macros

(defmacro with-xgtags-environment (&rest body)
  "Execute BODY with proper environment."
  `(let ((process-environment (copy-alist process-environment)))
     (when helm-xgtags-rootdir
       (setenv "GTAGSROOT" helm-xgtags-rootdir))
     ,@body))
(put 'with-xgtags-environment 'lisp-indent-function 0)

(defmacro* with-xgtags-buffer ((&key buffer save-excursion (read-only t))
                               &rest body)
  "Evaluate in helm-xgtags buffer."
  (let ((buffer-var (or buffer (gensym "buffer-"))))
    `(let ((,buffer-var (helm-xgtags--get-buffer)))
       (,(if save-excursion 'save-excursion 'progn)
        (set-buffer ,buffer-var)
        (let ((buffer-read-only ,read-only))
          ,@body)))))
(put 'with-xgtags-buffer 'lisp-indent-function 1)


;;; utilities

(defun helm-xgtags--list-sans-nil (&rest args)
  "Build a list from ARGS but leave nils out."
  (let ((result nil))
    (dolist (arg args (nreverse result))
      (when arg
        (push arg result)))))

(defun helm-xgtags--token-at-point ()
  "Return a default tag to search for, based on the text at point."
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (if bounds
        (buffer-substring-no-properties (car bounds) (cdr bounds)))))

(defun helm-xgtags--file-at-point ()
  "Return a default tag to search for, based on the text at point."
  (or
   (helm-xgtags-headerp)
   (cond
    ((looking-at "[0-9A-Za-z_\.]")
     (while (looking-at "[0-9A-Za-z_\.]")
       (forward-char -1))
     (forward-char 1)))
   (when (looking-at "[0-9A-Za-z_\.]+")
     (match-string-no-properties 0))))

(defun helm-xgtags--function-p ()
  "Is it a function?"
  (save-excursion
    (while (and (not (eolp)) (looking-at "[0-9A-Za-z_]"))
      (forward-char 1))
    (while (and (not (eolp)) (looking-at "[ \t]"))
      (forward-char 1))
    (looking-at "(")))

(defun helm-xgtags--definition-p ()
  "Is it a definition?"
  (save-excursion
    (if (or (and (string-match "\.java$" buffer-file-name)
                 (looking-at "[^(]+([^)]*)[ \t]*{"))
            (bolp))
        t
      (forward-word -1)
      (cond
       ((looking-at "define")
        (forward-char -1)
        (while (and (not (bolp)) (looking-at "[ \t]"))
          (forward-char -1))
        (and (bolp) (looking-at "#")))
       ((looking-at "ENTRY\\|ALTENTRY")
        (bolp))))))

(defun helm-xgtags--insert-with-face (string face)
  "Insert STRING with FACE into buffer."
  (let ((start (point)))
    (insert string)
    (put-text-property start (point) 'face face)))

(defun helm-xgtags--update-minor-mode-text ()
  "Update text in mode-line."
  (let ((length (length helm-xgtags--stack)))
    (if (zerop length)
        (setq helm-xgtags-minor-mode-text " helm-xgtags")
      (setq helm-xgtags-minor-mode-text (format " helm-xgtags[%d]" length)))))


;;; handling tags and the tags list

(defstruct helm-xgtags--tag
  query abs-file file line match)

(defun helm-xgtags--make-tag (&rest args)
  "Generate a TAG with ARGS."
  (let* ((tag (apply 'make-helm-xgtags--tag args))
         (abs-file (expand-file-name (helm-xgtags--tag-file tag))))
    (setf (helm-xgtags--tag-abs-file tag) abs-file)
    tag))

(defun helm-xgtags--tag-position (tag)
  "Return the position index of TAG in the tag-list."
  (let ((pos 0)
        (found nil)
        (tags helm-xgtags--tags))
    (while (and tags (not (setq found (eq (car tags) tag))))
      (setq pos (1+ pos))
      (setq tags (cdr tags)))
    (when found
      pos)))

(defun* helm-xgtags--next-tag (tag &optional (n 1))
  "Returns the N-th tag that follows TAG in the tag list or nil."
  (assert tag nil "No tag in call to helm-xgtags--next-tag")
  (let ((pos (helm-xgtags--tag-position tag)))
    (when (and (>= pos 0) (>= (+ pos n) 0))
      (nth (+ pos n) helm-xgtags--tags))))

(defun helm-xgtags--insert-tags (tags)
  "Insert TAGS into current buffer.
This function recieves a list of tags, erases the current buffer
and then inserts the tags nicely."
  (erase-buffer)
  (let ((current-file nil))
    (dolist (tag tags)
      (let ((file (helm-xgtags--tag-file tag)))
        (unless (equal current-file file)
          (when current-file
            (insert "\n"))
          (setq current-file file)
          (helm-xgtags--insert-tag-file tag)
          (insert "\n")))
      (helm-xgtags--insert-tag tag)
      (insert "\n"))))

(defun helm-xgtags--insert-tag-file (tag)
  "Insert the file entry TAG into the current buffer."
  (helm-xgtags--insert-with-face (helm-xgtags--tag-file tag) 'helm-xgtags-file-face))

(defun helm-xgtags--insert-tag (tag)
  "Insert a single tag TAG at point in the current buffer.a"
  (let ((start (point))
        (query (helm-xgtags--tag-query tag))
        (line (helm-xgtags--tag-line tag))
        (match (helm-xgtags--tag-match tag))
        (selected-p (eq tag helm-xgtags--selected-tag)))
    (helm-xgtags--insert-with-face query
                              (if selected-p
                                  'helm-xgtags-match-selected-face
                                'helm-xgtags-match-face))
    (insert "[")
    (helm-xgtags--insert-with-face (number-to-string line)
                              (if selected-p
                                  'helm-xgtags-line-number-selected-face
                                'helm-xgtags-line-number-face))
    (when match
      (insert "]\t\t")
      (helm-xgtags--insert-with-face match (if selected-p
                                          'helm-xgtags-line-selected-face
                                        'helm-xgtags-line-face)))
    (put-text-property start (point) 'helm-xgtags-tag tag)))

(defun helm-xgtags--update-tag (tag)
  "Search and update TAG.
This function searches the tag TAG in the current buffer and replaces the
current representation with an updated one."
  (let ((region (helm-xgtags--find-tag-region tag)))
    (when region
      (delete-region (car region) (cdr region))
      (goto-char (car region))
      (helm-xgtags--insert-tag tag))))

(defun helm-xgtags--find-tag-region (tag)
  "If TAG is found in the current buffer this functions returns a
list with the start and end positions, otherwise it returns nil"
  (when tag
    (let ((start (text-property-any (point-min) (point-max) 'helm-xgtags-tag tag)))
      (when start
        (cons start (next-single-property-change start 'helm-xgtags-tag))))))

(defun helm-xgtags--select-tag (tag &optional update)
  "Make TAG the selected tag.
If UPDATE is not nil, try to find the previous selected tag and TAG in the
current buffer, update their representation and move point to the beginning of
TAG."
  (let ((old-sel helm-xgtags--selected-tag))
    (setq helm-xgtags--selected-tag tag)
    (when update
      (helm-xgtags--update-tag old-sel)
      (helm-xgtags--update-tag helm-xgtags--selected-tag)
      (goto-char (or (car (helm-xgtags--find-tag-region helm-xgtags--selected-tag)) (point-min)))
      (when (get-buffer-window (current-buffer))
        (set-window-point (get-buffer-window (current-buffer)) (point))))))

(defun helm-xgtags--find-tag-near-point (&optional backwards)
  "Find the next selectable tag and move point to its beginning. If
there is none at the current line, step a line forward or backward to
find one."
  (beginning-of-line)
  (while (when (not (get-text-property (point) 'helm-xgtags-tag))
           (zerop (forward-line (and backwards -1)))))
  (get-text-property (point) 'helm-xgtags-tag))

(defun helm-xgtags--follow-tag (tag)
  "Jump to the place that TAG points to."
  (interactive)
  (find-file (helm-xgtags--tag-abs-file tag))
  (setq buffer-read-only (or buffer-read-only helm-xgtags-read-only))
  (helm-xgtags-mode 1)
  (goto-line (helm-xgtags--tag-line tag))
  (let ((match (helm-xgtags--tag-query tag))
        (found nil)
        (lines 0))
    (while (and (not found) (< lines 5))
      (let ((start (save-excursion (forward-line (- lines))
                                   (point)))
            (end (save-excursion (forward-line lines)
                                 (end-of-line)
                                 (point))))
        (save-excursion
          (goto-char start)
          (setq found (search-forward-regexp match end t))))
      (setq lines (1+ lines)))
    (when found
      (goto-char (match-beginning 0)))))

(defun helm-xgtags--map-tags (func)
  "Maps over all tags in the *xgtags* buffer, jumps to the tag and
funcalls FUNC with the match as argument."
  (mapc (lambda (tag)
          (with-xgtags-buffer (:read-only nil)
            (helm-xgtags--select-and-follow-tag tag)
            (funcall func (helm-xgtags--tag-query tag))))
        helm-xgtags--tags))

(defun helm-xgtags--test-map-tags ()
  (interactive)
  (helm-xgtags--map-tags
   (lambda (match)
     (message "foo: %s" match)
     (when (search-forward-regexp match
                                  (save-excursion (end-of-line) (point))
                                  t)
       (goto-char (match-beginning 0))
       (set-mark (match-end 0))
       (unless (y-or-n-p "More? ")
         (return-from helm-xgtags--test-map-tags))))))

;;; handling the context and the context stack

(defstruct helm-xgtags--context
  type buffer point tags selected-tag)

(defun helm-xgtags--make-context (&optional option)
  "Create a context object with OPTION."
  (make-helm-xgtags--context :type option
                        :buffer (current-buffer)
                        :point (point-marker)
                        :tags helm-xgtags--tags
                        :selected-tag helm-xgtags--selected-tag))

(defun helm-xgtags--stacked-p (buffer)
  "If BUFFER exists on the helm-xgtags stack."
  (memq buffer (mapcar 'helm-xgtags--context-buffer helm-xgtags--stack)))

(defun helm-xgtags--push-context (&optional option)
  (first (push (helm-xgtags--make-context option) helm-xgtags--stack)))

(defun helm-xgtags--pop-context ()
  "Pop context from stack and return it."
  (pop helm-xgtags--stack))


;;; handling the selection buffer

(defun helm-xgtags--get-buffer ()
  "Return the selection buffer. If it was kill recreate and fill it
with the previous query results."
  (let ((buffer (get-buffer helm-xgtags-select-buffer-name)))
    (unless buffer
      (setq buffer (get-buffer-create helm-xgtags-select-buffer-name))
      ;; using with-xgtags-buffer here is not possible because it uses
      ;; helm-xgtags--get-buffer itself
      (save-excursion
        (set-buffer buffer)
        (when helm-xgtags--stack
          (helm-xgtags--insert-tags helm-xgtags--tags))
        (helm-xgtags-select-mode)))
    buffer))

(defun helm-xgtags--select-update-mode-text (tag)
  (let ((pos (helm-xgtags--tag-position tag)))
    (if pos
        (setq mode-name (format "%s[%d/%d]"
                                helm-xgtags-select-mode-name
                                (1+ pos) (length helm-xgtags--tags)))
      (setq mode-name (format "%s[%d]"
                              helm-xgtags-select-mode-name
                              (length helm-xgtags--tags))))))

(defun helm-xgtags--update-buffer (context)
  (let ((tags (helm-xgtags--context-tags context))
        (selected-tag (helm-xgtags--context-selected-tag context)))
    (helm-xgtags--update-minor-mode-text)
    (when tags
      (with-xgtags-buffer (:save-excursion t :read-only nil)
        (setq helm-xgtags--tags tags)
        (helm-xgtags--insert-tags tags)
        (helm-xgtags--select-tag selected-tag t)
        (helm-xgtags--select-update-mode-text selected-tag)))))

(defun helm-xgtags--select-and-follow-tag (tag)
  "Select TAG and follow the link."
  (when tag
    (with-xgtags-buffer (:read-only nil)
      (helm-xgtags--select-tag tag t)
      (helm-xgtags--select-update-mode-text tag))
    (helm-xgtags--follow-tag tag)))


;;; options of the GLOBAL program

(defconst helm-xgtags--gtags-options-alist
  '(
    (definition . "%s: Definition not found")
    (file       . "%s: file not found")
    (path       . "%s: path not found")
    (pattern    . "%s: pattern not found")
    (reference  . "%s: reference not found")
    (symbol     . "%s: symbol not found")
    ))

(defconst helm-xgtags--prompt-alist
  '(
    (definition . "Find Definition: ")
    (path       . "Find File: ")
    (pattern    . "Find Pattern: ")
    (reference  . "Find Reference: ")
    (symbol     . "Find Symbol: ")
    ))

(defconst helm-xgtags--search-option-alist
  '(
    (definition . "-d")
    (file       . "-f")
    (path       . "-Poa")
    (pattern    . "-g")
    (reference  . "-r")
    (symbol     . "-s")
    ))

(defun helm-xgtags--option-string (symbol)
  (when symbol
    (assert (assoc symbol helm-xgtags--search-option-alist) t
            "Unknown gtags option symbol: %s" symbol)
    (cdr (assoc symbol helm-xgtags--search-option-alist))))

(defun helm-xgtags--option-error-msg (symbol)
  (let ((opt (assoc symbol helm-xgtags--gtags-options-alist)))
    (if (and opt (cdr opt))
        (cdr opt)
      "%s: tag not found")))


;;; GLOBAL process handling


(defun helm-xgtags--tag-directory ()
  (with-temp-buffer
    (if (getenv "GTAGSROOT")
        (getenv "GTAGSROOT")
      (unless (zerop (process-file "global" nil t nil "-p"))
        (error "GTAGS not found"))
      (goto-char (point-min))
      (when (looking-at "^\\([^\r\n]+\\)")
        (let ((tag-path (match-string-no-properties 1)))
          (file-name-as-directory
           (if (eq system-type 'cygwin)
               (cygwin-convert-file-name-from-windows tag-path)
             tag-path)))))))

(defun helm-xgtags--base-directory ()
  (let ((dir (cl-case helm-xgtags-path-style
               (root (helm-xgtags--tag-directory))
               (otherwise default-directory)))
        (remote (file-remote-p default-directory)))
    (if (and remote (not (file-remote-p dir)))
        (concat remote dir)
      dir)))

(defun helm-xgtags--call-global (buffer-dir option tagname)
  "In all BUFFER-DIR, call global commands with OPTION for TAGNAME."
  (let ((tags nil))
    (helm-xgtags--do-in-all-directories
     buffer-dir
     (lambda (dir)
       (let ((helm-xgtags-rootdir (and dir (file-name-as-directory dir))))
         (with-xgtags-environment
           (with-temp-buffer
             (let ((default-directory (helm-xgtags--base-directory))
                   (args (helm-xgtags--list-sans-nil
                          "--cxref"
                          (helm-xgtags--option-string option)
                          (if (eq helm-xgtags-path-style 'absolute)
                              "--absolute")
                          tagname)))
               (if helm-xgtags--debug
                   (message "Calling global %s" args))
               (if (zerop (apply #'call-process "global" nil t nil
                                 args))
                   (setq tags (append tags (helm-xgtags--collect-tags-in-buffer)))
                 (message (buffer-substring (point-min)(1- (point-max)))))))))))
    tags))

(defun helm-xgtags--do-in-all-directories (buffer-dir func)
  (let ((dirs (if helm-xgtags-find-multiple-db
                  (funcall helm-xgtags-find-multiple-db buffer-dir)
                (list helm-xgtags-rootdir))))
    (dolist (dir dirs)
      (funcall func dir))))

(defun helm-xgtags--collect-tags-in-buffer ()
  "This function searches the current buffer for tag items and returns
a list with those."
  (when helm-xgtags--debug
    (message "global output: %s" (buffer-string)))
  (save-excursion
    (goto-char (point-min))
    (let ((tags nil))
      (while (not (eobp))
        (cond
         ((looking-at helm-xgtags--tag-regexp)
          (let* ((query (match-string-no-properties 1))
                 (line (string-to-number (match-string-no-properties 2)))
                 (file (match-string-no-properties 3))
                 (match (match-string-no-properties 4)))
            (push (helm-xgtags--make-tag :query query
                                    :file file
                                    :line line
                                    :match match) tags)))
         ((looking-at helm-xgtags--file-regexp)
          (let* ((query (match-string-no-properties 1))
                 (line (string-to-number (match-string-no-properties 2)))
                 (file (match-string-no-properties 3)))
            (push (helm-xgtags--make-tag :query query
                                    :file file
                                    :line line) tags))))
        (forward-line))
      (nreverse tags))))


;;; navigating the selection buffer

(defun helm-xgtags--select-next-prev-tag (arg)
  "Select the next or previous tag in the previous select buffer."
  (let ((tag (helm-xgtags--next-tag helm-xgtags--selected-tag arg)))
    (assert tag nil "The %s of the *xgtags* buffer has been reached"
            (if (> arg 0) "end" "beginning"))
    (helm-xgtags--select-and-follow-tag tag)))

(defun helm-xgtags-select-next-tag (&optional arg)
  "Select the next tag in the previous select buffer."
  (interactive "p")
  (helm-xgtags--select-next-prev-tag arg))

(defun helm-xgtags-select-prev-tag (&optional arg)
  "Select the previous tag in the previous select buffer."
  (interactive "p")
  (helm-xgtags--select-next-prev-tag (- arg)))

;;; finding and selecting tags

(defun helm-xgtags--goto-tag (tagname &optional option)
  "Go find and goto tag (TAGNAME) with OPTION."
  (let* ((window (selected-window))
         (file-name (buffer-file-name))
         (buffer-dir (and file-name (file-name-directory file-name)))
         (tags (helm-xgtags--call-global buffer-dir option tagname))
         (num-tags (length tags))
         (type (if helm-xgtags--stack (helm-xgtags--context-type (car helm-xgtags--stack)) nil)))
    (if (= num-tags 0)
        (error (helm-xgtags--option-error-msg option) tagname)
      (helm-xgtags--push-context option)
      (helm-xgtags--update-minor-mode-text)
      (with-xgtags-buffer (:save-excursion t :read-only nil)
        (setq helm-xgtags--tags tags)
        (helm-xgtags--insert-tags tags))

      (when (eq type 'file) ;; update selected-tag if needed.
        (if (and helm-xgtags--selected-tag
                 (string= (helm-xgtags--tag-abs-file helm-xgtags--selected-tag)
                          (helm-xgtags--tag-abs-file (first tags))))
            (let ((old-file (helm-xgtags--tag-abs-file helm-xgtags--selected-tag)))
              (catch 'BREAK
                (dolist (tag tags)
                  (when (and (string= old-file
                                      (helm-xgtags--tag-abs-file tag))
                             (string= (helm-xgtags--tag-query helm-xgtags--selected-tag)
                                      (helm-xgtags--tag-query tag)))
                    (helm-xgtags--select-tag tag)
                    (throw 'BREAK t)))))))

      (if (= num-tags 1)
          (helm-xgtags--select-and-follow-tag (first tags))
        ;; (unless (eq type 'file)
        ;;   (helm-xgtags--select-and-follow-tag (first tags)))
        (helm-xgtags--activate (buffer-name))))))

(defun helm-xgtags-select-tag-near-point ()
  "Select the tag near point and follow it."
  (interactive)
  (helm-xgtags--select-and-follow-tag (helm-xgtags--find-tag-near-point)))


;;; interactive commands

(defun helm-xgtags-visit-rootdir ()
  "Tell tags commands the root directory of source tree."
  (interactive)
  (unless helm-xgtags-rootdir
    (with-temp-buffer
      (setq helm-xgtags-rootdir
            (if (zerop (call-process "global" nil t nil "-pr"))
                (file-name-as-directory (buffer-substring (point-min)
                                                          (1- (point-max))))
              default-directory))))
  (let ((input (read-file-name "Visit root directory: "
                               helm-xgtags-rootdir helm-xgtags-rootdir t)))
    (unless (equal "" input)
      (assert (file-directory-p input) t "%s is not directory" input)
      (setq helm-xgtags-rootdir (expand-file-name input)))))

(defun helm-xgtags--construct-options (type completion)
  (let ((find-file-p (eq type 'path))
        (gtagslibpath (getenv "GTAGSLIBPATH"))
        (options '("global")))
    (unless find-file-p
      (push "--result=grep" options))
    (when completion
      (push "-c" options))
    (if (assoc-default type helm-xgtags--search-option-alist)
        (push (assoc-default type helm-xgtags--search-option-alist) options))
    (push "-i" options)
    (when (and current-prefix-arg (not find-file-p))
      (push "-l" options))
    (when gtagslibpath
      (push "-T" options))
    options))

(defun helm-xgtags--complete (type string predicate code)
  (let* ((options (helm-xgtags--construct-options type t))
         (args (reverse (cons string options)))
         candidates)
    (with-temp-buffer
      (apply 'process-file "global" nil t nil args)
      (goto-char (point-min))
      (while (re-search-forward "^\\(.+\\)$" nil t)
        (push (match-string-no-properties 1) candidates)))
    (if (not code)
        (try-completion string candidates predicate)
      (all-completions string candidates predicate))))

(defun helm-xgtags--format-complete-cmd (type)
  "Format compete command."
  (let* ((options (reverse (helm-xgtags--construct-options type t))))
    (setq helm-xgtags--complete-cmd (mapconcat 'identity options " "))))

;; TODO: add more actions, keymaps.
(defun helm-xgtags--read-tagname (type &optional tagname)
  (let ((prompt (assoc-default type helm-xgtags--prompt-alist))
        (helm-xgtags--complete-source
         (helm-build-async-source "xgtags"
           :header-name (lambda (name)
                          (concat name "(C-c ? Help)"))
           :candidates-process 'helm-xgtags--collect-candidates
           ;; :filter-one-by-one 'helm-xgtags--filter-one-by-one
           :nohighlight t
           :candidate-number-limit 9999
           :help-message 'helm-xgtags--help-message
           :history 'helm-xgtags--completing-history
           :action (lambda (cand)
                     cand)
           :requires-pattern 2)))
    (when (and (not tagname) tagname)
      (setq tagname tagname))
    (when tagname
      (setq prompt (format "%s(default \"%s\") " prompt tagname)))
    (helm-xgtags--format-complete-cmd type)
    (helm
     :sources 'helm-xgtags--complete-source
     :buffer prompt
     :default nil
     :input nil
     :keymap helm-xgtags--map
     :history 'helm-xgtags--history
     :truncate-lines t)))


(defun* helm-xgtags--find-with (&key
                           (type 'definition)
                           (get-token 'helm-xgtags--token-at-point)
                           (dont-confirm t))
  (let* ((tagname (funcall get-token))
         (input (if (and dont-confirm tagname) tagname
                  (helm-xgtags--read-tagname type tagname))))
    (helm-xgtags--goto-tag input type)))


(defun helm-xgtags-query-replace-regexp (to-string)
  "Run over the current *xgtags* buffer and to `query-replace-regexp' for each tag."
  (interactive
   (list (read-from-minibuffer (format "Replace <%s> with: "
                                       (helm-xgtags--tag-query
                                        (save-excursion
                                          (set-buffer (helm-xgtags--get-buffer))
                                          (get-text-property (point) 'helm-xgtags-tag))))
                               nil nil nil
                               query-replace-to-history-variable nil t)))
  (helm-xgtags--map-tags
   (lambda (match)
     (query-replace-regexp match to-string nil
                           (point)
                           (save-excursion (end-of-line) (point))))))

(defun helm-xgtags--switch-buffer (other-window jump-to-start-p)
  (with-xgtags-buffer (:buffer buffer)
    (when jump-to-start-p
      (goto-char (point-min)))
    (if other-window
        (switch-to-buffer-other-window buffer)
      (switch-to-buffer buffer))))

(defun helm-xgtags-switch-to-buffer (&optional jump-to-start-p)
  (interactive "P")
  (helm-xgtags--switch-buffer nil jump-to-start-p))

(defun helm-xgtags-switch-to-buffer-other-window (&optional jump-to-start-p)
  (interactive "P")
  (helm-xgtags--switch-buffer t jump-to-start-p))

(defvar helm-xgtags-mode-map
  (let ((keymap (make-sparse-keymap))
        (sub-keymap (make-sparse-keymap)))
    (define-key keymap "\C-cs" sub-keymap)
    (define-key sub-keymap "c" 'helm-xgtags-find-reference)
    (define-key sub-keymap "s" 'helm-xgtags-find-symbol)
    (define-key sub-keymap "g" 'helm-xgtags-find-pattern)
    (define-key sub-keymap "h" 'helm-xgtags--activate)
    (define-key sub-keymap "n" 'helm-xgtags-select-next-tag)
    (define-key sub-keymap "p" 'helm-xgtags-select-prev-tag)
    (define-key sub-keymap "u" 'helm-xgtags-pop-stack)
    (define-key sub-keymap "f" 'helm-xgtags-find-file)
    (define-key sub-keymap "x" 'helm-xgtags-switch-to-buffer)
    (define-key sub-keymap "r" 'helm-xgtags-query-replace-regexp)
    keymap)
  "Keymap used in helm-xgtags minor mode.")


;;; definition and support for the minor mode

(easy-menu-define helm-xgtags-menu
  helm-xgtags-mode-map
  "xgtags menu"
  '("XGtags"
    [ "Find Tag" helm-xgtags-find-definition t ]
    [ "Find Tag Reference" helm-xgtags-find-reference t ]
    [ "Find Symbol" helm-xgtags-find-symbol t ]
    [ "Find Pattern" helm-xgtags-find-pattern t ]
    "-----------"
    [ "Find Previous Tag" helm-xgtags-select-prev-tag t ]
    [ "Find Next Tag" helm-xgtags-select-next-tag t ]
    [ "Query-replace Tag" helm-xgtags-query-replace-regexp t ]
    "-----------"
    [ "Find File" helm-xgtags-find-file t ]
    "-----------"
    [ "Parse File" helm-xgtags-parse-file t ]
    [ "Visit Tags Directory" helm-xgtags-visit-rootdir t ]))

;;;###autoload
(define-minor-mode helm-xgtags-mode
  "Toggle helm-xgtags-mode, a minor mode for browsing source code using GLOBAL.

Input tag name and move to the definition.
	\\[xgtags-find-definition]
Input tag name and move to the referenced point.
	\\[xgtags-find-reference]
Input symbol and move to the locations.
	\\[xgtags-find-symbol]
Input pattern, search with grep(1) and move to the locations.
	\\[xgtags-find-pattern]
Input pattern and move to the top of the file.
	\\[xgtags-find-file]
Get the expression as a tagname around here and move there.
	\\[xgtags-find-definition-from-here]
Move to previous point on the stack.
	\\[xgtags-pop-stack]

Key definitions:
\\{xgtags-mode-map}
Turning on helm-xgtags-mode calls the value of the variable `xgtags-mode-hook'
with no args, if that value is non-nil."
  :group      'helm-xgtags
  :init-value nil
  :global     nil
  :keymap     helm-xgtags-mode-map
  :lighter    helm-xgtags-minor-mode-text

  (if helm-xgtags-mode
      (when helm-xgtags-auto-update-db
        (add-hook 'after-save-hook 'helm-xgtags-update-tags nil t))
    (when helm-xgtags-auto-update-db
      (remove-hook 'after-save-hook 'helm-xgtags-update-tags t))))

;;; definition and support for the selection mode

(define-derived-mode helm-xgtags-select-mode fundamental-mode helm-xgtags-select-mode-name
  "Major mode for choosing a tag from tags list.

Select a tag in tags list and move there.
	\\[xgtags-select-tag-near-point]
Move to previous point on the stack.
	\\[xgtags-pop-stack]

Key definitions:
\\{xgtags-select-mode-map}
Turning on helm-xgtags-select mode calls the value of the variable
`xgtags-select-mode-hook' with no args, if that value is non-nil."
  (setq buffer-read-only t
        truncate-lines t)
  (goto-char (point-min)))

 ;; misc
(defun helm-xgtags-headerp ()
  "Return header file or nil."
  (save-excursion
    (beginning-of-line)
    (and (looking-at
          "^\\s-*#\\s-*\\(?:include\\|import\\)\\s-*[\"<]\\(?:[./]*\\)?\\(.*?\\)[\">]")
         (buffer-substring-no-properties (match-beginning 1) (match-end 1)))))
(defun helm-xgtags--real-file-name ()
  (let ((buffile (buffer-file-name)))
    (unless buffile
      (error "This buffer is not related to file."))
    (if (file-remote-p buffile)
        (tramp-file-name-localname (tramp-dissect-file-name buffile))
      (file-truename buffile))))


;; Database related functions...
(defun helm-xgtags-update-single(filename)
  "Update Gtags database for changes in a single file"
  (interactive)
  (start-process "update-gtags" "update-gtags" "bash" "-c"
                 (concat "cd " (gtags-root-dir) " ; gtags --single-update "
                         filename )))

(defun helm-xgtags-update-current-file()
  (interactive)
  (let ((filename (replace-regexp-in-string (gtags-root-dir) "."
                                            (buffer-file-name (current-buffer)))) )
    (gtags-update-single filename)
    (message "Gtags updated for %s" filename)))

(defun helm-xgtags-update-hook()
  "Update GTAGS file incrementally upon saving a file"
  (when helm-xgtags-mode
    (when (gtags-root-dir)
      (gtags-update-current-file))))

(defun helm-xgtags--read-tag-directory ()
  (let ((dir (read-directory-name "Directory tag generated: " nil nil t)))
    ;; On Windows, "gtags d:/tmp" work, but "gtags d:/tmp/" doesn't
    (directory-file-name (expand-file-name dir))))

(defsubst helm-xgtags--how-to-update-tags ()
  (cl-case (prefix-numeric-value current-prefix-arg)
    (4 'entire-update)
    (16 'generate-other-directory)
    (otherwise 'single-update)))

(defun helm-xgtags--update-tags-command (how-to)
  (cl-case how-to
    (entire-update '("global" "-u"))
    (generate-other-directory (list "gtags" (helm-xgtags--read-tag-directory)))
    (single-update (list "global" "--single-update" (helm-xgtags--real-file-name)))))

(defun helm-xgtags--update-tags-p (how-to interactive-p current-time)
  (and
   (= (call-process "global" nil nil nil "-p") 0)
   (or interactive-p
             (and (eq how-to 'single-update)
                  (buffer-file-name)
                  (or (not helm-xgtags-update-interval-second)
                      (>= (- current-time helm-xgtags--last-update-time)
                          helm-xgtags-update-interval-second))))))

(defmacro helm-xgtags--make-gtags-sentinel (action)
  `(lambda (process _event)
    (when (eq (process-status process) 'exit)
      (if (zerop (process-exit-status process))
          (message "Success: %s TAGS" ,action)
        (message "Failed: %s TAGS(%d)" ,action (process-exit-status process))))))


;; Functions can be autoloaded...

;;;###autoload
(defun helm-xgtags-find-definition ()
  "Input tag name and move to the definition."
  (interactive)
  (helm-xgtags--find-with))

;;;###autoload
(defun helm-xgtags-find-reference ()
  "Input tag name and move to the referenced point."
  (interactive)
  (helm-xgtags--find-with :type 'reference))

;;;###autoload
(defun helm-xgtags-find-symbol ()
  "Find tag that is a reference without a definition."
  (interactive)
  (helm-xgtags--find-with :type 'symbol))

;;;###autoload
(defun helm-xgtags-find-pattern ()
  "Input pattern, search with grep(1) and move to the locations."
  (interactive)
  (helm-xgtags--find-with :type 'pattern))

;;;###autoload
(defun helm-xgtags-find-file ()
  "Input pattern and move to the top of the file."
  (interactive)
  (helm-xgtags--find-with :type 'path))

;;;###autoload
(defun helm-xgtags-find-header ()
  "Open header file under cursor."
  (interactive)
  (if (helm-xgtags-headerp)
      (helm-xgtags--find-with
       :type 'path
       :get-token (lambda ()(helm-xgtags-headerp))
       :dont-confirm t)
    (error "Not a header file...")))

;;;###autoload
(defun helm-xgtags-find-definition-from-here ()
  "Get the expression as a tagname around here and move there."
  (interactive)
  (let ((tagname (helm-xgtags--token-at-point)))
    (when tagname
      (helm-xgtags--goto-tag tagname (cond ((not (helm-xgtags--function-p)) 'symbol)
                                      ((helm-xgtags--definition-p) 'reference)
                                      (t nil))))))
;;;###autoload
(defun helm-xgtags-pop-stack ()
  "Move to previous point on the stack."
  (interactive)
  (let ((delete (and helm-xgtags-kill-buffers
                     (not (helm-xgtags--stacked-p (current-buffer)))))
        (context (helm-xgtags--pop-context)))
    (assert context nil "The tags stack is empty")
    (when delete
      (kill-buffer (current-buffer)))
    (helm-xgtags--update-buffer context)
    (switch-to-buffer (helm-xgtags--context-buffer context))
    (goto-char (helm-xgtags--context-point context))))

;;;###autoload
(require 'which-func)
(defun helm-xgtags-parse-file (&optional filename)
  "Input FILENAME, parse it and show object list."
  (interactive)
  (let* ((input (or filename
                   (read-file-name "Parse file: "
                                   nil nil t
                                   (file-name-nondirectory buffer-file-name))))
         (fn (which-function))
         (helm-xgtags--selected-tag
          (if fn
              (helm-xgtags--make-tag
               :query (which-function)
               :line 1
               :file input))))
    (assert (not (equal input "")) nil "No file specified")
    (helm-xgtags--goto-tag (expand-file-name input) 'file)))

;;;###autoload
(defun helm-xgtags-update-tags ()
  "Update TAG file.
Update All files with `C-u' prefix.
Generate new TAG file in selected directory with `C-u C-u'."
  (interactive)
  (let ((how-to (helm-xgtags--how-to-update-tags))
        (interactive-p (called-interactively-p 'interactive))
        (current-time (float-time (current-time))))
    (when (helm-xgtags--update-tags-p how-to interactive-p current-time)
      (let* ((cmds (helm-xgtags--update-tags-command how-to))
             (proc (apply 'start-file-process "xgtags-update-tag" nil cmds)))
        (if (not proc)
            (message "Failed: %s" (mapconcat 'identity cmds " "))
          (set-process-sentinel proc (helm-xgtags--make-gtags-sentinel 'update))
          (setq helm-xgtags--last-update-time current-time))))))

 ;; helm support
(defun helm-xgtags--format-pased-files (tag)
  "Format parsed files."
  (let ((file (helm-xgtags--tag-file tag))
        (query (helm-xgtags--tag-query tag))
        (line (number-to-string (helm-xgtags--tag-line tag)))
        (match (helm-xgtags--tag-match tag))
        (selected-p (eq tag helm-xgtags--selected-tag)))
    (concat
     (propertize line 'face
                 (if selected-p
                     'helm-xgtags-line-number-selected-face
                   'helm-xgtags-line-number-face))
     (if (>= (length line) 4)
         "\t" "\t\t")

     (propertize query 'face (if selected-p
                                 'helm-xgtags-match-selected-face
                               'helm-xgtags-match-face))
     (when match
       (format "\t\t%s"
               (propertize match 'face (if selected-p
                                           'helm-xgtags-line-selected-face
                                         'helm-xgtags-line-face)))))))

(defun helm-xgtags--format-tag (tag)
  (let ((file (helm-xgtags--tag-file tag))
        (query (helm-xgtags--tag-query tag))
        (line (helm-xgtags--tag-line tag))
        (match (helm-xgtags--tag-match tag))
        (selected-p (eq tag helm-xgtags--selected-tag)))
    (concat
     (format "%s:%s\t"
             (propertize file 'face
                         (if selected-p
                             'helm-xgtags-file-selected-face
                           'helm-xgtags-file-face))
             (propertize (number-to-string line) 'face
                         (if selected-p
                             'helm-xgtags-line-number-selected-face
                           'helm-xgtags-line-number-face)))
     (when match
       (format "\t\t%s"
               (propertize match 'face (if selected-p
                                           'helm-xgtags-line-selected-face
                                         'helm-xgtags-line-face)))))))

(defun helm-xgtags--candidate-transformer (tag &optional escape)
  "Function to format TAG into a readable string.
If ESCAPE is t, try to escape special characters."
  (let* ((context (car helm-xgtags--stack))
         (str (if (and tag context)
                  (cl-case (helm-xgtags--context-type context)
                    (file (helm-xgtags--format-pased-files tag))
                    (otherwise (helm-xgtags--format-tag tag))))))
    (if (and str escape)
        (rx-to-string str)
        str)))

(defvar helm-source-xgtags-parse-tags nil "A.")

(setq helm-source-xgtags-parse-tags
      (helm-build-sync-source "Helm-Xgtags"
        :init nil
        :candidates 'helm-xgtags--tags
        :real-to-display 'helm-xgtags--candidate-transformer
        :fuzzy-match nil
        :action (lambda (cand)
                  (helm-xgtags--select-and-follow-tag cand))))

;; TODO: add more actions, keymaps.
(defun helm-xgtags--activate (&optional fn)
  "Active helm."
  (interactive)
  ;; (helm-attrset 'name (concat "GNU Global at " "TODO: "))
  (helm :sources '(helm-source-xgtags-parse-tags)
        :buffer "*helm helm-xgtags*"
        :preselect (if fn fn
                     (helm-xgtags--candidate-transformer helm-xgtags--selected-tag t))))

(defvar helm-xgtags--map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-<down>") 'helm-goto-next-file)
    (define-key map (kbd "M-<up>")   'helm-goto-precedent-file)
    (define-key map (kbd "C-c o")    'helm-grep-run-other-window-action)
    (define-key map (kbd "C-c C-o")  'helm-grep-run-other-frame-action)
    (define-key map (kbd "C-w")      'helm-yank-text-at-point)
    (define-key map (kbd "C-x C-s")  'helm-grep-run-save-buffer)
    (when helm-grep-use-ioccur-style-keys
      (define-key map (kbd "<right>")  'helm-execute-persistent-action)
      (define-key map (kbd "<left>")  'helm-grep-run-default-action))
    (delq nil map))
  "Keymap used in Grep sources.")

(defvar helm-xgtags--last-cmd-line nil "Nil.")

;;; Faces
;;
;;
(defgroup helm-xgtags--faces nil
  "Customize the appearance of helm-grep."
  :prefix "helm-"
  :group 'helm-grep
  :group 'helm-faces)

(defface helm-xgtags--match
  '((((background light)) :foreground "#b00000")
    (((background dark))  :foreground "gold1"))
  "Face used to highlight grep matches."
  :group 'helm-xgtags--faces)

(defface helm-xgtags--file
  '((t (:foreground "BlueViolet"
                    :underline t)))
  "Face used to highlight grep results filenames."
  :group 'helm-xgtags--faces)

(defface helm-xgtags--lineno
  '((t (:foreground "Darkorange1")))
  "Face used to highlight grep number lines."
  :group 'helm-xgtags--faces)

(defface helm-xgtags--running
  '((t (:foreground "Red")))
  "Face used in mode line when grep is running."
  :group 'helm-xgtags--faces)

(defface helm-xgtags--finish
  '((t (:foreground "Green")))
  "Face used in mode line when grep is finish."
  :group 'helm-xgtags--faces)

(defface helm-xgtags--cmd-line
  '((t (:inherit diff-added)))
  "Face used to highlight grep command line when no results."
  :group 'helm-xgtags--faces)

(defvar helm-xgtags--complete-cmd nil "Nil.")

;; TODO: start filtering when pressed space...
(defun helm-xgtags--collect-candidates ()
  (let* ((cmd (concat helm-xgtags--complete-cmd " " helm-pattern)))
    ;; Start grep process.
    (helm-log "Starting global process in directory `%s'" default-directory)
    (helm-log "Command line used was:\n\n%s"
              (concat ">>> " (propertize cmd 'face 'helm-xgtags--cmd-line) "\n\n"))
    (prog1
        (start-file-process-shell-command
         "global" helm-buffer cmd)
      ;; Init sentinel.
      (set-process-sentinel
       (get-buffer-process helm-buffer)
       #'(lambda (process event)
           (let ((noresult (= (process-exit-status process) 1)))
             (unless noresult
               (helm-process-deferred-sentinel-hook
                process event helm-ff-default-directory))
             (cond (noresult
                    (with-current-buffer helm-buffer
                      (insert (concat "* Exit with code 1, no result found,"
                                      " Command line was:\n\n "
                                      (propertize helm-xgtags--last-cmd-line
                                                  'face 'helm-xgtags--cmd-line)))
                      (setq mode-line-format
                            '(" " mode-line-buffer-identification " "
                              (:eval (format "L%s" (helm-candidate-number-at-point))) " "
                              (:eval (propertize
                                      "[global process finished - (no results)] "
                                      'face 'helm-xgtags--finish))))))
                   ((string= event "finished\n")
                    (with-helm-window
                      (setq mode-line-format
                            '(" " mode-line-buffer-identification " "
                              (:eval (format "L%s" (helm-candidate-number-at-point))) " "
                              (:eval (propertize
                                      "[global process finished - (no results)] "
                                      'face 'helm-xgtags--finish))))
                      (force-mode-line-update)))
                   ;; Catch error output in log.
                   (t (helm-log
                       "Error: global %s"
                       (replace-regexp-in-string "\n" "" event))))))))))

(defun helm-xgtags--filter-one-by-one (arg)
  arg
  )

(defun helm-xgtags--help-message ()
  "description"
  (interactive)

  )


(provide 'helm-xgtags)

;;; helm-xgtags.el ends here
