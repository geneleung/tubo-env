;;; logviewer.el -- Simple log viewer.

;; Author: YangYingchao <yangyingchao@gmail.com>
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;;
;;; Commentary:
;;
;;   This is a simple log viewer, with syntax highlight.
;;
;;   To use logviewer, you should put logviewer.el into the top of load-path
;; of emacs, the add following lines into your .emacs:
;; (require 'logviewer)
;;
;;   When log files are huge, it will try to split huge logs into small ones
;; to speed up loading. In that case, you can press "n" & "p" to go to next
;; part (or previous part) to the log file. You can custom variable
;; logviewer-split-line to proper number to control the size of the slice of
;; huge file.
;;

;;; Code:

(defcustom logviewer-log-pattern (rx "." (or "LOG" "log" "Log"))
  "Pattern to decide if a file is a log file."
  :type 'string
  :group 'logviewer
  )

(defcustom logviewer-tmp-dir "/tmp/emacs-logviewer/"
  "Temporary directory to hold splited files."
  :type 'string
  :group 'group
  )

(defcustom  logviewer-split-line 50000
  "Lines when trying to split files."
  :type 'integer
  :group 'group
  )


;; default mode map, really simple
(defvar logviewer-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "n"
      (lambda (&optional arg)
        (interactive "^p")
        (logviewer-next-part t (or arg 1))))
    (define-key keymap "p"
      (lambda (&optional arg)
        (interactive "^p")
        (logviewer-next-part nil (or arg 1))))
    (define-key keymap "R" 'logviewer-reload-file)
    (define-key keymap "F" 'logviewer-set-filter)
    keymap)
  "Keymap for logviewer mode.")

(defvar logviewer-font-lock-keywords
  `(
    ;; Date & time.
    (,(rx symbol-start
          (group (or "ERROR" "FATAL" "error" "fatal" "WARNING" "warning")) ":"
          (group (+ (*? not-newline))) line-end)
     (1 font-lock-warning-face) (2 font-lock-comment-face))
    (,(rx line-start
          (*? not-newline) (+ digit) ":" (+ digit) ":" (+ digit)
          (? "." (+ digit)))
     . font-lock-builtin-face)
    (,(rx symbol-start
          (group (*? not-newline) (+ digit) ":" (+ digit) ":" (+ digit) (? (or "." "-") (+ digit)))
          (1+ space) (group (+? (or alnum "-" "_"  blank))) (? "["(* digit) "]")":")
     (1 font-lock-builtin-face) (2 font-lock-variable-name-face))

    (,(rx symbol-start
          (group (or "info" "INFO" )) ":"
          (group (+ (*? not-newline))) line-end)
     (1 font-lock-function-name-face))
    (,(rx symbol-start
          (group (or "DEBUG" "debug" )) ":"
          (group (+ (*? not-newline))) line-end)
     (1 font-lock-keyword-face) (2 font-lock-doc-face))
    ))

;; (defvar logviewer-mode-syntax-table (make-syntax-table)
;;   "Syntax table for Logviewer mode")
;; (modify-syntax-entry ?# "<" logviewer-mode-syntax-table)
;; (modify-syntax-entry ?' "\"" logviewer-mode-syntax-table)
;; (modify-syntax-entry ?- "w" logviewer-mode-syntax-table)
;; (modify-syntax-entry ?.  "_" logviewer-mode-syntax-table)
;; (modify-syntax-entry ?:  "_" logviewer-mode-syntax-table)
;; (modify-syntax-entry ?\\ "_" logviewer-mode-syntax-table)
;; (modify-syntax-entry ?\n ">" logviewer-mode-syntax-table)
;; (modify-syntax-entry ?_  "w" logviewer-mode-syntax-table)
;; (modify-syntax-entry ?` "\\" logviewer-mode-syntax-table)
;; (modify-syntax-entry ?{ "(}" logviewer-mode-syntax-table)
;; (modify-syntax-entry ?} "){" logviewer-mode-syntax-table)
;; (modify-syntax-entry ?[ "(]" logviewer-mode-syntax-table)
;;                      (modify-syntax-entry ?] ")["
;;                      logviewer-mode-syntax-table)
;; (modify-syntax-entry ?( "()" logviewer-mode-syntax-table)
;;                      (modify-syntax-entry ?) ")("
;;                      logviewer-mode-syntax-table)


(defvar logviewer-current-file nil
  "Log file viewed by logviewer")

(defun logviewer-process-sentinel (process event)
  "description"
  (when (memq (process-status process) '(signal exit))
    (let* ((exit-status       (process-exit-status process))
           (command           (process-command process))
           (source-buffer     (process-buffer process))
           )

      (condition-case err
          (delete-process process)
        (error
         (let ((err-str (format "Error in process sentinel: %s"
                                 (error-message-string err))))
           (message err-str)))))))

(defvar logviewer-slice-hash
  (make-hash-table :test 'equal :size 64)
  "Current slice.")


(defun split-and-view-log (filename)
  "Split file specified by FILENAME and view the splits."
  (unless (file-exists-p logviewer-tmp-dir)
    (mkdir logviewer-tmp-dir t))
  (unless (car (file-attributes logviewer-tmp-dir))
    (error "Temporary directory (%s) is not a DIR!" logviewer-tmp-dir))

  (set (make-local-variable 'logviewer-current-file) filename)
  (let* ((bname (file-name-sans-extension (file-name-nondirectory filename)))
         (first-slice (format "%s/%s_%04d" logviewer-tmp-dir bname 0))
         (process
          (start-process "Split-process" "*logviewer*"
                         (executable-find "split")
                         "--suffix-length=4"
                         "-d" "-l"
                         (format "%d" logviewer-split-line)
                         (expand-file-name filename)
                         (format "%s/%s_" logviewer-tmp-dir bname))))

    (set-process-sentinel process  'logviewer-process-sentinel)
    (while (not (file-exists-p first-slice))
      (sleep-for 0.5))
    (puthash filename (cons bname 0) logviewer-slice-hash)
    (set-buffer (get-buffer-create bname))
    (erase-buffer)
    (insert-file-contents first-slice nil)
    (switch-to-buffer bname)
    (logviewer-mode)
    (error "See this instead")))

(advice-add
 'abort-if-file-too-large :around
 (lambda (func &rest args)
   (let ((size (car args))
         (op-type (cadr args))
         (filename (nth 2 args)))
     (if (string-match logviewer-log-pattern filename)
         (if (and (string= op-type "open")
                  (executable-find "split")
                  large-file-warning-threshold size
                  (> size large-file-warning-threshold))
             (if (y-or-n-p
                  (format "LogFile %s is large (%dMB), really %s? "
                          (file-name-nondirectory filename)
                          (/ size 1048576) op-type))
                 (split-and-view-log filename)
               (error "Abort")))
         (apply func args)))))

(defun logviewer-is-tmpfile ()
  "See whether current file is a temporary file or not."
  (if (string-match "log_cache" logviewer-current-file)
      t
    nil))

(defun logviewer-reload-file ()
  "Reload current file."
  (interactive)
  (let ((pt (point)))
    (read-only-mode -1)
    (erase-buffer)
    (insert-file-contents logviewer-current-file nil)
    (read-only-mode 1)
    (goto-char pt)
    (message "Readload finished.")))

(defun logviewer-next-part (next &optional num)
  "View next/previous file.
If NEXT = t, it returns next file, or it returns previous file.
NUM: prefix."
  (interactive "^p")
  (unless logviewer-current-file
    (error "Not a slice!"))

  (let* ((p (gethash logviewer-current-file logviewer-slice-hash))
         (bname (car p))
         (index (cdr p))
         (func (if next '+ '-))
         slice)
    (unless (and bname index)
        (error "Something wrong with global hash..."))

    (setq index (funcall func index (or num 1)))
    (let ((slice (format "%s/%s_%04d" logviewer-tmp-dir bname index)))
      (if (file-exists-p slice)
          (progn
            (puthash logviewer-current-file (cons bname index) logviewer-slice-hash)
            (message (format "Now viewing: %s -- %d" logviewer-current-file index))
            (read-only-mode -1)
            (insert-file-contents slice nil nil nil t)
            (read-only-mode 1))
        (error "%s dost not exist: %s of file reached.." slice
               (if next "Tail"  "Head"))))))


(defconst logviewer-levels
  '("FATAL" "ERROR" "WARRNING" "INFO" "DEBUG"))

(defvar logviewer-filter-level 9 "nil")

(defun get-lvl-str (num)
  "description"
  (let ((x (/ num 2))
        (lst nil))
    (while (>= x 0 )
      (setq x (1- x))
      (add-to-list  'lst (nth x logviewer-levels))
      )
    lst
    )
  )


(defun logviewer-get-filter (lvl)
  "Get filter beyond LVL."
  (if (string= lvl "FATAL")
      (progn
        (setq logviewer-filter-level 1)
        (rx bow "FATAL:"))

    (if (string= lvl "ERROR")
      (progn
        (setq logviewer-filter-level 3)
        (rx bow (or "FATAL" "ERROR") ":"))
      (if (string= lvl "WARRNING")
          (progn
            (setq logviewer-filter-level 7)
            (rx bow (or "FATAL" "ERROR" "WARRNING") ":"))
        (if (string= lvl "INFO")
            (progn
              (setq logviewer-filter-level 9)
              (rx bow (or "FATAL" "ERROR" "WARRNING" "INFO") ":")  ))
        )
      )
    )
  )

(defvar logviewer-filter-list '() "nil")

(defun logviewer-iter (reg-str)
  ""
  (if (search-forward-regexp reg-str (point-max) t)
      (progn
        (let ((pos1)
              (pos2))
          (move-beginning-of-line 1)
          (setq pos1 (point))
          (move-end-of-line 1)
          (setq pos2 (point))
          (cons pos1 pos2)
          (add-to-list 'logviewer-filter-list (cons pos1 pos2))
          (logviewer-iter reg-str)
          )
        )
    nil
      )
  )

(defun logviewer-set-filter ()
  "Set and show result of filter lvl"
  (interactive)
  (setq logviewer-filter-list nil)
  (let ((lvl nil)
        (cur-lvl   logviewer-filter-level ))
    (setq lvl (completing-read "Filter Level: " logviewer-levels))
    (if (string= lvl "DEBUG")
        (outline-flag-region (point-min) (point-max) nil)
      (progn
        (let ((logviewer-filter (logviewer-get-filter lvl))
              (content (buffer-substring-no-properties
                        (point-min) (point-max))))
          (if (< cur-lvl logviewer-filter-level)
              (outline-flag-region (point-min) (point-max) nil)
            )

          (goto-char (point-min))
          (logviewer-iter logviewer-filter)

          (outline-flag-region (point-min) (point-max) t)
          (if (> (length logviewer-filter-list) 0)
              (let ((i 0)
                    (len (length logviewer-filter-list))
                    (frange))
                (while (< i len)
                  (setq frange (nth i logviewer-filter-list))
                  (outline-flag-region (car frange) (1+ (cdr frange)) nil)
                  (setq i (1+ i))
                  ))
            )
          )))
    )
  )


;;;###autoload
(define-derived-mode logviewer-mode fundamental-mode "Log-Viewer"
  "Major mode for editing Logviewer files
Key definitions:
\\{logviewer-mode-map}"
                                        ; Setup font-lock mode.
  (set (make-local-variable 'font-lock-defaults) '(logviewer-font-lock-keywords))
  ;; (set-syntax-table logviewer-mode-syntax-table)
  (setq buffer-read-only t)
  (unless logviewer-current-file
    (setq logviewer-current-file (buffer-file-name))
    )
    (run-hooks 'logviewer-mode-hook))

(add-to-list 'auto-mode-alist '("\\.log\\'" .
                                logviewer-mode))
(provide 'logviewer)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; logviewer.el ends here
