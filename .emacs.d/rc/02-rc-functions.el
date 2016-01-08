;;; 02-rc-functions.el -- Brief introduction here.

;; Author: YangYingchao <yangyingchao@gmail.com>

;;; Commentary:

;;; Code:

(defvar YC-DEBUG nil "flag to debug or not")
(defconst YC-DEBUG-BUF "*YC-DEBUG*" "Debug buffer of my own.")
(defvar yc/debug-msg nil "A variable to be used for debuging in *scratch*")

 ;; Macros

(defmacro sif (sym &optional val)
  "Set SYM to VAL if SYM exists."
  `(when (boundp ',sym)
     (setq ,sym ,val)))

(defmacro csq (sym val)
  "Customize or Set value of SYM to VAL."
  `(funcall (or (get ',sym 'custom-set)
                'set-default)
            ',sym ,val))

(defmacro cdsq (sym val &optional doc)
  "Customize, Define or Set value of SYM to VAL, with DOC as document."
  `(if (boundp ',sym)
       (csq ,sym ,val)
     (defvar ,sym ,val ,doc)))

(defmacro dsq (sym val &optional doc)
  "Define or Set value of  value of SYM to VAL, with DOC as document."
  `(if (boundp ',sym)
       (setq ,sym ,val)
     (defvar ,sym ,val ,doc)))

(defmacro check-symbol (sym)
  "Return value or nil"
  `(if (boundp ',sym) ,sym nil))

(defmacro aif (test-form then-form &rest else-forms)
  "Like `if' but set the result of TEST-FORM in a temprary variable called `it'.
THEN-FORM and ELSE-FORMS are then excuted just like in `if'."
  (declare (indent 2) (debug t))
  `(let ((it ,test-form))
     (if it ,then-form ,@else-forms)))

(defmacro PDEBUG (&rest msgs)
  "Output msgs with file and line..."
  `(let ((ots debug-ts)
         (cts (current-time)))
     (setq debug-ts cts) ;; update timestamp.
     (message "%s(%d) - (%.02f): %s"
              ,(if (buffer-file-name)
                   (file-name-nondirectory (buffer-file-name)) "Unknown")
              (line-number-at-pos)
              (if ots  (float-time (time-since ots)) 0)
              (condition-case ()
                  (apply 'format (list ,@msgs))
                (error
                 (concat "Failed to format message:"
                         (with-temp-buffer
                           (print (list ,@msgs) (current-buffer))
                           (replace-regexp-in-string
                            "^\n" ""
                            (buffer-substring-no-properties (point-min) (point-max))
                            ))))))))

(defmacro yc/eval-cost (tip &rest args)
  `(let ((ts (current-time))
         (ret ,@args))
     (message "%s cost: %.02f" ,tip (float-time (time-since ts)))
     ret))

(defmacro load-and-bind (sym file key &optional map)
  "load symbol and bind to key map."
  `(progn
     (autoload ,sym ,file "" t)
     (define-key (or ,map  (current-global-map)) ,key ,sym)))

(defmacro yc/time (&rest args)
  `(let ((timestamp (current-time))
         (tip ,@args))
     (message "`%s' takes %.2f seconds."
              tip (float-time (time-since timestamp)))))

(defmacro yc/eval-after-load (name &rest args)
  "Macro to set expressions in `arg` to be executed after `name` is loaded."
  `(eval-after-load ,name
     ',(append (list 'progn
                     `(let ((ts (current-time)))
                        (message "Loading configuration for: %s..." ,name)
                        ,@args
                        (message "Configuration for %s finished in %.2f seconds" ,name
                                 (float-time (time-since ts ))))))))

(defmacro yc/autoload (func &rest args)
  "Wrapper to autoload FUNC.
ARGS provide extra information: first element in ARGS specifies whether this is an
  interactive funtion, second element in ARGS specifies filename where to load this
  FUNC."
  `,(list 'autoload func
          (cond
           ((stringp (car args)) (car args))
           ((stringp (cadr args)) (cadr args))
           (t `(symbol-name ,func)))
          "autoloaded function."
          (if (or (stringp (car args))
                  (not args)) t (car args))))

(defmacro yc/add-keyword (sym type)
  `(font-lock-add-keywords
    nil (list
         (list ,sym 1 ,type t ))))

(defmacro yc/set-mode (mode expr)
  "Set MODE EXPR into `auto-mode-alist."
  `(add-to-list 'auto-mode-alist
                (cons ,expr ,mode)))

(defmacro yc/with-prefix (func arg)
  "Call FUNC with ARG as prefix."
  `(lambda ()
     (interactive)
     (let ((current-prefix-arg ,arg))
       (call-interactively ,func))))

(defmacro yc/func-wrapper (func file)
  "Autoload func form file if necessary"
  `(lambda()
     (interactive)
     (unless (fboundp ,func)
       (load ,file))
     (funcall ,func)))
 ;; Functions

;;; Start debug on error.
(custom-set-variables
 '(debug-on-error nil))

(defun yc/toggle-debug ()
  "Toggle debug mode."
  (interactive)
  (setq YC-DEBUG (not YC-DEBUG))
  (setq debug-on-error YC-DEBUG
        helm-debug YC-DEBUG
        debug-on-quit YC-DEBUG))

(defun yc/debug-log (msg)
  "Out put debug message based on YC-DEBUG"
  (when YC-DEBUG
    (let ((buf (get-buffer-create YC-DEBUG-BUF)))
      (setq yc/debug-msg msg)
      (switch-to-buffer-other-window buf)
      (goto-char (point-max))
      (princ "\n\n" buf)
      (princ (format-time-string current-date-time-format (current-time)) buf)
      (princ " ======>\n" buf)
      (princ msg buf)
      (princ "\n" buf)
      (goto-char (point-max)))))

(defalias 'yc/debug 'yc/debug-log)

(defvar debug-ts nil "Timestamp used by PDEBUG.")

(defun reload-file ()
  (interactive)
  (save-excursion
    (find-file (buffer-file-name))))

(defun reload-all-files ()
  (interactive)
  (save-excursion
    (let ((fn nil)
          (noconfirm t)
          (cur-buffer (current-buffer)))
      (dolist (buffer (buffer-list))
        (setq fn (buffer-file-name buffer))
        (when (and fn;; reload buffer if it is file or dir.
                   (not (verify-visited-file-modtime buffer)))
          (if (not (file-exists-p fn))
              (when (yes-or-no-p (format "File %s does not exist, delete buffer?" fn))
                (kill-buffer buffer))
            (message (format "Reloading file %s ..." (file-name-nondirectory fn)))
            (if (buffer-modified-p buffer)
                (setq noconfirm nil))
            (switch-to-buffer buffer)
            (revert-buffer t noconfirm))
          ))
      (switch-to-buffer cur-buffer)
      (message "All buffer reloaded..."))))

(defalias 'rlf 'reload-all-files)

(autoload 'hippie-expand "hippie-exp" ""  t)
(defun indent-or-complete ()
  "Complete if point is at end of a word, otherwise indent line."
  (interactive)
  (if (looking-at "\\>")
      (progn
        (message "Completing")
        (hippie-expand nil))
    (indent-for-tab-command)))


;; *********** Fuctions for edit special rc-files quickly ************

(defun edit-emacs ()
  "Edit Emacs configuration."
  (interactive)
  (find-file "~/.emacs"))


;; ******************** Others ***************************************

(defun load-this-file ()
  (interactive)
  (load-file (buffer-name)))

(defun compile-this-file ()
  "Function to byte-compile current file."
  (interactive)
  (byte-compile (buffer-name)))

(defun lazy-set-key (key-alist &optional keymap key-prefix)
  "This function is to little type when define key binding.
`KEYMAP' is a add keymap for some binding, default is `current-global-map'.
`KEY-ALIST' is a alist contain main-key and command.
`KEY-PREFIX' is a add prefix for some binding, default is nil."
  (let ((map (if keymap keymap (current-global-map)))
        (prefix (if key-prefix (concat key-prefix " ") "")))

    (dolist (element key-alist)
      (let ((key (car element))
            (def (cdr element)))
        (cond ((stringp key) (setq key (read-kbd-macro (concat prefix key))))
              ((vectorp key) nil)
              (t (signal 'wrong-type-argument (list 'array key))))
        (define-key map key def)))))

(defalias 'yc/set-keys 'lazy-set-key)

(defun lazy-unset-key (key-list &optional keymap)
  "This function is to little type when unset key binding.
`KEYMAP' is add keymap for some binding, default is `current-global-map'
`KEY-LIST' is list contain key."
  (let (key)
    (or keymap (setq keymap (current-global-map)))
    (dolist (key key-list)
      (cond ((stringp key) (setq key (read-kbd-macro (concat key))))
            ((vectorp key) nil)
            (t (signal 'wrong-type-argument (list 'array key))))
      (define-key keymap key nil))))

;;;; dos-unix
(defun dos-unix ()
  (interactive)
  (save-excursion
    (if (and (buffer-file-name)
             (executable-find
              "dos2unix"))
        (let ((fn (buffer-file-name)))
          (call-process "dos2unix" nil nil nil fn)
          (revert-buffer t t))
      (goto-char (point-min))
      (while (search-forward "\r" nil t) (replace-match "")))))

(defun unix-dos ()
  (interactive)
  (save-excursion
    (if (and (buffer-file-name)
             (executable-find
              "unix2dos"))
        (let ((fn (buffer-file-name)))
          (call-process "unix2dos" nil nil nil fn)
          (revert-buffer t t))
      (goto-char (point-min))
      (while (search-forward "\n" nil t) (replace-match "\r\n")))))


;;;; 自动编译

(setq compilation-window-height 16)
(setq compilation-scroll-output t)

(setq compilation-finish-functions
      (lambda (buf str)
        (if (string-match "exited abnormally" str)

            ;;there were errors
            (message "compilation errors, F11 to goto next error.")

          ;;no errors, make the compilation window go away in 0.5 seconds
          ;;        (run-at-time 5.0 nil 'delete-windows-on buf)
          (message "NO COMPILATION ERRORS!"))))


(defcustom user-defined-opts nil
  "User dcustomized options when calling compiler"
  :group 'user)
(defun get-user-defined-opts ()
  "description"
  (if user-defined-opts
      user-defined-opts
    ""
    )
  )

(defun yc/get-cpu-number ()
  "Return CPU number."
  (case system-type
    ('gnu/linux
     (let ((cpuinfo (shell-command-to-string "cat /proc/cpuinfo | grep processor|wc -l"))
           (r-match-cpu (rx (+? digit ) eol))
           (cpu-number "1"))
       (if (string-match r-match-cpu cpuinfo)
           (setq cpu-number (match-string 0 cpuinfo)))
       (string-to-number cpu-number)))
    ('darwin
     (let ((cpuinfo (shell-command-to-string "sysctl -n hw.ncpu"))
           (r-match-cpu (rx (+? digit ) eol))
           (cpu-number "1"))
       (if (string-match r-match-cpu cpuinfo)
           (setq cpu-number (match-string 0 cpuinfo)))
       (string-to-number cpu-number)))
    (t 1)
    ))

(defun yc/get-env (env &optional func &rest backups)
  (let ((ret (getenv env)))
    (when (not ret)
      (when (not func) (setq func 'identity))
      (condition-case error
          (dolist (var backups)
            (when (funcall func var)
              (setq ret var)
              (signal error nil)))
        ('error nil)))
    ret))

;;;; Add new line before or after current line.
(defun zl-newline nil
  (interactive)
  (end-of-line)
  (newline-and-indent))
(global-set-key "\C-o" 'zl-newline)

(defun zl-newline-up nil
  (interactive)
  (beginning-of-line)
  (newline-and-indent))
(global-set-key (kbd "C-S-o") 'zl-newline-up)


(defun up-slightly ()
  (interactive) (scroll-up 3))
(defun down-slightly ()
  (interactive) (scroll-down 3))

(defun mode-hook-func  ()
  (set-process-sentinel (get-buffer-process (current-buffer))
                        #'kill-buffer-on-exit)
  '(auto-fill-mode nil))

(defun kill-buffer-on-exit (process state)
  (message "%s" state)
  (if (or
       (string-match "exited abnormally with code.*" state)
       (string-match "finished" state))
      (kill-buffer (current-buffer))))

;;;; Make current-buffer 10 lines higher.
(defun my-adjust-window (arg)
  "Adjust window quickly."
  (interactive)
  (enlarge-window (* arg 20) t))

(defun my-adjust-window-horizontal (arg)
  "Adjust window quickly."
  (interactive)
  (enlarge-window (* arg 20) t))

(global-set-key (kbd"C-M-^") (lambda () (interactive)
                               (my-adjust-window 1)))
(global-set-key (kbd "C-M->") (lambda () (interactive)
                                (my-adjust-window-horizontal 1)))
(global-set-key (kbd "C-M-<") (lambda () (interactive)
                                (my-adjust-window-horizontal -1)))
;; date and time
(defvar current-date-time-format "%a %b %d %H:%M:%S %Z %Y"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defvar current-time-format "%H:%M:%S"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defvar current-date-format "%Y-%m-%d"
  "Format of date to insert with `insert-current-date' func.
Note the weekly scope of the command's precision.")

(defvar current-year-format "%Y"
  "Format of date to insert with `insert-current-date' func.
Note the weekly scope of the command's precision.")

(defun insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
  (interactive)
  (insert "==========\n")
  (insert (format-time-string current-date-time-format (current-time)))
  (insert "\n")
  )

(defun insert-current-time ()
  "insert the current time (1-week scope) into the current buffer."
  (interactive)
  (insert (format-time-string current-time-format (current-time)))
  (insert "\n")
  )

(defun insert-current-date ()
  "insert the current time (1-week scope) into the current buffer."
  (interactive)
  (insert (format-time-string current-date-format (current-time))))

(defun insert-current-year ()
  "insert the current time (1-week scope) into the current buffer."
  (interactive)
  (insert (format-time-string current-year-format (current-time))))

(defun yc/insert-current-buffername ()
  "insert the current time (1-week scope) into the current buffer."
  (interactive)
  (insert (buffer-file-name (current-buffer)))
  )

(setq skeleton-pair t)

;;;; functions to setup platform depadent settings.

(defun skeleton-c-mode-left-brace (arg)
  (interactive "P")
  (if  (c-in-literal (c-most-enclosing-brace (c-parse-state)))
      (self-insert-command 1)
    ;; auto insert complex things.
    (let* ((current-line (delete-and-extract-region (line-beginning-position) (line-end-position)))
           (lines (and arg (mark t) (delete-and-extract-region (mark t) (point))))
           (after-point (make-marker)))
       ;;; delete extra blank begin and after the LINES
      (setq lines (and lines
                       (with-temp-buffer
                         (insert lines)
                         (goto-char (point-min))
                         (delete-blank-lines)
                         (delete-blank-lines)
                         (goto-char (point-max))
                         (delete-blank-lines)
                         (delete-blank-lines)
                         (buffer-string))))
      (save-excursion
        (let* ((old-point (point)))
          (insert (if current-line current-line "")  "{\n")
          (and lines (insert lines))
          (move-marker after-point (point))
          (insert "\n}")
          (indent-region old-point (point) nil)))
      (goto-char after-point)
      (c-indent-line))))

(defcustom yc/trailing-whitespace-modes
  nil
  "Modes when whitespaces need to removed automatically."
  :group 'user)

(setq yc/trailing-whitespace-modes
      '(emacs-lisp-mode
        latex-mode org-mode
        lisp-mode python-mode
        scheme-mode erlang-mode antlr-mode graphviz-dot-mode ruby-mode
        js2-mode))

(defun yc/trailing-whitespace-hook ()
  (when (member major-mode yc/trailing-whitespace-modes)
    (delete-trailing-whitespace)))

;; clean trailing whitespaces automatically
(add-hook 'before-save-hook 'yc/trailing-whitespace-hook)
(add-hook 'after-save-hook
          #'(lambda ()
              (and (save-excursion
                     (save-restriction
                       (widen)
                       (goto-char (point-min))
                       (save-match-data
                         (looking-at "^#!"))))
                   (not (file-executable-p buffer-file-name))
                   (shell-command (concat "chmod u+x " buffer-file-name))
                   (message
                    (concat "Saved as script: " buffer-file-name)))))

;;;; Untabify
(defvar yc/untabify-modes nil "Modes to run untabify.")

;; (setq yc/untabify-modes '(haskell-mode
;;                            scheme-mode
;;                            erlang-mode
;;                            python-mode
;;                            clojure-mode
;;                            text-mode))

(defun yc/untabify-hook ()
  (when (member major-mode yc/untabify-modes)
    (untabify (point-min) (point-max))))

;;;; untabify some modes
(add-hook 'before-save-hook 'yc/untabify-hook)

;;;; Shift regiion to left or right quickly.

(defvar shift-indent-offset 4)

(defun shift-region (start end count)
  "Indent lines from START to END by COUNT spaces."
  (save-excursion
    (goto-char end)
    (beginning-of-line)
    (setq end (point))
    (goto-char start)
    (beginning-of-line)
    (setq start (point))
    (indent-rigidly start end count)))

(defun shift-region-right (start end &optional count)
  "Shift region of Python code to the right."
  (interactive
   (let ((p (point))
         (m (mark))
         (arg current-prefix-arg))
     (if m
         (list (min p m) (max p m) arg)
       (list p (save-excursion (forward-line 1) (point)) arg))))
  (shift-region start end (prefix-numeric-value
                           (or count shift-indent-offset))))

(defun shift-region-left (start end &optional count)
  "Shift region of Python code to the left."
  (interactive
   (let ((p (point))
         (m (mark))
         (arg current-prefix-arg))
     (if m
         (list (min p m) (max p m) arg)
       (list p (save-excursion (forward-line 1) (point)) arg))))
  ;; if any line is at column zero, don't shift the region
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (back-to-indentation)
      (if (and (zerop (current-column))
               (not (looking-at "\\s *$")))
          (error "Region is at left edge"))
      (forward-line 1)))
  (shift-region start end (- (prefix-numeric-value
                              (or count shift-indent-offset)))))

(global-set-key "\C-c>" 'shift-region-right)
(global-set-key "\C-c<" 'shift-region-left)
;;;; Other Hooks.
(add-hook 'shell-mode-hook 'mode-hook-func)


(defun sort-uniq-region (beg end)
  "Remove duplicate lines.
   If tempted, you can just do <<C-x h C-u M-| uniq RET>> on Unix."
  (interactive "r")
  (let ((ref-line nil))
    (sort-lines nil beg end)
    (uniq beg end
          (lambda (line) (string= line ref-line))
          (lambda (line) (setq ref-line line)))))


(defun uniq-region-internal (beg end)
  "description"
  (let ((ref-line nil))
    (uniq beg end
          (lambda (line) (string= line ref-line))
          (lambda (line) (setq ref-line line))))
  )
(defun uniq-region (beg end)
  "Remove duplicate lines.
   If tempted, you can just do <<C-x h C-u M-| uniq RET>> on Unix."
  (interactive "rp")
  (uniq-region-internal beg end))

(defun uniq-remove-dup-lines (beg end)
  "Remove all duplicate lines wherever found in a file, rather than
   just contiguous lines."
  (interactive "r")
  (let ((lines '()))
    (uniq beg end
          (lambda (line) (assoc line lines))
          (lambda (line) (add-to-list 'lines (cons line t))))))

(defun uniq (beg end test-line add-line)
  (save-restriction
    (save-excursion
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (not (eobp))
        (if (funcall test-line (thing-at-point 'line))
            (kill-line 1)
          (progn
            (funcall add-line (thing-at-point 'line))
            (forward-line))))
      (widen))))


(defun yc/txt-to-png ()
  "Change a txt file into png file using ditaa"
  (interactive)
  (let* ((fname (buffer-file-name))
         (txt2png-buf-name "*txt2png*"))
    (get-buffer-create txt2png-buf-name)
    (pop-to-buffer txt2png-buf-name)
    (start-process "txt-to-png" txt2png-buf-name "java" "-jar"
                   (expand-file-name "~/.emacs.d/site-lisp/org_contrib/scripts/ditaa.jar") fname "--overwrite")
    (message "This may take for a while, refer to *txt2png* to see whether it has been finished or not"))
  )


(defun yc/comment-dwim-line (&optional arg)
  "Replacement for the comment-dwim command. If no region is selected and
current line is not blank and we are not at the end of the line, then
comment current line. Replaces default behaviour of comment-dwim, when it
inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
	  (comment-or-uncomment-region
       (line-beginning-position) (line-end-position))
	(comment-dwim arg)))

(global-set-key "\M-;" 'yc/comment-dwim-line)

(defun auto-rename-buffer ()
  "Rename current buffer to the basename"
  (interactive)
  (let ((newname (concat (buffer-name) "-" (format-time-string current-time-format (current-time)))))
    (rename-buffer newname)))

(defalias 'string-split 'split-string)

(defun yc/strip-ws-in-string (src)
  "description"
  (let ((str-array (split-string src split-string-default-separators)))
    (mapconcat 'identity str-array "_")))

(defun yc/update-info ()
  "Update info"
  (interactive)
  (when (member system-type '(windows-nt ms-dos)) ;; do this only for Windows, Linux will do it
    ;; automatically.
    (dolist (dir (append Info-directory-list Info-default-directory-list))
      (let ((cmd (format "bash -c \"cd %s && install-info --dir-file=dir \"" dir)))
        (dolist (fn (directory-files dir t))
          (if (file-directory-p fn)
              (message "Skipping directory: %s" fn)
            (message "Processing file: %s" fn)
            (shell-command (concat cmd (file-name-nondirectory fn)))))))))

(defun yc/set-c-env (&rest args)
  (setenv "LANG" "C"))


(defun yc/setup-exec-path ()
  "Setup for exec-path"
  (mapcar
   (lambda (path)
     (when (and (file-exists-p path)
                (not (member path exec-path)))
       (add-to-list 'exec-path path)))
   (string-split (getenv "PATH") ":")))
(yc/setup-exec-path)


(defvar yc/color-them 'dark
  "dark or light")

(defun yc/setup-color-theme ()
  (when window-system
    (csq custom-theme-directory "~/.emacs.d/themes")
    (csq custom-safe-themes t)
    (if (eq yc/color-them 'dark)
        (load-theme 'tb-dark)
      (load-theme 'tb-light))))

(defun yc/toggle-color-theme ()
  "Toggle color theme"
  (interactive)
  (setq yc/color-them (if (eq yc/color-them 'dark) 'light 'dark))
  (yc/setup-color-theme))

(defcustom available-width '(72 78 82 86 92)
  "Available column width for Emacs to choose.."
  :group 'user)

(defun get-column-width (width)
  (let ((target 78)
        (final-value nil)
        (tmp 0)
        (tmp-next 0)
        (pos 0)
        (font-string (face-font 'default))
        (font-width 12)
        (available-width (sort available-width '<))
        (r-match-font
         (rx "-" (+? (or alpha "-")) "*-"
             (group (+? digit)) "-*" (+ (or alpha "-" "*")))))

    (if (string-match r-match-font font-string)
        (setq font-width (string-to-number (match-string 1 font-string))))

    (setq target (/ width (+ 3 font-width))) ;; Hardcoded value.

    (while (and (< pos (length available-width))
                (not final-value))
      (setq tmp (nth pos available-width))
      (setq pos (1+ pos))
      (setq tmp-next (nth pos available-width))
      (if (and (<= tmp target)
               (or (not tmp-next)
                   (>= tmp-next target)))
          (setq final-value tmp)))
    final-value))


(defun yc/setup-column()
  "setup column width, fill-colum can be overwriten by 100-private.el"
  (let ((x-width (x-display-pixel-width)))
    (when x-width
      (setq-default fill-column (get-column-width x-width))
      )))

(defun increase-font-size ()
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (ceiling (* 1.10
                                  (face-attribute 'default :height)))))
(defun decrease-font-size ()
  (interactive)
  (set-face-attribute 'default
                      nil
                      :height
                      (floor (* 0.9
                                (face-attribute 'default :height)))))

(global-set-key (kbd "C-+") 'increase-font-size)
(global-set-key (kbd "C--") 'decrease-font-size)

(defun yc/setup-font ()
  (when window-system
    (cond ((string= system-type "darwin")
           (set-face-attribute
            'default nil :font "Monaco 13")
           (dolist (charset '(kana han symbol cjk-misc bopomofo))
             (set-fontset-font (frame-parameter nil 'font)
                               charset
                               (font-spec :family "Hiragino Sans GB"
                                          :size 16)))
           )
          ((string= system-type "gnu/linux")
           (set-face-attribute
            'default nil :font "Monaco 11")
           (dolist (charset '(kana han symbol cjk-misc bopomofo))
             (set-fontset-font (frame-parameter nil 'font)
                               charset
                               (font-spec :family "WenQuanYi Micro Hei"
                                          :size 18)))
           )
          ((string= system-type "windows-nt")
           (set-face-attribute
            'default nil :font "Consolas 11")
           (set-frame-font
            "-outline-Consolas-normal-normal-normal-mono-15-*-*-*-c-*-iso8859-1")
           (dolist (charset '(kana han symbol cjk-misc bopomofo))
             (set-fontset-font (frame-parameter nil 'font)
                               charset
                               (font-spec :family "Microsoft YaHei"
                                          :size 18)))
           )
          (t
           (message "Font not set up, choose it yourself.")
           ;; (set-face-attribute
           ;;  'default nil :font "Monaco 10")
           ;; (set-frame-font
           ;;  "-outline-Monaco-normal-normal-normal-mono-15-*-*-*-c-*-iso8859-1")
           ;; (dolist (charset '(kana han symbol cjk-misc bopomofo))
           ;;   (set-fontset-font (frame-parameter nil 'font)
           ;;                     charset
           ;;                     (font-spec :family "WenQuanYi Micro Hei"
           ;;                                :size 16)))
           ))))

(defun yc/setup-display ()
  "Setup display, including: font, colortheme, and fill-column."
  (interactive)
  (yc/setup-color-theme)
  (when window-system
    (yc/setup-font)
    (yc/setup-column)))

(yc/setup-display)

(defcustom trailing-whitespace-autoremove nil
  "Auto remove trailing whitespaces or not."
  :group 'user
  )

(defun yc/toggle-autoremove-spaces ()
  "Toggle auto remove tailing whitespace."
  (interactive)
  (make-local-variable 'trailing-whitespace-autoremove)
  (setq trailing-whitespace-autoremove
        (not trailing-whitespace-autoremove))
  (if trailing-whitespace-autoremove
      (add-hook 'before-save-hook 'delete-trailing-whitespace t t)
    (remove-hook 'before-save-hook 'delete-trailing-whitespace)))
(global-set-key (kbd "<C-S-f12>") 'yc/toggle-autoremove-spaces)



(defun maximize-frame ()
  (interactive)
  (set-frame-position (selected-frame) 0 0)
  (set-frame-size (selected-frame) 1000 1000))


(defun change-file-permissions-to-writable ()
  "to be run from find-file-hook, change write permissions"
  (interactive)
  (when (file-exists-p buffer-file-name)
    (let ((attr (file-attributes buffer-file-name))
          (msg (format "Make file: %s writable... " buffer-file-name)))
      ;; Change file mode if this file belongs to me, and it is writeable.
      (when (and (not (car attr))
                 (= (user-uid) (caddr attr))
                 (not (file-writable-p buffer-file-name)))
        (cond
         ((executable-find "chmod")
          (progn
            (call-process (executable-find "chmod") nil nil nil "+w"
                          buffer-file-name)))
         (t (chmod buffer-file-name
                   (file-modes-symbolic-to-number
                    "u+w" (file-modes buffer-file-name)))))
        (setq msg (concat msg (if (file-writable-p buffer-file-name)
                                  "Succeeded\n" "Failed\n" )))
        (message msg)))
    )
  )

(add-hook 'before-save-hook 'change-file-permissions-to-writable)

(defun yc/compile-rc-files ()
  "Compile all init files."
  (interactive)
  (let ((init-dir (expand-file-name "~/.emacs.d/rc"))
        (file-name nil))
    (when init-dir
      (dolist (file-name (directory-files init-dir t ".*el"))
        (if file-name
            (byte-compile-file file-name))))))


(defun yc/read-file-content-as-string (file)
  "If FILE exists and is readable returns the contents as a string otherwise
return nil.
Note: No major/minor-mode is activated and no local variables are evaluated
for FILE, but proper EOL-conversion and character interpretation is done!"
  (let ((exp-filename (expand-file-name file)))
    (if (and (file-exists-p exp-filename)
             (file-readable-p exp-filename))
        (with-temp-buffer
          (erase-buffer)
          (insert-file-contents exp-filename)
          (buffer-string)))))

(defun yc/remove-empty-lines (&optional pos)
  "Remove empty lines around this position..."
  (interactive)
  (if pos
      (goto-char pos))

  ;; Find next non new-line and non-empty character
  (skip-chars-forward " 	\n")
  (setq pos (point))
  (when
      (<= (skip-chars-backward " 	\n") -2)
    (delete-region (1+ (point)) pos)))


(defun concat-string-array (array &optional sep)
  "Concat string array"
  (when (listp array)
    (mapconcat 'identity array sep)))

(defun yc/disable-add-new-line ()
  "Don't add new line at end of file."
  (interactive)
  (setq require-final-newline nil))

(defun yc/decode-hex-color ()
  "Decode hex color"
  (interactive)
  (save-excursion
    (let ((r-match-hex-color (rx (? (or (: "0" (or "x" "X")) "#"))
                                 (group (= 2 hex)) (group (= 2 hex)) (group (= 2 hex)))))
      (skip-chars-backward "abcdefABCDEF0123456789xX#")
      (when (looking-at r-match-hex-color)
        (message "Color: (%d, %d, %d)"
                 (string-to-number (match-string 1) 16)
                 (string-to-number (match-string 2) 16)
                 (string-to-number (match-string 3) 16))
        ))))

(defun yc/encode-hex-color ()
  "Encode hex color"
  (interactive)
  (save-excursion
    (let ((r-match-hex-color (rx (group (+ digit)) (? (: (* whitespace) "," (* whitespace)))
                                 (group (+ digit)) (? (: (* whitespace) "," (* whitespace)))
                                 (group (+ digit)) (? (: (* whitespace) "," (* whitespace)))
                                 (? (group (+ digit)) "%"))))
      (print (point))
      (skip-chars-backward "0123456789, %")
      (print (point))
      (when (search-forward-regexp r-match-hex-color)
        (let* ((r (match-string 1))
               (g (match-string 2))
               (b (match-string 3))
               (a (match-string 4))
               (hexcolor (format (concat "#%02X%02X%02X" (if a "%02X" "%s"))
                                 (string-to-number (match-string 1))
                                 (string-to-number (match-string 2))
                                 (string-to-number (match-string 3))
                                 (if (not a) ""
                                   (/ (* 255 (string-to-number a)) 100)))))
          (kill-new hexcolor)
          (message "Original color: %s, HexColor: %s, copied to yank!"
                   (match-string 0)
                   hexcolor))))))

(defun yc/decode-hex-color-region (start end)
  "Decode region of colors"
  (interactive "rp")
  (let ((content (buffer-substring-no-properties start end))
        (r-match-hex-color (rx bol (*? ascii)
                               (group (+? (or alnum "*" ".")))
                               (: ":" (* whitespace)) (or (: "0" (or "x" "X")) "#")
                               (group (= 2 hex)) (group (= 2 hex)) (group (= 2 hex)) eol))
        res ele)

    (defun iter (pos)
      (when (string-match  r-match-hex-color content pos)
        (setq res (cons (format "Color %s: (%d, %d, %d)"
                                (match-string 1 content)
                                (string-to-number (match-string 2 content) 16)
                                (string-to-number (match-string 3 content) 16)
                                (string-to-number (match-string 4 content) 16)) res)
              pos (match-end 0))
        (iter pos)))
    (iter 0)
    (setq res (nreverse res))
    (while (setq ele (pop res))
      (message "%s" ele))))

(defun yc/insert-line-number ()
  "Insert line number."
  (interactive)
  (let ((number 1))
    (save-excursion
      (when (search-backward-regexp (rx bol (* blank) (group (+ digit)) ".") nil t)
        (setq number (1+ (string-to-number (match-string 1))))))
    (if (and (looking-at "$")
             (not (looking-back "^")))
        (insert  "\n\n")    )
    (insert (format "%d." number))))


(defun reload-emacs ()
  "reload emacs configuration"
  (interactive)
  (load-file "~/.emacs"))

(defun yc/open-with-external-app (&optional fn)
  "Open file with external app"
  (interactive)
  (let ((fn (or fn (buffer-file-name)))
        (app (case system-type
               ('darwin "open")
               ('gnu/linux "xdg-open")
               (t nil))
             ))
    (unless fn
      (error "Not file to operate..."))
    (if app
        ;; (start-process "yc/open-with-external-app"
        ;;                (if YC-DEBUG YC-DEBUG-BUF nil)
        ;;                app fn)
        (call-process app nil nil
                      (if YC-DEBUG YC-DEBUG-BUF nil)
                      fn)
      (error "Can't find proper app to open file %s." fn))))

(defun yc/command-output-to-string (&rest args)
  "Execute a p4 command and return result as string.
args should be a list, but to make caller's life easier, it can accept one atom instead of a
  list."
  (let* ((cmd (car args))
         (args (cdr args))
         (cmd-output (with-output-to-string
                       (with-current-buffer standard-output
                         (apply #'process-file
                                cmd
                                nil (list t t) nil
                                (if (listp (car args))
                                    (car args)
                                  args))))))
    (ansi-color-apply cmd-output)))

(defun yc/replace-string (content from to)
  "description"
  (interactive)
  (with-temp-buffer
    (insert content)
    (goto-char (point-min))
    (while (search-forward from nil t)
      (replace-match to nil t))
    (buffer-substring (point-min) (point-max))))

(defalias 'yc/string-replace 'yc/replace-string)

(defun yc/expand-variable (var)
  "Expand VAR into real string."
  (print var)
  (let ((r-match-abbr (rx "$" (group (+ (or alnum "_"))) eow)))
    (if (string-match r-match-abbr var)
        (let ((prefix (substring var 0 (match-beginning 0)))
              (suffix (substring var (match-end 0)))
              (v (getenv (match-string 1 var))))
          (if v (yc/expand-variable (concat prefix v suffix))
            nil))
      var)))

(defun yc/load-shell-env (fn)
  "Load environment variables from FN."
  (unless (stringp fn)
    (error "Wrong input: %s .." fn))
  (unless (file-regular-p fn)
    (error "%s is not a regular file!" fn))

  (with-temp-buffer
    (insert-file-contents fn)
    (goto-char (point-min))
    (while (search-forward-regexp (rx bol (? (: "export" space)) (* space)
                                      (group (+? (or alnum "_")))
                                      "="
                                      (group (+? anything)) eol) nil t)
      (let ((k (match-string 1))
            (v (yc/expand-variable (match-string 2))))
        (print v)
        (if (and k v)
            (setenv k v))))))

(defcustom yc/wp-path "~/Documents/Email/WeeklyReports/"
  "Path to store weekly reports."
  :type 'string
  :group 'user
  )


(defun yc/new-wp ()
  "Create new weekly-report."
  (interactive)
  (find-file (expand-file-name
              (format "%swp_%s.org"
                      yc/wp-path
                      (format-time-string  "%Y-%m-%d" (current-time))))))

(defun yc/expand-color (color)
  "Expand COLOR of form #FFF into #FFFFFF."
  (mapconcat
   (lambda (X) (make-string (if (= X ?#) 1 2 )X)) color ""))

(defun yc/syntax-color-hex ()
  "Show color text of form #fffffff in current buffer."
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[abcdef[:digit:]]\\{6\\}"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (match-string-no-properties 0)))))
     ("#[abcdef[:digit:]]\\{3\\}\\>"
      (0 (put-text-property
          (match-beginning 0)
          (match-end 0)
          'face (list :background (yc/expand-color (match-string-no-properties 0))))))))
  (font-lock-fontify-buffer))

(defun yc/asm-add-offset ()
  "Add offset to current file."
  (interactive)
  (let ((r-match-func  (rx bol  (+ alnum) (+ space) "<" (+ (or "_" alnum)) ">:" eol))
        (r-match-addr  (rx (+ space) (group (+ alnum)) ":" space))
        (r-match-codes (rx ":" (+ space) (* (repeat 2 alnum) space ) (* space)))
        (r-match-offset (rx "+" "0x" (group (+ alnum))  ">"))
        pos )

    (defun get-address ()
      "Get address"
      (if (looking-at r-match-addr)
          (let* ((m-data (match-data 1))
                 (addr-str (buffer-substring-no-properties (nth 2 m-data) (nth 3 m-data))))
            (string-to-number addr-str 16))))

    ;; first, add a space around "+"
    (save-excursion
      (while (search-forward-regexp r-match-offset nil t)
        (replace-match "+ 0x\\1 >"))
      )

    ;; then, remove instruction codes...
    (save-excursion
      (while (search-forward-regexp r-match-codes nil t)
        (replace-match ":	")))

    ;; last, calculate offset for instruction addresses.
    (save-excursion
      (while (setq pos (search-forward-regexp r-match-func nil t))
        (let* ((pos (1+ pos))
               (end (or (search-forward-regexp r-match-func nil t)
                        (point-max))))
          (goto-char end)
          (setq end (point-at-eol -1)) ;; update end position, we'll go back here later.
          (goto-char pos)
          (aif (get-address)
              (while (<= pos end)
                (goto-char pos)
                (unless (looking-at-p (rx (or (: bol eol)
                                              (: (* space)";" ))))
                  (let* ((addr (get-address))
                         (tmp-string (format "0x%x" (- addr it)))
                         (off-string (format "%s%s" tmp-string
                                             (make-string
                                              (- 7 (length tmp-string)) ? ))))
                    (insert off-string)
                    (setq end (+ end (length off-string)))))
                (setq pos (point-at-bol 2))))
          (goto-char end)
          (forward-line -1))))))

(defun uniq-stack ()
  "Make stacks unique."
  (interactive)
  (let ((r-match-thread (rx bol "Thread" (* space) (group (+ digit)) (* space)
                            "(Thread" (+? ascii) ":" eol))
        (r-match-suspicious (rx (+? ascii)
                                (or "segfault" "segment fault" "signal handler called"
                                    "abort" "raise" "__assert_fail")
                                (? space) (? "()")
                                (+? ascii)))
        (htable-stack (make-hash-table :test 'equal :size 2048))
        (htable-threads (make-hash-table :test 'equal :size 2048))
        ordered-numbers suspects)
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp r-match-thread nil t)
        (forward-char)
        (let* ((pos (point))
               (end (cond
                     ((search-forward-regexp r-match-thread nil t) (point-at-bol))
                     (t (point-max))))
               (stack (buffer-substring-no-properties pos end))
               (num (gethash stack htable-stack 0)))
          (puthash stack (1+ num) htable-stack)
          (goto-char end))))

    ;; Sort tacks based on number of threads.
    (maphash (lambda (stack repeated)
               (let ((lst (gethash repeated htable-threads nil)))
                 (puthash repeated (cons stack lst) htable-threads))
               (add-to-list 'ordered-numbers repeated)
               ) htable-stack)
    (sort ordered-numbers '>)

    ;; Now insert stacks and highlight suspicious ones.
    (with-current-buffer (get-buffer-create (format "Uniq-Stack of: %s" (buffer-name)))
      (read-only-mode -1)
      (erase-buffer)
      (dolist (number ordered-numbers)
        (let ((stack-list (gethash number htable-threads)))
          (dolist (stack stack-list)
            (insert (format "\nNumber of Threads: %d" number))
            (let (added-to-list)
              (dolist (str (string-split stack "\n"))
                (insert (format "\n%s" str))
                (when (string-match r-match-suspicious str)
                  (unless added-to-list
                    (setq added-to-list t
                          suspects (cons (1+ (line-number-at-pos)) suspects)))
                  (overlay-put (make-overlay (point-at-bol) (point-at-eol))
                               'face `(:underline (:style wave :color "Red1")))))))))
      (goto-char (point-min))
      (insert (format "Unique Stacks: %d, suspicious lines: " (length ordered-numbers)))
      (if suspects
          (progn
            (setq suspects (nreverse suspects))
            (insert (format "%d" (pop suspects)))
            (while suspects
              (insert (format ", %d" (pop suspects)))))
        (insert "none"))
      (insert ".\n")

      ;; show this buffer.
      (read-only-mode 1)
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

 ;; Advice

;; Auto indent regions for prog/sgml based modes.
(dolist (command (list 'yank 'yank-pop))
  (advice-add command :after
              (lambda (&rest args)
                (when (and (not current-prefix-arg)
                           (or (derived-mode-p 'prog-mode)
                               (derived-mode-p 'sgml-mode)))
                  (let ((mark-even-if-inactive transient-mark-mode))
                    (indent-region (region-beginning) (region-end) nil))))))

(defun yc/install-package-on-error (func &rest args)
  "Apply FUNC with ARGS.
And install necessary packages if there are errors while executing FUNC."
  (interactive)
  (condition-case err (apply func args)
    (file-error
     (let ((msg (prin1-to-string err)))
       (message "Failed to open file: %s" msg)
       (if (string-match ".*Cannot open load file.*" msg)
           (if (listp err)
               (let* ((package-name (nth (1- (length err)) err))
                      (fmt "Package %s can not be loaded, install it with ELPA? "))
                 (if package-name
                     (when (yes-or-no-p (format fmt package-name))
                       (package-install (intern package-name))
                       (set-auto-mode))))))))))

;; Handle file-error and suggest to install missing packages...
(advice-add 'set-auto-mode :around #'yc/install-package-on-error)

(advice-add
 'command-execute :around #'yc/install-package-on-error)

(provide '02-rc-functions)
;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 02-rc-functions.el ends here
