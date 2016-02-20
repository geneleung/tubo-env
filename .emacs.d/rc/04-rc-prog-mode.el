;;; 04-rc-prog-mode.el -- Brief introduction here.

;; Author: YangYingchao <yangyingchao@gmail.com>

;;; Commentary:

;;; Code:

(autoload 'which-function-mode "which-func" "" t)
(autoload 'autopair-mode "autopair" ""  t)
(autoload 'srecode-minor-mode "mode.el" ""  t)
(autoload 'flyspell-prog-mode "flyspell" ""  t)
(autoload 'highlight-parentheses-mode "highlight-parentheses"  "" t)
(autoload 'flycheck-mode "flycheck" ""  t)

;;;; Function and macro to add an regular expression string formed by (rx)
;;;; macro into specified face.
(defun yc/show-prog-keywords ()
  "Highlight additional keywords."
  (let ((yc/r-match-warning
         (rx (group (or bow "@" ) (or "todo" "fixme" "yyc" "bug" "XXX" "Fixme" "FixMe"
                                      "TODO" "FIXME" "YYC" "BUG" "TODOLIST" "note" "NOTE")
                    (?? ":")) eow))
        (yc/r-match-builtin
         (rx bow (group (or "PDEBUG" "NEW")) eow ))
        (yc/r-match-macros ;; upper case macros.
         (rx bow (group (+ (or upper "_"))) (* blank) "("))
        (yc/r-match-keyword-type
         (rx bow (group (or "NEW" "DELETE")) (+ blank) (group (+ (or alnum "_"))) eow (* blank)
             (? (:"[" (+ alnum) "]")) (* blank)";"))
        (yc/r-match-longline
         (rx (repeat 120 not-newline) (group (+? not-newline)) eol)))
    (yc/add-keyword yc/r-match-warning 'font-lock-warning-face)
    (yc/add-keyword yc/r-match-builtin 'font-lock-builtin-face)
    (yc/add-keyword yc/r-match-macros 'font-lock-builtin-face)

    (font-lock-add-keywords
     nil
     '(("\\<\\(\\(?:NEW\\)\\)[[:blank:]]+\\(\\(?:[[:alnum:]]\\|_\\)+\\)\\>[[:blank:]]*\\(?:\\[[[:alnum:]]+]\\)?[[:blank:]]*;"
        1 font-lock-keyword-face
        2 font-lock-type-face
        t)))
    (yc/add-keyword yc/r-match-longline 'font-lock-warning-face)))


(yc/autoload 'semantic-mode)

(defun yc/common-program-hook ()
  "My program hooks."
  (semantic-mode 1)
  (which-function-mode 1)
  (autopair-mode t)
  (srecode-minor-mode t)
  (flyspell-prog-mode)
  (highlight-parentheses-mode)
  (yc/show-prog-keywords)
  (yc/basic-prog-keybinding)
  (setup-program-keybindings)
  (autopair-mode 1)
  (flycheck-mode 1))

(yc/eval-after-load
 "prog-mode"
 (add-hook 'prog-mode-hook 'yc/common-program-hook))

 ;; autopair-mode
(autoload 'autopair-global-mode "autopair" ""  t)
(autopair-global-mode 1)
(custom-set-variables
 '(autopair-pair-criteria 'always))

 ;;;; CEDET Settings
;; http://lists.gnu.org/archive/html/help-gnu-emacs/2012-04/msg00430.html
(unless (boundp 'x-max-tooltip-size)
  (setq x-max-tooltip-size (cons 80 40)))

(require 'cedet)
(custom-set-variables '(pulse-flag 'never))

 ;;; Semantic settings..
(require 'semantic)
(require 'semantic/ia)
(require 'semantic/lex-spp)
(require 'semantic/analyze/complete)
(require 'semantic/analyze/refs)
(require 'semantic/decorate/include)

;;;;  Helper tools.
(custom-set-variables
 '(semantic-default-submodes
   (quote (global-semantic-decoration-mode
           global-semantic-idle-scheduler-mode
           global-semanticdb-minor-mode
           global-semantic-idle-summary-mode
           global-semantic-mru-bookmark-mode)))
 '(semantic-idle-scheduler-idle-time 1)
 '(semantic-idle-scheduler-max-buffer-size 102400))

(yc/eval-after-load
 "lex"
 (setq semantic-lex-maximum-depth 20))


;; (semantic-load-enable-code-helpers)


(advice-add
 'semantic--tag-link-secondary-overlays :around
 (lambda (func &rest args)
   (condition-case err
       (apply func args)
     (error nil))))


(add-hook 'semantic-lex-reset-hooks 'semantic-lex-spp-reset-hook nil t)

     ;;;; Semanticdb 定制
;; Semantic DataBase存储位置
(require 'semantic/db)
(setq-mode-local c-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))
(setq-mode-local c++-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))

(global-semanticdb-minor-mode 1)
(custom-set-variables
 '(semanticdb-default-save-directory
   (expand-file-name (concat yc/emacs-cache-dir "semanticdb"))))



;; ;; if you want to enable support for gnu global
(require 'cedet-global)
(when (cedet-gnu-global-version-check t)
  (require 'semantic/db-global)
  (semanticdb-enable-gnu-global-databases 'c-mode)
  (semanticdb-enable-gnu-global-databases 'c++-mode))

;; (require 'eieio-loaddefs)
;; (require 'cogre-loaddefs)
;; (yc/eval-after-load "cogre-uml" (cogre-uml-enable-unicode))

 ;;; Projectile
(yc/autoload 'helm-projectile-find-other-file "helm-projectile")

(defun eassist-string-without-last (string n)
  "This function truncates from the STRING last N characters."
  (substring string 0 (max 0(- (length string) n))))

(defun eassist-string-ends-with (string end)
  "Check whether STRING ends with END substring."
  (string= end (substring string (- (length end)))))

;; ================================== CPP-H switch ===========================
;;;###autoload
(defvar eassist-header-switches '(("h" . ("cpp" "cc" "c"))
                                  ("hpp" . ("cpp" "cc"))
                                  ("cpp" . ("h" "hpp"))
                                  ("c" . ("h"))
                                  ("C" . ("H"))
                                  ("H" . ("C" "CPP" "CC"))
                                  ("cc" . ("h" "hpp")))
  "This variable defines possible switches for `eassist-switch-h-cpp' function.
Its format is list of (from . (to1 to2 to3...)) elements.  From and toN are
strings which are extentions of the files.")

;;;###autoload
(defun eassist-switch-h-cpp ()
  "Switch header and body file according to `eassist-header-switches' var.
The current buffer's file name extention is searched in
`eassist-header-switches' variable to find out extention for file's counterpart,
for example *.hpp <--> *.cpp."
  (interactive)
  (let* ((ext (file-name-extension (buffer-file-name)))
         (base-name (eassist-string-without-last (buffer-name) (length ext)))
         (base-path (eassist-string-without-last (buffer-file-name) (length ext)))
         (count-ext (cdr (find-if (lambda (i) (string= (car i) ext)) eassist-header-switches))))
    (cond
     (count-ext
      (unless
          (or
           (loop for b in (mapcar (lambda (i) (concat base-name i)) count-ext)
                 when (bufferp (get-buffer b)) return
                 (if (get-buffer-window b)
                     (switch-to-buffer-other-window b)
                   (if (get-buffer-window b t)
                       (switch-to-buffer-other-frame b)
                     (switch-to-buffer b))))
           (loop for c in (mapcar (lambda (count-ext) (concat base-path count-ext)) count-ext)
                 when (file-exists-p c) return (find-file c)))
        (error "There is no corresponding pair (header or body) file.")))
     (t
      (error "It is not a header or body file! See eassist-header-switches
variable.")))))

;; ================================== CPP-H switch end =========================

(defun yc/swith-h-cpp ()
  "Switch between headers and source files."
  (interactive)
  (condition-case error
      (eassist-switch-h-cpp)
    (error (helm-projectile-find-other-file))))

(defun yc/print-cur-tag ( )
  "Print current tag."
  (interactive)
  (let ((tag (semantic-current-tag)))
    (if tag
        (yc/debug-log tag)
      (error "No tag retrived."))))


;; EDE Mode
(require 'ede)
(setq ede-project-directories t)
(global-ede-mode 1)


;; Which function mode
(yc/eval-after-load "which-func" (setq which-func-unknown "unknown"))

;; ;;;; Speedbar-frame-mode
(autoload 'speedbar-frame-mode "speedbar" "popup a speedbar frame" t)
(autoload 'speedbar-get-focus "speedbar" "jump to speedbar frame" t)
(autoload 'speedbar "speedbar" "jump to speedbar frame" t)
(autoload 'sr-speedbar-toggle "sr-speedbar" ""  t)

(global-set-key [f7] 'sr-speedbar-toggle)

(yc/eval-after-load
 "speedbar"
 (require 'sb-texinfo)
 (require 'sb-html)
 (setq speedbar-mode-hook '(lambda ()
                             (interactive)
                             (other-frame 0))))
(custom-set-variables
 '(speedbar-use-images nil))


;; 

(defvar mru-tag-stack '()
  "Tag stack, when jumping to new tag, current tag will be stored here,
and when jumping back, it will be removed.")

(defvar yc/tag-stack nil "nil")

(defun yc/store-mru-tag (pt)
  "Store tag info into mru-tag-stack"
  (interactive "d")
  (setq yc/tag-stack (cons 'semantic yc/tag-stack))
  (let* ((tag (semantic-mrub-find-nearby-tag pt)))
    (if tag
        (let ((sbm (semantic-bookmark (semantic-tag-name tag)
                                      :tag tag))
              (cur-point (point)))
          (if (or (not mru-tag-stack) ;; mru-tag-stack is empty.
                  (not (= cur-point (car (car mru-tag-stack)))))
              (progn
                (semantic-mrub-update sbm pt 'mark)
                (add-to-list 'mru-tag-stack (cons cur-point sbm)))))
      (error "No tag to go!"))))

(defun yc/goto-func-any (pt)
  "Store current postion and call (semantic-ia-fast-jump)"
  (interactive "d")
  (yc/store-mru-tag pt)
  (semantic-complete-jump))

(defun yc/symref (pt)
  (interactive "d")
  (yc/store-mru-tag pt)
  (semantic-symref))

(defun yc/symref-symbol ()
  (interactive)
  (error "Error: Not supported any more, use other tools instead.")
  ;; (interactive (list (semantic-tag-name (semantic-complete-read-tag-buffer-deep
  ;;                                        "Symrefs for: "))))
  ;; (yc/store-mru-tag (point))
  ;; (semantic-symref-symbol sym)
  )

(autoload 'helm-xgtags-find-header "helm-xgtags" ""  t)

(defun yc/open-header (pt)
  "Open header file lied under current cursor"
  (interactive "d")
  (yc/store-mru-tag pt)
  (condition-case error
      (semantic-decoration-include-visit)
    (error (helm-xgtags-find-header))))

(defun yc/return-func()
  "Return to previous tag."
  (interactive)
  (case (pop yc/tag-stack)
    ('semantic
     (let ((tag (cdr (pop mru-tag-stack))))
       (if tag
           (semantic-mrub-switch-tags tag)
         (error "No more tags in stack.."))))
    (t (helm-xgtags-pop-stack))))

(defun yc/clear-tag-stack()
  "Clear tag stacks."
  (interactive)
  (setq mru-tag-stack nil))
;; C mode

;;;; Include settings
(defvar yc/system-include-dirs nil
  "A list of directories where the header files are stored.
It is derived from `semantic-gcc-get-include-paths,
and is reversed for better performence.")

(defvar yc/tmp-include-dirs nil
  "Temporary directories containing header fiels. It can be overwrited.")

(defun yc/add-common-includes ( )
  "Add common includes"
  (setq yc/system-include-dirs
        (reverse (append (semantic-gcc-get-include-paths "c++") '("./"))))
  (sif semantic-c-dependency-system-include-path
                        yc/system-include-dirs)
  (sif semantic-dependency-system-include-path
                        yc/system-include-dirs)
  (mapc (lambda (dir)
          (semantic-add-system-include dir 'c-mode)
          (semantic-add-system-include dir 'c++-mode))
        yc/system-include-dirs))

(defun yc/add-cumtome-includes ( )
  "Add common includes"
  (interactive)
  (print (directory-files "." t ))
  (let* ((files (mapcar (lambda (x)
                          (file-name-directory x))
                        (directory-files "." t )))
         (location
          (completing-read
           "Location:"
           files nil nil (car files))))
    (semantic-add-system-include location 'c++-mode)
    (semantic-add-system-include location 'c-mode)))


(defun DE-imply-includes-in-directory (dir)
  "Add all header files in DIR to `semanticdb-implied-include-tags'."
  (let ((files nil))
    (when (file-exists-p dir)

      (setq files (directory-files dir t "^.+\\.h[hp]*$" t))
      (defvar-mode-local c++-mode semanticdb-implied-include-tags
        (mapcar (lambda (header)
                  (semantic-tag-new-include
                   header
                   nil
                   :filename header))
                files)))))

(defun yc/add-semantic-symbol (symbol)
  "Add symbol to semantic-lex-c-preprocessor-symbol-map"
  (add-to-list 'semantic-lex-c-preprocessor-symbol-map symbol)
  )

(defun yc/parse-wx-include-dir ( )
  "Parse include directory for wx-widgest."
  (let ((r-match-cflag-incs (rx bow "I" (group (+ ascii)) eow ))
        (tmp-array nil)
        (result
         (if (executable-find "wx-config")
             (shell-command-to-string "wx-config --cxxflags")
           nil))
        (wx-inc-dirs nil))
    (when (and (stringp result)
               (> (length result) 1))
      (setq tmp-array (split-string result " "))
      (dolist (item tmp-array)
        (if (string-match r-match-cflag-incs item)
            (add-to-list 'wx-inc-dirs (match-string 1 item)))))
    wx-inc-dirs))



(defun yc/add-wx-support ()
  "Add wxwidget related files."
  (interactive)
  (let* ((wx-base-dir (yc/parse-wx-include-dir))
         (wx-symbol-list (list '("WXDLLEXPORT" . "")
                               '("WXDLLIMPEXP_CORE" . "")
                               '("WXDLLIMPEXP_ADV" . "")
                               '("WXDLLIMPEXP_BASE" . "")
                               '("WXDLLIMPEXP_FWD_CORE" . "")
                               '("WXDLLIMPEXP_FWD_XML" . "")
                               '("WXDLLIMPEXP_FWD_BASE" . "")
                               ))
         )

    ;; include files for wxwidgets
    (when wx-base-dir
      (mapc 'yc/add-semantic-symbol
            wx-symbol-list)
      (dolist (var wx-base-dir)
        (semantic-add-system-include var 'c++-mode)
        (DE-imply-includes-in-directory (concat var "/wx/gtk"))
        )
      ;; preprocessor macro


      )))

(defun yc/add-qt-support (&optional basedir)
  "Add qt related files."
  (interactive)
  (let ((qt-base-dir (or basedir (completing-read
                                  "QT Base dir:"
                                  (apply
                                   'append
                                   (mapcar
                                    (lambda (dir)
                                      (when (file-exists-p dir)
                                        (remove-if
                                         (lambda (x) (or (not (stringp x))
                                                         (string-match (rx (or "." "..")) x)))
                                         (directory-files dir t))))
                                    '("/" "."))) nil nil
                                    "/usr/include/qt4" nil "/usr/include/qt4")))
        qt-gui-dir qt-core-dir)
    (setq qt-gui-dir (concat qt-base-dir "/QtGui"))
    (setq qt-core-dir (concat qt-base-dir "/QtCore"))
    (semantic-add-system-include qt-base-dir 'c++-mode)
    (semantic-add-system-include qt-core-dir 'c++-mode)
    (semantic-add-system-include qt-gui-dir 'c++-mode)

    (add-to-list 'semantic-lex-c-preprocessor-symbol-file
                 (concat qt-base-dir "/Qt/qconfig.h"))
    (add-to-list 'semantic-lex-c-preprocessor-symbol-file
                 (concat qt-base-dir "/Qt/qconfig-dist.h"))
    (add-to-list 'semantic-lex-c-preprocessor-symbol-file
                 (concat qt-base-dir "/Qt/qglobal.h"))

    (add-to-list 'semantic-lex-c-preprocessor-symbol-map
                 '("Q_GUI_EXPORT" . ""))
    (add-to-list 'semantic-lex-c-preprocessor-symbol-map
                 '("Q_CORE_EXPORT" . ""))
    )
  )


(defun yc/add-gtk-support ( )
  "Add gtk related files."
  (interactive)
  (let ((prefix (completing-read
                 "Prefix (optional):"
                 (apply
                  'append
                  (mapcar
                   (lambda (dir)
                     (when (file-exists-p dir)
                       (remove-if
                        (lambda (x) (or (not (stringp x))
                                        (string-match (rx (or "." "..")) x)))
                        (directory-files dir t))))
                   '("/" "."))) nil nil "/usr/include" nil "/usr/include"))
        (gtk-related-includes (list
                               "bits"
                               "glib-2.0"
                               "gtk-3.0")))
    (mapc (lambda(dir)
            (let ((path (if prefix
                            (concat prefix "/" dir) dir)))
              (semantic-add-system-include path 'c++-mode)
              (semantic-add-system-include path 'c-mode)))
          gtk-related-includes)))

;;;; TAGS Menu
(add-hook 'semantic-init-hooks
          (lambda ()
            (condition-case nil (imenu-add-to-menubar "TAGS") (error nil))))

;;;; Custom template for srecode
(require 'srecode)
(yc/eval-after-load
 "srecode/map"
 (dolist (dir '("~/.emacs.d/templates/srecode"
                "~/.emacs.d/templates/srecode/private"))
   (add-to-list 'srecode-map-load-path (expand-file-name dir))))

;;;; Customized functions to generate code quickly.

(defun yc/insert-single-comment ()
  "Insert signle line of comment using srecode"
  (interactive)
  (condition-case err
      (progn
        (srecode-load-tables-for-mode major-mode)
        (yc/remove-empty-lines (point-min))
        (srecode-insert "declaration:comment-single-line")
        )
    (error (insert " /* */"))))



;;;; Hide-ifdefs

(autoload 'hide-ifdef-mode "hideif" ""  t)

(yc/eval-after-load
 "hideif"
 (custom-set-variables
  '(hide-ifdef-initially t)
  '(hide-ifdef-read-only nil)
  '(hide-ifdef-shadow t))
 (advice-add
  'hide-ifdefs :around
  (lambda (&rest args)
    (interactive)
    (setq hif-outside-read-only buffer-read-only)
    (unless hide-ifdef-mode (hide-ifdef-mode 1)) ; turn on hide-ifdef-mode
    (if hide-ifdef-hiding
        (show-ifdefs))			; Otherwise, deep confusion.
    (setq hide-ifdef-hiding t)
    (hide-ifdef-guts)
    (setq buffer-read-only (or hide-ifdef-read-only hif-outside-read-only))))

 (defun yc/add-to-ifdef-env (lst)
   "Helper function to update ifdef-env."
   (let (kvp k v)
     (while (setq kvp (pop lst))
       (setq k (car kvp)
             v (cdr kvp))
       (hif-set-var (intern k) t)
       (when (and (symbolp k) (symbolp v))
         (add-to-list 'semantic-lex-c-preprocessor-symbol-map (cons (symbol-name k)
                                                                    (symbol-name v)))))
     (condition-case msg
         (hide-ifdefs)
       (error nil))))

 (add-hook 'hide-ifdef-mode-hook
           (lambda ()
             ;; (setq hide-ifdef-define-alist
             ;;       '((default DEBUG)))
             ;; (hide-ifdef-use-define-alist 'default)
             )) ; use list2 by default


 (defun yc/toggle-hide-if-def-shadow ()
   "Toggle shadow"
   (interactive)
   (setq hide-ifdef-shadow (not hide-ifdef-shadow))
   (hide-ifdefs))

 ;; Copied from Ahei:
 ;; http://code.google.com/p/dea/source/browse/trunk/my-lisps/hide-ifdef-settings.el
 (defun hif-goto-endif ()
   "Goto #endif."
   (interactive)
   (unless (or (hif-looking-at-endif)
               (save-excursion)
               (hif-ifdef-to-endif))))

 (defun hif-goto-if ()
   "Goto #if."
   (interactive)
   (hif-endif-to-ifdef))

 (defun hif-goto-else ()
   "Goto #else."
   (interactive)
   (hif-find-next-relevant)
   (cond ((hif-looking-at-else)
          'done)
         ((hif-ifdef-to-endif) ; find endif of nested if
          (hif-goto-endif)) ; find outer endif or else

         ((hif-looking-at-else)
          (hif-goto-endif)) ; find endif following else

         ((hif-looking-at-endif)
          'done)

         (t
          (error "Mismatched #ifdef #endif pair"))))

 (defun yc/hide-ifdef-define ()
   "Define symbol at postion"
   (interactive)
   (let ((symbol (or (symbol-at-point)
                     (read-from-minibuffer "Define what:"))))
     (when symbol
       (hif-set-var symbol 1)
       (if hide-ifdef-hiding (hide-ifdefs)))))

 (defun yc/hide-ifdef-undefine ()
   "Define symbol at postion"
   (interactive)
   (let ((symbol (or (symbol-at-point)
                     (read-from-minibuffer "Undefine What:"))))
     (when symbol
       (hif-set-var symbol nil)
       (if hide-ifdef-hiding (hide-ifdefs)))))

 (define-key hide-ifdef-mode-map "\C-c@d" 'yc/hide-ifdef-define)
 (define-key hide-ifdef-mode-map "\C-c@u" 'yc/hide-ifdef-undefine)
 )

 ;; *************************** TAGS Database Settings *********************

;;;; xgtags settings.

(autoload 'helm-xgtags-mode "helm-xgtags" ""  t)
(autoload 'helm-xgtags-find-definition "helm-xgtags" ""  t)
(autoload 'helm-xgtags-update-tags "helm-xgtags" ""  t)

(defun yc/guess-available-tag-system (pt)
  "Check available tagging system at point PT."
  (let* (ctxt tag)
    (condition-case msg
        (progn
          (setq ctxt (semantic-analyze-current-context pt))
          (setq tag (car (and ctxt (reverse (oref ctxt prefix))))))
      (if (semantic-tag-p tag)
          (case (semantic-tag-class tag)
            ('variable 'semantic)
            (t 'both)))
      (error 'xgtags-only))))


(defun yc/goto-with-semantic (pt)
  "Store current postion and call (semantic-ia-fast-jump)"
  (interactive "d")
  (yc/store-mru-tag pt)
  (semantic-ia-fast-jump pt))

(defun yc/goto-with-xgtags ()
  "description"
  (interactive)
  (helm-xgtags-find-definition)
  (setq yc/tag-stack (cons 'xgtags yc/tag-stack)))

(defun yc/goto-with-any (pt)
  "description"
  (interactive "d")
  (condition-case msg
      (yc/goto-with-xgtags)
    (error
     (yc/goto-with-semantic pt))))

(defun yc/find-tag-dwim (pt)
  "Wrapper of `xgtags-find-tag'"
  (interactive "d")
  (case (yc/guess-available-tag-system pt)
    ('semantic (yc/goto-with-semantic pt))
    ('xgtags-only  (yc/goto-with-xgtags))
    (t (yc/goto-with-any pt))))

(yc/eval-after-load
 "helm-xgtags"
 (lazy-set-key
  (list  (cons "M-." 'yc/goto-with-any)
         (cons "M-*" 'yc/return-func)
         (cons "C-M-." 'helm-xgtags-find-definition)
         (cons "\C-csU" 'helm-xgtags-update-tags))
  helm-xgtags-mode-map))

(define-key global-map [remap find-tag] 'yc/find-tag-dwim)
;; (yc/set-keys '(("M-." . 'yc/goto-with-xgtags)
;;                ("M-*" 'yc/return-func)))

 ;; *************************** Python Settings ****************************
(yc/set-mode 'python-mode
             "\.py\\'")
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(autoload 'python-mode "python" ""  t)


(yc/eval-after-load
 "python"
 (cdsq yc/r-match-pyfile
   (rx (group (+? ascii)) ".py" (? (or "c" "o"))))

 (defvar yc/system-py-modules nil "Python modules installed in system directory")


 (defun yc/get-python-files-of-dir (dir)
   "return all python files without extension"
   (let ((fn-list nil))
     (when (and dir
                (file-exists-p dir))
       (dolist (fn (directory-files dir nil yc/r-match-pyfile))
         (if (string-match yc/r-match-pyfile fn)
             (progn
               (setq fn-list (append fn-list (list (match-string 1 fn))))
               ))))
     fn-list))


 (defun yc/get-python-modules ()
   "description"
   (if (not yc/system-py-modules)
       (setq yc/system-py-modules
             (mapcar
              (lambda (f) (list f ))
              (apply
               'append
               (mapcar
                'yc/get-python-files-of-dir
                semantic-dependency-system-include-path)))
             ))
   (append (yc/get-python-files-of-dir ".") (copy-sequence yc/system-py-modules)))

 (define-skeleton skeleton-python-import
   "generate include<>" ""
   > "import "
   (completing-read
    "Import File:" (yc/get-python-modules)))

 (autoload 'highlight-indentation-mode "highlight-indentation" t nil)

 (add-hook
  'python-mode-hook
  (lambda ()
    (setq fill-column 78) ;; PEP8
    (highlight-indentation-mode t)
    ;; (add-to-list 'ac-sources 'ac-source-pycomplete)

    ;; (set (make-local-variable 'ac-sources)
    ;;      (append ac-sources '(ac-source-rope) '(ac-source-yasnippet))
    ;;      )
    ;; (autoload 'pymacs-apply "pymacs")
    ;; (autoload 'pymacs-call "pymacs")
    ;; (autoload 'pymacs-eval "pymacs" nil t)
    ;; (autoload 'pymacs-exec "pymacs" nil t)
    ;; (autoload 'pymacs-load "pymacs" nil t)
    ;; (pymacs-load "ropemacs" "rope-")
    ;; (setq ropemacs-enable-autoimport t)
    ;; (rope-open-project "~/.emacs.d/database/python/")
    ;; (define-key python-mode-map (kbd "<C-h f>") 'python-describe-symbol)
    ))

 (defun yc/expand-python-Binds (start end)
   "Function to expand member functions called by Bind"
   (interactive "rp")
   (let ((r-match-Bind
          (rx (*? ascii) "Bind" (* blank) "("             ;; Start
              (* blank) (+ alnum) (* blank) ","           ;; Event
              (group (+? ascii)) (* blank)                ;; Function
              (? (: ","  (* blank) (+? ascii) (* blank))) ;; Possible source.
              ")"                                         ;; End
              ))
         (r-match-method-name
          (rx "self."
              bow (group (+? (or alnum "_"))) eow
              (? (: "." bow (group (+ (or "_" alnum))) eow))))
         func-list)
     (goto-char start)
     (while (search-forward-regexp r-match-Bind end t)
       (let ((tmp (match-string-no-properties 1))
             (method nil)
             )
         (when (string-match r-match-method-name tmp)
           (if (match-string 2 tmp)
               (setq method (match-string 2 tmp))
             (setq method (match-string 1 tmp)))
           (add-to-list 'func-list method t))))
     (if func-list
         (with-temp-buffer
           (mapc
            (lambda (x)
              (insert (format "
    def %s(self, evt):
        \"\"\"
        Handling %s event.
        Arguments:
        - `evt`: Event to be prcessed.
        \"\"\"
        pass

" x x))
              )
            func-list)
           (kill-ring-save (point-min) (point-max))
           (message "Now please go to a place to Yank the methods ..."))
       (message "No proper methods foud!"))))

 (mapc
  (lambda (table)
    (define-abbrev-table table '(
                                 ("imp" "" skeleton-python-import 1)
                                 )))
  '(python-mode-abbrev-table)))

(autoload 'pydoc "pydoc" ""  t)


(autoload 'c++-mode "cc-mode" "" t)
(autoload 'objc-mode "cc-mode"  "" t)

(yc/set-mode 'c++-mode
             (rx "." (or "H" "cc" "hh" "c" "h" "moc"
                         (: "include/" alnum )) eow))

(yc/set-mode 'c-mode (rx "." (or "C" "c" "ic") eow))

(yc/set-mode 'objc-mode
             (rx "." (or "mm" "m") eow))

(yc/eval-after-load
 "cc-mode"
 (require 'semantic/bovine/c)
 (require 'semantic/bovine/gcc)
 (when (not (member system-type '(gnu gnu/linux darwin cygwin)))
   (if (executable-find "gcc")
       (semantic-gcc-setup)
     (message "GCC is not installed, semantic analysis will be restriced.")))
 (yc/add-common-includes)

 ;; Customized doc-font
 (cdsq tbdoc-font-lock-doc-comments
   (let ((symbol "[a-zA-Z0-9_]+")
         (header "^ \\* "))
     `((,(concat header "\\("     symbol "\\):[ \t]*$")
        1 ,c-doc-markup-face-name prepend nil)
       (,(concat                  symbol     "()")
        0 ,c-doc-markup-face-name prepend nil)
       (,(concat header "\\(" "@" symbol "\\):")
        1 ,c-doc-markup-face-name prepend nil)
       (,(concat "[#%@]" symbol)
        0 ,c-doc-markup-face-name prepend nil)
       (,(concat "\\\\" symbol)
        0 ,c-doc-markup-face-name prepend nil)
       )))

 (cdsq tbdoc-font-lock-doc-protection
   `(("< \\(public\\|private\\|protected\\) >"
      1 ,c-doc-markup-face-name prepend nil)))

 (cdsq tbdoc-font-lock-keywords
   `((,(lambda (limit)
         (c-font-lock-doc-comments "/\\*\\*.*$" limit
           tbdoc-font-lock-doc-comments)
         (c-font-lock-doc-comments "/\\*!.*" limit
           tbdoc-font-lock-doc-comments)
         (c-font-lock-doc-comments "/\\*!-+" limit
           tbdoc-font-lock-doc-comments)
         (c-font-lock-doc-comments "/\\*!< " limit
           tbdoc-font-lock-doc-comments)
         (c-font-lock-doc-comments "/\\*< " limit
           tbdoc-font-lock-doc-protection)
         (c-font-lock-doc-comments "///.*$" limit
           tbdoc-font-lock-doc-comments)))))

 ;;;; This is a sample, real c-doc-comment-style will be set in "10-emacs-custome.el"
 ;; Base style, added my own doc-style.
 (c-add-style
  "t-base"
  `("bsd"
    (c-recognize-knr-p . nil)
    (c-basic-offset . 4)
    (tab-width . 4)
    (indent-tabs-mode . nil)
    (comment-column . 40)

    (c-doc-comment-style . ((c-mode . tbdoc)
                            (c++-mode . tbdoc)
                            (objc-mode . tbdoc)
                            (java-mode . tbdoc)
                            (awk-mode . autodoc)
                            (other . tbdoc)))))

 (defun c-lineup-arglist-tabs-only (ignored)
   "Line up argument lists by tabs, not spaces"
   (let* ((anchor (c-langelem-pos c-syntactic-element))
          (column (c-langelem-2nd-pos c-syntactic-element))
          (offset (- (1+ column) anchor))
          (steps (floor offset c-basic-offset)))
     (* (max steps 1)
        c-basic-offset)))

 (c-add-style
  "kernel"
  `("linux"
    (tab-width . 8)
    (indent-tabs-mode . t)
    (c-offsets-alist
     (arglist-cont-nonempty
      c-lineup-gcc-asm-reg
      c-lineup-arglist-tabs-only))))

 ;; "Based on Google C/C++ Programming Style"
 "Based on Google C/C++ Programming Style"
 (c-add-style
  "tubo"
  `("t-base"
    (c-hanging-braces-alist . ((defun-open after)
                               (defun-close before after)
                               (class-open after)
                               (class-close before after)
                               (namespace-open after)
                               (inline-open after)
                               (inline-close before after)
                               (block-open after)
                               (block-close . c-snug-do-while)
                               (extern-lang-open after)
                               (extern-lang-close after)
                               (statement-case-open after)
                               (substatement-open after)))
    (c-hanging-colons-alist . ((case-label)
                               (label after)
                               (access-label after)
                               (member-init-intro before)
                               (inher-intro)))
    (c-hanging-semi&comma-criteria
     . (c-semi&comma-no-newlines-for-oneline-inliners
        c-semi&comma-inside-parenlist
        c-semi&comma-no-newlines-before-nonblanks))
    (c-indent-comments-syntactically-p . nil)
    (c-cleanup-list . (brace-else-brace
                       brace-elseif-brace
                       brace-catch-brace
                       empty-defun-braces
                       defun-close-semi
                       list-close-comma
                       scope-operator))
    (c-offsets-alist . ((func-decl-cont . ++)
                        (member-init-intro . +)
                        (member-init-cont  . c-lineup-multi-inher)
                        (inher-intro . ++)
                        (comment-intro . 0)
                        (arglist-close . c-lineup-arglist)
                        (topmost-intro . 0)
                        (block-open . 0)
                        (inline-open . 0)
                        (substatement-open . 0)
                        (statement-cont
                         . c-lineup-assignments)
                        (label . /)
                        (case-label . 0)
                        (statement-case-open . 0)
                        (statement-case-intro . +) ; case w/o {
                        (access-label . -)
                        (inextern-lang . 0)
                        (innamespace . 0)))
    ))

 ;; style for express engine of mysql
 (c-add-style
  "express-engine"
  `("tubo"
    (indent-tabs-mode . t)
    (arglist-cont-nonempty
     c-lineup-gcc-asm-reg
     c-lineup-arglist-tabs-only)))

;; Coding style for MySql
 (c-add-style
  "mysql"
  '("t-base"
    (c-basic-offset . 2)
    (indent-tabs-mode . nil)
    (c-comment-only-line-offset . 0)
    (c-offsets-alist . ((statement-block-intro . +)
                        (knr-argdecl-intro . 0)
                        (substatement-open . 0)
                        (label . -)
                        (statement-cont . +)
                        (arglist-intro . c-lineup-arglist-intro-after-paren)
                        (arglist-close . c-lineup-arglist)
                        (innamespace . 0)
                        (inline-open . 0)
                        (statement-case-open . +)
                        ))
    ))

 (defvar yc/c-file-mode-mapping
   (list (cons (rx (or "linux-" "kernel" "driver" "samba")) "kernel")
         (cons (rx (or "curl" "emacs" "gnome")) "gnu")
         (cons "storage/express/" "express-engine")
         (cons (rx (or "mysql" "gbase" ) (*? ascii) "/") "mysql"))
   "List of possible coding styles")

 (defun yc/guess-c-stype ()
   "Guess c-style based on input filename"
   (interactive)
   (message "Style is %s" (yc/get-c-stype (buffer-file-name))))

 (defun yc/get-c-stype (filename)
   "Guess c-style based on input filename"
   (let (style
         (n 0)
         (l (length yc/c-file-mode-mapping)))
     (when filename
       (while (and (not style)
                   (< n l))
         (let* ((mm (nth n yc/c-file-mode-mapping)))
           (if (string-match (car mm) filename)
               (setq style (cdr mm))
             (setq n (1+ n))))))
     (or style "tubo")))

 (custom-set-variables
  '(c-doc-comment-style (quote ((c-mode . tbdoc) (c++-mode . tbdoc)
                                (objc-mode . tbdoc)
                                (java-mode . javadoc) (pike-mode . autodoc)))))

 (autoload 'global-cwarn-mode "cwarn")
 (autoload 'ctypes-auto-parse-mode "ctypes" ""  t)
 (autoload 'uml/struct-to-UML "semantic-uml" ""  t)
 (autoload 'uml/struct-to-dia "semantic-uml" ""  t)
 (add-hook 'c-mode-common-hook
           (lambda ()
             (let ((style (yc/get-c-stype (buffer-file-name))) )
               (c-set-style style)
               ;; (unless (string= style "tubo")
               ;;   (setq flycheck-checker 'c/c++-gcc)
               ;;   )
               (when (string= style "kernel-coding")
                 (add-to-list
                  'hide-ifdef-define-alist
                  '(kernel __KERNEL__ CONFIG_SMP CONFIG_PCI CONFIG_MMU))
                 (hide-ifdef-use-define-alist 'kernel)))
             (local-set-key "\C-csD" 'uml/struct-to-dia)
             (local-set-key "\C-csd" 'uml/struct-to-UML)
             (local-set-key "\C-c\C-h" 'yc/swith-h-cpp)
             (local-set-key "\M-:" 'yc/enable-disable-c-block)
             ;; (setq semantic-dependency-include-path 'yc/system-include-dirs)
             (global-cwarn-mode 1)
             (ctypes-auto-parse-mode 1)
             (c-setup-doc-comment-style)
             (ede-turn-on-hook)
             (when ede-object
               (yc/add-to-ifdef-env (ede-preprocessor-map ede-object)))

             (condition-case msg
                 (hide-ifdef-mode 1)
               (error nil)))))

(defun yc/list-include-dirs ()
  "List all include directories"
  (interactive)
  (let* ((bf
          (get-buffer-create (format "**Include path for %s"
                                     (if (buffer-file-name)
                                         (file-name-nondirectory (buffer-file-name)) "None"))))
         (obj ede-object))
    (with-current-buffer bf
      (erase-buffer)
      (print (ede-system-include-path obj) (current-buffer)))
    (display-buffer bf)))

(defun yc/get-all-includes ()
  "Return all include directories"
  (-flatten (list
             (if ede-object (ede-system-include-path ede-object) nil)
             (check-symbol semantic-dependency-include-path)
             (check-symbol semantic-dependency-system-include-path))))


;; 输入 inc , 可以自动提示输入文件名称,可以自动补全.
(mapc
 (lambda (table)
   (define-abbrev-table table '(
                                ("inc" "" skeleton-include 1)
                                )))
 '(c-mode-abbrev-table c++-mode-abbrev-table objc-mode-abbrev-table))

(mapc
 (lambda (table)
   (define-abbrev-table table '(
                                ("imp" "" skeleton-import 1)
                                )))
 '(objc-mode-abbrev-table))

(defvar inc-minibuffer-compl-list nil "nil")

(defun yc/update-minibuffer-complete-table ( )
  "Complete minibuffer"
  (interactive)
  (let ((prompt (minibuffer-prompt))
        (comp-part (minibuffer-contents-no-properties))
        (filter-dot-dir
         (lambda (x)
           "Function to filter out . & .. or invalid items from list"
           (or (not (stringp x))
               (string-match (rx (or "." "..")) x)))))

    (setq minibuffer-completion-table
          (cond
           ;; Used when adding include or import.
           ((and (or (string= "Include File:" prompt)
                     (string= "Import File:" prompt))
                 (> (length comp-part) 0))
            (append minibuffer-completion-table
                    (let ((inc-files nil)
                          (dirname nil)
                          (tmp-name nil))
                      (mapc
                       (lambda (d)
                         (setq dirname (format "%s/%s" d comp-part))
                         (when (file-exists-p dirname)
                           (mapc
                            (lambda (x)
                              (when (not (or (string= "." x)
                                             (string= ".." x)))
                                (setq tmp-name (format "%s/%s" comp-part x))
                                (add-to-list 'inc-files tmp-name)))
                            (directory-files dirname))))
                       yc/tmp-include-dirs)
                      inc-files)))
           ;; Used when completing for yc/add-gtk-support
           ((or (string= "Prefix (optional):" prompt)
                (string=  "QT Base dir:"  prompt))
            (if (not (file-exists-p comp-part))
                minibuffer-completion-table
              (append minibuffer-completion-table
                      (remove-if filter-dot-dir (directory-files comp-part t)))))
           (t minibuffer-completion-table))))
  (insert "/"))

(define-key minibuffer-local-completion-map "/" 'yc/update-minibuffer-complete-table)

(defun yc/incfile-is-local (inc-file)
  "Judge whether inc-file is local."
  (let ((inc-fullpath nil)
        (local-dirs '("."))
        (ret-val nil))
    (if ede-object-project
        (setq local-dirs
              (append local-dirs
                      (ede-system-include-path ede-object-project))))
    (catch 'incfile
      (mapc
       (lambda (dir)
         (if (file-exists-p (format "%s/%s" dir inc-file))
             (progn
               (setq ret-val t)
               (throw 'incfile t))))
       local-dirs)
      )
    ret-val))


(defun yc/update-inc-marks ( )
  "Update place markers."
  (let ((statement (buffer-substring-no-properties
                    (point-at-bol) (point-at-eol)))
        (inc-file nil)
        (prompt nil)
        (to-begin nil)
        (to-end nil)
        (yc/re-include
         (rx (group (or "#include" "#import"))
             (+ blank) "|XXX|" (group (*? ascii)) "|XXX|")))
    (when (string-match yc/re-include statement)
      (setq prompt (match-string 1 statement))
      (setq inc-file (match-string 2 statement))
      (if (yc/incfile-is-local inc-file)
          (setq to-begin "\"" to-end "\"")
        (setq to-begin "<" to-end ">"))
      (move-beginning-of-line 1)
      (kill-line)
      (insert (format "%s %s%s%s" prompt to-begin inc-file to-end))
      (move-end-of-line 1)
      (if (functionp 'ac-abort) ;; Stop AC if necessary.
          (ac-abort))))
  (setq yc/tmp-include-dirs nil))

(define-skeleton skeleton-include
  "generate include<>" ""
  > "#include |XXX|"
  (completing-read
   "Include File:"
   (apply
    'append
    (mapcar
     (lambda (dir)
       (when (file-exists-p dir)
         (directory-files dir nil (rx bol (+? (or alnum "_" "-")) (or (: "." (or "h" "hpp")) eol) ))))
     (if (and ede-object-project
              (ede-system-include-path ede-object-project))
         (progn
           (setq yc/tmp-include-dirs
                 (append (ede-system-include-path ede-object-project) yc/system-include-dirs))
           yc/tmp-include-dirs)
       (progn
         (setq yc/tmp-include-dirs
               semantic-dependency-system-include-path)
         yc/tmp-include-dirs)))))
  "|XXX|"
  (yc/update-inc-marks))

(define-skeleton skeleton-import
  "generate include<>" ""
  > "#import |XXX|"
  (completing-read
   "Include File:"
   (mapcar
    (lambda (f) (list f ))
    (apply
     'append
     (mapcar
      (lambda (dir)
        (when (file-exists-p
               dir)
          (directory-files dir nil (rx (+? ascii) (or ".h" ".hpp")))))
      (if ede-object-project
          (append (ede-system-include-path ede-object-project) yc/system-include-dirs)
        semantic-dependency-system-include-path)
      ))))
  "|XXX|"
  (yc/update-inc-marks))



(autoload 'expand-member-functions "member-function" ""  t)
(yc/eval-after-load "member-function"
                    (setq mf--source-file-extension "cpp"
                          mf--insert-commentary t))
(add-hook 'c++-mode-hook
          (lambda ()
            (local-set-key
             "\C-cm"
             'expand-member-functions)))

;;;; Function to change settings for tab.
(defun yc/toggle-tab-mode ()
  "Function to change tabs quickly"
  (interactive)
  (if indent-tabs-mode
      (progn
        (setq-default tab-width 4)
        (setq-default c-basic-offset 4)
        (setq-default indent-tabs-mode nil))
    (progn
      (setq-default tab-width 8)
      (setq-default c-basic-offset 8)
      (setq-default indent-tabs-mode t)
      )
    )
  )



(defun yc/header-make ()
  "Make header based on srecode"
  (interactive)
  (condition-case err
      (progn
        (srecode-load-tables-for-mode major-mode)
        (yc/remove-empty-lines (point-min))
        (goto-char (point-min))
        ;; (beginning-of-buffer)
        (srecode-insert "file:fileheader")
        (yc/remove-empty-lines (point-max))
        (goto-char (point-max))
        (srecode-insert "file:fileheader_end")
        )
    (error (srecode-insert "file:filecomment")))
  (delete-trailing-whitespace))

(defun yc/insert-empty-template ()
  "Make header based on srecode"
  (interactive)
  (save-excursion
    (srecode-load-tables-for-mode major-mode)
    (srecode-insert "file:empty")
    (delete-trailing-whitespace)))

 ;; Irony mode
(yc/autoload 'irony-mode "irony")
(add-hook 'c-mode-common-hook
          (lambda ()
            (irony-mode 1)))

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(add-hook 'irony-mode-hook
          (lambda ()
            (irony-cdb-autosetup-compile-options)
            (define-key irony-mode-map [remap complete-symbol]
              'irony-completion-at-point-async)))


;;;; Common Program settings

(defun yc/basic-prog-keybinding ()
  "Some Basic keybinds fro programming"
  (interactive)
  (local-set-key (kbd "M-|") 'align)
  ;; (local-set-key  [(tab)] 'indent-or-complete)
  )

(cdsq if-0-start
  "// TODO: Remove this ifdef!
#if 0
")

(cdsq if-0-end
  "
#endif // End of #if 0
")

(defun yc/enable-disable-c-block (start end)
  "Enable or disable c blocks using #if 0/#endif macro"
  (interactive "rp")
  (save-excursion
    (let ((r-match-if0 (rx (* (or space "
"))  "// TODO: Remove this ifdef!
#if 0
"  (group (+ anything)) "#endif // End of #if 0" (* "
"))))
      (goto-char start)
      (if (and (looking-at r-match-if0)
               (search-forward-regexp r-match-if0 end t))
          (replace-match "\\1") ;; remove if-endif tag.
        (goto-char end)
        (insert if-0-end)
        (goto-char start)
        (insert if-0-start)
        (goto-char (+ end (length if-0-start) (length if-0-end)))
        (setq end (skip-chars-forward " \t\n")))))
  (indent-region start end))

(autoload 'helm-semantic-or-imenu "helm-semantic" ""  t)

(defun yc/show-methods-dwim (arg)
  "Show methods found in current file, using any possible way.."
  (interactive "P")
  (condition-case error
      (helm-xgtags-parse-file (buffer-file-name))
    (error (helm-semantic-or-imenu arg))))

(defun setup-program-keybindings()
  ;;;; Common program-keybindings
  (interactive)
  (helm-xgtags-mode 1) ;; keybindings for xgtags.

  ;;;; "keybindings for semantic"
  (semantic-default-c-setup)
  ;; (local-set-key "." 'semantic-complete-self-insert)
  ;; (local-set-key ">" 'semantic-complete-self-insert)
  (local-set-key (kbd "M-n") 'senator-next-tag)
  (local-set-key (kbd "M-p") 'senator-previous-tag)
  ;; (local-set-key (kbd "C-M-;") 'yc/enable-disable-c-block)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol)
  (local-set-key "\C-c}" 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-cJ" 'semantic-analyze-proto-impl-toggle)
  (local-set-key "\C-cP" 'semantic-ia-show-summary)
  (local-set-key "\C-cb" 'semantic-mrub-switch-tags)
  (local-set-key "\C-c\C-j" 'yc/goto-func-any)
  (local-set-key "\C-cj" 'yc/goto-with-semantic)
  (local-set-key "\C-co"    'yc/open-header)
  (local-set-key "\C-co"    'yc/open-header)
  (local-set-key "\C-cR" 'yc/symref)
  (local-set-key "\C-cr" 'yc/symref-symbol)
  (local-set-key [S-f8] 'yc/return-func)
  (local-set-key [M-S-f12] 'yc/return-func)
  (local-set-key (kbd "C-x SPC") 'yc/store-mru-tag)
  (local-set-key (kbd "C-c SPC") 'senator-fold-tag-toggle)
  (local-set-key "\C-cp" 'semantic-ia-show-doc)
  (local-set-key "\C-cdp" (lambda (pt) "Look up symbol through Devhelp"
                            (interactive "P")
                            (let ((sym nil))
                              (when (and sym (executable-find "devhelp"))
                                (shell-command (format "devhelp -s \"%s\"" sym))))))
  (local-set-key (kbd "<M-return>") 'semantic-ia-complete-symbol)

  ;;;; Keybindings for srecode
  (local-set-key "\C-cdc" 'srecode-document-insert-comment)
  (local-set-key "\C-cdf" 'srecode-document-insert-function-comment)
  (local-set-key "\C-cdh" 'yc/header-make)
  (local-set-key "\C-cde" 'yc/insert-empty-template)
  (local-set-key "\C-cds" 'yc/insert-single-comment)
  (local-set-key "\C-cdv" 'srecode-document-insert-variable-one-line-comment)
  ;; (local-set-key "\C-cl" 'yc/list-attentions)
  ;;;; Others
  (local-set-key "\C-c\C-r" 'semantic-symref)
  (local-set-key "\M-m" 'yc/show-methods-dwim))



(autoload 'highlight-parentheses-mode "highlight-parentheses"
  "Minor mode to highlight the surrounding parentheses."  t)

(yc/eval-after-load
 "highlight-parentheses"
 (setq hl-paren-colors '("red" "yellow" "cyan" "magenta" "green" "red")))


 ;; Lisp mode.

(dsq yc/lisp-keywords
      (rx bow (group (or "add-to-list" "try-require" "add-hook" "autoload"
                         "yc/eval-after-load" "try-require-autoloads"
                         "fboundp" "boundp" "featurep" "cdsq"
                         "cds" "dsq" "sif" "aif"
                         (: "yc/" (+ (or alnum "-" "_")))
                         "csq" "cdsq"  "defun*" "defmacro*")) eow)
      "My Lisp keywords")

(define-skeleton skeleton-require
  "generate req<>" ""
  > "(require '"
  (completing-read
   "Require File:"
   (apply
    'append
    (mapcar
     (lambda (dir)
       (when (file-exists-p dir)
         (mapcar 'file-name-sans-extension
                 (directory-files dir nil (rx (+? (or alnum "_" "+" "-")) (? (or ".el" ".gz")))))
         ))
     load-path)))
  ")")

(defun my-lisp-hook ()
  "Hook to run for Lisp mode."
  (yc/add-keyword yc/lisp-keywords 'font-lock-keyword-face)
  (set (make-local-variable 'header-field-list)
        '(lisp_desc blank copyright blank author blank n_emacs gpl
                    blank e_comment blank))
  (make-local-variable 'autopair-skip-whitespace)
  (setq autopair-skip-whitespace 'chmop
        fill-column 86))

(define-abbrev-table 'emacs-lisp-mode-abbrev-table
  '(
    ("req" "" skeleton-require 1)
    ))

(defalias 'elisp-mode 'eamcs-lisp-mode)
(add-hook 'emacs-lisp-mode-hook 'my-lisp-hook)
(add-hook 'lisp-mode-hook 'my-lisp-hook)
(yc/set-mode 'emacs-lisp-mode (rx "." (or "el" "sexp") eol))

;; ***************** sh-mode *****************
(autoload 'sh-mode "sh-script.el" t)
(yc/set-mode 'sh-mode
             (rx (or (: "/etc/init.d/" (+ ascii))
                     (: (+? ascii) ".zsh")
                     ) eol))

(yc/eval-after-load
 "sh-script"
 (add-hook 'sh-mode-hook
           (lambda ()
             (let ((zsh-file (executable-find "zsh"))
                   (fname (buffer-file-name)))
               (when (and fname
                          (string-match (rx (*? ascii) (| "zsh" "zsh")) fname)
                          zsh-file)
                 (setq-default sh-shell-file zsh-file)
                 (sh-set-shell (file-name-nondirectory zsh-file))))
             ;; remap some keybinding to nil. those keys are handled by autopair!
             (define-key sh-mode-map "<" nil)
             (define-key sh-mode-map "(" nil)
             (define-key sh-mode-map "{" nil)
             (define-key sh-mode-map "[" nil)
             (define-key sh-mode-map "'" nil)
             (define-key sh-mode-map "`" nil)
             (define-key sh-mode-map "\"" nil)))
 (custom-set-variables
  '(sh-builtins
    (quote
     (
      (shell "cd" "echo" "eval" "set" "shift" "umask" "unset" "wait")

      (bash sh-append shell "." "alias" "bg" "bind" "builtin" "caller" "compgen" "complete"
            "declare" "dirs" "disown" "enable" "fc" "fg" "help" "history" "jobs" "kill"
            "let" "local" "popd" "printf" "pushd" "shopt" "source" "suspend" "typeset"
            "unalias" "command" "hash" "test" "type" "eval" "export" "getopts" "newgrp" "pwd"
            "read" "readonly" "times" "ulimit" "alias" "bg" "false" "fc" "fg" "jobs" "kill" "let" "print"
            "time" "typeset" "unalias" "whence")

      (zsh sh-append bash "autoload" "bindkey" "builtin" "chdir" "compctl" "declare" "dirs"
           "disable" "disown" "echotc" "enable" "functions" "getln" "hash" "history"
           "integer" "limit" "local" "log" "popd" "pushd" "r" "readonly" "rehash" "sched"
           "setopt" "source" "suspend" "true" "ttyctl" "type" "unfunction" "unhash"
           "unlimit" "unsetopt" "vared" "which" "zle" "compdef" "compinit" "zstyle" "colors")
      ))))
 )



(defcustom yc/c-compiler "gcc"
  "prefer c compiler."
  :group 'user
  );; (if (executable-find "clang")
;;   (setq yc/c-compiler "clang")
;;   (setq yc/c-compiler "gcc"))
(setenv "cc" yc/c-compiler)



(yc/set-mode 'shell-script-mode
             "\\.ebuild$")

 ;;;;;;;; configurations  about gdb;

(when (autoload 'gdb-many-windows "gdb-mi.el" nil t nil)
  (yc/eval-after-load
   "gud"
   (define-key gud-minor-mode-map (kbd "<f5>") 'gud-go)
   (define-key gud-minor-mode-map (kbd "<S-f5>") 'gud-until)
   (define-key gud-minor-mode-map (kbd "<f6>") 'gud-next)
   (define-key gud-minor-mode-map (kbd "<f7>") 'gud-step)
   (define-key gud-minor-mode-map (kbd "<f8>") 'gud-finish)
   (define-key gud-minor-mode-map (kbd "<f9>") 'gud-break)
   (define-key gud-minor-mode-map (kbd "M-S-SPC") 'gdb-toggle-breakpoint)
   (define-key gud-minor-mode-map "\C-c\C-c" 'gdb-io-interrupt)
   (define-key gud-minor-mode-map "\C-c\C-z" 'gdb-io-stop)
   (define-key gud-minor-mode-map "\C-c\C-\\" 'gdb-io-quit)
   (define-key gud-minor-mode-map "\C-c\C-p" 'gud-print)
   (define-key gud-minor-mode-map (kbd "<S-f1>") 'gud-print)
   (define-key gud-minor-mode-map (kbd "C-M-S-n") 'gud-down)
   (define-key gud-minor-mode-map (kbd "C-M-S-p") 'gud-up)
   )

  (custom-set-variables
   '(gdb-many-windows t)
   '(gdb-non-stop-setting nil)
   '(gdb-show-threads-by-default t)
   '(gdb-switch-when-another-stopped nil)
   '(gdb-speedbar-auto-raise t)
   )

  (advice-add
   'gdb-setup-windows :around
   (lambda (&rest args)
     (interactive)
     (gdb-get-buffer-create 'gdb-locals-buffer)
     (gdb-get-buffer-create 'gdb-stack-buffer)
     (gdb-get-buffer-create 'gdb-breakpoints-buffer)
     (set-window-dedicated-p (selected-window) nil)
     (switch-to-buffer gud-comint-buffer) ;;0
     (delete-other-windows)
     (let* ((win-src (selected-window))
            (win-gud (split-window-right))
            (win-stack (split-window win-src ( / ( * (window-height win-src) 3) 4)))

            )

       ;; (gdb-set-window-buffer (gdb-locals-buffer-name) nil win-local)
       (set-window-buffer
        win-src
        (if gud-last-last-frame
            (gud-find-file (car gud-last-last-frame))
          (if gdb-main-file
              (gud-find-file gdb-main-file)
            ;; Put buffer list in window if we
            ;; can't find a source file.
            (list-buffers-noselect))))

       (setq gdb-source-window win-src)
       (gdb-set-window-buffer (gdb-stack-buffer-name) nil win-stack)
       (select-window win-gud)))))


;;;;;;;; configurations of powershell-mode ;;;;;;;;
(autoload 'powershell-mode "powershell-mode" ""  t)
(yc/set-mode 'powershell-mode
             "\\.ps1\\'")

(yc/eval-after-load "powershell-mode"
                    (defun yc/pws-find-tag (function)
                      "find defination of function under current poin"
                      (interactive
                       (let* ((fn (thing-at-point 'symbol))
                              (val nil))
                         (message fn)
                         (setq val (completing-read (if fn
                                                        (format "search for (default %s): " fn)
                                                      "search for: ")
                                                    obarray 'fboundp t nil nil
                                                    ))
                         (list (if (equal val "")
                                   fn (intern val)))))

                      (let ((cmd nil))
                        (if (null function)
                            (message "you didn't specify a function")
                          (progn
                            (setq cmd (concat "egrep -i \"^function +" function "\" . -ri"))
                            (eshell-command cmd)
                            (pop-to-buffer (get-buffer "*grep*"))
                            (setq buffer-read-only nil)
                            (goto-char (point-min))
                            (kill-line 3)
                            (insert (concat "*********************** find tag for:"
                                            function "********************\n\n"))
                            (setq buffer-read-only t)
                            (goto-char (point-min))))))

                    (defun yc/pws-get-help (function)
                      "display the documentation of function (a symbol)."
                      (interactive
                       (let ((fn (thing-at-point 'symbol))
                             val)
                         (message fn)
                         (setq val (completing-read (if fn
                                                        (format "help for (default %s): " fn)
                                                      "help for: ")
                                                    obarray 'fboundp t nil nil
                                                    ))
                         (list (if (equal val "")
                                   fn (intern val)))))

                      (if (null function)
                          (message "you didn't specify a function")
                        (progn
                          (start-process "powershell-help" nil "devhelp" "-s" function))))



                    (add-hook 'powershell-mode-hook
                              (lambda()
                                (progn
                                  (yc/common-program-hook)
                                  (local-set-key [(f1)] 'yc/pws-get-help)
                                  (local-set-key [(meta .)] 'yc/pws-find-tag)
                                  ))))

 ;; windows batch-mode for bat files.
(autoload 'batch-mode "batch-mode" ""  t)
(yc/set-mode 'batch-mode
             (rx "." (or "bat" "cmd")))

 ;;;;;;;;;;;;;;;; lua-mode ;;;;;;;;;;;;;;;;;;;;;

;; (try-require 'lua-mode)
;; (autoload 'lua-mode "lua-mode")
;; (setq lua-default-application "/usr/bin/lua")
(autoload 'makefile-mode "make-mode" nil t)

(yc/set-mode 'makefile-mode
             (rx (or (: (or "Makefile" "makefile") "." (+ alnum))
                     (: (+ alnum) ".mk"))))


(autoload 'cmake-mode "cmake-mode" ""  t)
(autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
(yc/set-mode 'cmake-mode (rx (or "CMakeList.txt" "CMakeLists.txt" ".cmake")))


(yc/eval-after-load
 "cmake-mode"


 (defun helm-cmake-help ()
   "Queries for any of the four available help topics and prints out the approriate page."
   (interactive)
   (let* ((default-entry (cmake-symbol-at-point))
          (command-list (cmake-get-list "command"))
          (variable-list (cmake-get-list "variable"))
          (module-list (cmake-get-list "module"))
          (property-list (cmake-get-list "property"))
          (all-words (append command-list variable-list module-list property-list))
          (h-source
           (helm-build-sync-source "Helm-Cmake"
             :init nil
             :candidates 'all-words
             :fuzzy-match nil
             :action 'helm-cmake--show-help)))

     (defun helm-cmake--show-help (cand)
       "description"
       (if (string= cand "")
           (error "No argument given")
         (if (member cand command-list)
             (cmake-command-run "--help-command" cand "*CMake Help*")
           (if (member cand variable-list)
               (cmake-command-run "--help-variable" cand "*CMake Help*")
             (if (member cand module-list)
                 (cmake-command-run "--help-module" cand "*CMake Help*")
               (if (member cand property-list)
                   (cmake-command-run "--help-property" cand "*CMake Help*")
                 (error "Not a know help topic.") ; this really should not happen
                 ))))))

          (helm :sources 'h-source
                :buffer "*helm helm-xgtags*"
                :preselect default-entry
                )))

 (add-hook 'cmake-mode-hook
           (lambda ()
             (yc/common-program-hook)
             (cmake-font-lock-activate)
             (let ((map (make-sparse-keymap)))
               (define-key map "\C-ch" 'helm-cmake-help)
               (define-key map "\C-cl" 'cmake-help-list-commands)
               (define-key map "\C-cu" 'unscreamify-cmake-buffer)
               (use-local-map map))))

 (define-skeleton skeleton-cmake-include
   "generate include()" ""
   > "include ("
   (completing-read
    "module name:"
    (string-split (yc/command-output-to-string "cmake" "--help-module-list")))
   ")")

 (define-abbrev-table 'cmake-mode-abbrev-table
   '(("inc" "inc" skeleton-cmake-include 1))))




;; ;; (try-require 'jde)
;; (require 'android-mode)
;; (setq android-mode-sdk-dir "/opt/android-sdk-update-manager")


;; (autoload 'protobuf-mode "protobuf-mode.el" "" t)
;; (add-to-list 'auto-mode-alist '("\\.proto\\'" . protobuf-mode))


;; Kconfig-mode
(autoload 'kconfig-mode "kconfig-mode.el")
(yc/set-mode 'kconfig-mode
             "Kconfig")




 ;; Ruby-mode
;; (autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)
;; (autoload 'run-ruby "inf-ruby"  "Run an inferior Ruby process" t)
;; (autoload 'inf-ruby-keys "inf-ruby"  "Set local key defs for inf-ruby in ruby-mode")

;; (add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
;; (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))


;; (yc/eval-after-load
;;  "ruby"
;;  (defun ruby-send-buffer ()
;;    "Send current buffer to execute"
;;    (interactive)
;;    (ruby-send-region (point-min) (point-max)))

;;  (add-hook 'ruby-mode-hook
;;            '(lambda ()
;;               (inf-ruby-keys)
;;               (define-key ruby-mode-map "\C-c\C-x" 'ruby-send-buffer)
;;               ))

;;  )


;; (autoload 'erlang-mode "erlang" "erlang"  t)
;; (add-to-list 'auto-mode-alist '("\\.erl\\'" . erlang-mode))
;; (add-to-list 'auto-mode-alist '("\\.escript\\'" . erlang-mode))
;; (yc/eval-after-load "erlang"
;;   (require 'erlang-flymake)
;;   (let ((erl-root
;;          (cond
;;           ((string= system-type "windows-nt") nil)
;;           ((string= system-type "darwin") nil)
;;           (t "/usr/lib/erlang"))))
;;     (setq erlang-root-dir erl-root)))


;; (autoload 'glsl-mode "glsl-mode" nil t)
;; (add-to-list 'auto-mode-alist
;;              (cons (rx "." (or "glsl" "vert" "frag" ".geom")) 'glsl-mode))

 ;; Javascript mode
(yc/autoload 'js2-mode)

(yc/set-mode 'js2-mode (rx (or (: bow "manifest")
                               ".json"
                               ".js"
                               ) eol))
(yc/eval-after-load
 "js2-mode"
 ;; (setq js2-mode-must-byte-compile nil
 ;;       js2-lazy-commas t
 ;;       js2-lazy-operators t
 ;;       js2-lazy-dots t
 ;;       js2-expr-indent-offset 4
 ;;       js2-paren-indent-offset 4
 ;;       js2-square-indent-offset 4
 ;;       )
 (define-key js2-mode-map "\C-c\C-x" 'executable-interpret)
 (add-hook 'js2-mode-hook
           (lambda ()
             (yc/common-program-hook))))

(custom-set-variables
 '(safe-local-variable-values
   (quote
    ((js2-basic-offset . 4)
     (emacs-lisp-docstring-fill-column . 75)
     (py-indent-offset . 4)))))

(autoload 'gjs-mode "gjs-mode" ""  t)
(yc/set-mode 'gjs-mode
             (rx "." "gjs" eow))


(defun yc/show-project-include-path ()
  (interactive)
  (if ede-object
      (print (ede-system-include-path ede-object))
    (message "Not controlled by EDE...")))

 ;; Flycheck..
(yc/autoload 'flycheck-tip-cycle "flycheck-tip")
(yc/autoload 'flycheck-tip-cycle-reverse "flycheck-tip")
(yc/eval-after-load
 "flycheck"
 (define-key flycheck-mode-map [f9] 'flycheck-tip-cycle)
 (define-key flycheck-mode-map (kbd "<M-f9>")  'flycheck-tip-cycle-reverse)
 (define-key flycheck-mode-map (kbd "<S-f9>") 'flycheck-list-errors)
 (define-key flycheck-mode-map (kbd "<M-S-f9>")  'flycheck-first-error))


 (add-hook 'flycheck-before-syntax-check-hook
           (lambda ()
             (let ((includes (yc/get-all-includes)))
               (setq flycheck-clang-include-path includes
                     flycheck-gcc-include-path includes))))



(autoload 'emr-show-refactor-menu "emr" "Show the refactor menu at point."  t)
(autoload 'emr-initialize "emr" "Initialize Emacs Refactor"  nil)

(global-set-key (kbd "<S-f6>") 'emr-show-refactor-menu)

(advice-add
 'emr-initialize :before
 (lambda (&rest args)
   (load-library "emr-prog2")))

(yc/eval-after-load
 "emr"
 (emr-initialize))

;; (custom-set-variables
;;  '(emr-clang-format-style '(("BasedOnStyle" . "LLVM")
;;                             ("IndentWidth" . "4")
;;                             ("BreakBeforeBraces" . "Stroustrup")
;;                             ("AllowShortIfStatementsOnASingleLine" . "false")
;;                             ("AlignConsecutiveAssignments". "true")
;;                             ("IndentCaseLabels" . "false")
;;                             ("PointerAlignment" . "Left")
;;                             ("UseTab" . "Never")))
;;  )


 ;; SQL Mode
(yc/eval-after-load
 "sql"
 (sql-set-product 'mysql))



(defun yc/get-compiling-threads ()
  "Return proper number of threads."
  (1+ (yc/get-cpu-number)))

(defun make-command(arg)
  (let* ((file (if buffer-file-name (file-name-nondirectory buffer-file-name)))
         (ext-name (if file (file-name-extension file)))
         (real_cmd
          (cond
           ;; Makefile
           ((or (file-exists-p "makefile")
                (file-exists-p "Makefile"))
            (if arg
                (format "make -j%d %s" (yc/get-compiling-threads) arg)
              (format "make -j%d"
                      (yc/get-compiling-threads))))
           ;; CMake
           ((file-exists-p "CMakeLists.txt")
            (format "cmake CMakeLists.txt" ))
           ;; C
           ((or (equal ext-name "cc")
                (equal ext-name "cpp"))
            (format "%s %s %s %s -std=c++11 -g -o %s"
                    (yc/get-env "CXX" 'executable-find
                                "g++" "clang++"  "mingw-g++")
                    (or (getenv "CPPFLAGS")"-Wall  ")
                    (get-user-defined-opts)
                    file
                    (file-name-sans-extension file)))
           ;; C++
           ((or (equal ext-name "c")
                (equal ext-name "C"))
            (format "%s -o %s %s %s %s %s %s -g -std=c99"
                    (yc/get-env "CXX" 'executable-find
                                "gcc" "clang"  "mingw-g++")
                    (file-name-sans-extension file)
                    (or (getenv "GTKFLAGS") "-Wall ")
                    (or (getenv "CPPFLAGS")"-DDEBUG=9  ")
                    (or (getenv "CFLAGS") "-Wall ")
                    (get-user-defined-opts)
                    file))
           ;; Tex
           ((or (equal ext-name "tex")
                (equal ext-name "TEX"))
            (format "xelatex %s" file))
           ((or (equal ext-name "sh")
                (equal ext-name "SH"))
            (format "./%s" file))
           ;; cmake.
           ((or (equal file "CMakeLists.txt")
                (equal ext-name ".cmake"))
            (format "cmake %s" file))
           ;; swig
           ((or (equal ext-name "i")
                (equal ext-name "swig"))
            (format "%s -c++ -java %s" (executable-find "swig") file))
           (t compile-command))))
    (if (and (executable-find "notify-send")
             (not (string-match ".*notify-send.*" real_cmd)))
        (message
         "%s && [ $? -eq 0 ] && notify-send --hint=int:transient:1 \"Compile finished: succeed\" ||
notify-send --hint=int:transient:1 \"Compile finished: failed\""
         real_cmd)
      real_cmd)))

(defun do-compile (arg)
  "Save buffers and start compile with ARG."
  (interactive "P")
  (let ((source-window (get-buffer-window))
        (compile-window nil))
    (save-some-buffers t)
    (setq compilation-read-command nil)
    (when (not (get-buffer-window "*compilation*"))
      (setq compile-window (split-window-vertically))
      (select-window compile-window)
      (get-buffer-create "*compilation*")
      (switch-to-buffer "*compilation*")
      (select-window source-window))
    (setq compilation-read-command nil)
    (make-local-variable 'compile-command)
    (setenv "LANG" "C")
    (compile (make-command arg))))


(global-set-key (kbd "<f6>")  'do-compile)
(global-set-key (kbd "<C-f6>")
                (lambda (arg)
                  (interactive "P")
                  (let ((buffer (get-buffer "*compilation*")))
                    (if buffer
                        (with-current-buffer buffer
                          (recompile arg))
                      (do-compile arg)))))

(yc/eval-after-load
 'compile
 (yc/set-keys
  (list
   (cons (kbd "<S-f9>")
         (lambda ()
           (interactive)
           (let ((cur (point)))
             (goto-char (point-min))
             (unless (search-forward-regexp (rx (* space) "error:" (* space)) nil t)
               (goto-char cur)))))
   (cons "<f9>" 'next-error))
  compilation-mode-map))



(autoload 'helm-make "helm-make" "\
Call \"make -j ARG target\". Target is selected with completion.

\(fn &optional ARG)" t nil)

(autoload 'helm-make-projectile "helm-make" "\
Call `helm-make' for `projectile-project-root'.
ARG specifies the number of cores.

\(fn &optional ARG)" t nil)

(advice-add 'helm-make :before #'yc/set-c-env)
(advice-add 'helm-make-projectile :before #'yc/set-c-env)

(global-set-key (kbd "<M-f6>") (yc/with-prefix 'helm-make (yc/get-compiling-threads)))
(global-set-key (kbd "<C-S-f6>") (yc/with-prefix 'helm-make-projectile (yc/get-compiling-threads)))

 ;; php mode

(defvar php-extra-constants 'nil "\
A list of additional strings to treat as PHP constants.")

(custom-autoload 'php-extra-constants "php-mode" nil)

(add-to-list 'interpreter-mode-alist (cons "php" 'php-mode))

(autoload 'php-mode "php-mode" "\
Major mode for editing PHP code.

\\{php-mode-map}

\(fn)" t nil)

(yc/set-mode 'php-mode
             (rx "." (or (: "php" (+ (or "s" "t" digit)))
                         "phtml" "Amkfile" "amk")))


(provide '04-rc-prog-mode)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; 04-rc-prog-mode.el ends here
;;; 04-rc-prog-mode.el ends here
