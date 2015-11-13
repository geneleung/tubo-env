;;; x86-help.el -- Brief introduction here.

;; Author: YangYingchao <yangyingchao@gmail.com>

;;; Commentary:
;; http://www.felixcloutier.com/x86/
;;; Code:
(require 'helm-source)
(yc/autoload 'eww)

(defcustom x86-help-reference-dir "~/Documents/TechBooks/x86-help/"
  "Folder to put references.."
  :type 'string
  :group 'x86-help)

(defvar x86-help--candidates nil "List of candidates.")


(defun x86-help--candidates ()
  "Generate a list of candidates."
  (print "enter")
  (unless (file-exists-p x86-help-reference-dir)
    (mkdir x86-help-reference-dir t))
  (unless x86-help--candidates
    (setq x86-help--candidates (mapcar 'file-name-sans-extension
                                       (directory-files x86-help-reference-dir nil
                                                        ".*.html"))))
  (unless x86-help--candidates
    (error "Nothing found in %s.
Download it from: http://www.felixcloutier.com/x86/ and decompress it to: %s."
           x86-help-reference-dir x86-help-reference-dir))
  x86-help--candidates)

(defun x86-help--show (item)
  "Show ITEM with eww.."
  (eww (format "file://%s/%s.html" (expand-file-name x86-help-reference-dir) item)))

;;;###autoload
(defun x86-help ()
  "Look up reference of an x86 assembly instruction."
  (interactive)
  (let ((x-source
         (helm-build-sync-source "Help-X86"
           :init nil
           :candidates (x86-help--candidates)
           ;; :fuzzy-match t
           :persistent-action 'x86-help--show
           :action  (helm-make-actions
                    "Open File" 'x86-help--show
                    "Edit File" 'find-file)
           )))
    (helm :sources 'x-source
          :buffer "*helm*"
          :preselect (thing-at-point 'symbol))
    ))

(provide 'x86-help)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; x86-help.el ends here
