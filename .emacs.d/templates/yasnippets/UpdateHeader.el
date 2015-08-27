;;; UpdateHeader.el -- Brief introduction here.

;; Author: YangYingchao <yangyingchao@gmail.com>

;;; Commentary:
;;;  Update header of snippets..
;;; Code:

(defun process-mode-marker ()
  (save-excursion
    (unless (looking-at (rx "# -*- mode: snippet; require-final-newline: nil -*-"))
      (if (looking-at (rx "# -*- mode: snippet -*-"))
          (kill-line))
      (insert "# -*- mode: snippet; require-final-newline: nil -*-\n"))))

(defconst r-match-leading-spaces
  (rx bol (+ space) (group (+? anything)) eol)
  "regular expression to match leading spaces."
  )

(defun process-indentation ()
  (while (re-search-forward r-match-leading-spaces nil t)
    (replace-match "\\1$>" nil nil)))

(dolist (dir (directory-files "." t (rx alnum)))
  (when (file-directory-p dir)
    (message "\nProcessing files in directory %s" dir)
    (dolist (item (directory-files dir t (rx bol alnum)))
      (unless (file-directory-p item)
        (save-excursion
          (message "Processing file: %s" item)
          (find-file item)
          (goto-char (point-min))
          (process-indentation)
          (save-buffer)
          (kill-buffer))))
    ))





(provide 'UpdateHeader)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; UpdateHeader.el ends here
