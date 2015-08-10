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

(dolist (dir (directory-files "." t))
  (when (file-directory-p dir)
    (message "\nProcessing files in directory %s" dir)
    (dolist (item (directory-files dir t (rx bol alnum)))
      (unless (file-directory-p item)
        (message "\t Processing file: %s" item)
              (with-temp-file item
                (insert-file-contents-literally item)
                (goto-char (point-min))
                (process-mode-marker))))))





(provide 'UpdateHeader)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; UpdateHeader.el ends here
