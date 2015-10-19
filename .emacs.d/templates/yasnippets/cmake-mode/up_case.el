;;; up_case.el -- Brief introduction here.

;; Author: YangYingchao <yangyingchao@gmail.com>

;;; Commentary:

;;; Code:

(let ((r-start-with-dot (rx bol "." ))
      (r-match-command (rx (group (+ (or alnum "_"))) (* space)
                                  "("
                                  (group (+? anything))
                                  ")")))
  (dolist (fn (directory-files "." nil))
    (unless (string-match r-start-with-dot fn)
      (message "fn: %s" fn)
      (find-file fn)
      (goto-char (point-min))
      (condition-case error
          (while (search-forward-regexp r-match-command nil t)
            (replace-match (format "%s(%s)" (upcase (match-string 1))
                                   (match-string 2)) nil nil)
            )
        (error ".."))

      )
    )
  )

(provide 'up_case)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; up_case.el ends here
