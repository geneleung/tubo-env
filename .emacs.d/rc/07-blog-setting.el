;;;  07-blog-setting.el -- settings about my blog
;;; Commentary:
;;; Code:


(defconst github-io
  (expand-file-name "~/Work/yangyingchao.github.io")
  )

(defcustom yc/media-object-suffix-list
  '("jpg" "jpeg" "png" "gif" "mp4" "zip" "gz" "bz2")
  "希望处理的媒体文件类型"
  :group 'mwb
  :type 'list)

(defvar yc/b2p-method-alist
  '((org-mode yc/b2p-org)
    (nxml-mode yc/b2p-html)
    (html-mode yc/b2p-html))
  "Buffer to Post method alist")

(defvar yc/buffer-content nil "Content of buffer.")


(defun yc/make-media-object-file-data (media-path)
  "根据给出的文件路径返回相应的FileData，文件不存在返回nil"
  (if (file-exists-p media-path)
      (format "../../../assets/img/%s"
              (file-name-nondirectory media-path))
    nil))

(defun yc/replace-media-object-location (buf-str)
  "处理BUF-STR中的媒体文件，返回处理后的字符串"
  (mapc (lambda (suffix)
          (let ((regexp
                 (concat "[file]*[:]?[/\\]*[a-z]?[:]?[^:*\"?<>|#]+."
                         suffix))
                (current 0))
            (while (string-match regexp buf-str current)
              (let* ((media-path (match-string 0 buf-str))
                     (media-url
                      (save-match-data
                        (and (file-exists-p media-path)
                             (yc/make-media-object-file-data
                              media-path)))))

                (if media-url
                    (progn
                      (setq current
                            (+ (match-beginning 0)
                               (length media-url)))
                      (setq buf-str
                            (replace-match media-url
                                           t
                                           t
                                           buf-str)))
                  (setq current
                        (match-end 0)))))))
        yc/media-object-suffix-list)
  buf-str)

(defun yc/new-post ()
  "description"
  (interactive)
  (let ((name (expand-file-name
               (format "%s/org/%s-%s.org"
                       github-io
                       (format-time-string  "%Y-%m-%d" (current-time))
                       (completing-read "Name: " '("Unmaed"))))))
    (find-file name)))

(define-or-set blog-escape-alist
  '(
    (?: . "&#58;")
    (?< . "&#60;")
    (?= . "&#61;")
    (?> . "&#62;")
    (?  . "&#32;")
    )
  "escape table")

(defun yc/escape-html-characters (input)
  "Escape special characters in INPUT."
  (when (stringp input)
    (apply 'concat (mapcar (lambda (x)
                              (let ((kv (assoc x blog-escape-alist)))
                                (if kv (cdr kv) (string x))))
                            input))))

(defun yc/fetch-field (fmt &optional default &optional escape)
  (if (string-match fmt yc/buffer-content)
      (let ((result (match-string 1 yc/buffer-content)))
        (when (string-match "(nil)" result)
          (setq result default))
        (if escape
            (yc/escape-html-characters result)
          result))))

(defun yc/fetch-fields (fmt &optional default tolower)
  "Fetch all fields, and return a concatenated string.
FMT: format used by regex.
DEFAULT: default value to return if no match found.
TOLOWER: if specified, turn fields into lower case."
  (if (string-match fmt yc/buffer-content)
      (let ((result (match-string 1 yc/buffer-content)))
        (if (string-match "(nil)" result)
            default
          (concat (mapcar (lambda (x)
                            (if (= ?, x) ? (if (and (<= x ?Z) (>= x ?A) t) (+ x 32) x)))
                          result))))
    default))

(defun yc/b2p-html ()
  (cons (list
         ;; title
         (cons "title"
               (or (car (yc/fetch-fields (rx (or "<title>" "<TITLE>") (group (+? anything))
                                             (or "</title>" "</TITLE>"))))
                   "Unamed"))

         ;; categories
         (cons "categories"
               (let ((categories-list
                      (yc/categories-string-to-list
                       (car (yc/fetch-fields "CATEGORIES")))))
                 (or
                  categories-list
                  '("Copies"))))

         ;; tags
         (cons "mt_keywords"
               (or
                (yc/fetch-fields "KEYWORDS")
                ""))

         ;; dateCreated
         (cons "dateCreated"
               (list
                :datetime
                (let ((ctime (current-time)))
                  (cons (car ctime) (cadr ctime)))))
         ;; description
         (cons "description"
               (yc/replace-media-object-location
                (buffer-substring-no-properties
                 (yc/point-template-head-end)
                 (point-max))))))
)

(defun yc/b2p-other ()
  (delq nil
        (list
         ;; title
         (cons "title" (buffer-name))

         ;; categories
         (cons "categories" "Unknown")

         ;; tags
         (cons "keywords" "")

         ;; dateCreated
         (cons "dateCreated"
               (list
                :datetime
                (let ((ctime (current-time)))
                  (cons (car ctime) (cadr ctime)))))
         ;; description
         (cons "description"
               (let* ((bf (htmlize-buffer))
                      (content (with-current-buffer bf
                                 (buffer-substring-no-properties (point-min) (point-max)))))
                 (kill-buffer bf)
                 content)))))

(defun yc/current-buffer-to-post ()
  (setq yc/buffer-content (buffer-substring-no-properties (point-min) (point-max)))
  (let ((func (cadr (assoc major-mode yc/b2p-method-alist))))
    (if func
        (funcall func)
      (yc/b2p-other))))

(defmacro yc/mkfield (x)
  `(rx bol ,x (* space) (group (+? nonl) eol)))

(defun yc/b2p-org ()
  (cons (list
         ;; title
         (cons "title"  (yc/fetch-field (yc/mkfield  "#+TITLE:") "Unamed" t))

         ;; "description"
         (cons "description" (yc/fetch-field (yc/mkfield "#+DESCRIPTION:")))

         ;; categories
         (cons "categories" (yc/fetch-field (yc/mkfield "#+CATEGORY:")  "未分类"))

         ;; tags
         (cons "tags" (yc/fetch-fields (yc/mkfield "#+KEYWORDS:") nil t)))

        ;; Contents.
        (with-current-buffer (org-export-to-buffer 'html "*Org HTML Export*"
                               nil nil t t)
          (let ((buf-str
                 (yc/replace-media-object-location
                  (buffer-substring-no-properties
                   (point-min)
                   (point-max)))))
            (kill-buffer)
            buf-str))))

(defun yc/get-post-name (bfn)
  "BFN: base file name."
  (concat (if (string-match
               (rx bol
                   (repeat 4 digit) "-"   ;; year
                   (repeat 1 2 digit) "-" ;; month
                   (repeat 1 2 digit)     ;; day
                   "-")  bfn)
              bfn
            (concat
             (format-time-string  "%Y-%m-%d" (current-time)) "-" bfn))
          ".html"))

(defun yc/prepare-post (&optional skip)
  "Convert current buffer into html file and prepare send to github.
SKIP: skip running magit."
  (interactive)
  (let* ((fn (buffer-file-name))
         (ofn (file-name-nondirectory fn))
         (bfn (file-name-sans-extension ofn))
         (hfn (yc/get-post-name bfn))
         (dir (file-name-directory fn))
         (git (executable-find "git"))
        (content (yc/current-buffer-to-post))
        (final-content ))
    (with-temp-file (format "%s/_posts/%s" github-io hfn)
      (goto-char (point-min))
      (insert "---
layout : post
")
      (dolist (item (car content))
        (if (and (car item) (cdr item))
            (insert (concat (car item) " : " (cdr item) "\n"))))
      (insert (concat  "---\n" (cdr content))))

    (message "Post prepared..！")

    (if (and (fboundp 'magit-status)
             (not skip))
        (magit-status dir))))

(provide '07-blog-setting)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; 07-blog-setting.el ends here
