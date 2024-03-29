;;; tblog.el -- Brief introduction here.

;; Author: YangYingchao <yangyingchao@gmail.com>

;;; Commentary:

;;; Code:

(defgroup tblog nil
  "Helper tool to create/commit posts to github.io.")

(defcustom tblog-repository (expand-file-name "~/Work/yangyingchao.github.io")
  "Path of blog repository."
  :type 'string
  :group 'tblog)

(defcustom tblog-media-object-suffix-list
  '("jpg" "jpeg" "png" "gif" "mp4" "zip" "gz" "bz2")
  "希望处理的媒体文件类型"
  :group 'tblog
  :type 'list)

(defcustom tblog-categories nil
  "Categories of posts."
  :type 'list
  :group 'tblog)

(defcustom tblog-tags nil
  "tags of posts."
  :type 'list
  :group 'tblog)

(defvar tblog--b2p-method-alist
  '((org-mode tblog-b2p-org)
    (nxml-mode tblog-b2p-html)
    (html-mode tblog-b2p-html))
  "Buffer to Post method alist.")

(defvar tblog--buffer-content nil "Content of buffer.")

(defun tblog-make-media-object-file-data (media-path)
  "根据给出的文件路径返回相应的FileData，文件不存在返回nil"
  (if (file-exists-p media-path)
      (format "../../../assets/img/%s"
              (file-name-nondirectory media-path))
    nil))

(defun tblog-replace-media-object-location (buf-str)
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
                             (tblog-make-media-object-file-data
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
        tblog-media-object-suffix-list)
  buf-str)

;;;###autoload
(defun tblog-new-post ()
  "Start a new post."
  (interactive)
  (let ((name (expand-file-name
               (format "%s/org/%s-%s.org"
                       tblog-repository
                       (format-time-string  "%Y-%m-%d" (current-time))
                       (completing-read "Name: " '("Unmaed"))))))
    (find-file name)))

(defvar blog-escape-alist
  '(
    (?: . "&#58;")
    (?< . "&#60;")
    (?= . "&#61;")
    (?> . "&#62;")
    (?  . "&#32;")
    )
  "escape table")

(defun tblog-escape-html-characters (input)
  "Escape special characters in INPUT."
  (when (stringp input)
    (apply 'concat (mapcar (lambda (x)
                             (let ((kv (assoc x blog-escape-alist)))
                               (if kv (cdr kv) (string x))))
                           input))))

(defun tblog--fetch-field (fmt &optional func)
  "Fetch all fields, and return a concatenated string.
FMT: format used by regex.
FUNC: function to be called."
  (let ((result (if (string-match fmt tblog--buffer-content)
                    (match-string 1 tblog--buffer-content))))
    (if (string-match "(nil)" result)
        (setq result nil))
    (if func
        (setq result (funcall func result)))
    result))

(defun tblog-b2p-html ()
  (cons (list
         ;; title
         (cons "title"
               (or (car (tblog--fetch-field (rx (or "<title>" "<TITLE>") (group (+? anything))
                                             (or "</title>" "</TITLE>"))))
                   "Unamed"))

         ;; categories
         (cons "categories"
               (let ((categories-list
                      (tblog-categories-string-to-list
                       (car (tblog--fetch-field "CATEGORIES")))))
                 (or
                  categories-list
                  '("Copies"))))

         ;; tags
         (cons "mt_keywords"
               (or
                (tblog--fetch-field "KEYWORDS")
                ""))

         ;; dateCreated
         (cons "dateCreated"
               (list
                :datetime
                (let ((ctime (current-time)))
                  (cons (car ctime) (cadr ctime)))))
         ;; description
         (cons "description"
               (tblog-replace-media-object-location
                (buffer-substring-no-properties
                 (tblog-point-template-head-end)
                 (point-max)))))))

(defun tblog-b2p-other ()
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

(defun tblog--current-buffer-to-post ()
  (set (make-variable-buffer-local 'tblog--buffer-content)
        (buffer-substring-no-properties (point-min) (point-max)))
  (let ((func (cadr (assoc major-mode tblog--b2p-method-alist))))
    (if func
        (funcall func)
      (tblog-b2p-other))))

(defmacro tblog-mkfield (x)
  `(rx bol ,x (* space) (group (+? nonl) eol)))

(defun tblog--update-category (&optional cat)
  "Update category.
If CAT is specified, add it to database and return it unchanged.
Or, select/create one from DB and return a new one."
  (tblog--get-category-list)
  (unless cat
    (setq cat (tblog--choose-or-create "Category" tblog-categories)))
  (add-to-list 'tblog-categories cat)
  (tblog--db-write (tblog--get-category-db-path) tblog-categories)
  cat)

(defun tblog--update-tag (&optional tag)
  "Update tags.
If TAG is specified, add it to database and return it unchanged.
Or, select/create one from DB and return a new one."
  (tblog--get-tag-list)
  (unless tag
    (setq tag (tblog--choose-or-create "Tag" tblog-tags)))
  (setq tag (concat (mapcar (lambda (x)
                              (if (= ?, x) ? (if (and (<= x ?Z) (>= x ?A) t) (+ x 32) x)))
                            tag)))
  (add-to-list 'tblog-tags tag)
  (tblog--db-write (tblog--get-tag-db-path) tblog-tags)
  tag)

(defun tblog--choose-or-create (name candidates)
  "Choose one candidate for NAMAE from CANDIDATES for create a new one, with HELM."
  (interactive)
  (let ((h-source
         (helm-build-sync-source (format "Choose One %s" name)
           :init nil
           :candidates candidates
           :action (lambda (cand)
                      cand)))
        (fallback-source
         `((name . ,(format "Create New %s" name))
           (dummy)
           (action . (("Default" . (lambda (candidate)
                                     (message "%s" helm-pattern))))))))
    (helm :sources '(h-source fallback-source)
          :buffer "*helm*"
          :preselect nil)))

(defun tblog-b2p-org ()
  (save-excursion
    (goto-char (point-min))
    (when (search-forward-regexp (tblog-mkfield "#+CATEGORY:") nil t)
      (let ((cat (match-string-no-properties 1)))
        (when (and (string-match "(nil)" cat)
                   (yes-or-no-p "Category is not specified, add one?"))
          (setq cat (tblog--update-category))
          (goto-char (point-at-bol))
          (search-forward-regexp (tblog-mkfield "#+CATEGORY:") nil t)
          (replace-match (format "#+CATEGORY: %s" cat))
          (setq tblog--buffer-content
               (buffer-substring-no-properties (point-min) (point-max)))))))
  (cons (list
         ;; title
         (cons "title"  (tblog--fetch-field (tblog-mkfield  "#+TITLE:")))

         ;; "description"
         (cons "description" (tblog--fetch-field (tblog-mkfield "#+DESCRIPTION:")))

         ;; categories
         (cons "categories" (tblog--fetch-field (tblog-mkfield "#+CATEGORY:") 'tblog--update-category))

         ;; tags
         (cons "tags" (tblog--fetch-field (tblog-mkfield "#+KEYWORDS:") 'tblog--update-tag)))

        ;; Contents.
        (with-current-buffer (let ((org-html-head-extra nil))
                               (org-export-to-buffer 'html "*Org HTML Export*"
                                 nil nil t t))
          (let ((buf-str
                 (tblog-replace-media-object-location
                  (buffer-substring-no-properties
                   (point-min)
                   (point-max)))))
            (kill-buffer)
            buf-str))))

(defun tblog-get-post-name (bfn)
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

;;;###autoload
(defun tblog-prepare-post (&optional skip)
  "Convert current buffer into html file and prepare send to github.
SKIP: skip running magit."
  (interactive)
  (let* ((fn (buffer-file-name))
         (ofn (file-name-nondirectory fn))
         (bfn (file-name-sans-extension ofn))
         (hfn (tblog-get-post-name bfn))
         (dir (file-name-directory fn))
         (git (executable-find "git"))
         (content (tblog--current-buffer-to-post))
         (final-content ))
    (with-temp-file (format "%s/_posts/%s" tblog-repository hfn)
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
        (magit-status-internal dir))))


;; Operations related to categories and tags

(defvar tblog-categories nil "Categories.")
(defvar tblog--tags nil "Tags.")

(defun tblog--get-category-db-path ()
  "Return path of category database."
  (concat tblog-repository "/category.db"))

(defun tblog--get-tag-db-path ()
  "Return path of tag database."
  (concat tblog-repository "/tags.db"))

(defun tblog--get-category-list (&optional force)
  "Get a list of categories.
It only read files from database when FORCE is t or tblog-categories is nil."
  (when (or force (not tblog-categories))
    (setq tblog-categories (tblog--read-db-as-list (tblog--get-category-db-path))))
  tblog-categories)

(defun tblog--get-tag-list (&optional force)
  "Get a list of tags.
It only read files from database when FORCE is t or tblog--tags is nil."
  (when (or force (not tblog--tags))
    (setq tblog--tags (tblog--read-db-as-list (tblog--get-tag-db-path))))
  tblog--tags)

(defun tblog--read-db-as-list (path)
  "Read and return content of file (specified as PATH), and return as list."
  (when (file-exists-p path)
    (with-temp-buffer
      (insert-file-contents path)
      (goto-char (point-min))
      (read (current-buffer)))))

(defun tblog--db-write (path content)
  "Write CONTENT into file specified as PATH."
  (with-temp-file path
    (insert "(")
    (dolist (i content) (pp i (current-buffer)))
    (insert ")")))

(provide 'tblog)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; tblog.el ends here
