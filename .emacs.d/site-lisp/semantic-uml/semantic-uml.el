;;; semantic-uml.el -- Brief introduction here.

;; Author: YangYingchao <yangyingchao@gmail.com>
;;
;; Copyright (C) 2015 Yang,Ying-chao
;;
;; Author: Yang,Ying-chao <yangyingchao@gmail.com>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; Usage:
;; Open a file that can be analyzed by CEDET and select a region, calls uml/struct-to-UML
;; and it will generate a node which can be drawn by graphiviz.
;; (add-hook 'c-mode-common-hook
;; (lambda ()
;;              (local-set-key "\C-csD" 'uml/struct-to-UML)
;;              (local-set-key "\C-csd" 'uml/struct-to-UML-full)))

;;; Code:


 ;; Function used to add fields of struct into a dot file (for Graphviz).

(autoload 'semantic-find-tag-by-overlay "semantic-find")

(require 'cl)
(require 'eieio)

;;;;; Dot templates
(defconst uml/dot-node-head
  "node_%s[shape=record
            label=\"{<f0> %s |\\"
  "Format of node head.")
(defconst uml/dot-node-tail "
}\"];
"
  "Format of node tail.")

(defconst uml/dot-attr "\n%s\\l\\" "Foramt of attr")


;;;;; Regular expressions to match a field of a struct.

(defclass uml/object-node ()
  ((name :initarg :name
         :initform nil
         :type (or null string))
   (funcs :initarg :funcs
          :type (or null list)
          :initform nil)
   (attrs :initarg :attrs
          :type (or null list)
          :initform nil)
   (subnodes :initarg :subnodes
             :initform nil
             :type (or null list))
   (parents :initarg :parents
            :initform nil
            :type (or null list)))
  :document "A Semantic object which can be stringified."
  )

(defclass uml/object-attr ()
  ((name :initarg :name
         :initform nil
         :type (or null string)
         )
   (type :initarg :type
         :type string
         :initform "")

   (visibility :initarg :visibility
               :initform 1 ;;visibility: 0 -- public, 1 -- private, 2 -- protected
               :type number
               )
   (params :initarg :params ;; For functions only...
           :initform nil
           :type (or null list))
   )
  :document "A Semantic object for member fields and member functions."
  )

;;visibility: 0 -- public, 1 -- private, 2 -- protected

(defun uml/stringify-semantic-ele (ele  &optional type-only)
  "Stringify semantic type"
  (let* ((result (nreverse (uml/ele-to-obj-attr ele type-only)))
         (varname (subseq result 0 2))
         (vartype (subseq result 2))
         (result  (append varname (nreverse vartype))))
    (mapconcat 'identity  result " ")))


(defun Tag-To-ObjNode (tag)
  "Parse semantic TAG and convert it into a uml/object-node which can be stringified later."
  (let* ((type (semantic-tag-get-attribute tag :type))
         (attrs (semantic-tag-get-attribute tag :members))
         (node (make-instance 'uml/object-node))
         (visibility 1) ;;visibility: 0 -- public, 1 -- private, 2 -- protected
         func-list attr-list subnodes
         ele)

    (defun uml/parse-func-args (pl)
      (let (res)
        (dolist (item pl)
          (add-to-list 'res (semantic-tag-get-attribute tag :type) t )
          res)))

    (defun uml/ele-to-obj-attr (ele)
      "Parse an element and and generate a object-attr."
      (let ((name (semantic-tag-name ele))
            (type (semantic-tag-type ele))
            (modifier "")
            node params)

        (yc/debug "ELE:" ele "Type" type "is eieio-object" (semantic-tag-p type))
        (unless (or (semantic-tag-get-attribute ele :constructor-flag)
                    (semantic-tag-get-attribute ele :destructor-flag))
          (setq node (make-instance 'uml/object-attr))
          (if (semantic-tag-p type)
              (setq type (semantic-tag-name type)))
          (case (semantic-tag-class ele)
            ('function ;; member functions
             (setq params (semantic-tag-get-attribute ele :arguments)))
            ('variable
             t
             ;; (print "variable")
             ))

          (cond
           ((semantic-tag-get-attribute ele :template-specifier)
            t
            ;; (print (uml/stringify-semantic-ele
            ;;         (car (semantic-tag-get-attribute ele
            ;;                                          :template-specifier))))
            )
           ((semantic-tag-get-attribute ele :functionpointer-flag)
            t
            ;; (print (cons ele "function pointer...."))
            )
           ((semantic-tag-get-attribute ele :pointer)
            (setq modifier "*"))
           ((semantic-tag-get-attribute ele :dereference)
            (setq modifier "[]")))

          (when (stringp type)
            (setq type (concat type modifier)))

          (oset node :name name)
          (oset node :type type)
          (oset node :visibility visibility)
          (oset node :params params))
        node))

  ;; start of real parsing...
  (when (string= type "typedef")
    (let ((tmp-tag (semantic-tag-get-attribute tag :typedef)))
      (setq type (semantic-tag-get-attribute tmp-tag :type)
            attrs (semantic-tag-get-attribute tmp-tag :members))))
  ;;todo: Add more case handling if necessary.

  (oset node :name (semantic-tag-name tag))

  (while (setq ele (pop attrs)) ;; Walk through all attributes of this Tag.
    (case (semantic-tag-class ele)
      ('function ;; member functions
       (aif (uml/ele-to-obj-attr ele)
           (add-to-list 'func-list it t)))
      ('variable ;; memer fields
       (aif (uml/ele-to-obj-attr ele)
           (add-to-list 'attr-list it t)))
      ('label    ;; Lables (public/protected/private)
       (aif (semantic-tag-name ele)
           (cond
            ((string= it "public")
             (setq visibility 0))
            ((string= it "protected")
             (setq visibility 2))
            ((string= it "private")
             (setq visibility 1))
            (t
             (message (format "Skip lable (%s), should be added later." it))))))
      ('type  ;; Other embedded types, will be processed later.
       (add-to-list 'subnodes (Tag-To-ObjNode (semantic-tag-copy ele))))
      (t (message (format "Skip type (%s), should be added later."
                          (semantic-tag-class ele))))))

  (oset node :funcs func-list)
  (oset node :attrs attr-list)
  (oset node :subnodes subnodes)
  (oset node :parents (cons (semantic-tag-type-superclasses tag)
                            (semantic-tag-type-interfaces tag)))
  node))

(defun uml/dot-allign-concat (e &optional l)
  "Align and concat strings."
  (let* ((result (car e);; (uml/strip-ws-in-string (car e))
                 )
         (cl (length result))
         (padding ""))
    (when (and l (> l cl))
      (dotimes (k (- l cl))
        (setq padding (concat padding "&#32;"))))
    (concat result padding " : " (cdr e))))

(defun uml/connect-parent-node (parents child)
  "Connect this node with its parents"
  (let ((fmt "node_%s -> node_%s [%s];\n")
        (inh "arrowtail=empty style=solid dir=back")
        (imp "arrowtail=empty style=dashed dir=back")
        result)
    (dolist (parent (car parents))
      (setq result (concat result (format fmt parent child inh))))
    (dolist (parent (cdr parents))
      (setq result (concat result (format fmt parent child imp))))
    result))

(defun uml/replace-ws-in-string (src dst)
  "Replace whitespaces in SRC into DST."
  (let ((str-array (split-string src split-string-default-separators)))
    (mapconcat 'identity str-array dst)))

(defun uml/dot-fmt-attr (attr &optional is-func &optional align)
  "Format an `ATTR' into dot language.
If `IS-FUNC' is nil, it is treated as a function, parameters will be formatted.
If `ALIGN' is specified, make sure `:' is aligned."
  (yc/debug-log "attr" attr)
  (yc/debug-log "attr: " attr ", name: " (oref attr :name)
                "Visibility" (oref attr :visibility))
  (let* ((name (oref attr :name))
         (type (oref attr :type))
         (params (oref attr :params))
         (visibility (oref attr :visibility))
         (prefix (case visibility
                   (0 "+")
                   (1 "-")
                   (2 "#"))))
    (uml/dot-allign-concat (cons (concat prefix name
                                         (if is-func
                                             (concat "(" ")")) ;; TODO: Parameter type and names
                                         ) type) align)))

(defun uml/node-to-dot (node)
  "Convert NODE into dot file."
  (yc/debug-log "Node:" node)
  (when (eieio-object-p node)
    (let* ((name  (or (oref node :name) "Unamed Object"))
           (funcs (oref node :funcs))
           (attrs (oref node :attrs))
           (subnodes (oref node :subnodes))
           (parents (oref node :parents))
           (node-str (format uml/dot-node-head
                             (uml/replace-ws-in-string name "_") name))
           (mx 0)
           str-list)

      (yc/debug "Starting formatting member functions.")
      ;; start of single node
      (mapc
       (lambda (x)
         (setq mx (max mx (length (oref x :name))))) ;; name length.
       funcs)
      (dolist (func funcs)
        (yc/concat node-str
                   (format uml/dot-attr (uml/dot-fmt-attr func t mx))))

      (setq mx 0)
      (mapc
       (lambda (x)
         (setq mx (max mx (1+ (length (oref x :name)))))) ;; name length.
       attrs)
      (dolist (attr attrs)
        (yc/concat node-str
                   (format uml/dot-attr (uml/dot-fmt-attr attr nil mx))))
      (yc/concat node-str uml/dot-node-tail)
      (setq str-list (cons node-str str-list))

      ;; start of subnodes.

      (dolist (subnode subnodes)
        (if subnode
            (yc/append str-list (uml/node-to-dot subnode))))

      ;; start of node-links..
      (setq str-list (cons (uml/connect-parent-node parents name) str-list))
      (mapconcat 'identity str-list "\n"))))

;;;###autoload
(defun uml/struct-to-UML (start end)
  "Generated a UML-like dot graph for tags between START and END."
  (interactive "rp")
  (save-excursion
    (let ((tags (semantic-find-tag-by-overlay start))
          (strs nil))
      (if (not tags)
          (error "No tags found!")
        (dolist (tag tags)
          (add-to-list 'strs (uml/node-to-dot (Tag-To-ObjNode tag))))
        (if strs
            (kill-new (mapconcat 'identity strs "\n"))
          (error "Failed to format tags!")))))
  (deactivate-mark)
  (message "Finished, node copied to killing-ring."))

;;;###autoload
(defun uml/struct-to-UML-full (start end)
  "Generated a UML-like dot graph for tags between START and END."
  (interactive "rp")
  (save-excursion
    (let ((tags (semantic-find-tag-by-overlay start))
          (strs nil))
      (if (not tags)
          (error "No tags found!")
        (dolist (tag tags)
          (add-to-list 'strs (uml/node-to-dot (Tag-To-ObjNode tag))))
        (if strs
            (kill-new (mapconcat 'identity strs "\n"))
          (error "Failed to format tags!")))))
  (deactivate-mark)
  (message "Finished, node copied to killing-ring."))

 ;; DIA support

(defvar uml/dia-id nil "Last ID")

(defun uml/dia-fmt-attr (name  &optional type &optional val)
  "Format an attribute of TYPE, with NAME & VAL."
  (if val
      (let ((res))
        (add-to-list 'res (format "<dia:attribute name=\"%s\">" name) t)
        (add-to-list 'res (case type
                            (string (format "<dia:%s>#%s#</dia:%s>"
                                            (symbol-name type) val (symbol-name type)))
                            (t (format "<dia:%s val=\"%s\" />" (symbol-name type) val))) t)
        (add-to-list 'res  "</dia:attribute>" t)
        (mapconcat 'identity res "\n"))
    (format "<dia:attribute name=\"%s\"/>" name)))

(defun uml/dia-fmt-funcs (funcs)
  "Format functions (FUNCS)."
  (let (result )
    (yc/append-item result "<dia:attribute name=\"operations\">") ;; head

    (dolist (func funcs)
      (yc/append-item result "<dia:composite type=\"umloperation\">")

      (yc/append-item result (uml/dia-fmt-attr "name" 'string (oref func :name)))
      (yc/append-item result (uml/dia-fmt-attr "type" 'string (oref func :type)))
      (yc/append-item result (uml/dia-fmt-attr "visibility" 'enum (oref func :visibility)))
      (yc/append-item result (uml/dia-fmt-attr "value" 'string ""))
      (yc/append-item result (uml/dia-fmt-attr "comment" 'string ""))
      (yc/append-item result (uml/dia-fmt-attr "abstract" 'boolean "false"))
      (yc/append-item result (uml/dia-fmt-attr "class_scope" 'boolean "false"))
      ;; (yc/append-item result (uml/dia-fmt-attr "parameters" 'boolean (oref func :params)))
      (yc/append-item result "</dia:composite>"))

    (yc/append-item result "</dia:attribute>\n") ;; tail

    (mapconcat 'identity result "\n")))

(defun uml/dia-fmt-attrs (attrs)
  "Format member fields (ATTRS)."
  (let (result)
    (yc/append-item result "<dia:attribute name=\"attributes\">");; head

    (dolist (attr attrs)
      (yc/append-item result "<dia:composite type=\"umlattribute\">")
      (yc/append-item result (uml/dia-fmt-attr "name" 'string (oref attr :name)))
      (yc/append-item result (uml/dia-fmt-attr "type" 'string (oref attr :type)))
      (yc/append-item result (uml/dia-fmt-attr "visibility" 'enum (oref attr :visibility)))
      (yc/append-item result (uml/dia-fmt-attr "value" 'string ""))
      (yc/append-item result (uml/dia-fmt-attr "comment" 'string ""))
      (yc/append-item result (uml/dia-fmt-attr "abstract" 'boolean "false"))
      (yc/append-item result (uml/dia-fmt-attr "class_scope" 'boolean "false"))
      (yc/append-item result "</dia:composite>"))

    (yc/append-item result "</dia:attribute>\n") ;; tail

    (yc/debug-log result)
    (mapconcat 'identity result "\n")))

(defun uml/node-to-dia (node)
  "Convert NODE to xml string which can be displayed with DIA."
  (setq uml/dia-id (if uml/dia-id (1+ uml/dia-id)
                     (+ 1000 (% (random 1000000) 111111))))
  (let ((class-head "<dia:object type=\"UML - Class\" version=\"0\" id=\"%d\">")
        (name  (or (oref node :name) "Unamed Object"))
        (funcs (oref node :funcs))
        (attrs (oref node :attrs))
        (subnodes (oref node :subnodes))
        result)
    (yc/append-item result (format class-head uml/dia-id)) ;; head

    (yc/append-item result (uml/dia-fmt-attr "name" 'string name))
    (yc/append-item result (uml/dia-fmt-attr "visible_operations" 'boolean "true"))
    (yc/append-item result (uml/dia-fmt-attr "visible_attributes" 'boolean "true"))
    (yc/append-item result (uml/dia-fmt-attr "template" 'boolean "false"))
    (yc/append-item result (uml/dia-fmt-attr "templates"))

    (when funcs
      (yc/append-item result (uml/dia-fmt-funcs funcs))
      )

    (when attrs
      (yc/append-item result (uml/dia-fmt-attrs attrs))
      )

    ;; TODO: subnodes...

    ;; tail
    (yc/append-item result "</dia:object>")
    (mapconcat 'identity result "\n")))

;;;###autoload
(defun uml/struct-to-dia (start end)
  "Generated a UML-like dot graph for tags between START and END."
  (interactive "rp")
  (save-excursion
    (let ((tags (semantic-find-tag-by-overlay start))
          (strs nil))
      (if (not tags)
          (error "No tags found!")
        (dolist (tag tags)
          (add-to-list 'strs (uml/node-to-dia (Tag-To-ObjNode tag))))
        (if strs
            (kill-new (mapconcat 'identity strs "\n"))
          (error "Failed to format tags!")))))
  (deactivate-mark)
  (message "Finished, node copied to killing-ring."))

(provide 'semantic-uml)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
;;; semantic-uml.el ends here
