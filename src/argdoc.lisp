;;; -----------------------------------------------------
;;; argdoc
;;; -----------------------------------------------------
;;; File:     /Users/edward/Documents/Code/common-lisp/projects/argdoc/src/argdoc.lisp
;;; Date:     16:33:43 of Friday, 6/28/2019 (GMT+1)
;;; Author:   Edward Puccini
;;; -----------------------------------------------------

(in-package :argdoc)

(defvar *test* nil)

(defclass doc-type ()
  ())

(defclass doc-plaintext (doc-type)
  ())

(defclass doc-html (doc-type)
  ())

(defclass doc-richtext (doc-type)
  ())

(defun document (&key package filename path doc-type-object)
  "Document creates package documentation at path.
*Arguments
- PACKAGE :: Package name
- FILENAME :: Document file name
- PATH :: Documentation path
- DOC-TYPE-OBJECT :: RICHTEXT, HTML or PLAINTEXT doc-type-objext
*Returns
Document source."
  (asdf:load-system package)
  (with-open-file (stream (merge-pathnames path filename)
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :overwrite)
    (inspect-package package stream doc-type-object)))

(defgeneric ->key (thing))

(defmethod ->key ((thing string))
  (intern (string-upcase thing) :keyword))

(defmethod ->key ((thing symbol))
  (if (keywordp thing)
      thing
      (intern (symbol-name thing) :keyword)))

(defgeneric dependencies-of (system))
(defmethod dependencies-of ((system symbol))
  (mapcar #'->key (slot-value (asdf/system:find-system system) 'asdf/component:sideway-dependencies)))

(defun ordered-dep-tree (dep-tree)
  (let ((res))
    (labels ((in-res? (dep-name) (member dep-name res))
             (insert-pass (remaining)
                (loop for (dep . sub-deps) in remaining
                      for unmet-sub-deps = (remove-if #'in-res? sub-deps)
                      if (null unmet-sub-deps) do (push dep res)
                      else collect (cons dep unmet-sub-deps) into next-rems
                      finally (return next-rems))))
      (loop for (dep . callers) in dep-tree for deps-of = (dependencies-of dep)
            if (null deps-of) do (push dep res)
            else collect (cons dep deps-of) into non-zeros
            finally (loop while non-zeros
                          do (setf non-zeros (insert-pass non-zeros)))))
      (reverse res)))

(defgeneric dependency-tree (system))
(defmethod dependency-tree ((system symbol))
  (let ((res (make-hash-table)))
    (labels ((rec (sys) 
               (loop with deps = (dependencies-of sys)
                  for dep in deps for dep-k = (->key dep)
                  unless (gethash dep-k res) do (rec dep)
                  do (pushnew (->key sys) (gethash dep-k res)))))
      (rec system))
    (ordered-dep-tree (alexandria:hash-table-alist res))))

(defun all-function-symbols (package)
  "Retrieves all functions in a package.
*Arguments
- PACKAGE :: Package name
*Returns
VALUES of Symbols and Documentation strings"
  (when (find-package package)
    (let ((syms (list))
          (docs (list)))
      (do-all-symbols (sym package)
        (when (and (fboundp sym)
                   (eql (symbol-package sym)
                        (find-package package)))
          (prog1
              (push sym syms)
            (push (documentation sym 'function) docs))))
      (values syms docs))))

(defun inspect-package (package stream doc-type-object)
  "Inspekt package and its content
*Arguments
- PACKAGE :: Package name
- STREAM :: File stream
- DOC-TYPE-OBJECT :: Document type object"
  (write-document-header package stream doc-type-object)
  (write-group-header stream "Dependencies" doc-type-object)
  (write-dependencies package stream doc-type-object)
  (write-group-footer stream "Dependencies" doc-type-object)
  (write-group-header stream "Functions" doc-type-object)
  (write-functions package stream doc-type-object)
  (write-group-footer stream "Functions" doc-type-object)
  (write-document-footer package stream doc-type-object)
  doc-type-object)

(defun write-functions (package stream doc-type-object)
  "Write all package functions to file.
*Arguments
- PACKAGE :: Package name
- STREAM :: File stream
- DOC-TYPE-OBJECT :: Document type object"
  (multiple-value-bind (syms docs) (all-function-symbols package)
                       (mapcar #'(lambda (s d)
                                   (write-function s d stream doc-type-object))
                               syms docs)
                       syms))

(defun write-dependencies (package stream doc-type-object)
  "Write all package dependencies to file.
*Arguments
- PACKAGE :: Package name
- STREAM :: File stream
- DOC-TYPE-OBJECT :: Document type object"
  (let ((deps (dependency-tree package)))
    (mapcar #'(lambda (d)
                (write-dependency d stream doc-type-object))
            deps)
    deps))


(defmethod write-document-header (package stream (doc-type-object doc-html))
  (format stream "<head><title>~a</title></head><body><h1>~a - Package documentation</h1><br>~%"
          package package))

(defmethod write-document-header (package stream (doc-type-object doc-plaintext))
  (format stream "~%~a - Package documentation~%~%" package))

(defmethod write-document-header (package stream (doc-type-object doc-richtext))
  (format stream "~%~a - Package documentation~%~%" package))

(defmethod write-group-header (stream type (doc-type-object doc-html))
  (format stream "<div class='header'><h1>~a</h1></div><br><div class='group'>~%" type))

(defmethod write-group-header (stream type (doc-type-object doc-plaintext))
  (format stream "~%~a~%~%" type))

(defmethod write-group-header (stream type (doc-type-object doc-richtext))
  (format stream "~%~a~%~%" type))

(defmethod write-function (function doc stream (doc-type-object doc-html))
  (let ((html-doc (cl-ppcre:regex-replace-all
                   (format nil "~a" #\newline) doc "<br>")))
    (format stream "<div class='function'><b>Function</b>: ~a</div>~%" function)
    (format stream "<div class='documentation'><b>Documentation</b>:<br>~a</div><br>~%" html-doc)))

(defmethod write-function (function doc stream (doc-type-object doc-plaintext))
  (format stream "Function: ~a~%" function)
  (format stream "Documentation:~%~a~%~%" doc))

(defmethod write-function (function doc stream (doc-type-object doc-richtext))
  (format stream "Function: ~a~%" function)
  (format stream "Documentation:~%~a~%~%" doc))

(defmethod write-dependency (dependency stream (doc-type-object doc-html))
  (format stream "<div class='dependency'><b>Dependency</b>: ~a</div>~%"
          dependency))

(defmethod write-dependency (dependency stream (doc-type-object doc-plaintext))
  (format stream "Dependency: ~a~%" dependency))

(defmethod write-dependency (dependency stream (doc-type-object doc-richtext))
  (format stream "Dependency: ~a~%" dependency))


(defmethod write-group-footer (stream type (doc-type-object doc-html))
  (format stream "</div><div class='footer'>~a End</div><br><br>~%" type))

(defmethod write-group-footer (stream type (doc-type-object doc-plaintext))
  (format stream "~%~a End~%~%" type))

(defmethod write-group-footer (stream type (doc-type-object doc-richtext))
  (format stream "~%~a End~%~%" type))

(defmethod write-document-footer (package stream (doc-type-object doc-html))
  (format stream "~a - Package End<br></body>~%" package))

(defmethod write-document-footer (package stream (doc-type-object doc-plaintext))
  (format stream "~a - Package End~%" package))

(defmethod write-document-footer (package stream (doc-type-object doc-richtext))
  (format stream "~a - Package End~%" package))

  
