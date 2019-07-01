;;; -----------------------------------------------------
;;; argdoc
;;; -----------------------------------------------------
;;; File:     /Users/edward/Documents/Code/common-lisp/projects/argdoc/src/argdoc.lisp
;;; Date:     16:33:43 of Friday, 6/28/2019 (GMT+1)
;;; Author:   Edward Puccini
;;; -----------------------------------------------------

(in-package :argdoc)

(defclass doc-type ()
  ())

(defclass doc-plaintext (doc-type)
  ())

(defclass doc-html (doc-type)
  ())

(defclass doc-stdout (doc-type)
  ())

(defun document (&key package filename path doc-type)
  "Document creates package documentation at path.
*Arguments
- PACKAGE :: Package name
- FILENAME :: Document file name
- PATH :: Documentation path
- DOC-TYPE :: STDOUT, HTML or PLAINTEXT doc-type-objext
*Returns
Document source."
  (asdf:load-system package)
  (with-open-file (stream (merge-pathnames path filename)
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
    (inspect-package package stream
                     (make-instance doc-type))))

(defgeneric ->key (thing))

(defmethod ->key ((thing string))
  "Key method handles string.
*Arguments
- THING :: Key-object string
*Returns
- Key"
  (intern (string-upcase thing) :keyword))

(defmethod ->key ((thing symbol))
  "Key method handles symbols.
*Arguments
- THING :: Key-object symbol
*Returns
- Symbold"
  (if (keywordp thing)
      thing
      (intern (symbol-name thing) :keyword)))

(defgeneric dependencies-of (system))
(defmethod dependencies-of ((system symbol))
  "Get dependency of system.
*Arguments
- SYSTEM :: System symbol
*Returns
List of symbols."
  (mapcar #'->key
          (slot-value (asdf/system:find-system system)
                      'asdf/component:sideway-dependencies)))

(defun ordered-dep-tree (dep-tree)
  "Get dependency tree.
*Arguments
- DEP-TREE :: Part of dependency-tree to traverse.
*Returns
Ordered dependency tree"
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
  "Get dependency tree of a system package.
*Arguments
- SYSTEM :: System packge of type symbol.
*Returns
List of package dependencies"
  (let ((res (make-hash-table)))
    (labels ((rec (sys) 
               (loop with deps = (dependencies-of sys)
                  for dep in deps for dep-k = (->key dep)
                  unless (gethash dep-k res) do (rec dep)
                  do (pushnew (->key sys) (gethash dep-k res)))))
      (rec system))
    (ordered-dep-tree (alexandria:hash-table-alist res))))


(defun all-class-symbols (package)
  "Retrieves all classes in a package.
*Arguments
- PACKAGE :: Package name
*Returns
VALUES of Symbols and Documentation strings"
  (when (find-package package)
    (let ((syms (list))
          (docs (list)))
      (do-all-symbols (sym package)
        (handler-case
            (when (and (class-name (find-class sym))
                       (eql (symbol-package sym)
                            (find-package package)))
              (let ((class-doc (with-output-to-string (*standard-output*) (describe sym))))
                (push sym syms)
                (push class-doc docs)))
          (error (condition))))
      (values syms docs))))


(defun all-variable-and-function-symbols (package)
  "Retrieves all classes, variables and functions in a package.
*Arguments
- PACKAGE :: Package name
*Returns
VALUES of Symbols and Documentation strings"
  (when (find-package package)
    (let ((syms (list)))
      (do-all-symbols (sym package)
        (handler-case
            (when (or
                   (and (boundp sym)
                        (eql (symbol-package sym)
                             (find-package package)))
                   (and (fboundp sym)
                        (eql (symbol-package sym)
                             (find-package package)))
                   (and (class-name (find-class sym))
                        (eql (symbol-package sym)
                             (find-package package))))
              (push sym syms))
          (error (condition))))
      syms)))

(defun all-variable-symbols (package)
  "Retrieves all variables in a package.
*Arguments
- PACKAGE :: Package name
*Returns
VALUES of Symbols and Documentation strings"
  (when (find-package package)
    (let ((syms (list))
          (docs (list)))
      (do-all-symbols (sym package)
        (when (and (boundp sym)
                   (eql (symbol-package sym)
                        (find-package package)))
          (prog1
              (push sym syms)
            (push (documentation sym 'variable) docs))))
      (values syms docs))))

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
          (let ((fun-doc (documentation sym 'function))
                (desc-doc (with-output-to-string (*standard-output*) (describe sym))))
            (push sym syms)
            (if fun-doc
                (push fun-doc docs)
                (push desc-doc docs)))))
    (values syms docs))))

(defun inspect-package (package stream doc-type-object)
  "Inspekt package and its content
*Arguments
- PACKAGE :: Package name
- STREAM :: File stream
- DOC-TYPE-OBJECT :: Document type object"
  (write-document-header stream package doc-type-object)
  (write-group-header stream "Index" doc-type-object)
  (write-index stream package doc-type-object)
  (write-group-footer stream "Index" doc-type-object)
  (write-group-header stream "Dependencies" doc-type-object)
  (write-dependencies stream package doc-type-object)
  (write-group-footer stream "Dependencies" doc-type-object)
  (write-group-header stream "Variables" doc-type-object)
  (write-variables stream package doc-type-object)
  (write-group-footer stream "Variables" doc-type-object)
  (write-group-header stream "Functions" doc-type-object)
  (write-functions stream package doc-type-object)
  (write-group-footer stream "Functions" doc-type-object)
  (write-group-header stream "Classes" doc-type-object)
  (write-classes stream package doc-type-object)
  (write-group-footer stream "Classes" doc-type-object)
  (write-document-footer stream package doc-type-object)
  doc-type-object)

(defun write-classes (stream package doc-type-object)
  "Write all package classes to file.
*Arguments
- PACKAGE :: Package name.
- STREAM :: File stream.
- DOC-TYPE-OBJECT :: Document type object."
  (multiple-value-bind (syms docs) (all-class-symbols package)
                       (mapcar #'(lambda (s d)
                                   (write-symbol stream s "Class" d doc-type-object))
                               syms docs)
                       syms))
(defun write-index (stream package doc-type-object)
  "Write all package functions and variables to file.
*Arguments
- PACKAGE :: Package name.
- STREAM :: File stream.
- DOC-TYPE-OBJECT :: Document type object."
  (let ((syms (sort (all-variable-and-function-symbols package) #'string<)))
    (mapcar #'(lambda (s)
                (write-indices stream s doc-type-object))
            syms)
    syms))

(defun write-functions (stream package doc-type-object)
  "Write all package functions to file.
*Arguments
- PACKAGE :: Package name.
- STREAM :: File stream.
- DOC-TYPE-OBJECT :: Document type object."
  (multiple-value-bind (syms docs) (all-function-symbols package)
                       (mapcar #'(lambda (s d)
                                   (write-symbol stream s "Function" d doc-type-object))
                               syms docs)
                       syms))

(defun write-variables (stream package doc-type-object)
  "Write all package variables to file.
*Arguments
- PACKAGE :: Package name.
- STREAM :: File stream.
- DOC-TYPE-OBJECT :: Document type object."
  (multiple-value-bind (syms docs) (all-variable-symbols package)
                       (mapcar #'(lambda (s d)
                                   (write-symbol stream s "Variable" d doc-type-object))
                               syms docs)
                       syms))

(defun write-dependencies (stream package doc-type-object)
  "Write all package imports to file.
*Arguments
- PACKAGE :: Package name.
- STREAM :: File stream.
- DOC-TYPE-OBJECT :: Document type object."
  (let ((deps (dependency-tree package)))
    (mapcar #'(lambda (d)
                (write-dependency stream d doc-type-object))
            deps)
    deps))

(defgeneric write-document-header (stream package doc-type-object))
(defmethod write-document-header (stream package (doc-type-object doc-html))
  "Write document header as html.
*Arguments
- PACKAGE :: Package to document.
- STREAM :: File output stream.
- DOC-TYPE-OBJECT :: Type class instance."
  (format stream "<head><title>~a</title></head><body><div class='large-header'><b>~a - Package documentation</b></div><br><link rel='stylesheet' type 'text/css' href='../doc/styles.css' />~%"
          package package))

(defmethod write-document-header (stream package (doc-type-object doc-plaintext))
  "Write document header as plaintext.
*Arguments
- PACKAGE :: Package to document.
- STREAM :: File output stream.
- DOC-TYPE-OBJECT :: Type class instance."
  (format stream "~%~a - Package documentation~%~%"
          package))

(defmethod write-document-header (stream package (doc-type-object doc-stdout))
  "Write document header to stdout.
*Arguments
- PACKAGE :: Package to document.
- STREAM :: File output stream.
- DOC-TYPE-OBJECT :: Type class instance."
  (format t "~%~a - Package documentation~%~%" package))

(defgeneric write-group-header (stream type doc-type-object))
(defmethod write-group-header (stream type (doc-type-object doc-html))
    "Write group header as html.
*Arguments
- GROUP :: Group to document.
- STREAM :: File output stream.
- DOC-TYPE-OBJECT :: Type class instance."
  (format stream "<div class='group'><div class='header'><b>~a</b></div><br>~%" type))

(defmethod write-group-header (stream type (doc-type-object doc-plaintext))
    "Write group header as plaintext.
*Arguments
- GROUP :: Group to document.
- STREAM :: File output stream.
- DOC-TYPE-OBJECT :: Type class instance."
  (format stream "~%~a~%~%" type))

(defmethod write-group-header (stream type (doc-type-object doc-stdout))
    "Write group header to stdout.
*Arguments
- GROUP :: Group to document.
- STREAM :: File output stream.
- DOC-TYPE-OBJECT :: Type class instance."
  (format t "~%~a~%~%" type))

(defgeneric write-symbol (stream sym type doc doc-type-object))
(defmethod write-symbol (stream sym type doc (doc-type-object doc-html))
  "Write symbol with documentation as html.
*Arguments
- STREAM :: File output stream.
- SYM :: Symbol to document.
- DOC :; Documentation
- DOC-TYPE-OBJECT :: Type class instance."
  (let ((html-doc (regex-replace-all
                   (format nil "~a" #\newline) doc "<br>")))
    (format stream "<div class='symbol'><b>~a</b>: <a id='~a'>~a</a></div>~%" type sym sym)
    (format stream "<div class='documentation'><b>Documentation</b>:<br>~a</div><br>~%" html-doc)))

(defmethod write-symbol (stream sym type doc (doc-type-object doc-plaintext))
  "Write symbol with documentation as plaintext.
*Arguments
- STREAM :: File output stream.
- SYM :: Symbol to document.
- TYPE :: Type of symbol to document.
- DOC :; Documentation
- DOC-TYPE-OBJECT :: Type class instance."
  (format stream "~a: ~a~%" type sym)
  (format stream "Documentation:~%~a~%~%" doc))

(defmethod write-symbol (stream sym type doc (doc-type-object doc-stdout))
    "Write symbol with documentation to stdout.
*Arguments
- STREAM :: File output stream.
- SYM :: Symbol to document.
- TYPE :: Type of symbol to document.
- DOC :; Documentation
- DOC-TYPE-OBJECT :: Type class instance."
  (format t "~a: ~a~%" type sym)
  (format t "Documentation:~%~a~%~%" doc))


(defgeneric write-indices (stream sym doc-type-object))
(defmethod write-indices (stream sym (doc-type-object doc-html))
  "Write symbol with documentation as html.
*Arguments
- STREAM :: File output stream.
- SYM :: Symbol to document.
- DOC-TYPE-OBJECT :: Type class instance."
    (format stream "<div class='index'><a href='#~a'>~a</a></div>~%" sym sym))

(defmethod write-indices (stream sym (doc-type-object doc-plaintext))
  "Write symbol with documentation as plaintext.
*Arguments
- STREAM :: File output stream.
- SYM :: Symbol to document.
- DOC-TYPE-OBJECT :: Type class instance."
  (format stream "~a~%" sym))

(defmethod write-indices (stream sym (doc-type-object doc-stdout))
    "Write symbol with documentation to stdout.
*Arguments
- STREAM :: File output stream.
- SYM :: Symbol to document.
- DOC-TYPE-OBJECT :: Type class instance."
  (format t "~a~%" sym))

(defgeneric write-dependency (stream dependency doc-type-object))
(defmethod write-dependency (stream dependency (doc-type-object doc-html))
  "Write dependency with documentation as html.
*Arguments
- STREAM :: File output stream.
- DEPENDENCY :: dependency to document.
- DOC-TYPE-OBJECT :: Type class instance."
    (format stream "<div class='dependency'><b>Dependency</b>: ~a</div>~%"
          dependency))

(defmethod write-dependency (stream dependency (doc-type-object doc-plaintext))
  "Write dependency with documentation as plaintext.
*Arguments
- STREAM :: File output stream.
- DEPENDENCY :: dependency to document.
- DOC-TYPE-OBJECT :: Type class instance."
  (format stream "Dependency: ~a~%" dependency))

(defmethod write-dependency (stream dependency (doc-type-object doc-stdout))
  "Write dependency with documentation to stdout.
*Arguments
- STREAM :: File output stream.
- DEPENDENCY :: dependency to document.
- DOC-TYPE-OBJECT :: Type class instance."
  (format t "Dependency: ~a~%" dependency))

(defgeneric write-group-footer (stream type doc-type-object))
(defmethod write-group-footer (stream type (doc-type-object doc-html))
    "Write group footer documentation as html.
*Arguments
- STREAM :: File output stream.
- TYPE :: dependency to document.
- DOC-TYPE-OBJECT :: Type class instance."
  (format stream "<br><div class='footer'><b>~a End</b></div></div><br><br>~%" type))

(defmethod write-group-footer (stream type (doc-type-object doc-plaintext))
    "Write group footer documentation as plaintext.
*Arguments
- STREAM :: File output stream.
- TYPE :: dependency to document.
- DOC-TYPE-OBJECT :: Type class instance."
    (format stream "~%~a End~%~%" type))

(defmethod write-group-footer (stream type (doc-type-object doc-stdout))
    "Write group footer documentation to stdout.
*Arguments
- STREAM :: File output stream.
- TYPE :: dependency to document.
- DOC-TYPE-OBJECT :: Type class instance."
    (format t "~%~a End~%~%" type))

(defgeneric write-document-footer (stream package doc-type-object))
(defmethod write-document-footer (stream package (doc-type-object doc-html))
    "Write document footer documentation as html.
*Arguments
- STREAM :: File output stream.
- PACKGAE :: document package.
- DOC-TYPE-OBJECT :: Type class instance."
    (format stream "<br><b><div class='footer'>~a - Package End</b></div><br></body>~%" package))

(defmethod write-document-footer (stream package (doc-type-object doc-plaintext))
    "Write document footer documentation as plaintext.
*Arguments
- STREAM :: File output stream.
- PACKGAE :: document package.
- DOC-TYPE-OBJECT :: Type class instance."
    (format stream "~a - Package End~%" package))

(defmethod write-document-footer (stream package (doc-type-object doc-stdout))
    "Write document footer documentation to stdout.
*Arguments
- STREAM :: File output stream.
- PACKGAE :: document package.
- DOC-TYPE-OBJECT :: Type class instance."
    (format t "~a - Package End~%" package))

  
