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
  (let ((content ""))
    (asdf:load-system package)
    (with-open-file (stream (merge-pathnames path filename)
                            :direction :output
                            :if-does-not-exist :create
                            :if-exists :overwrite)
      (setf content (inspect-package package stream doc-type-object))
      content)))

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
  (write-functions package stream doc-type-object))

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

(defmethod write-function (function doc stream (doc-type-object doc-html))
  (format stream "<div class='function'><b>Function</b>: ~a</div>~%" function)
  (format stream "<div class='documentation'><b>Documentation</b>: ~a</div><br>~%" doc))

(defmethod write-function (function doc stream (doc-type-object doc-plaintext))
  (format stream "Function: ~a~%" function)
  (format stream "Documentation: ~a~%~%" doc))

(defmethod write-function (function doc stream (doc-type-object doc-richtext))
  (format stream "Function: ~a~%" function)
  (format stream "Documentation: ~a~%~%" doc))


  
