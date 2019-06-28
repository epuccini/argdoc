;;; -----------------------------------------------------
;;; argdoc
;;; -----------------------------------------------------
;;; File:     /Users/edward/Documents/Code/common-lisp/projects/argdoc/src/argdoc.lisp
;;; Date:     16:33:43 of Friday, 6/28/2019 (GMT+1)
;;; Author:   Edward Puccini
;;; -----------------------------------------------------

(in-package :argdoc)

(defclass doc-type ()
  ((stream :accessor stream-of
           :initarg :stream
           :initform nil
           :documentation "Document file stream")))

(defclass doc-plaintext (doc-type)
  ())

(defclass doc-html (doc-type)
  ())

(defclass doc-richtext (doc-type)
  ())

(defun document (&key package filename path type)
  "Document creates package documentation at path.
*Arguments
- PACKAGE :: Package name
- FILENAME :: Document file name
- PATH :: Documentation path
- TYPE :: 'RICHTEXT 'HTML 'PLAINTEXT
*Returns
Document source."
  (let ((content "")
        (doc-type-object nil))
    (asdf:load-system package)
    (with-open-file (stream (merge-pathnames path filename)
                            :direction :output
                            :if-does-not-exist :create
                            :if-exists :overwrite)
      (if (equal type "HTML")
          (setf doc-type-object (make-instance 'doc-html :stream stream)))
      (if (equal type "PLAINTEXT")
          (setf doc-type-object (make-instance 'doc-plaintext :stream stream)))
      (if (equal type "RICHTEXT")
          (setf doc-type-object (make-instance 'doc-richtext :stream stream)))
      (setf content (inspect-package package stream doc-type-object))
      content)))

(defun all-function-symbols (package)
  "Retrieves all functions in a package."
  (when (find-package package)
    (let ((res (list))
          (docs (list)))
      (do-all-symbols (sym package)
        (when (and (fboundp sym)
                   (eql (symbol-package sym)
                        (find-package package)))
          (prog1
              (push sym res)
            (push (documentation sym 'function) docs))))
      (values res docs))))

(defun inspect-package (package stream doc-type-object)
  "Inspekt package and its content"
  (multiple-value-bind (syms docs) (all-function-symbols package)
                       (mapcar #'(lambda (s d)
                                   (write-function s d stream doc-type-object))
                               syms docs)
    syms))

(defmethod write-function (function doc stream (doc-type-object doc-html))
  (format stream "<div class='function'><b>Function</b>: ~a</div>~%" function)
  (format stream "<div class='documentation'><b>Documentation</b>: ~a</div>~%" doc))

(defmethod write-function (function doc stream (doc-type-object doc-plaintext))
  (format stream "Function: ~a~%" function)
  (format stream "Documentation: ~a~%~%" doc))

(defmethod write-function (function doc stream (doc-type-object doc-richtext))
  (format stream "Function: ~a~%" function)
  (format stream "Documentation: ~a~%~%" doc))


  
