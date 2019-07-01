;;; -----------------------------------------------------
;;; argdoc - package file
;;; -----------------------------------------------------
;;; File:     /Users/edward/Documents/Code/common-lisp/projects/argdoc/src/package.lisp
;;; Date:     16:33:43 of Friday, 6/28/2019 (GMT+1)
;;; Author:   Edward Puccini
;;; -----------------------------------------------------
(in-package :cl-user)

(defpackage :argdoc
  (:documentation "An package documentation tool.")
  (:use #:common-lisp #:cl-ppcre)
  (:export
   #:all-function-symbols
   #:all-variable-symbols
   #:document
   #:inspect-package
   #:write-document-header
   #:write-group-header
   #:write-functions
   #:write-symbol
   #:write-dependencies
   #:write-group-footer
   #:write-document-footer
   #:doc-type
   #:doc-html
   #:doc-plaintext
   #:doc-stdout
   #:dependency-tree))

