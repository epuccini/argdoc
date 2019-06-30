;;; -----------------------------------------------------
;;; argdoc - package file
;;; -----------------------------------------------------
;;; File:     /Users/edward/Documents/Code/common-lisp/projects/argdoc/src/package.lisp
;;; Date:     16:33:43 of Friday, 6/28/2019 (GMT+1)
;;; Author:   Edward Puccini
;;; -----------------------------------------------------

(defpackage :argdoc
    (:use "COMMON-LISP")
    (:export
     #:all-function-symbols
     #:document
     #:inspect-package
     #:write-document-header
     #:write-group-header
     #:write-functions
     #:write-function
     #:write-group-footer
     #:write-document-footer
     #:doc-type
     #:doc-html
     #:doc-plaintext
     #:doc-richtext))

