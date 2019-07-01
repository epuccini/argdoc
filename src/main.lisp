;;; -----------------------------------------------------
;;; argdoc - application main file
;;; -----------------------------------------------------
;;; File:     /Users/edward/Documents/Code/common-lisp/projects/argdoc/src/main.lisp
;;; Date:     16:33:43 of Friday, 6/28/2019 (GMT+1)
;;; Author:   Edward Puccini
;;; -----------------------------------------------------

(require :argdoc)

(defun main ()
  "Test application for argdoc package documentation tool."
  (argdoc:document :package :argdoc
                   :filename "argdoc.html"
                   :path "../doc/"
                   :doc-type 'argdoc:doc-html))
