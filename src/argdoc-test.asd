;;; -----------------------------------------------------
;;; argdoc - systems test asd-file
;;; -----------------------------------------------------
;;; File:     /Users/edward/Documents/Code/common-lisp/projects/argdoc/src/argdoc-test.asd
;;; Date:     16:33:43 of Friday, 6/28/2019 (GMT+1)
;;; Author:   Edward Puccini
;;; -----------------------------------------------------

(require 'asdf)

(defsystem "argdoc-test"
    :version "0.0.0"
    :maintainer "Edward Puccini"
    :licence "BSD"
    :description "Generated by pm 2017."
    :components ((:file "package")
                 (:file "argdoc")
                 (:file "test")))
