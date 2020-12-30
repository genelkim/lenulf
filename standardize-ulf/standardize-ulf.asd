;; ULF standardizer for Len's Treebank to ULF parser.

(asdf:defsystem :standardize-ulf
  :name "standardize-ulf"
  :serial t
  :version "0.0.1"
  :author "Gene Louis Kim <gkim21@cs.rochester.edu>"
  :license "MIT"
  :depends-on (:ttt :cl-strings :cl-util :cl-ppcre :ulf-lib :ulf2english :py4cl)
  :components ((:file "package")
               (:file "standardize-ulf"))
  :around-compile (lambda (next)
                    (proclaim '(optimize (debug 3) (safety 3) (space 1) (speed 1)))
                    (funcall next)))

