;; Len's Preliminary Treebank to ULF parser
;; Packaged on 2020-10-01

(in-package :cl-user)

(defpackage :lenulf
  (:use :cl :cl-user)
  (:export :english-to-ulf
           :parse-tree-to-ulf))

;; This is to store the path to the source code
;; suggested here https://xach.livejournal.com/294639.html
(defpackage #:lenulf/config (:export #:*base-directory* #:*raw-parser-directory*))
(defparameter lenulf/config:*base-directory*
  (asdf:system-source-directory "lenulf"))
(defparameter lenulf/config:*raw-parser-directory*
  (merge-pathnames "parser/" lenulf/config:*base-directory*))

(in-package :lenulf)
(defparameter *show-stages* t)

