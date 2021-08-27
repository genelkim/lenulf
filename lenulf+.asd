;; Len's Treebank ULF parser---extended to support some Python-based parsers.

(asdf:defsystem :lenulf+
  :name "lenulf"
  :version "0.0.1"
  :author "Lenhart K. Schubert, packaged and extened by Gene Louis Kim"
  :depends-on (:py4cl :ptb2cf)
  :components ((:file "package")
               (:file "gene-util")
               (:file "parser/english-to-ulf")
               (:file "parser/parse")
               (:file "parse-more")
               (:file "parser/parse-tree-to-ulf")
               (:file "parser/preprocess-tree-for-ulf")
               (:file "parser/tt")
               (:file "parser/tt-match-predicates")
               (:file "parser/pos+word-to-ulf")
               (:file "parser/preprocessing-rules")
               (:file "parser/stem")
               (:file "parser/lexical-features")
               (:file "parser/verb-transitivity-lists")
               (:file "parser/isa")
               (:file "parser/postprocess-ulf-tree")
               (:file "parser/postprocessing-rules")
               (:file "parser/insert-gaps")
               (:file "parser/subcat-pref"))
  :around-compile (lambda (next)
                    ; For debugging/development.
                    ; NB: debug 3 caused a heap space error.
                    ;(proclaim '(optimize (debug 2) (safety 3) (space 1) (speed 1)))
                    ; For production.
                    (proclaim '(optimize (debug 0) (safety 1) (space 1) (speed 3)))
                    (funcall next)))

;; This is to store the path to the source code
;; suggested here https://xach.livejournal.com/294639.html
(defpackage #:lenulf+/config (:export #:*base-directory*))
(defparameter lenulf+/config:*base-directory* 
  (asdf:system-source-directory "lenulf+"))

