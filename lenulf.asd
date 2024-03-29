;; Len's Treebank to ULF parser
;; Packaged on 2020-10-01

(asdf:defsystem :lenulf
  :name "lenulf"
  :version "0.3.1"
  :author "Lenhart K. Schubert, packaged by Gene Louis Kim"
  :components ((:file "package")
               (:file "gene-util")
               (:file "parser/english-to-ulf")
               (:file "parser/parse")
               (:file "parser/parse-tree-to-ulf")
               (:file "parser/preprocess-tree-for-ulf")
               (:file "parser/tt")
               (:file "parser/tt-match-predicates")
               (:file "parser/pos+word-to-ulf")
               (:file "parser/preprocessing-rules")
               (:file "parser/stem")
               (:file "parser/lexical-semantic-features"); very partial, not used yet
               (:file "parser/transitivity-lists")
               (:file "parser/isa")
               (:file "parser/postprocess-ulf-tree")
               (:file "parser/postprocessing-rules")
               (:file "parser/insert-gaps")
               (:file "parser/subcat-pref")
               (:file "parser/extract-data-from-alvey"))
  :around-compile (lambda (next)
                    ; For debugging/development.
                    ; NB: debug 3 caused a heap space error.
                    ;(proclaim '(optimize (debug 2) (safety 3) (space 1) (speed 1)))
                    ; For production.
                    (proclaim '(optimize (debug 0) (safety 1) (space 1) (speed 3)))
                    (funcall next)))

