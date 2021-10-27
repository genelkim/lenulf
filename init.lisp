;; For parsing English into ULF;
;; If repair rules pre-existing from "elf-from-sentences" are added, 
;; ttt and ttt rule files will have to be added from there as well.

(in-package :cl-user)

(load "package")
(load "gene-util")
(load "parser/english-to-ulf")
(load "parser/parse")
(load "parser/parse-tree-to-ulf")
(load "parser/preprocess-tree-for-ulf")
(load "parser/tt")
(load "parser/tt-match-predicates.lisp")
(load "parser/pos+word-to-ulf")
(load "parser/preprocessing-rules")
(load "parser/stem")
(load "parser/lexical-semantic-features.lisp"); very partial, not used yet
(load "parser/transitivity-lists.lisp")
(load "parser/isa.lisp")
(load "parser/postprocess-ulf-tree.lisp")
(load "parser/postprocessing-rules.lisp")
(load "parser/insert-gaps.lisp")
(load "parser/subcat-pref.lisp")
(load "parser/extract-data-from-alvey.lisp")

(in-package :lenulf)

(defun trace-main () 
  (trace simple-tree pos+word-to-raw-ulf aux-inflection 
         preprocess-tree-for-ulf parse-tree-to-raw-ulf))

(defun trace-rules ()
  (trace apply-rule apply-rule-bottom-up apply-rule-top-down))

(format t "~%NB: (trace-main) can be used for high-level tracing")
(format t "~%    (trace-rules) can be used for tracing preprocessing")

; (load "test...pos+word-to-ulf.lisp")

(setq *show-stages* t)

(let ()
 (format t "~%Example of using 'parse-tree-to-ulf' (from Brown, file p16):~% ~
             (parse-tree-to-ulf~%    ~
                 '((S (NP (PRP I))~%         ~
                      (AUX (VBP \\'m))~%         ~
                      (VP (VBN scared)~%             ~
                          (PP (IN of) (NP (DT the) (NN nightmare))))) (\\. \\.)))")
 (format t "~%~%Suggested handy definition:~%
         (defun p (tree) (format t ~s (parse-tree-to-ulf tree)))" "~%~s")
 (format t "~%~%Suggested handy definition for English to ULF:~%
         (defun ulf (str) (format t ~s (english-to-ulf str)))" "~%~s")
 (format t "~%~%To avoid showing processing stages, do~%  ~
           (setq *show-stages* nil)")
 '     -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-)
