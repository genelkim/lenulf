(ql:quickload :standardize-ulf)
(ql:quickload :lenulf)
(in-package :standardize-ulf)

(defun parse-ulf (str)
  "Parse an English sentence into ULF.
  Uses the lenulf package for the core annotation, which is then post-processed."
  (let*
    ((rawparse
       ;; The lenulf package requires *package* to be :lenulf
       ;; TODO: find a solution that is lenulf internal.
       ;; TODO: remove intern-symbols-recursive and add a callpkg argument to lenulf's external functions.
       (let ((*package* (find-package :lenulf)))
	 (intern-symbols-recursive (lenulf:english-to-ulf str)
				   :standardize-ulf)))
     (token-removed (remove-token-indices rawparse))
     (fixed
       (standardize-ulf token-removed :pkg :ulf-nlog))
     (canon (canonicalize fixed)))
    canon))
