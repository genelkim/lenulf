;                                                               Sep 7/21
; Some code to extract words with given features from a list of Alvey entries

(in-package :lenulf)

(defun select-words (entries feats ~feats)
;`````````````````````````````````````````
; Select the words that have the features in feats but not those in ~feats
 (let (words feat-vals)
      (dolist (entry entries)
         (setq feat-vals (extract-feat-vals entry))
         (if (and (subsetp feats feat-vals)
                  (null (intersection ~feats feat-vals)))
             (push (car entry) words)))
      (remove-duplicates (reverse words))
 )); end of select-words

(defun extract-feat-vals (entry)
;```````````````````````````````
; Extract the values paired with feature names in the given Alvey entry.
; e.g., (induce "" (V (ARITY 3) (SUBCAT OC_INF) (SUBTYPE EQUI)) INDUCE ())
;       ==> (3 OC_INF EQUI)
 (let (feats)
      (dolist (x (cdaddr entry))
         (if (and (listp x) (car x) (cadr x)) (push (cadr x) feats)))
      (reverse feats)
 )); end of extract-feat-vals


(defun associate-feats-with-np-pp-taking-verbs ()
;```````````````````````````````````````````````
; Form of Alvey *v.le* entries (note features in 3rd list element): 
;    (<verb> "" (V (f1 v1) ... (fk vk)) <log-atom> nil) 
; Run through *v.le*, checking for each entry with feature NP_PP whether
; it's included in my filtered hash table *strong-np-pp-taking-verbs*,
; and if so, if it has a corresponding (PFORM <prep>) feature where <prep>
; is not one of {of, to, by}, and if so, adding this to output list
;    result: ((v1 f1) (v2 f2) (v3 f3) ...)
; where a verb vi may appear multiple time with various features. The
; reason for omitting {of, to, by} is just that these are reliably argument
; suppliers, and the reason for constructing the present list is to *rule
; out* argument-supplying PPs after and NP, so, {of, to, by} can be ruled
; out a priori.
;
 (let (result p)
   (if (not (boundp '*v.le*))
     (load (merge-pathnames lenulf/config:*raw-parser-directory*
                            "alvey-verbs.lisp")))
   (dolist (v *v.le*)
      (if (and (gethash (car v) *strong-np-pp-taking-verbs*)
               (find-if 
                  #'(lambda (x) (and (listp x) (eq (second x) 'NP_PP))) (third v))
               (setq p (find-if #'(lambda (x) (and (listp x) (eq (car x) 'PFORM)))
                          (third v)))
               (not (find (second p) '(of to by))))
          (push (list (car v) (second p)) result)))
   (remove-duplicates (reverse result) :test 'equal)
 )); end of associate-feats-with-np-pp-taking-verbs

(defparameter *v_np_pp* (make-hash-table :test 'equal))
;`````````````````````````````````````````````````````
; for registering whether for a given pair (<verb> <prep>), where the verb
; subcategorizes for np_pp, the PP expected by the verb can be of type 
;   PP[<prep>].
; E.g., (bestow on) and (bestow upon) will yield T, while (bestow for)
; yields nil. Prepositions {to, of, by} are omitted, because our ultimate
; interest is actually in which PPs should become ADVP -- namely, if
; they *don't* appear in *v_np_pp*, and {to, of, by} PPs in a VP almost 
; never do and so don't require a a check in *v_np_pp*.

; Now fill the hash table:
(mapcar #'(lambda (pair) (setf (gethash pair *v_np_pp*) T))
        (associate-feats-with-np-pp-taking-verbs))

