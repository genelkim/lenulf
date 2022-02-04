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


(defun associate-feats-with-np+pp-taking-verbs ()
;```````````````````````````````````````````````
; Form of Alvey *v.le* entries (note features in 3rd list element): 
;    (<verb> "" (V (f1 v1) ... (fk vk)) <log-atom> nil) 
; Run through *v.le*, checking for each entry with feature NP_PP whether
; it's included in my filtered hash table *strong-np+pp-taking-verbs*,
; and if so, if it has a corresponding (PFORM <prep>) feature where <prep>
; is not one of {of, to, by}, and if so, adding this to output list
;    result: ((v1 f1) (v2 f2) (v3 f3) ...)
; where a verb vi may appear multiple time with various features. The
; reason for omitting {of, to, by} is just that these are reliably argument
; suppliers, and the reason for constructing the present list is to *rule
; out* argument-supplying PPs after and NP, so, {of, to, by} can be ruled
; out a priori.
;
 (if (not (boundp '*strong-np+pp-taking-verbs*)) 
     (load (merge-pathnames lenulf/config:*raw-parser-directory*
                            "transitivity-lists.lisp")))
 (let (result p)
   (if (not (boundp '*v.le*))
     (load (merge-pathnames lenulf/config:*raw-parser-directory*
                            "alvey-verbs.lisp")))
   (dolist (v *v.le*)
      (if (and (gethash (car v) *strong-np+pp-taking-verbs*)
               (find-if 
                  #'(lambda (x) (and (listp x) (eq (second x) 'NP_PP))) (third v))
               (setq p (find-if #'(lambda (x) (and (listp x) (eq (car x) 'PFORM)))
                          (third v)))
               (not (find (second p) '(of to by))))
          (push (list (car v) (second p)) result)))
   (remove-duplicates (reverse result) :test 'equal)
 )); end of associate-feats-with-np+pp-taking-verbs

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
        (associate-feats-with-np+pp-taking-verbs))

(defparameter *weakly-transitive-verbs* nil)
; Will become a list of weakly transitive verbs, with property @count,
; which is the excess of intransitive forms over trasitive forms (>= 0).

(defparameter *v_{np}_* (make-hash-table))
;`````````````````````````````````````````````
; Verbs that have more intransitive forms (i.e., not subcategorizing for 
; an NP) than transitive forms (subcategorizing at least for an NP; e.g.,
; agree, become, overwork, relax, etc. We increment property @count by 1
; for every entry for a verb that has none of the features
; {NP, NP_PP, NP_PP_PP, OC_INF, NP_SFIN, NP_LOC, NP_WHS, NP_WHVP, NP_ADL, 
; NP_ADVP, NP_AP, SC_NP, SC_NP_NP, SC_NP_AP, SC_NP_INF, NP_MP, NP_SBSE, 
; NP_NP_SFIN, OC_NP, SFIN, OC_ING, OC_AP}, and decrement @count for every
; entry that has at least one of these features. Verbs with count 0 or
; more are added to *weakly-transitive-verbs*.

; Now fill in the hash table:
(if (not (boundp '*v.le*))
  (load (merge-pathnames lenulf/config:*raw-parser-directory*
                         "alvey-verbs.lisp")))
(let ((entries; select base-form entries (as far as easily possible)
        (mapcar #'(lambda (x) 
                    (if (intersection (extract-feat-vals x) '(+ EN ING)) NIL x))
                 *v.le*))
      (np-feats '(NP NP_PP NP_PP_PP OC_INF NP_SFIN NP_LOC NP_WHS NP_WHVP NP_ADL 
                  NP_ADVP NP_AP SC_NP SC_NP_NP SC_NP_AP SC_NP_INF NP_MP NP_SBSE 
                  NP_NP_SFIN OC_NP SFIN OC_ING OC_AP))
      verb prev-verb feat-vals)
     (setq entries (remove nil entries))
     (dolist (ent entries) (setq verb (car ent)) (setf (get verb '@count) 0))
     (dolist (ent entries)
             (setq verb (car ent))
             (setq feat-vals (extract-feat-vals ent))
             (if (intersection feat-vals np-feats)
                 (decf (get verb '@count))
                 (incf (get verb '@count))))
     (dolist (ent entries)
             (setq verb (car ent))
             (when (and (not (eq verb prev-verb)) (>= (get verb '@count) 0))
                   (push verb *weakly-transitive-verbs*) 
                   (setf (gethash verb *v_{np}_*) (get verb '@count)))
             (setq prev-verb verb))
     (setq *weakly-transitive-verbs* (reverse *weakly-transitive-verbs*))
 ); end of code filling *weakly-transitive-verbs* hash table
