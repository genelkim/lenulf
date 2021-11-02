;;; Fix relational-nouns 
;;; Winnie Wan 
;;; April 12, 2021

(in-package :standardize-ulf)

;; Abstract parameter that contains obvious relational-nouns.
;; (defparameter *relational-nouns* '(PLUR INHABITANT.N))

;; Function is-relational? returns whether it's a relational-noun 
;; sentence.
;; condition: plur, .n
(defun is-relational? (sym)
  (or (equal sym 'INHABITANT.N)
      (equal sym 'resident.n)))

;; Function fix-relational-noun takes a erroneous parsed relational-noun 
;; sentence and returns the correct form.
;; Example:
;; ((THE.D (N+PREDS (PLUR INHABITANT.N) (OF.P | CAMBRIDGE|)))
;; ((PAST VOTE.V) (FOR.P (A.D LABOURMP.N))))
;; 
;; ((THE.D (INHABITANT-OF.N | CAMBRIDGE|)
;; ((PAST VOTE.V) (FOR.P (A.D LABOURMP.N))))
(defun fix-relational-noun! (sym)
  (remove-n+preds (convert-noun sym)))

;; Function convert-noun takes a parsed sentence and the noun 
;; and converts it into (<noun-of> <term>). 
;; 
;; For example: (<noun> (of.p <term>)) ->  (<noun-of> <term>)
(defun convert-noun (sym)
  (ttt:apply-rules
    '((/ (_*1 is-relational? (of.p _!) _*2)
         (_*1 (fix-noun! (is-relational? of.p _!)) _*2))
      (/ (_*1 (plur is-relational?) (of.p _!) _*2)
         (_*1 (plur (fix-noun! (is-relational? of.p _!))) _*2)))
    sym))

;; Function remove-n+preds takes a parsed sentence and checks whether 
;; it should be removed if it has three or more predicates. 
;;
;; Example: 
;; (n+preds (<noun>-of.n <term>)) -> (<noun>-of.p <term>)
;; (n+preds <noun>-of.n <term>) -> (<noun>-of.p <term>)
;; (n+preds <noun> (of.p <term>)) -> (<noun>-of.n <term>) 
;; (n+preds <noun> (of.p <term>) <pred>) -> (n+preds (<noun>-of.n <term>) <pred>) 
(defun remove-n+preds (sym)
  (let ((initial-fix (ttt:apply-rules
                       '((/ (n+preds _!) _!)
                         (/ (n+preds relational-ulf-form? term?)
                            (relational-ulf-form? term?))
                         (/ (n+preds is-relational? (of.p term?))
                            ((make-relational! is-relational?) term?))
                         (/ (n+preds is-relational? (of.p term?) pred?)
                            (n+preds ((make-relational! is-relational?) term?) pred?)))
                       sym)))
    ;; Lift 'plur' if necessary.
    (ttt:apply-rules
      '((/ ((plur relational-ulf-form?) term?)
           (plur (relational-ulf-form? term?))))
       initial-fix)))

(defun make-relational! (atm)
  (fuse-into-atom (list (split-by-suffix atm) '-of.n)
                  :pkg :standardize-ulf))

;; Function to fix the merged list by modifying noun & of. 
(defun fix-noun! (lst)
  ;; remove of.p 
  (delete (nth 1 lst) lst)

  ;; edit noun to have -of.n
  ;; split-by-suffix splits the '. and returns the value.
  ;; fuse-into-atom merges all the symbols
  (setf (nth 0 lst)
        (fuse-into-atom (list (split-by-suffix (nth 0 lst)) '-OF.N) 
                        :pkg :standardize-ulf))
  lst)

;; Function that checks whether this atom is of the form of a relational ulf
;; form. That is, it ends in -of.n
(defun relational-ulf-form? (atm)
  (when (not (atom atm))
    (return-from relational-ulf-form? atm))

  (equal '(#\- #\O #\F #\. #\N)
         (let ((chars (coerce (symbol-name atm) 'list)))
           (subseq chars
                   (- (length chars) 5)))))

