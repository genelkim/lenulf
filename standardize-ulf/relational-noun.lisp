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
  (equal sym '(PLUR INHABITANT.N)))

;; Function fix-relational-noun takes a erroneous parsed relational-noun 
;; sentence and returns the correct form.
;; Example:
;; ((THE.D (N+PREDS (PLUR INHABITANT.N) (OF.P | CAMBRIDGE|)))
;; ((PAST VOTE.V) (FOR.P (A.D LABOURMP.N))))
;; 
;; ((THE.D (INHABITANT-OF.N | CAMBRIDGE|)
;; ((PAST VOTE.V) (FOR.P (A.D LABOURMP.N))))
(defun fix-relational-noun (sym)
  (remove-n-preds (convert-noun sym)))

;; Function convert-noun takes a parsed sentence and the noun 
;; and converts it into (<noun-of> <term>). 
;; 
;; For example: (<noun> (of.p <term>)) ->  (<noun-of> <term>)
(defun convert-noun (sym)
  (cond
    ((not (listp sym)) sym)
    ((< (length sym) 2) (mapcar #'convert-noun sym))
    (t
      (let ((fst (convert-noun (car sym)))
            (scd (convert-noun (cadr sym))))
        (cond
          ((and 
             (is-relational? fst) 
             (has-of? scd))     
           (cons (fix-noun (append fst scd)) (convert-noun (cddr sym))))            
          (t (cons fst (convert-noun (cdr sym)))))))))

;; Function remove-n-preds takes a parsed sentence and checks whether 
;; it should be removed if it has three or more predicates. 
;;
;; Example: 
;; (n+preds (<noun>-of.n <term>)) -> (<noun> (of.p <term>))
;; (n+preds (<noun>-of.n <term>) <pred>) -> (n+preds <noun> (of.p <term>) <pred>)
(defun remove-n-preds (sym) 
  (cond
    ((not (listp sym)) sym)
    (t
      (let ((fst (remove-n-preds (car sym))))
        (cond
          ((and 
             (is-npreds? fst)
             (= (length sym) 2))
           (remove fst sym)) 
          (t (mapcar #'remove-n-preds sym)))))))

;; Function to fix the merged list by modifying noun & of. 
(defun fix-noun (lst)
  ;; remove of.p 
  (delete (nth 2 lst) lst)

  ;; edit noun to have -of.n
  ;; split-by-suffix splits the '. and returns the value.
  ;; fuse-into-atom merges all the symbols
  (setf (nth 1 lst)
        (fuse-into-atom (list (split-by-suffix (nth 1 lst)) '-OF.N) 
                        :pkg :standardize-ulf))
  lst)

;; Function that checks whether the list has OF.P
(defun has-of? (lst)
  (member 'OF.P lst))

;; Function that checks whether the list has N+PREDS
(defun is-npreds? (lst)
  (equal 'N+PREDS lst))

