;; Junis Ekmekciu, 2021-02-23 
;; 
;; Some small tests for standardize-ulf.lisp file

(in-package :standardize-ulf/tests)

(defun possible-relative-clause? (inx)
  (in-intern (inx x :standardize-ulf)
    (standardize-ulf::possible-relative-clause? x)))

;; Tests that check the possible-relative-clause? function on
;; standardize-ulf.lisp

(define-test relative-clause-correction
  (:tag :relative-clause)
  (assert-true
    (not (null (possible-relative-clause? '(where.pro (pres want.v))))))
  (assert-true
    (not (null (possible-relative-clause?
                 '(which.pro ((pres alter.v)
                              (= (the.d (plur finding.n)))))))))
  (assert-true
    (not (null (possible-relative-clause?
                 '((which.pro (pres alter.v))
                   (when.ps (it.pro (sub (k alteration.n)
                                         ((pres find.v) *h)))))))))
  (assert-true
    (not (null '(sub who.pro (you.pro ((pres know.v) *h))))))
  (assert-true
    (not (null
           (possible-relative-clause? 
             '(who.rel ((past break.v) (= (the.d vase.n))))))))
  (assert-true
    (not (null (possible-relative-clause? 
                 '(sub which.rel 
                       (*h ((past roam.v) (on.p (the.d |Earth|.n)))))))))
  (assert-true
    (not (null (possible-relative-clause? 
                 '(that.p ((past be.v) entertaining.a)))))))

;; Example tests.

(defun add-of (x)
  (intern (concatenate 'string
                       (symbol-name x)
                       "-of")))

(define-test example-tests
  (:tag :examples :fun-examples)

  (let ((ulf-part1 'chairman.n)
        (ulf-part2 '(the.d department.n)))
    
    (assert-equal '(chairman-of.n (the.d department.n))
                  (list (add-of ulf-part1)
                        ulf-part2)
                  ulf-part1
                  ulf-part2
                  (add-of ulf-part1)))

  (assert-equal 1 2)
  (assert-equal 1 1)
  (assert-true t)
  (assert-true nil)

  (assert-equal 5 (+ 1 3))
  ;;(assert-true (equal '(chairman-of.n (the.d department.n))
  ;;                    (fix-relational-noun '(n+preds chairman.n (of.p (the.d department.n))))))


  ;;(assert-equal '(chairman-of.n (the.d department.n))
  ;;              (fix-relational-noun '(n+preds chairman.n (of.p (the.d department.n)))))

  ;;(assert-error 'type-error (bad-function 'bad-input))

  )

(define-test relational-bad-n+preds
  ..)

(defune-test relational-bad-plur
  ..)

;; Relational Nouns.
;; Winnie Wan, 2021-03-23
;;
;; Tests that check the relational noun erroneous patterns using 
;; examples from FraCaS dataset.
(defun fix-relational-noun (inx)
  (inout-intern (inx x :standardize-ulf :callpkg :standardize-ulf/tests)
    (standardize-ulf::fix-relational-noun x)))

(defun convert-noun (inx)
  (inout-intern (inx x :standardize-ulf :callpkg :standardize-ulf/tests)
    (standardize-ulf::convert-noun x)))
  
(defun remove-n-preds (inx)
  (inout-intern (inx x :standardize-ulf :callpkg :standardize-ulf/tests)
    (standardize-ulf::remove-n-preds x)))
  
(define-test relational-noun-correction
  (:tag :relational-nouns)

  ; Tests function fix-relational-noun that takes erroneous relational noun sentence 
  ; and returns the correct relational noun form.
  (assert-true (equal '((THE.D (PLUR (INHABITANT-OF.N (| CAMBRIDGE|)))) ((PAST VOTE.V) (FOR.P (A.D LABOURMP.N))))
                        (fix-relational-noun '((THE.D (N+PREDS (PLUR INHABITANT.N) (OF.P | CAMBRIDGE|))) ((PAST VOTE.V) (FOR.P (A.D LABOURMP.N)))))))

  ; Tests function convert-noun that takes in the erroneous relational noun sentence 
  ; and the relational noun and returns (<noun-of> <term>) - combining noun + of + term.
  (assert-true (equal '((THE.D (N+PREDS (PLUR (INHABITANT-OF.N (| CAMBRIDGE|))))) ((PAST VOTE.V) (FOR.P (A.D LABOURMP.N))))
                        (convert-noun '((THE.D (N+PREDS (PLUR INHABITANT.N) (OF.P | CAMBRIDGE|))) ((PAST VOTE.V) (FOR.P (A.D LABOURMP.N)))))))

  ; Tests function remove-n-preds that takes in the erroneous relational noun sentence 
  ; and determines whether you remove n+preds depending on the number of predicates. 
  ; If the sentence ahs 3 or more predicates, then remove n+preds. Else, nothing changes.
  (assert-true (equal '((THE.D ((PLUR INHABITANT-OF.N | CAMBRIDGE|)))((PAST VOTE.V) (FOR.P (A.D LABOURMP.N))))
                        (remove-n-preds '((THE.D (N+PREDS (PLUR INHABITANT-OF.N | CAMBRIDGE|))) ((PAST VOTE.V) (FOR.P (A.D LABOURMP.N)))))))

  (assert-true (equal '((EVERY.D (CANADIAN.A RESIDENT.N))((PRES BE.V)(= (A.D (N+PREDS RESIDENT-OF.N (THE.D ((NORTH.A AMERICAN.A) CONTINENT.N)))))))
                        (remove-n-preds '((EVERY.D (CANADIAN.A RESIDENT.N)) ((PRES BE.V) (= (A.D (N+PREDS RESIDENT-OF.N (THE.D ((NORTH.A AMERICAN.A) CONTINENT.N)))))))))))
