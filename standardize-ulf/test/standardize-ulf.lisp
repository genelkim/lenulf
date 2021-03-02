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

