;; Junis Ekmekciu, 2021-02-23 
;; 
;; Some small tests for standardize-ulf.lisp file

(in-package :standardize-ulf)

;; Tests that check the possible-relative-clause? function on
;; standardize-ulf.lisp
(defun run () 
    (format t "First example prints: ~a ~%"  
        (standardize-ulf::possible-relative-clause? '(where.pro (pres want.v))))
    (format t "Second example prints: ~a ~%" 
        (standardize-ulf::possible-relative-clause? '(which.pro ((pres alter.v)
            (= (the.d (plur finding.n))) ))))
    (format t "Third example prints: ~a ~%" (standardize-ulf::possible-relative-clause? 
      '(sub who.pro (you.pro ((pres know.v) *h)))))
    (format t "Fourth example prints: ~a ~%" 
       (standardize-ulf::possible-relative-clause? 
          '(who.rel ((past break.v) (= (the.d vase.n))))))
    (format t "Fifth example prints: ~a ~%" (and t 
       (standardize-ulf::possible-relative-clause? 
          '(sub which.rel 
                    (*h ((past roam.v) (on.p (the.d |Earth|.n))))))))
    (format t "Sixth example prints: ~a ~%" (standardize-ulf::possible-relative-clause? 
      '(that.p ((past be.v) entertaining.a)))))
