; MONADIC MATCH-PREDICATES FOR MODIFYING PARSE TREES AND ULF TREES.
;
; NOTE: LOADING THIS FILE BUILD THE HASH TABLE OF PREDICATES FROM SCRATCH.
;       SO 'DEFPRED' EITHER SHOULDN'T BE USED ELSEWHERE, OR ANY OTHER FILES
;       CONTAINING 'DEFPRED' SHOULD BE RELOADED IF THIS ONE IS LOADED.
; =================================================================

(in-package :lenulf)

(defparameter *implicit-pred* (make-hash-table))
;  This is indexed by var names like !term, ?term, *term, +term, ?[np], ...
;  and provides the corresponding function name, like !term, ![np], ...
;  This is to avoid having to explode the var names when applying the
;  corresponding function.
;
(defparameter *-variant* (make-hash-table))
;  This is for lookup of *-variants of defined predicates (which start
;  with a '!'). This is for fast determination of these variants (without
;  break-down into characters, etc.), as needed in 'match', when a +-var
;  has been matched once, and we need to allow for addirional possible
;  matches using the *-variant of the underlying predicate.
;

; SOME USEFUL PARSE-TREE PREDICATES
; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
; Viewed as predicates, the {!,?,*,+}-variants of a name are all treated as
; synonyms; the punctuation prefix just determines how they are used in
; pattern matching. We will store the !-versions as the defined functions,
; where that name is reached by using any of the 4 variants as keys for
; hash table *implicit-pred*
;

(defmacro defpred (name var body) ; Oct 18/20; briefly tested
;```````````````````````````````
; name: e.g., !list, !atom, !np, ... (expected to start with '!');
; var: variable to be used in the body of the 1-place pred being defined;
; add storage of variant names in hash table *implicit-pred* to the 'defun';
;
 (list 'progn 
   (list 'defun name (list var) body)
   (list 'store-pred-name-variants (list 'quote name)))) 


(defun store-pred-name-variants (name)
;````````````````````````````````````
; name: e.g., !atom, !list, !expr, ![np], ![tense+aux], ...
; result: e.g., (![np] ?[np] *[np] +[np])
; effect: At the same time, these 4 variants of the predicate (functioning
;         as match variables) are used as keys into hash table *implicit-pred*,
;         where one then gets back 'name' (e.g., ![np]); and in addition,
;         the *-variant of the name is stored in another hash table, *-variant*,
;         where using 'name' as key gives its *-variant. This is needed in 
;         the 'match' function, where after matching a +-variant of a pred,
;         we want to allow 0 or more additional instanced of the pred.
;         (While the *-variant could be computed from the basic !-variant,
;         which is the Lisp-defined function, a hash-table lookup should be
;         more efficient.)
;
 (let (str result)
      (cond ((not (symbolp name))
             (format t "~%**ERROR: 'pred-name-variants' wants symbol arg, ~
              got ~a" name))
            ((and (setq str (string name)) nil) nil); continue after setq
            ((not (char= (schar str 0) #\!))
             (format t "~%**ERROR: 'pred-name-variants' wants !<name>, ~
              got ~a" name))
            (t (setq str (string-left-trim "!" str))
               (dolist (punc '("+" "*" "?"))
                  (push (intern (concatenate 'string punc str)) result)
                  ; for the *-variant of 'name', store that variant in 
                  ; the *-variant* hash table, with 'name' as key
                  (if (string= punc "*")
                      (setf (gethash name *-variant*) (car result))))
               (setq result (cons name result))
               (dolist (variant result)
                  (setf (gethash variant *implicit-pred*) name))
               result))
 )); end of store-pred-name-variants

; The following definitions automatically define {?,*,+}-variants
; All variants correspond to the same predicate (with an initial '!') but
; have different significance in patterns.
;
; NB: Functions with apparent phrasal names like !np are true of atoms 
;     (like 'NP), while ones with square brackets, like ![vp] are true 
;     of phrases, like '(VP ...)). However, '!expr' and '!list' are
;     exceptions -- the former holds for any atom or expression, 
;     and the latter generally holds for lists, but also for 'nil'.
; 
(defpred !expr x T) 
(defpred !atom x (atom x)) 
(defpred !list x (listp x))
(defpred !\, x (eq x '\,)); NB: this will also define optional comma, ?\,
                          ;     but this won't detect (\. \.)
(defpred ![comma] x (equal x '(\, \,)))
(defpred !pre-nn-pos+word x (find (car x) '(DT CD JJ JJR JJS WDT)))
(defpred ![nn-premod] x 
         (and (listp x) (find (car x) '(DT CD JJ JJR JJS ADJP WDT NN NNS PRP$))))
(defpred !advp x (find x '(ADVP WHADVP PP))); added PP Jan 8/21, tentatively
(defpred ![v-adjunct] x (and (listp x); V-adjunct possibilities
                             (find (car x) '(PP VP ADVP SBAR))))
(defpred ![np] x (and (listp x) 
                   (or (find (car x) '(NP WHNP NP-1 NP-2 NP-3 NP-4))
                       (equal (car x) '(-none- =))))); equative complements
(defpred ![np-or-sbar] x (and (listp x)
                          (or (find (car x) '(NP SBAR WHNP NP-1 NP-2 NP-3 NP-4))
                              (equal (car x) '(-none- =))))); equative complements
(defpred ![pers-pron-np] x (and (listp x); e.g., (NP (PRP you))
                                (eq (car x) 'NP)
                                (find (second (second x)) 
                                      '(I you we he she who whom))))
(defpred ![non-np-compl] x (and (listp x) 
                                (find (car x) '(S VP PP ADVP ADJP))))
(defpred !non-np x (or (atom x) 
                       (not (find (car x) '(NP WHNP NP-1 NP-2 NP-3 NP-4)))))
(defpred ![non-ex-np] x (and (listp x)
                             (find (car x) '(NP WHNP NP-1 NP-2 NP-3 NP-4))
                             (not (equal (second x) '(EX THERE)))))
(defpred !non-vp x (or (atom x) (not (find (car x) '(VP S SBAR)))))
(defpred ![non-vp] x (and (listp x) (not (eq (car x) 'VP))))
(defpred ![vp] x (and (listp x) (eq (car x) 'VP)))
(defpred ![advp] x (and (listp x) 
                        (find (car x) '(ADVP WHADVP PP RB WRB RBR RBS NEG))))
                                                   ;^^ may lack adv operator
(defpred ![pred] x (and (listp x) (find (car x) '(ADJP JJ JJR JJS PP))))
(defpred ![verb] x (and (listp x) (atom (car x)) (find (car x) 
            '(VB VBZ VBP VBD VBG VBN VBEN AUX AUXZ AUXD AUXP AUXG AUXEN MD))))
(defpred ![inf] x (and (listp x) (eq (car x) 'VP) (eq (cadadr x) 'TO)))
(defpred ![sbar] x (and (listp x) (eq (car x) 'SBAR)))
(defpred ![non-aux-be-verb] x ; not of form (<aux-pos> <be-verb>)
     (or (atom x) (not (isa (car x) '.aux)) (not (isa (cadr x) '.be))))
(defpred ![non-aux-part] x (or (atom x) (not (isa (car x) '.aux))))
(defpred ![punc-or-coord] x
     (and (listp x) (find (second x) '(\, \; - -- \( { [ and or & but nor))))
(defpred ![a{n}] x (and (listp x) (member (car x) '(a an))))
(defpred ![det-sing-alone] x (and (listp x) (find (second x) '(this that))))
         ; these can occur without a head noun
(defpred ![det-plur-alone] x (and (listp x) (find (second x) 
         '(these those all many most few)))); can occur without a head noun
(defpred ![non-det] x (or (atom x) (not (member (car x) '(DT PRP$)))))
(defpred !-ing x 
         (let (str n)
              (cond ((not (symbolp x)) nil)
                    (t (setq str (string x) n (length str))
                       (if (< n 6) nil
                           (string-equal (subseq str (- n 3)) "ing"))))))

(defpred ![v_whs] x ; example of x: (VBD knew)
;`````````````````````````
; Decide whether the verb (stem) subcategorizes for interpreting an
; immediately following when/where-clause as a nominal (rather than 
; adverbial); E.g., "He wondered when the tornado was arriving" (vs.,
; "He panicked when the tornado was arriving").
  (cond ((atom x) nil)
        ((isa (car x) '.vb) 
         (> (v_whs-pref! x) 0))
        (t nil)) )

(defpred ![v_np_whs] x
;``````````````````````
; Decide whether the verb (stem) subcategorizes for interpreting a when/
; where-clause following an np as a nominal (rather than adverbial);
; E.g., "He told Alice when he was arriving" (vs., "He phoned Alice when
; he was arriving").
  (cond ((atom x) nil)
        ((isa (car x) '.vb) 
         (> (v_np_whs-pref! x) 0))
        (t nil)) )
 
(defpred ![v_pp_whs] x
;``````````````````````
; Decide whether the verb (stem) subcategorizes for interpreting a when/
; where-clause following a pp as a nominal (rather than adverbial);
; E.g., "He confided to Alice when he was arriving" (vs., "He chatted
; with Alice when he was arriving").
  (cond ((atom x) nil)
        ((isa (car x) '.vb)
         (> (v_pp_whs-pref! x) 0))
        (t nil)) )

(defpred !big-expr x ; as least 4 words, for diplaced modifiers
;```````````````````
  (> (atom-count x) 3))

(defun atom-count (x)
  (cond ((atom x) 0)
        ((and (atom (car x)) (atom (second x)) (null (cddr x))) 1)
        (t (apply #'+ (mapcar #'atom-count x)))))

(defpred ![big-np] x
;```````````````````
  (and (listp x) (isa (car x) '.NP) (!big-expr x)))


; DOT-ATOMS: These are also predicates, for matching particular atoms
; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
; These don't allow for iteration; 'isa' remains to be extended so that
; a dot-predicate is satisfied iff there is an isa-path from the word 
; being matched to the feature represented by the dot-atom. (Currently
; the feature must be reached in one step, whereas we want to allow dot-
; atoms themselves to have features that are other dot-atoms, allowing
; chaining.)

(defun isa (atm feat)
;```````````````````
; Various "dot-atoms" are allowed as features that can be used to match
; the corresponding atoms. The dot-atom repertoire can be freely expanded.
;
 (case feat (.SQ (find atm '(SQ SINV)))
            (.S (find atm '(S SQ SINV S1 SBAR SBARQ)))
            (.SBAR (find atm '(SBAR SBARQ)))
            (.VP/SQ (find atm '(VP SQ)))
            (.VP/S (find atm '(VP S)))
            (.XP (find atm '(S NP VP AP ADJP ADVP PP WHNP WHADJP WHPP WHADVP)))
            (.XP-NON-VP (find atm '(S NP AP ADJP ADVP PP WHNP WHADJP WHPP WHADVP)))
            (.XP-OR-S (find atm '(S NP VP AP ADJP ADVP PP S SQ S1 SBAR SBARQ WHNP 
                             WHADJP WHPP WHADVP)))
            (.NON-PP (find atm '(S NP VP AP ADJP ADVP S SQ S1 SBAR SBARQ WHNP
                             WHADJP WHADVP)))
            (.NOT-NONE (not (eq atm '-NONE-)))
            (.NN (find atm '(NN NNS)))
            (.NNP (find atm '(NNP NNPS)))
            (.NN/NNP (find atm '(NN NNP))); e.g., "{last} March" can be NN or NNP
            (.NOT-DT (not (eq atm 'DT)))
            (.NP (find atm '(NP WHNP)))
            (.N-PROPOS-OBJ (gethash atm n-propos-obj))
            (.N-COMMUN-OBJ (gethash atm n-commun-obj))
            (.N-COG-OBJ (or (gethash atm n-propos-obj)
                            (gethash atm n-commun-obj)))
            ; CAVEAT: Predicates like the next two, subsuming both lexical
            ;         and phrasal elements, can lead to recursive run-away
            ;         when rules that combine phrases are applied as often
            ;         as possible!
            (.ADJP (find atm '(ADJP JJ JJR JJS)))
            (.ADVP (find atm '(ADVP WHADVP RB RBR RBS NEG PP)))
            (.NON-ADVP (find atm '(NP VP AP ADJP PP SQ SBARQ WHNP WHADJP WHPP)))
                ; XP, but not explicit 'ADVP'; aimed at topicalization. Assume
                ; fronted SBARs are not considered topicalized: "Although he won,
                ; he's dissatisfied"; allow "Why are you leaving, I ask" as 
                ; topicalized, though probably will be misparsed aby BLLIP.
            (.THIS-DAY (find atm '(yesterday today tomorrow)))
            (.WEEKDAY (find atm '(|Monday| | Monday| Monday 
              |Tuesday| | Tuesday| Tuesday |Wednesday| | Wednesday| Wednesday
              |Thursday| | Thursday| Thursday |Friday| | Friday| Friday
              |Saturday| | Saturday| Saturday |Sunday| | Sunday| Sunday)))
            (.TIME-PERIOD (find atm '(day week month year morning evening night
              January February March April May June July August September October
              November December Christmas spring summer fall winter)))
            (.next/last (find atm '(next last)))
            (.WHNP (find atm '(WHNP WDT)))
            (.WHXP (find atm '(WHNP WHADJP WHPP WHADVP)))
            (.DT (find atm '(DT CD PRP$))); initial CD is often a DT
            (.a/an (find atm '(a an)))
            (.RB (find atm '(RB RBR RBS NEG))); ex. of NEG: (NEG (NOT))
            (.NOT-PREP-OR-SYMB (not (member atm '(IN -SYMB-))))
            (.NP-i (find atm '(NP-1 NP-2 NP-3 NP-4))); e.g., rightward displace't
            (.CC (find atm '(CC \, \;))); for detecting end of an NP
            (.and/or (find atm '(and \& or \'r)))
            (.EXTRAP-S (find atm '(SBAR-1 SBAR-2 SBAR-3 SBAR-4 S-1 S2 S-3 S-4)))
             ; these SBAR/S variants are used in Brown to correlate it-extra
             ; with a corresponding clause; in ULF, we expect that it-extra
             ; provides enough of a clue to locate the extraposed clause.
            (.PP (find atm '(PP WHPP PP-1 PP-2 PP-3 PP-4)))
            (.with (find atm '(with without))); "with" is special, allowing 
                                              ; verbless S-complements
            (.when (find atm '(when where whenever wherever why how)))
                                        ; e.g., when/where there is peace ...
            (.PRED (find atm '(ADJP PP))); (VP (VBG|VBN ...) ...) also, but ....
            (.ADJP/NP/PP/SBAR/UCP/PRED 
                   (find atm '(ADJP NP PP SBAR UCP PRED)));copular complements
            (.POSTMOD (find atm '(ADJP PP SBAR))); (VP (VBG ...) ...) also, but
                                                 ; allowing general VP seems risky
                                                 ; Use separate rules for VBG &
                                                 ; VBN postmodifiers
            (.DIS-POSTMOD (find atm '(PP-1 PP-2 PP-3 PP-4 SBAR-1 SBAR-2 SBAR-3
               SBAR-4 VP-1 VP-2 VP-3 VP-4 ADJP-1 ADJP-2 ADJP-3 ADJP-4))); more?
            (.JJ (find atm '(JJ JJR JJS))); NB: .ADJP also inludes these
            (.VB (find atm '(VB VBZ VBP VBD VBG VBN VBEN)))
            (.VBG/VBN (find atm '(VBG VBN)))
            (.VBG/AUXG (find atm '(VBG AUXG)))
            (.AUX (find atm '(TO AUX AUXZ AUXD AUXP AUXF AUXG AUXEN AUX-CF MD)))
            (.MD/TO (find atm '(MD TO)))
            (.VB/AUX (find atm '(VB VBZ VBP VBD VBG VBN VBEN 
                                 AUX AUXZ AUXD AUXP AUXG AUXEN AUX-CF MD)))
            (.POSS-BASE-V (find atm '(VB VBZ VBP AUX AUXZ AUXP))); only VB & AUX
                      ; (excluding MD) should actually be base forms, but this
                      ; list allows for parsing errors
            (.PASPART (find atm '(VBN VBEN VBD AUX AUXEN))); possible parser-
                                ; assigned POS's for a past/passive participle
            (.final-punc (find atm '(\. ? !)))
            (.it/they/them/this/that (find atm '(it they them this that)))
            (.have (find atm '(have \'ve has \'s having had \'d)))
            (.have/ve (find atm '(have \'ve)))
            (.be (find atm '(be is \'s are am \'m was were being been \'re)))
            (.non-be (not (find atm 
                           '(be is \'s are am \'m was were being been \'re))))
            (.is/s/am/m (find atm '(is \'s am \'m))) 
            (.was/were (find atm '(was were)))
            (.are/re (find atm '(are \'re)))
            (.been/being (find atm '(been being)))
            (.be/become (find atm '(be is are am was were being been \'s \'re
                                    become becomes became becoming)))
            (.go (find atm '(go goes going gone went)))
            (t (format t "~%## Warning: ~s is not a defined dot-atom!" feat))
 )); end of isa

; SOME (POTENTIALLY) USEFUL ULF-TREE PREDICATES
; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

;; Initial notes:
;; e.g., change (<adv>.adv <adj>.a) to (<adv>.mod-a <adj>.a)
;; e.g., change because.p to because.ps
;; e.g., Numeric age-tags and year-tags, as in "Smith, 54", or "May, 1958"
;;       can probably be handled in postprocessing (they are paired in
;;       brackets, with the comma in between)
;; How useful postprocessing rules actually are is not yet clear -- they're
;; more complicated than constituent-tree processing rules, because the types
;; of subexpressions are implicit, and word-derived ULFs have trailing word
;; indices, "hiding" the atomic type ...

; To allow for loading this file w/o worrying about others that are needed:

(if (not (boundp '*ulf-atoms*))   ; table of ULF symbols, without word indices,
                                  ; accessed both via themselves and via any
                                  ; encountered versions with word in dices
    (defparameter *ulf-atoms* (make-hash-table)))

(if (not (boundp '*type-suffixes)); table of type suffixes, as strings, accessed
                                  ; via the ulf atom (as well as via versions
                                  ; with attached word indices)
    (defparameter *type-suffixes* (make-hash-table)))

(if (not (fboundp 'match)) (load "/u/schubert/tt.lisp"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;                                                       ;;
  ;; UTILITY ROUTINES TO SUPPORT ULF PREDICATE DEFINITIONS ;;
  ;; ````````````````````````````````````````````````````` ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Mostly these routines are intended to make it a matter of lookup
; to find the ULF atom that a word index has been attached to (i.e.,
; a tilde "~" plus some digits) instead of having to "take apart" the
; indexed ULF atom every time. But this is complicated by the need for
; ensuring that if an indexed atom (or even an unidexed one) hasn't yet
; had its unindexed form and type suffix stored (in the derivation of 
; the raw ULFs), the routines will still work -- and will put the
; required entries into the hash tables.

(defun recover-unindexed-atom (indexed-atom); briefly tested May 20/21
;``````````````````````````````````````````
; Retrieve the ULF atom of a (possibly) 'indexed-atom' from the *ulf-atoms*
; hash-table formed when a word-indexed atom is formed. E.g.,
;       (unindexed-atom 'from.p-arg~7) ==> from.p-arg.
; If 'indexed-atom' isn't in the *ulf-atoms* hash table, then it is
; processed by 'remove-word-index-and-store', putting the unindexed atom  
; into that hash table under both 'indexed-atom' and its unindexed form
; as keys. Also the type suffix, if any, is stored in hash table *type-
; suffixes*, under those same keys.
;
 (let (atm)
      (cond ((symbolp indexed-atom)
             (setq atm (gethash indexed-atom *ulf-atoms*))
             (if atm atm (remove-word-index-and-store indexed-atom)))
            (t indexed-atom)); return non-symbols unchanged
 )); end of recover-unindexed-atom


(defun remove-word-index-and-store (symb); briefly tested May 20/21
;``````````````````````````````````````
; Given a symbol 'symb' possibly with an attached word index (e.g., in 
;`AND~7' or FOR.P-ARG~13), return a version 'sym' of 'symb' without the
; word index (e.g., 'AND' or 'FOR.P-ARG'). There will be storage side-
; effects if symb was not yet stored. A non-symbol input will cause an error.
;
; Specifically, first look in hash table *ulf-atoms* under key 'symb',
; and return the value found, if it is non-nil. If it is nil, derive the
; unindexed symbol 'sym' from 'symb', and store sym in *ulf-atoms* under
; both key 'symb' and 'sym'. (So, if there is no word index, the value 
; stored under the (single) key 'symb' in *ulf-atoms* will just be 'symb'
; itself.) Whenever storing a previously unstored symbol 'symb' in hash
; table *ulf-atoms*, we also store the type suffix, if any, as a string
; in hash table *type-suffixes*, under both key 'symb' and 'sym' (which
; as noted may or may not be the same). 
;
 (let ((sym (gethash symb *ulf-atoms*)))
      (if sym (return-from remove-word-index-and-store sym))
      (prog ((str (string symb)) st n ch)
         (setq n (length str))
         (when (= n 1)
               (setf (gethash symb *ulf-atoms*) symb)
               (return symb)) ; store symb as itself, and return it;
                              ; there is no type suffix in this case
         ; trim initial digits from symb as a reverse string
         (setq st (reverse str))
         (loop (setq ch (char st 0))
               (if (digit-char-p ch)
                   (setq st (subseq st 1)); trim 1st character
                   (return nil))); exit loop when no more digits
         (when (= (length st) n); no final digits processed? 
               ; based on the initial check, symb is not yet stored; find
               ; type suffix & store both symb & its type suffix (if any)
               (setf (gethash symb *ulf-atoms*) symb); store symb as itself
               (setq st (dot-suffix str)); type suffix, as upper-case string
               (when st (setf (gethash symb *type-suffixes*) st))
               (return symb))
         ; digits were trimmed; does the remainder rule out an indexed atom?
         (when (or (< (length st) 2) (not (char= (char st 0) #\~)))
               (setf (gethash symb *ulf-atoms*) symb)
               (return symb)) ; store symb as itself, and return it
         ; there was material plus "~" preceding the digits:
         (setq str (reverse (subseq st 1))); original str minus word index
         (setq sym (intern str))
         (setf (gethash symb *ulf-atoms*) sym)
         ; don't yet store under key sym (possibly redundant storage commmand)
         (setq st (dot-suffix str)); type suffix, as upper-case string
         (when st ; sym does have a type-suffix (so sym =/= symb)
               (setf (gethash sym *type-suffixes*) st)
               (setf (gethash symb *type-suffixes*) st)
               (setf (gethash sym *ulf-atoms*) sym))
         (return sym))
 )); end of remove-word-index-and-store


(defun recover-type-suffix (symb); briefly tested May 20/21
; For any symbol 'symb', if it has no type suffix (1 or more characters
; following a final dot that is not at the beginning of 'symb', possibly
; followed by a word index consisting of '~' and one or more digits), 
; return nil. Otherwise return the type suffix, as a string. Also store
; the suffix, as a string, in *type-suffixes* if it's not yet stored.
;
; First see if the suffix can be recovered from hash table *type-suffixes*,
; using 'symb' as key; if not, recover the unindexed version of 'symb', 
; ('sym' as an atom, yielding 'str' as a string); find the type suffix of
; 'str' as a string, & if non-nil store it under keys 'symb' and 'sym', 
; and return it.
;
 (let (st sym str)
      (cond ((not (symbolp symb)) nil)
            (t (setq st (gethash symb *type-suffixes*))
               (cond (st st)
                     ; either the symbol has no type suffix, or it
                     ; has not been stored yet in *type-suffixes*
                     (t (setq sym (gethash symb *ulf-atoms*))
                        (if (null sym)
                            (setq sym (recover-unindexed-atom symb)))
                            ; this also has storage side-effects
                        (gethash sym *type-suffixes*)))))
 )); end of recover-type-suffix


(defun dot-suffix (str); tested May 18/21.
;`````````````````````
; Find the rightmost substring of str that is right after a dot; NIL if
; there is no such (nonempty) substring. A non-string str causes an error.
 (let ((n (length str)) k)
      (cond ((= n 1) nil)
            ((char= (char str (- n 1)) #\.) nil); str ends in a dot
            (t (setq k -1)
               ; find where the rightmost dot is (if any), and record
               ; the index k right after it
               (dotimes (i (- n 1))
                   (if (char= (char str (- n i 2)) #\.)
                       (return (setq k (- n i 1)))))
               (if (> k -1) (string-upcase (subseq str k)) nil)))
 )); end of dot-suffix


(defun contains-if (pred x); briefly tested May 18/21
;`````````````````````````
; Check if x or any subexpression of it satifyies predicate `pred`.
; `Pred` is assumed *not* to be true of NIL. The first-encountered 
; (sub)expression satifying `pred` is returned
;
 (let (result)
      (cond ((funcall pred x) x); allows for atomic & nonatomic argument
            ((atom x) nil); if it was atomic failed, we're done
            ((funcall pred (car x)) (car x))
            (t (dolist (y (cdr x)); left-to-right search, early termination
                  (setq result (contains-if pred y))
                  (if result (return-from contains-if result)))))
 )); end of contains-if


 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;;                                                                              ;;
 ;; PREDICATES OVER ULF EXPRESSIONS (USE SYMBOLS ENDING IN "~" BY CONVENTION)    ;;
 ;; `````````````````````````````````````````````````````````````````````````  ` ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defpred !coord~ x  ; briefly tested May 17/21
;`````````````````
; Is x a coordinator, possibly with an added ~ and word index?
; ** I'm only allowing "and" and "or" here currently, because I'm maily
;    concerned with con-/dis-junctions of length > 2; e.g., "but" and
;    "as well as" don't iterate; but maybe I need to check for AND, OR,
;    and *any* words with type suffix CC.
  (and (symbolp x) 
       (let ((sym (recover-unindexed-atom x)))
            (find sym '(AND OR AND.CC OR.CC)))))


(defpred !pred~ x
;````````````````
; Is x an atomic adjectival, nominal, or verbal predicate, or a nonatomic
; pred such as a preposition plus object, pred with args, (pre- or post-) mod-
; ified pred, equative pred, (unscoped) coordinated pred, or rel-clause pred?
;
 (cond ((!atomic-pred~ x) x)
       ((atom x) nil)
       ((!atomic-pred~ (car x)) x); prefix pred applied to args gives a pred
       ((find (car x) '(plur n+preds np+preds)) x)
       ((and (= (length x) 2)
             (!vp-pred~ (second x)); operator + VP-pred?
             (or (find (car x) '(pres past cf pasv perf (pres prog) (past prog)
                                 (pres perf) (past perf) (pres pasv) (past pasv)
                                 (cf perf)); Note: the only ones of the preceding
                                           ; binary combinations that are really
                                           ; allowed are ones of form (... perf),
                                           ; where 'perf' stands for 'have.aux-s'
                                           ; whereas, e.g., ((pres prog) walk.v)
                                           ; should be (pres (prog walk.v)),
                                           ; or ((pres be.aux) (prog walk.v)),
                                           ; with 'prog' coming from "walking",
                                           ; not "is" (= (pres be.aux) = pres;
                                           ; be.aux is the identity operator).
                                           ; But, we ensure robustness here ...
                                :test 'equal)
                 (!aux-op~ (car x))
                 ; "modified" auxiliary, e.g., (pres can.aux-v):
                 (and (listp (car x)) (!aux-op~ (second (car x))))))
         x)
       ; Note: the preceding "operator + VP-pred" cases don't cover
       ;       adverbial premodific'n of VP-preds; the following does
       ((and (atom (car x)) (equal (recover-type-suffix (car x)) "ADV-S")
                   (!pred~ (second x))) x); e.g., "surprisingly rich"
       ((and (!pred-premod~ (car x)) (!pred~ (second x))) x)
       ((and (!pred~ (car x)) (!pred-postmod~ (second x))) x)
       ((equal (car x) '=) x); equative pred
       ((and (!pred~ (car x)) (find-if #'!coord~ (cdr x))) x); coordinated pred
       ; relative-clause predicates:
       ((and (!rel-pro~ (car x)) (!vp-pred~ (second x))) x); e.g., [man] who left
       ((and (eq (car x) 'sub) 
             (or (!rel-pro~ (second x)) (contains-if #'!rel-pro~ (second x))))
             x); e.g., 
       (t nil)
 )); end !pred~


(defpred !atomic-pred~ x
;```````````````````````
; Is x an atomic pred (of arity >= 1)? If so, return x, o/w nil.
 (let (str)
      (cond ((not (symbolp x)) nil)
            ((find x '(= > < >= <=)) x); identity & arithmetic preds;
                                       ; ALL OTHER PREDS HAVE DOT-SUFFIXES
            (t (setq str (recover-type-suffix x))
               (if (null str) (return-from !atomic-pred~ nil))
               (cond ((and (= (length str) 1); simple type suffix
                           (find (char str 0) 
                                '(#\N #\V #\P #\A) :test 'char-equal)) x)
                     (t nil))))
               ; Note: Suffixes p-arg, ps, pq, adv, adv-a, adv-s, etc., 
               ; though starting with p and a don't designate predicates.
               ; Subclassification of predicative prepositions, if desired,
               ; should be done like this: 'at-time.p', 'at-loc.p', ...
 )); end of !atomic-pred~


(defpred !vp-pred~ x
;```````````````````
; Is x an atomic verbal pred, or such a pred applied to args and/or adjuncts,
; or an auxiliary applied or such a pred, or operated on by tense, aspect, etc.?
; 
 'TBC ); not sure how useful the predicates here will actually be!!)


(defpred !aux-op~ x
;``````````````````
; Is x an auxiliary operator (derived from perf/prog/pasv and modal auxiliaries)?
; Note that in general auxiliaries are semantically ambiguous between predicate
; modifiers and sentence modifiers. However, in the raw ULF, as in syntax,
; sentence-modifying auxiliaries appear as VP-operators in declarative sentences
; (e.g., "He may faint"); in questions, they appear as VP sisters (e.g., "Did
; he faint?"). So in any case, it makes sense to recognize that any expression
; with an auxiliary (on the left) applied to it must be a predicate. So here
; we don't need to distinguish predicate- and sentence-modifying auxiliaries.
;
; Note also that if perfect, progressive, or passive auxiliaries are interpreted
; as 'perf', 'prog', or 'pasv' respectively, possibly prefixed with a tense
; operator, this is taken care of separately in the definition of !pred~.
; TBC
;
 (let (str)
      (cond ((not (symbolp x)) nil)
            ((null (setq str (recover-type-suffix x))) nil); no suffix => not aux
            (t (and (> (length str) 2) (string-equal (subseq str 0 3) "AUX"))))
 )); end of !aux-op~


(defpred !rel-pro~ x
;```````````````````
; Note: "whose" is the outlier here, as it's almost always a relative determiner,
;       not a relative pronoun (exception "If it's not yours, then whose is it?").
;       However, the check (contains-if #'!rel-pro~ ...) in the !pred~ predicate
;       might as well check for "whose" as well -- e.g., "the man whose car you
;       bought"; the ULF might contain 'who.rel' or 'that.rel', etc., or 'whose.dr'
;       or even 'whose.rel', but in all cases we have a constituent to be sub-
;       stituted for *h, while a predicate is abstracted from this.
; Note: "what" and "whatever" are never relative pronouns (*"things what he did",
;       vs "things which he did"; & there seems to be no postmodifier using
;       "whatever", analogous to the role of "whenever" in, e.g.,"the applause
;       whenever he sang ..." (though of course we can have appositions as in
;       "this thing, whatever it is" or adverbials as in "It's inexcusable,
;       whatever he may say")
 (and (symbolp x)
      (find (recover-unindexed-atom x)
            '(who.rel whom.rel that.rel tht.rel which.rel when.rel why.rel 
              where.rel whenever.rel wherever.rel whereto.rel wherefrom.rel
              whither.rel whose.dr whose.rel))))


(defpred !prep-pred~ x
;`````````````````````
; This is non-nil iff symbol x, after removal of any word index, ends in .p
; Note that e.g., 'on.p-arg' is not a predicate, but an identity operator.
 (if (and (symbolp x) (string-equal (recover-type-suffix x) "P")) x nil))


(defpred !pred-premod~ x
;```````````````````````
; Check if x is a predicate premodifier when operating on a predicate.
; This covers premodifiers like adjectival or nominal "predicates" in
; prenominal position (or explicitly operated on by a pred-->pred-premod
; type shifter), or adverbials in pre-adjectival, pre-prepositional or
; pre-verbal predicate position. 
 (let (str)
      (cond ((symbolp x); consider atomic premodifiers first
             (setq str (recover-type-suffix x))
             (cond (str 
                  ; does the suffix start with 'a' (allowing for adjectival
                  ; and adverbial predicates) but is not 'aux..."? If so,
                  ; it can be a pred premodifier:
                     (cond ((and (char-equal (char str 0) #\A)
                                 (or (= (length str) 1) 
                                     (not (char-equal (char str 1) #\U)))) x)
                  ; does the suffix start with 'n' (allowing for nominal
                  ; predicates) but is not 'name'? If so, it can be a pred
                  ; premodifier: 
                  ; {** allow e.g (Coke.name bottle.n), (| Coke| bottle.n) ??
                  ;     Do we REQUIRE a type shifter, e.g., ((nnp | Coke|) ...)?}
                  ;     We probably should, o/w this all gets too tricky...
                           ((and (char-equal (char str 0) #\N)
                                 (or (= (length str) 1)
                                     (not (char-equal (char str 1) #\A)))) x)
                  ; Is the suffix "mod-a" or "mod-n"?
                          ((find str '("mod-a" "mod-n") :test #'string-equal) x)
                          (t nil))); no further type-suffix possibilities

                   (t nil))); x is a symbol without a type-suffix 
                            ; (e.g., a type-shifter, tense/aspect operator,
                            ; determiner, coordinator, or name; in certain
                            ; combinations of tense/aspect operators with preds
                            ; the operator could be thought of as being like a
                            ; predicate premodifier, but we handle such constructs
                            ; in the non-atomic modifier section below, using
                            ; '!pred~', which recognizes such constructs.
            ; x is not a symbol
            ((atom x) nil); e.g., a string
            ; complex expression -- e.g., derived from APs, PPs, ADVPs
            ((find (car x) 
                  '(adv adv-a adv-e adv-f adv-m mod-a mod-n nnp)) x)
            ; x is not a modifier created by a predicate-modifier creator
            ; Note: "surprisingly" in a context like "surprisingly happy man"
            ;   is of type adv-s, so this is handled in !pred~ rather than
            ;   here, because normally <...>.adv-s is a sentence modifier
            (t nil))
 )); end of !pred-premod~

