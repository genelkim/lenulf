; Oct 16/20  Enhanced TT transduction language
;
; Started as a variant of tt.lisp, allowing arbitrary predicates in
; patterns, starting with !, ?, *, or +, in addition to dot-predicates
; (which are based on 'isa'). Also, output templates will allow
; computable functions ending in '!'. The prefixed punctuation in
; pattern predicates as before indicates the kind of iteration allowed
; over sequences of constituents (! = exactly 1, ? = 0 or 1, * = 0 or
; more, + = 1 or more). I'll use a convention of having predicates
; over ULF constituents end in '~'; for instance, '.v~' would match 
; the indexed verbal predicate 'see.v~2'. ** LATER: I'M NOT SURE I
;   DID IT THAT WAY. BUT IT'S INTENDED TO DISTIGUISH ULF PREDS FROM
;   ENGLISH WORD-LEVEL & PHRASE LEVEL PREDS (WHERE PHRASE-LEVEL ONES
;   ARE SQUARE-BRACKETED; E.G., .ADVP MIGHT STAND FOR THE ATOM, 'ADVP'
;   ALONG WHITH VARIANTS LIKE 'WHADVP' OR EVEN 'PP', WHEREAS ![ADVP]
;   WOULD BE TRUE OF AN ACTUAL PHRASAL EXPRESSION.
;
; This was motivated by operations like inserting "holes" *h in ULFs
; for questions, relative clauses, etc., and perhaps other ULF post-
; processing operations. It may also be that the rules for processing 
; BLLIP parse trees prior to ULF extraction can be simplified with the 
; above predicate devices, but this remains open at the time of writing. 

; =====================================================================
; Apr 30/20  The TT transduction language
;
; These are tree transduction tools for making Treebank trees more nearly
; suitable for direct conversion to ULF (essentially by dropping phrase
; category names like NP, VP, SBAR, etc., while keeping the bracketing,
; and converting lexical items of form (POS word) to ULF)
;
; We use a pattern matching/transduction language TT that combines elements
; of TTT and the matching language in the dialogue manager for LISSA, ETA,
; etc. In particular, we use numerical location indices corresponding to
; constituents of an expression, in preference to the names of match 
; variables, for picking out subexpressions of a matched expression; this
; is actually more readable than variable namess. But we also use prefixes
; in the set {! ? * +} on match variables to indicate match to a single
; expression, to 0 or 1 expression, to 0 or more expressions, and to 1 or
; more expressions. Because we refer to subexpressions via indices, we can
; keep reusing the same variables in a single match expression; the names
; indicate type restrictions: E.g., !atom matches an atom, while +list
; matches a sequence of one or more lists; the atoms we prefix with 
; {! ? * +} to form variables are restricted to {atom, list, expr}, where
; wxpr subsumes {atom, list}. Thus there are just 12 basic variables, which
; we can reuse arbitrarily in patterns. But we can add some other special
; variables, e.g., ?\, (optional comma) turns out to be useful.
;
; TT also allows "dot-atoms" like .NN, .S, which can be thought of as 
; features (or superordinate categories) of small groups of atoms, 
; e.g. (respectively) {NN, NNS}, {S, SBAR, SINV}, or whatever might be
; needed. An atom matches a dot-atom if (isa atom dot-atom) is 
; true. (This in principle allows for any computable predicate on atoms)
;
; The 'match' function finds a finds rightmost matches (while TTT finds 
; leftmost matches), but that could fairly easily be changed. Rightmost 
; matches are better when "soft variables" (ones allowing binding to 
; the empty sequence, coded as (:seq)) are more likely to remain empty
; than being filled. (Conversely, leftmost matches are appropriate if
; most optional constituents are expected to be filled.)
;
; The match process, if successful, reproduces the matched expression
; except that it wraps subsequences that were matched by variables of
; type ?, *, or + and are not of length 1 in (:seq ...). (Thus (:seq)
; codes the empty sequence, (:seq A (B C)) codes 'A (B C)', etc. This
; is what allows access to any element via position indices, as follows
;  0 -- the expression as a whole;
;  1, 2, 3, ... -- top-level elements of the match result
;  1.1, 1.2, 1.3, ..., 2.1, 2.2, 2.3, ... -- second-level elements
;  1.1.1, 1.1.2, 1.1.3, ... 1.2.1, 1.2.2, 1.2.3, ... -- 3rd-level elements
;  etc.
; These indices can then be used freely in the reassembly templates;
; reassembly is finished by "flattening" any (:seq ...) elements occurring
; in the result

(in-package :lenulf)

; Utility for using complex indices for expression accesss;
; `````````````````````````````````````````````````````````
(defun indexed-element-of (expr index); May 1/20
;`````````````````````````````````````
; e.g., (indexed-element-of '(a (b c) d) 2) --> (b c)
; e.g., (indexed-element-of '(a (b c) d) '(2 1)) --> b
; e.g., (indexed-element-of '(a (b (c d) e) (f g)) '(2 2 2)) --> d
; N.B.: index is assumed to be an integer or list of integers, like those 
;       above, o/w a crash will occur somewhere along the line.
 (let (ii result)
      ; assume index 0 refers to the entire expr, with (:seq ...) "unwrapped"
      (cond ((eq index 0) (flatten-sequences expr))
            ((not (listp expr)) 
             (format t "~%***Bad first argument in (indexed-element-of ~a ~a)"
                       expr index)
             '*ERROR*)
            ((integerp index) (nth (- index 1) expr))
            ((or (null index) (find-if #'(lambda (x) (not (integerp x))) index))
             (format t "~%***Bad second argument in (indexed-element-of ~a ~a)"
                       expr index)
             '*ERROR*)
            (t (setq result expr)
               (dolist (i index)
                 (setq result (nth (- i 1) result)))
               result))
 )); end of indexed-element-of


(defun match (pa ex); match pattern pa to expression ex; May 4/20; tested
; `````````````````````````````````````````````````````
; pa: a pattern, which is any atom or list structure where some atoms may
;     be of form !<pred>, ?<pred>, *<pred>, or +<pred>, interpreted as
;     match variables that can match one expression, 0 or 1 expression,
;     0 or more expressions, or 1 or more expressions respectively, 
;     with each matched expression satisfying <pred> (i.e., yielding 
;     a non-nil value)
; ex: any atom or list structure
;
; Result: Failure is :nil, because we want (match nil nil) = nil (success).
;     TBC: REVISE THE COMMENTS, AS NOW WE ALWAYS RETURN (:SEQ ...) FOR
;     NON-!-VARS, I.E., SEQUENCE VARIABLES (STARTING WITH ?, *, OR +).

;     for a successful (non-nil) result, the result will resemble the
;     input expression 'ex' that was matched, except that sequences of 2
;     or more constituents matched by a variable of type *<pred>, ?<pred>,
;     or +<pred> will be encoded as (:seq <item1> <item2> ...). If 'ex'
;     is the empty list and 'pa' matched it, the result will be ((:seq)),
;     i.e., a list containing the empty sequence (thus implicitly nil,
;     once (:seq <item1> <item2> ...) constructs have been "flattened"
;     to leave just <item1> <item2> ... .)
;
; The form of the result allows retrieval of values of matched pattern 
; elements by the positions of the pattern elements. E.g., 3 indexes 
; whatever matched the 3rd top-level element of the pattern; e.g.,
; '3.2.2 (note: a symbol) indexes whatever matched the second element of
; the second element of the 3rd top-level element of the given pattern.
;
; Patterns identical to given expressions, or parts of patterns indentical
; to corresponding parts of given expressions, give back those same expressions
; or parts of expressions. But match predicates can match any expressions or
; sequences of expressions. Where they match an expression, the result will
; have the same expression in it; where they match a sequence of 0 expressions
; or 2 or more expressions, the result will contain (:seq ...), where '...'
; lists the matched expressions.
; 
; Note: a predicate variable like ?foo, +foo or *bar won't produce a (:seq ...)
;    construct if it matches exactly one expression -- just that expression
;    is put in the result (as if the variable had been !foo). If it matches
;    the empty list, we get (:seq), and if it matches multiple elements we
;    get (:seq el1 el2 ...). 
;
; The match produced, if any, is the leftmost one; nil = failure.
;
; e.g., (match '(!expr *list (f g) ?atom !atom) 
;              '((a) (b c) (d e) (f g) h))
;       --> ((A) (:SEQ (B C) (D E)) (F G) (:SEQ) H)
;                                  (:seq) is the place-holder for the empty
;                                  sequence; w/o it, we couldn't retrieve
;                                  matched pa elements by their positions
; e.g., (match '(+expr !atom) 
;              '((a b) c d))
;       --> ((:SEQ (A B) C) D)
; e.g., (match '(+expr !atom) 
;              '((a b) c))
;      --> ((:SEQ (A B)) C)  note the 1-element sequence
; e.g., (match '(*expr +atom) '(a b c)) 
;      --> ((:SEQ) (:SEQ A B C)) 
; e.g., (match '(!atom (a b *list c) ?atom) '(x (a b (u) (v w) c) d))
;      --> (X (A B (:SEQ (U) (V W)) C) (:SEQ D))
;
; e.g., (match '(?atom +expr (*list (d (*atom (?atom)))) ?expr) 
;              '(      (a) b ( (c)  (d ( e f  (  g  )))) ((h))))
;      --> ((:SEQ) (:SEQ (A) B) ((:SEQ (C)) (D ((:SEQ E F) ((:SEQ G))))) 
;                                                             (:SEQ ((H))))
;
; e.g. (dot-atoms), (match '(.ADVP +expr) '(PP (IN on) (NNP Monday)))
;      --> (PP (:SEQ (IN ON) (NNP MONDAY)))
;
; e.g. (new as of Oct 2020, allowing arbitrary match functions), 
;      where by convention we use square bracketing (like ![...]) for
;      predicates matching non-atomic expressions only; and we'll use 
;      a trailing '~' for functions on ULF expressions (as opposed to
;      syntactic parse expressions). (The ULF functions assumed in the
;      example below are speculative; it's unclear how much postprocessing
;      we'll do, but such methods could also be used for conversion to
;      EL, or for inference):
;
;      (match '(.XP ![verb] *[advp])
;             '(VP (VB TALK) (PP (TO TO) (:XP 2)) (ADVP (RB BRIEFLY))))
;      --> (VP (VB TALK) (:SEQ (PP (TO TO) (:XP 2)) (ADVP (RB BRIEFLY))))
;
;      (match '(!aux~ !term~ *non-v~ (.trans-v~ *non-term~))
;             '((pres will.aux-s~2) he.pron~3 (do.v~4 afterwards.adv-e~5)))
;      ---> ((PRES WILL.AUX-S~2) (:SEQ HE.PRON~3) (DO.V~4 
;                                                  (:SEQ AFTERWARDS.ADV-E~5)))
;           {We might use such a match to insert *h after 'DO.V~4'. But
;           I decided to do (-none- *h) insertions in the syntactic parse}
;
; NOTE: match predicates like ![advp] apply to phrasal expressions, and if
;       we want at most one occurrence, or at least one, or any number,
;       we would use ?[advp], +[advp], *[advp] respectively -- but all are
;       evaluated using a presumed Lisp function ![advp].
;
; Special cases for atomic ex (matching ?/*/+ var's gives a sequence):
; e.g., (match 'a 'a) --> A
; e.g., (match '!atom 'a) --> a
; e.g., (match '?atom ()) --> (:SEQ nil); single-element sequence
; e.g., (match '!expr ()) --> NIL (success wth empty list)
; e.g., (match '!atom 3.14) --> 3.14
; e.g., (match '?atom 'a) --> (:SEQ A); single-element sequence 
; e.g., (match '*atom 'a) --> (:SEQ A); single-element sequence
; e.g., (match '+atom 'a) --> (:SEQ A); single-element sequence
;
; Basic match variables are {!atom, !list, !expr}, where '!' means 
; exactly 1 occurrence, and may be replaced by '?' (0 or 1 occurrences),
; '*' (0 or more occurrences), or '+' (1 or more occurrences). The three
; !-variables respectively match a Lisp atom, list, or any expression.
;
; In addition, user-defined match variables (see above) can be used in
; the same ways (!, ?, *, +).
;
 (let (p e m mm *v)
      (cond ((null pa) 
             (if (null ex) nil ; success with match-result nil 
                 :nil)); failure
            ((null ex)
             ; a null expression ex can only be matched by non-null pa if 
             ; either pa is a match variable that succeeds on (), thus
             ; yielding nil for a !-variable and (:seq nil) for sequence
             ; variables, or it is a list of "soft" variables (of type
             ; ?<pred> or *<pred>) that will match empty sequences, 
             ; with result ((:seq) (:seq) ...).
             (if (and (match-var pa) (val pa ex)); NB: PRED EVALUATION:
                                     ;`````````    for preds, nil = failure
                 (if (eq pa (gethash pa *implicit-pred*)) nil '(:seq nil))
                     ;````````````````````````````````` true if pa is a !-var
                     ; match success, either nil (for !-var), or (:seq nil) o/w
                 (if (listp pa)
                     (if (member-if #'listp pa) :nil ; fail if there's a sublist
                                                     ; NB: can't use 'find-if'
                                                     ; because that gives nil
                                                     ; for (nil)!
                         (if (member-if #'hard-atom pa) :nil ; fail if there's a
                                                    ; hard variable or non-var
                             ; pa is a list of soft variables:
                             (mapcar #'(lambda (x) '(:seq)) pa)))
                     ; pa is an atom but not a match-var & not nil
                     :nil))) ; failure

            ((equal pa ex) ex); Note: both are non-null here
            ((and (dot-atom pa) (symbolp ex) (isa ex pa)) ex)

            ; pa, ex are non-null and unequal (& pa is not a feature of ex):
            ((atom pa)
             (if (and (match-var pa) (val pa ex))
                                     ;`````````NB: PRED EVALUATION, e.g., T or nil
                 (if (seq-var pa) (list :seq ex) ex)
                 :nil)); pa neither equals, nor is a feature of, nor matches ex

            ; pa is a non-null list, and pa, ex are unequal
            ((atom ex) :nil); failure

            ; pa and ex are unequal lists. We now have 6 cases depending
            ; on p, the car of pa; we use e for the car of ex;

            ; set p and e -- and conjoin with nil, to make the condition false
            ; and hence to force continuing to the next cond packet:
            ((and (setq p (car pa) e (car ex)) nil) nil)

            ; 1. If p is a list, we try matching it to the initial element
            ; of ex, and if successful, recurse on the tails of pa and ex.
            ((listp p)
             (setq m (match p e)); if successful, returns e or a variant with
                                 ; subspans matching seq-var's in form (:seq ...)
             (if (ok m) (prog2 (setq mm (match (cdr pa) (cdr ex)))
                               (if (ok mm) (cons m mm); success 
                                   :nil)) ; failure
                 :nil)) ; failure

            ; Initial element p is a non-nil atom;
            ; ```````````````````````````````````
            ; 2. For non-variable p, if it is equal to, or a feature of the
            ; 1st element of ex, we recurse on the tails of pa and ex, o/w fail
            ((not (match-var p))
             (if (or (eq p e) (and (symbolp e) (dot-atom p) (isa e p)))
                 (prog2 (setq mm (match (cdr pa) (cdr ex)))
                        (if (ok mm) (cons e mm) :nil))
                 :nil))

            ; 3. For a !-variable p, we try an initial-element match, and if
            ; successful, recurse on the tails of pa and ex;
            ((!-var p)
             (setq m (match p e))
             (if (ok m) (prog2 (setq mm (match (cdr pa) (cdr ex)))
                        (if (ok mm) (cons m mm) :nil))
                 :nil))

            ; 4. For a ?-variable, we try prepending (:seq) to a recursive 
            ; match of the tail of pa to ex; if the recursion fails,
            ; we try an initial-element match, and if successful, recurse
            ; on the tails of pa & ex;
            ((?-var p)
             (prog2 (setq mm (match (cdr pa) ex))
                    (if (ok mm)
                        (cons '(:seq) mm)
                        ; recursion with empty match to p failed
                        (prog2 (setq m (match p e)); single-element :seq
                               (if (ok m)
                                   (prog2 (setq mm (match (cdr pa) (cdr ex)))
                                          (if (ok mm) (cons m mm) :nil))
                                   :nil)))))

            ; 5. For a *-variable, we try prepending (:seq) to a recursive 
            ; match of the tail of pa to ex; if the recursion fails,
            ; we try an initial-element match, and if successful, recurse
            ; on pa (unchanged) and the tail of ex;
            ((*-var p)
             (prog2 (setq mm (match (cdr pa) ex)); try leaving p empty
                    (if (ok mm )
                        (cons '(:seq) mm); success with p left empty;
                        ;
                        ; recursion with p left empty failed; so match p to e,
                        ; and if successful, recurse with pa (since the *-var
                        ; may add to its match sequence) and the tail of ex:
                        (prog2 (setq m (match p e))
                               (if (ok m); success with single-element :seq
                                   ; 2nd recursion attempt:
                                   (prog2 (setq mm (match pa (cdr ex)))
                                          (if (ok mm); mm is ((:seq ...) ...)
                                          ; combine m with the initial :seq 
                                          ; of mm and add the rest of mm:
                                              (cons (append m (cdar mm))
                                                    (cdr mm))
                                          ; 2nd recursion failed
                                          :nil))
                                   :nil)) ))); initial-element match failed

            ; 6. For a +-variable, we try an initial-element match, and if
            ; successful, recurse on pa and (cdr ex) with the initial 
            ; +-variable of pa replaced by the corresponding *-variable
            ((+-var p)
             (setq m (match p e))
             (if (ok m); single-element :seq
                 (progn
                   (setq *v (*-variant-of-+-var p))
                   (setq mm (match (cons *v (cdr pa)) (cdr ex)))
                   (if (ok mm); initial element of mm is a :seq
                       ; combine m with the initial element of mm,
                       ; and add the rest of mm 
                       (cons (append m (cdar mm)) (cdr mm))
                       ; recursion failed
                       :nil))
                 :nil) ) ; initial-element match failed
            (t (format t "~%*** (match ~a ~a) ~%   ~
                           gave neither success nor failure!" pa ex)
               '***ERROR))
 )); end of match


(defun ok (x) (not (eq x :nil)))
; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


(defun *-variant-of-+-var (+var)
;``````````````````````````````
; Return the *-variant (allowing 0 or more matches) of the given +-var,
; a (predicative) variable requiring 1 or more matches.
;
; First deal with the three basis predicates
 (case +var (+atom '*atom) (+list '*list) (+expr '*expr)
   ; +var must be a a +-variant of a !-predicate defined by 'defpred'); 
   ; so find the !-predicate, and use this in turn to find the *-variant:
   (t (gethash (gethash +var *implicit-pred*) *-variant*))
 )); end of *-variant-of-+-var


(defun val (var expr)
;```````````````````
; var:  a sequence variable formed from a predicate by prefixing one of {!,?,*,+};
;       NB: {*h !,?,*,+} ARE NOT ALLOWED AS MATCH VARIABLES.
; expr: an expression to which the predicate is to be applied; the actual Lisp
;       predicate is expected to start with '!'.
; E.g., find predicate name '!term' under any of keys '?term', '!term', '*term',
;       '+term' in hash table '*implicit-pred*'; apply this predicate, !term,
;       to the given expression, expr. (In defining pattern predicates, we store
;       corresponding variables in the hash table.)
  (let ((fname (gethash var *implicit-pred*)))
       (when (null fname)
          (format t "~%**ERROR: ~s HAS NOT BEEN DEFINED AS A PREDICATE" VAR)
          (return-from val nil))
       (eval `(,fname ,(list 'quote expr))) ))


(defun dot-atom (atm); dot-atom variables can only match atoms
    (and (symbolp atm) (not (eq atm '\.)) (char= #\. (schar (string atm) 0))))
            
(defun ?-var (var) 
    (and (symbolp var) (not (eq var '?)) (char= #\? (schar (string var) 0))))

(defun !-var (var) 
    (and (symbolp var) (not (eq var '!)) (char= #\! (schar (string var) 0))))

(defun *-var (var) 
  ; NB: *h (the place-holder for gaps) is not allowed as a match variable
    (and (symbolp var) (not (find var '(* *h))) 
                       (char= #\* (schar (string var) 0))))

(defun +-var (var) 
   (and (symbolp var) (not (eq var '+)) (char= #\+ (schar (string var) 0))))

(defun match-var (var) 
; NB: *h (the place-holder for gaps) is not allowed as a match-variable!
   (and (symbolp var) (not (find var '(! ? * *h +)))
        (find (schar (string var) 0) '(#\! #\? #\* #\+))))

(defun seq-var (var)
   (and (match-var var) (not (eq var (gethash var *implicit-pred*)))))

(defun soft-var (var)
   (and (symbolp var) (not (find var '(? *)))
        (find (schar (string var) 0) '(#\? #\*))))

(defun hard-atom (atm) (and (atom atm) (not (soft-var atm))))


(defun find-patt-inst (patt expr); Oct 30/20
;```````````````````````````````
; Return the first expression (if any) in 'expr' matched by 'patt',
; in a top-down, left-to-right search of 'expr'
; 
 (prog (result results)
       (setq result (match patt expr))
       (if (ok result) (return expr))
       (if (atom expr) (return nil))
       (setq results (mapcar #'(lambda (x) (find-patt-inst patt x)) expr))
       (setq result (find-if #'(lambda (x) x) results)); first non-null result
       (return result)
 )); end of find-patt-inst


(defun apply-rule (rule expr); May 4/20; briefly tested
;``````````````````````````````````````
; Apply the given tree-transformation rule to expr (at the top level)
;
; [IMPORTANT: ALL NUMBERS APPEARING IN THE TEMPLATE PART (I.E., SECOND 
;   PART) OF 'RULE' ARE INTERPRETED AS REFERENCES TO ELEMENTS OF THE
;   RESULT OF MATCHING THE PATTERN PART (I.E., FIRST PART) OF 'RULE' TO
;   'EXPR'. SO, IF WE WANT TO SPECIFY EXPLICIT *NUMBERS* (NOT INTERPRETED
;   AS REFERENCES TO ELEMENTS OF THE MATCH RESULT, WE NEED TO USE SOME
;   NON-NUMERIC REPLACEMENTS FOR THEM AT LEAST TEMPORARILY. NIL APPEARING
;   IN A TEMPLATE IS ALSO AN ISSUE, ... ?]
;
;  **THE FOLLOWING COMMENTS NOW SEEM FAULTY -- IT'S TEMPLATES, NOT EXPR, THAT
;    SHOULDN'T HAVE NUMBERS; & I'M NOT SURE WHAT THE PROBLEM WITH NIL IS ...
; rule: a (<pattern> <template>) pair, where patterns are expressions where
;   some atoms may be !-/?-/*-/+-variables, of three types, viz. ones
;   that match atoms (OTHER THAN () AND NUMBERS -- WHICH NEED TO BE REPLACED
;   BY NON-NUMERIC ATOMS, E.G., |3.2| INSTEAD OF 3.2), ones that match lists, 
;   and ones that match any expressions; the template is another expression,
;   often echoing parts of the pattern but in general containing position
;   indicators like 3, 3.2, '3.3.2, where pieces of the match-result are to
;   be placed; here, 3rd element of match result, 2nd element of the third
;   element of the match result, etc.
; expr: an expression containing no numbers -- any that are present must be
;   (temporarily) replaced by atoms, e.g., 3.2 becomes |3.2|; also "NIL" as
;   a word should be replaced by |NIL|, lest it be regarded as a list rather
;   than atom in matching to list-var's.
;
 (let (result)
    ; (if (equal (second rule) '(1 2 (ADJP1 3 4) 5)); DEBUGGING
    ;     (format t "~%## *comb-adv-adjp* called with argument~%   ~s" expr))
      (setq result (match (car rule) expr))
      (if (eq result :nil) expr ; for failure, return exp unchanged
          ; o/w, fill template (w/ sequences flattened), and return the result:
          (fill-template (second rule) result))
 )); end of apply-rule


; MAY NOT BE NEEDED
(defun symbolize-numbers-and-nil (expr)
;``````````````````````````````````````
; Replace any occurrences of numbers, or of NIL in expr by those elements
; placed in @@...@@; e.g.,
;       (A (B 2) NIL 3.7 (D)) becomes (A (B @@2@@) @@NIL@@ @@3.7@@ (D)) 
;
 (let (sym)
      (cond ((or (numberp expr) (null expr))
             (setq sym (intern (format nil "@@~a@@" expr)))
             (setf (get sym 'unsymbolized-value); store number or t on Plist
                   (if (numberp expr) expr t)); t here means "derived from nil"
             sym)
            ((atom expr) expr)
            (t (mapcar #'symbolize-numbers-and-nil expr)))
 )); end of symbolize-numbers-and-nil

; MAY NOT BE NEEDED
(defun unsymbolize-numbers-and-nil (expr)
;`````````````````````````````````````````
; Replace any occurrences of |x|, where x is a number or nil, back to a number
; or nil respectively; e.g.,
;      (A (B @@2@@) @@NIL@@ @@3.7@@ (D)) becomes (A (B 2) NIL 3.7 (D)) 
;
 (let (x)
      (cond ((listp expr) (mapcar #'unsymbolize-numbers-and-nil expr))
            ((not (symbolp expr)) expr); e.g., quoted string or number
            (t (setq x (get expr 'unsymbolized-value))
               (if (equal x t) nil; x was derived from nil
                   (if x x expr)))); other non-null x is always numeric
 )); end of unsymbolize-numbers-and-nil


(defun apply-rule-till-stable (rule expr)
;````````````````````````````````````````
; Apply 'rule' to 'expr', if necessary repeatedly until the result is stable
;
 (let ((ex (apply-rule rule expr)))
      (if (equal ex expr) expr (apply-rule-till-stable rule ex))
 )); end of apply-rule-till-stable


(defun apply-rule-bottom-up (rule expr); May 7/20; briefly tested
;````````````````````````````````````````
; Apply 'rule' recursively to the parts of 'expr', assembling results from
; the atomic level upward; then apply the rule at the top level, repeating
; this until there is no change.
; 
 (let (ex)
      (if (atom expr) (apply-rule rule expr)
          (prog2 
            (setq ex (mapcar #'(lambda (x) (apply-rule-bottom-up rule x)) expr))
            (apply-rule-till-stable rule ex)))
 )); end of apply-rule-bottom-up


(defun apply-rule-top-down (rule expr)
;````````````````````````````````````
; Apply 'rule' to the top level of 'expr', repeating this until the top level
; no longer changes; then apply 'rule' recursively to the parts via mapcar
;
 (let ((ex (apply-rule-till-stable rule expr)))
      (if (atom ex) ex
          (mapcar #'(lambda (x) (apply-rule-top-down rule x)) ex))
 )); end of apply-rule-top-down

                           
(defun fill-template (template match-result); May 7/20
;````````````````````````````````````````````
; template: an expression, in general containing position indicators 
;   like 3, 3.2, '3.3.2, where pieces of the match-result are to be placed
;   e.g., here, the references are to the 3rd element of match-result, 
;   the 2nd element of the third element of the match-result, etc. The 
;   index 0 is special -- it refers to match-result as a whole, with
;   sequences (:seq ...) flattened, i.e., the expression that was matched.
; match-result: an expression in general containing sequence expressions
;   such as (:seq (a b) c (d (f g))) (a 3-element sequence); (:seq) is
;   the empty sequence.
;
; result: the template is filled in with "pieces" from match-result, as
;   indicated by the position indices it contains. Where a position index
;   indexes a sequence, i.e., (:seq ...), the elements of the sequence
;   (if any) are inserted in the result, without the (:seq ...) wrapper.
;
 (let (val ii)
      (cond ((eq template 0); unlikely case - refers to entire expression
                            ; matched, which we can reconstruct by
                            ; flattening any sequences in match-result;
                            ; a faster way would be to include the original
                            ; expression as argument of 'fill-template',
                            ; but that's inelegant special-case handling
             (flatten-sequences match-result))
            ((setq ii (position-index template))
             (flatten-sequences (indexed-element-of match-result ii)))
            ((atom template) template)
            ((listp (car template))
             (cons (fill-template (car template) match-result)
                   (fill-template (cdr template) match-result)))
            ((setq ii (position-index (car template)))
             (setq val (indexed-element-of match-result ii))
             (if (and (listp val) (eq (car val) :seq))
                 (append (flatten-sequences (cdr val))
                         (fill-template (cdr template) match-result))
                 (cons (flatten-sequences val)
                       (fill-template (cdr template) match-result))))
            ((spec-function (car template))
             (eval (cons (car template) 
                         (fill-template (cdr template) match-result))))
            (t ; (car template) is an atom but not a position index or function
              (cons (car template) (fill-template (cdr template) match-result))))
 )); end of fill-template
                

(defun flatten-sequences (expr); May 4/20; briefly tested;
;``````````````````````````````  slightly revised Apr 26/21
; E.g., (x (y z) (:seq (a b) c (d (f g))) u (:seq v (w w))) 
;       becomes
;       (x (y z) (a b) c (d (f g)) u v (w w)) ;
; i.e., lists enclosed by (:seq ... ) are replaced by the sequences they 
; contain. (:seq ...) expressions are not allowed to contain such 
; expressions as sequence elements. (These would not be flattened. It
; would be easy to allow them, but for the pattern transductions here no
; such embeddings occur, because sequence expressions are match-values of
; single sequence variables, like *atom or +expr, and these are matched
; only against input expressions free of sequence expressions.)
;
; A top-level expr like (:seq a) or (:seq nil) or (:seq (a b)) becomes
; A, NIL, or (A B) respectively, i.e., a 1-element sequence is turned
; into that one element. However, an empty sequence (:seq) or longer
; sequence like (:seq a b) is left unchanged, since removing (:seq ...)
; doesn't leave a single valid lisp expression.
;
  (cond ((atom expr) expr)
        ((eq (car expr) :seq) 
         (if (= (length expr) 2) (second expr)
             (prog2 (format t "~%## WARNING: 'flatten-sequences' cannot ~
                               flatten top-level sequence~%##          ~s ~
                               ~%##          as this would give a non-Lisp ~
                               expression" expr)
             expr))); return unexpected expr unchanged
        ((atom (car expr)) 
         (cons (car expr) (flatten-sequences (cdr expr))))
        ((eq (caar expr) :seq)
         (append (cdar expr) (flatten-sequences (cdr expr))))
        (t
         (cons (flatten-sequences (car expr)) (flatten-sequences (cdr expr))))
 )); end of flatten-sequences

      
(defun position-index (i); June 2/21; tested
;````````````````````````
; Determine whether i is a position index, and if so, if i is simply
; an integer, return it, and if it contains dots and conforms with the
; syntax of position indices, return the list of integers it encodes.
; Otherwise, return NIL.
;
; Syntax: Position indices in tree transductions are here
; 0, 1, 2, 3, ..., 
;   (equivalently, 0., 1., 2., 3., ..., but NOT 0.0, 1.0, 2.0, 3.0, ...)
; or 1.1, 1.2, 1.3, ..., 2.1, 2.2, 2.3, ..., etc.,
;   (equivalently 1.1., 1.2., 1.3., ..., 2.1., 2.2., 2.3., ...)
; or 1.1.1, 1.1.2, ..., 1.2.1, 1.2.2, ..., 2.1.1, 2.1.2, ... etc.
;   (equivalently 1.1.1., 1.1.2., ..., 1.2.1., 1.2.1., 1.2.2., ... )
; etc. (for as many integers chained together with dots as we like,
; not in general limited to single digits). To allow for SBCL, we
; also allow outside pipes, e.g., |2.3.1|.
;
; BUT: NO ISOLATED OR TRAILING 0 DIGITS, except for a standalone 0.
; Something like '13.20.4' is definitely permitted, though position
; indices as large as this -- i.e., linguistic or logical expressions --
; with that many elements -- are unlikely. Not usable: '13.20', because
; this will give result (13 2); but we can use '13.20.' in such a case.
; Note that the issue of "illegal" trailing 0's arises only for indices
; containing one dot, because as soon as we have two dots, Lisp treats
; this as a symbolic atom.
;
 (let (ii n iii prev-dot (ndots 0))
   (cond ((integerp i) i)
         ((null i) nil)
         ((not (atom i)) nil)
         ; The following allows for indices that look like decimal numbers
         ; (i.e., with one dot), but neither starting with nor ending in 0.
         ; Other than that, we can have two or more dots separating integers,
         ; and possibly a final dot (e.g., '13.20.', or '13.7.20.' -- this  
         ; latter case is the same as '13.7.20', i.e., without the final dot))
         (t (setq ii (format nil "~s" i)); i as character string
            ; quickly eliminate instances with alphabetic characters:
            (if (find-if #'alpha-char-p ii) (return-from position-index nil))
            (setq n (length ii)); number of characters
            (if (< n 2) (return-from position-index nil))
            ; remove outside pipes, if any:
            (when (and (char= (char ii 0) #\|) (char= (char ii (- n 1)) #\|) )
                  (setq ii (remove #\| ii))
                  (setq n (length ii))
                  (if (< n 2) (return-from position-index nil)))
            ; now the string should start with a non-zero integer
            (if (or (not (digit-char-p (char ii 0))) (char= (char ii 0) #\0)) 
                (return-from position-index nil)) 
            (setq iii (coerce ii 'list)); a char list starting with a non-0 digit
            ; check syntax, returning NIL if invalid
            (dolist (ch iii)
               (cond ((char= ch #\.); sequential dots not allowed
                      (incf ndots)
                      (if prev-dot (return-from position-index nil)
                          (setq prev-dot t)))
                     ((not (digit-char-p ch))
                      (return-from position-index nil))
                     ((and prev-dot (char= ch #\0))
                      (return-from position-index nil))
                     (t (setq prev-dot nil))))
            ; input passed the syntax check, except for final-0 check:
            (if (and (= ndots 1) (char= (char ii (- n 1)) #\0))
                (return-from position-index nil))
            ; it's definitely a position index; make it into an integer list
            (setq ii (substitute #\Space #\. ii))
            (read-from-string (concatenate 'string "(" ii ")"))))
 )); end of position-index


(defun spec-function (atm) ; May 30/20; changed to look for *final* '!' Oct/16/20
;`````````````````````````
; Does atm end with '!', exclusive of (stand-alone) '!' ? 
; If so, it is treated as a user-defined function in tt output templates.
;
 (let (str n)
      (cond ((symbolp atm) 
             (setq str (string atm) n (1- (length str))); n= index of last char
             (if (and (not (zerop n)) (char= (schar str n) #\!)) t nil))
            (t nil)) ))

; The NAME-REWRITING FUNCTIONS are intended to serve two disparate purposes:
; - if the name constituents (involving NNP, NNPS) are produced by an off-
;   the-shelf syntactic parser, the result will be in a form without 
;   blank-prefixes or lower-case characters, and this will cause attachment
;   of '.name' by the parse-tree-to-ULF routines;
; - if the name constituents are from the Brown corpus, postprocessed to
;   preserve the case structure of names (i.e., using pipes), the result
;   will be a blank-prefixed name in pipes, and no dot-extension will be
;   added by the parse-tree-to-ULF routines.
; Therefore, to get names with blank prefixes and with u.c./l.c. preserved,
; one would need routines that correlate tokens from an input tokenizer
; with the u.c. words appearing in the parse tree and replacing these with
; their pipe-protected case structure, before handing the tree to the parse-
; tree-to-ULF routines.


;; NOTE: MAYBE THE CONVERSION FUNCTIONS BELOW (USED IN RULE OUTPUT TEMPLATES)
;;       SHOULD BE PUT IN A SEPARATE FILE, FOR ORGANIZATIONAL UNIFORMITY
       
(defun format-name-for-ulf! (name) ; Oct 17/20
;````````````````````````````````
; name e.g.'s: UPTON; | Upton| (already formatted); |Upton| (lacking blank);
;               | USA|; e.g., USA
; result e.g.'s: UPTON, | Upton|, | Upton| respectively for examples above; 
;              | USA|, USA for the further examples above
; This is aimed at meeting both a requirement for correct name representations
; when interpreting Brown trees where names have been put in pipes, and 
; meeting L.L.'s preference (leading to .name attachment when parsing directly
; from English.
 (let ((str (string name)))
      (cond ((char= #\Space (schar str 0)) name) ; initial blank
            ((not (string= (string-upcase str) str)) ; some l.c. char's
             (intern (concatenate 'string " " str))); add initial blank
            ; all upper case, no initial blank:
            (t name))
 )); end of format-name-for-ulf!


(defun merge-names! (names); Oct 17/20
;`````````````````````````
; names: e.g., (| New| | York| | State|); (|New| |York| |State|), (NEW YORK STATE)
; result: e.g., | New York State| (first 2 cases), |NEW YORK STATE| (3rd case)
;     I.e., if there is an initial blank or some lower case characters, 
;     we ensure there is an initial blank, and l.c. char's are retained;
;     o/w, we just concatenate. The former is aimed at Brown parses with names
;     in pipes, while the latter will lead to .name attachment in the ULF.
;
 (let* ((strings (mapcar #'string names))
        (strings- (mapcar #'(lambda (s) (string-left-trim " " s)) strings))
        strings+)
       (if (equal strings (mapcar #'string-upcase strings-)); no names in pipes?
           (setq strings+
                 (cons (car strings-)
                       (mapcar #'make-blank-prefixed-name-string! (cdr strings-))))
           (setq strings+ (mapcar #'make-blank-prefixed-name-string! strings-)))
       (intern (apply #'concatenate 'string strings+))

 )); end of merge-names!


(defun make-blank-prefixed-name-string! (str); Oct 17/20
; ``````````````````````````````````````````
; name: e.g., UPTON, | UPTON|, |Upton|, |upton|
; result: e.g., " UPTON", "| UPTON|", "| Upton|", "| upton|" respectively
  (if (char= #\Space (schar str 0))
      str (concatenate 'string " " str) ))


(defun make-multiword! (words); Apr 6/01
;~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; This is a slight variant of 'make-multiword' in the "stem" file (which
; allows specification of any string (not just "_") for connecting the words).
;
; Return a symbol consisting of the given words (symbols) connected by '_'.
 (cond ((atom words) words)
       ((null (cdr words)) (car words))
       (T (let (strings)
               (push (string (car words)) strings)
               (dolist (w (cdr words))
                       (push "_" strings)
                       (push (string w) strings) )
               (intern (apply #'concatenate
                              (cons 'string (reverse strings)) ))))
 )); end of make-multiword!


(defun copular-verb! (pos+word); May 1/21
; `````````````````````````````
; This corrects copular "be" misparsed as AUX to a main verb;
; E.g., (AUX is) ==> (VBZ is)
  (let ((word (second pos+word)))
       (case word ((is are) `(VBZ ,word)) (be `(VB ,word)) (been `(VBEN ,word)) 
                  ((was were) `(VBD ,word)) (being `(VBG ,word)) (t pos+word))
                  ; ** actually "were" (& even "was") can be subjenctive, but
                  ; this needs ULF postprocessing.
 )); end of copula!


(defun inflect-aux! (aux+lex); May 28/21
;````````````````````````````
; Change BLLIP auxiliaries, of form (AUX word), (MD word), or (TO to), 
; and Brown corpus auxiliaries, of form (AUX (POS word)), into a uniform
; (AUX... word) format, where AUX... is a suitable AUX-inflection, 
; except that (TO to) retains that form. For Brown, 'POS' is one of
;      {TO MD VB VBZ VBP VBD VBN VBEN VBG VB-CF},
; assuming the VB-CF (counterfactual) may be introduced by prior rules. 
; The 'word' is usually a form of 'be', 'have', or 'do', 'to', or a modal 
; auxiliary (rarely, 'dare', 'need', 'better', 'had better', 'had best', if
; these are recognized by the parser or prior rules). Example transductions:
;
;  (AUX has) -> (AUXZ has), (AUX are) -> (AUXP are), (MD will) -> (AUXZ will),
;  (AUX (VBZ has)) -> (AUXZ has), (AUX (MD will)) -> (AUXZ will), etc.,
;  (but (AUX (TO to)) -> (TO to)).
;
  (let ((lex (second aux+lex)) aux pos)
       ; First convert Brown modals of form (AUX (MD word)) to (MD word),
       ; since in this case (as for BLLIP parses), there's no tense implied.
       (if (and (listp lex) (eq (car lex) 'MD)); 
           (setq lex (second lex))); pick out the word
       (cond ((atom lex); so this is in BLLIP format (or from a Brown modal)
              (setq aux
                (case lex (to 'TO) (has 'AUXZ) ((had \'d) 'AUXD) (having 'AUXG)
                      ; no change to (AUX have) -- but context-dependent
                      ; rules may change this to (AUXP have), e.g., in 
                      ; "You have ..." or "They have ...". Concerning 'd,
                      ; this could be 'had' or 'would', and the question
                      ; is whether the BLLIP parser ususally gets it right...
                      (does 'AUXZ) (did 'AUXD); no change to (AUX do), but
                      ; in context this may need changing to (AUXZ do).
                      ((is am \'s \'m) 'AUXZ ) ((are \'re) 'AUXP) 
                      ((was were) 'AUXD) (being 'AUXG); no change for (AUX be)
                      ; (AUXD were) could become (AUX-CF were): "If I were ..."
                      ((will \'ll shall may can must better need dare) 'AUXZ) 
                      ((might should ought could would had_better had_best)
                       'AUXF) (dared 'AUXD) ; AUXF means finite (pres or past;
                        ; or cf for "could", "would", "were", {coll. "was"});
                        ; "had" can be a past aux. CF ("If I had not left, ...")
                        ; or past or pres main verb ("If I had a hammer, ...")
                      (t (car aux+lex))))
              (list aux lex))
             (t; 'lex' is a list, so aux+lex is in Brown format (& not an MD)
                (setq pos (car lex) lex (second lex))
                (setq aux 
                   (case pos (TO 'TO) (VB 'AUX) (VBZ 'AUXZ) (VBP 'AUXP) (VBD 'AUXD)
                             (VBN 'AUXN) (VBEN 'AUXEN) (VBG 'AUXG) (VB-CF 'AUX-CF)
                             ; AUX-CF can apply to {had, were, {was}, would, could}
                             (t 'AUX)))
                (list aux lex)))
 )); end of inflect-aux!


(defun convert-aux-to-v! (aux+word); May 26/21
;``````````````````````````````````
; E.g., Change (AUX did) to (VBD did), (AUX be) to (VB be), (AUX has) to (VBZ has)
; This is needed in correcting some BLLIP errors, in examples like "I know what
; you did" (with final parse element (VP (AUX did))), where the final verb can
; be assumed to be finite, even in cases like "The problem I have ..."
  (cond ((atom aux+word) aux+word)
        (t (let ((word (second aux+word)) pos)
                (cond ((or (null word) (not (atom word))) aux+word)
                      (t (setq pos
                          (case word
                            ((did was had) 'VBD)
                            ((does do are am is has have) 'VBZ)
                            ((done been) 'VBEN)))
                         (list pos word)))))
 )); end of convert-aux-to-v!

; ** May need something similar for modals like (MD will), (MD would), ...
