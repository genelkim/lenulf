;; THIS FILE ISN'T USED AT PRESENT (SEP 2021). I'VE KEPT IT FOR THE TWO
;; "SEMANTICALLY" SIMILAR GROUPS OF NOUNS IT IDENTIFIES.
;;
;; THE DOT-FEATURES BASED ON THE LISTS HEREIN (VIZ., .N-PROPOS-OBJ,
;; .N-COMMUN-OBJ, .N-COG-OBJ), THOUGH APPEARING IN "isa.lisp", AREN'T
;; ACTUALLY USED ANYWHERE. THE LISTS ULTIMATELY USED FOR GAP INSERTION
;; ARE IN "transitivity-lists.lisp".
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Originally this was started for listing word grroups with syntactic and 
; semantic features seemingly important for many parse disambiguations
; (this is relevant to gap insertion, but I ended up using lists of lexical
; features for that, in "transitivity-lists.lisp")
;
; E.g., consider "the car that he bought", vs. "the fact that he smokes",
;       "the pipe that he smokes".
;       Here both the head noun semantics (phy-sobj vs cog-obj, say) and the
;       verb subcat (trans vs. intrans) make a big difference to the rel-clause
;       vs appositive clause distinction.
; E.g., consider "What do you plan to do _?" vs "What do you do _ to stay fit?"
;       Here again subcat -- transitivity -- is crucial. In the first sentence
;       "plan" already has a complement, whereas "do" doesn't, so the trace is
;       embedded at VP-level 2, while in the second sentence the level-1 verb
;       calls for a complement whereas the level-2 verb does not, so the trace
;       is at level 1.
;
; So the idea was to create hash-table lexicons where we can check for features.
;
; The question I pondered was whether to use words or features as keys. 
; I decided to use features as keys, because then I could use "tests"
; like .V_np, .N-phys-obj in tt patterns, and these would just be lookups
; in corresponding hash tables V_np, N-phys-obj, etc., with non-nil result
; providing confirmation of the feature. Also the lists of words with
; particular features would then be easy to examine, and to fill in missing 
; items.
;
; At the time I hadn't yet generalized 'isa' to allow for feature hierarchies,
; and wondered if the contemplated approach would work for hierarchies?
; I decided that I could make do with combinatory feature tests, e.g., 
; .N-cog-obj could actually be a disjunctive test for .N-propos-obj 
; (e.g., "belief") or .N-commun-obj (e.g., "assertion").
;
; (In the following I used the comparison predicate below on a raw word list,
; collected from the Brown corpus for key "(SBAR (IN that)", noting ones
; that were NP appositives rather than rel-clauses (also throwing in nominals
; for some verb objects, e.g., "stated that ..." => "statement"); I stopped
; after checking all instances in a01.cmb, a02.cmb, ... , g33.cmb, at which 
; point I was getting very few new examples:
; #'(lambda (x y) (string-lessp (string x) (string y)))

(in-package :lenulf)

(defparameter *n-propos-obj*
; Nouns that indicate an attitude towards, or truth-status of, their
; propositional complement
;
'(AWARENESS BELIEF CHANCE CONCERN CONCLUSION CONVICTION DELUSION
 DESIRE DISCOVERY DOUBT EVIDENCE FACT FEAR FEELING FINDING HOPE IDEA 
 ILLUSION IMPLICATION IMPRESSION INFORMATION INTENTION KNOWLEDGE NEED 
 NOTION OPINION POINT POLICY POSSIBILITY PRESUMPTION PRESUPPOSITION 
 PROOF REALIZATION REASON RECOGNITION REQUIREMENT RESULT SENSE SIGN 
 SUSPICION THOUGHT UNDERSTANDING UNEASINESS))

(defparameter *n-commun-obj* 
; Nouns that indicate communication of their propositional complement
;
'(ACKNOWLEDGEMENT ADMISSION ADVICE ANNOUNCEMENT ASSERTION ASSURANCE
  CHARGE CLAIM COMMENT CONFESSION CONTENTION DECLARATION DEMAND 
  DEMONSTRATION DENIAL DICTUM ILLUSTRATION INDICATION INSISTENCE LESSON
  OBSERVATION PROMISE PROPHESY PROPOSAL RECOMMENDATION REMARK REMINDER
  REPORT STATEMENT STIPULATION SUGGESTION))

