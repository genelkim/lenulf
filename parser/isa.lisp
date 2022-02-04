;; Jan 4/22
;;
;; I've added predicate functions 'event-noun' and 'attribute-noun' from
;; the KNEXT code (though I'm not sure whether I'll need the latter).
;; We may want to define 
;;   (defpred ![event-np] x ...)
;; if we want to check for event NPs directly in the preprocessing rules.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; July 27/21
;;
;; This file is is a revision of the way 'isa' works, so that the ETA pattern
;; matcher can be easily converted to using "dot-variables" (which will be
;; distinct from explicit word matches). 
;;
;; [The conversion will also require converting 'nil' in ETA input patterns
;; to !atom (or maybe something simpler like !1, !at or !ex, using a defpred).]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; In ETA, features are specified as property lists of words and features.
;; Mapcar is applied to 'attachfeat' for each list of form (feat atm1 atm2 ...),
;;
;; In TT, we want to use hash tables to obtain all the features of a word.
;; Prior to the revisions here, 'isa' directly accessed features of a dot-
;; atom via a 'case' list, and checked if the atom being tested was on the
;; list; however, there were also 4 instances where a property was checked
;; via a hash table (in particular, argument-taking properties of verbs).
;;
;; The new method (like the ETA method) should allow for isa-chaining. 
;; However, some negative features like .NOT-DT needed to be converted
;; to !-predicates, like !not-dt.
;;
;; Suppose the general hash table is *isa-features*. Then in trying to match
;; a feature f specified as .f (i.e., prefixed with a '.') in a pattern
;; to an atom w [allow  expressions to have features as well??], we first
;; check if f = w, in which case the result is true; otherwise we look up
;; w in *isa-features*, and check if f is on the list of features  f1, f2, ...,
;; fk that we retrieve. If so, the match succeeds. If not, we go through
;; f1, f2, ..., fk, looking up each of them in *isa-features*, to see if f
;; appears in any of the features we retrieve for them. This is iterated.
;;
;; The isa-hierarchy is very unlikely to reach very high (and gets sparser
;; the higher we go), so this should be efficient enough. But we do have to
;; guard against circularity (as in ETA) because there's no requirement in
;; feature specification to avoid circularity (e.g., synonyms are by their
;; nature cicular). So we keep a list of features we have used for access
;; into *isa-features*, and don't look up any given feature a second time. 
;;
;; One question is whether it's worthwhile to somehow distinguish features
;; that have parents in the isa-hierarchy from those that don't. E.g., we
;; could use two separate lists for each feature entry, or we could mark
;; a "has-parent" property. But for now, the added programming complexity
;; doesn't seem worthwhile for the time that would be saved...
;;
;; Note: we also want features (specified in a pattern with a dot) to match
;; themselves. E.g., specifying '.doctor' in a pattern should match 'doctor',
;; not only 'surgeon', 'podiatrist', etc. Do we always want this? ETA assumes
;; yes (a feature is just closely related to the words with that feature, 
;; and as such certainly related to themselves), but what if we want to use
;; feature categories, e.g., would we want '.name' to match word 'name'?
;; And would we want '.expletive' to match 'expletive'? I think a reasonable
;; answer is that if we don't want that match, we should instead use 
;; !-predicates, e.g., '!name', '!expletive', which also has the advantage
;; of allowing iteration, e.g., '?name', '*name', '+name', '?expletive', etc.
;; I think we still want to allow categories as isa-features, e.g., POS's
;; like 'aux', but keeping in mind that this will also match 'aux' appearing
;; in text.

(in-package :lenulf)

(defparameter *isa-features* (make-hash-table)); the table with 'isa' data,
;``````````````````````````````````````````  linking atoms to dot-features
;                                            (which may also have dot-feats)
;
(defparameter *underlying-feat* (make-hash-table)); contains feat for
;`````````````````````````````````````````````````; dot-predicates of
                                                  ; form .feat

(defun attachfeat (feat-xx)
;``````````````````````````
; feat-xx: a list of form (feat x1 x2 ... xk)
;       where feat is a symbol, regarded as a feature
;       & x1, x2, ... are symbols (perhaps allowing expressions in future?) 
;       that will hereby be assigned feat, i.e., (isa xi feat) will be
;       true for each xi among x1, x2, ..., xk.
; We store feat as a feature of x1, x2, ..., xk in the *isa-features* table.
; We avoid duplication, for any xi that already has that feature.
; We also store feat under key .feat (the dot-prefixed version of feat)
; in table *underlying-feat*, for easy access to feat from .feat in the 
; 'match' function. 
;
 (let* ((feat (car feat-xx)) 
        (dot-pred (intern (concatenate 'string "." (string feat)))))
       (setf (gethash dot-pred *underlying-feat*) feat)
       (dolist (x (cdr feat-xx))
          (if (not (isa x feat))
              (push feat (gethash x *isa-features*))))
 )); attachfeat

;; NOTE: We can 'mapc' onto a list of feat-xx lists, like 'attachfeat' in the
;;       current ETA code (to be replaced using 'attachfeat'). But the
;;       initial feat should always be a dot-feature.
          

(defun isa (x feat) ; briefly tested July 27/21
;``````````````````
; x:    a symbol (e.g., 'surgeon' or 'doctor')
; feat: another symbol (e.g., 'professional'). In general, there is
;       no clear presumed logical relation between x and feat -- feat
;       simply "indicates" the sort of thing x refers to, in some sense.
;
; NB: an atom is always assumed to have itself as a feature; e.g., 
;     (isa 'this 'this) is true. So using '.occupation' in a pattern
;     will match 'occupation', even if the pattern coder wrote down
;       (occupation physicist cobbler poet teacher ...)
;     with the intention of using '.occupation' to match 'physicist', etc.,
;     in a sentence like "Alice is a physicist", and concluding "Alice
;     is employed". The sentence "Computer programming is an occupation"
;     would then likewise lead to the conclusion "Computer programming is
;     employed"! The moral is, if you want to use a match-predicate that
;     is true of various expressions but not itself, define it as a
;     !predicate rather than a feature.
;
; Method:
;     Look up x in the *isa-features* hash table and see if feat appears in the
;     retrieved list or in the lists (in the same table) associated with the
;     retrieved features, etc., while guarding against loops. Return T if 
;     the feature is found and NIL otherwise. Case is ignored. Both x and
;     feat are allowed to be dot-atoms (i.e., preceded by a dot), but the
;     dot(s) are stripped away before the hash-table lookup is done.
;     (Dot-atoms just serve to indicate to the matcher to do an isa-test.)
;
  (prog (ff f closed fff)
        (if (eq x feat) (return t))
        ; The next 3 lines aren't needed for the 'match' function, which
        ; looks in hash table *underlying-feat* to find the feature corres-
        ; ponding to a dot-atom. However, in the syntax tree preprocessing 
        ; rules, we use some predicates that directly call "isa", where the
        ; second argument is a dotted atom. The next few lines remove the dot.
        (if (dot-atom x) 
            (setq x (intern (string-left-trim "." (string x)))))
        (if (dot-atom feat) 
            (setq feat (intern (string-left-trim "." (string feat)))))
        (if (eq x feat) (return t))
        ; frequent case: feat is on the list retrieved from *isa-features* for x:
        (setq ff (gethash x *isa-features*))
        (if (find feat ff) (return t))
        ; maybe a feature on list ff has feature feat, etc. (iteration)
        (if (null ff) (return nil))
        (setq closed (list x feat)); ff will serve as the open list
        (loop (setq f (pop ff))
              (when (not (find f closed))
                    (setq fff (gethash f *isa-features*))
                    (when fff (if (find feat fff) (return-from isa t))
                              (push f closed)
                              (setq ff (union fff ff))
                              (setq ff (remove f ff))); just in case
                    (if (null ff) (return-from isa nil))))
 )); end of isa
                              

; DOT-ATOMS .<FEATURE>: Can be viewed as pred's, for matching specific atoms
; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
; But they don't allow for !/?/*/+ forms; a dot-predicate is satisfied iff
; there is an isa-path from the word being matched to the feature after
; the dot in the dot-atom. The dot-atom repertoire can be freely expanded.
;
; NB: 'isa' as defined above ensures that a dot-atom predicate is always
;     true of the atom following the dot (this is case-insensitive).
;
; The feature assignments explicitly enumerated here are for features that
; don't apply to more than about a dozen atoms. For ones applying to large
; numbers of atoms, we use separate statements below, that refer to named
; global lists containing those atoms.
;
; I also considered a special notation in TT for feature negation, e.g.,
; -.NP (doesn't have feature .NP), but decided for now this isn't
; tremendously useful, as we can also use defpred, e.g., 
;    (defpred !-NP x (not (isa x '.NP)))
; or, if we don't think the variants ?-NP, *-NP, +-NP will be needed,
;    (defun !-NP (x) (not (isa x '.NP)))
; Currently, the negative !-preds, defined via the 'defpred' macro, use
; !non- or !not- at the beginning of their names.
;
 (mapc #'attachfeat
   '((SQ SINV)
     (S SQ SINV S1 SBAR SBARQ)
     (SBAR SBARQ)
     (VP/SQ VP SQ)
     (VP/S VP S)
     (XP S NP VP AP ADJP ADVP PP WHNP WHADJP WHPP WHADVP)
     (XP-NON-VP S NP AP ADJP ADVP PP WHNP WHADJP WHPP WHADVP)
     (XP-OR-S S NP VP AP ADJP ADVP PP S SQ S1 SBAR SBARQ WHNP 
                      WHADJP WHPP WHADVP)
     (NON-PP S NP VP AP ADJP ADVP S SQ S1 SBAR SBARQ WHNP
                      WHADJP WHADVP)
     ; (NOT-NONE (not (eq atm '-NONE-); changed to !not-none
     (NN NNS)
     (NNP NNPS)
     (NN/NNP NN NNP); e.g., "{last} March" can be NN or NNP
     ; (NOT-DT (not (eq atm 'DT); changed to !not-dt
     (NP WHNP)
     ; CAVEAT: Feature predicates like the next two, subsuming both lexical
     ;         and phrasal elements, can lead to recursive run-away 
     ;         when rules that combine phrases are applied as often
     ;         as possible!
     (ATTR-JJ ; strictly attributive adjectives; collected mostly from
              ; https://guinlist.wordpress.com/tag/attributive-adjectives/
       former erstwhile latter previous past future ultimate 
       eventual consummate fake prospective elder eldest main mere 
       live sheer chief leading lone only principal sole primary major
       utter upper very adverse alternative)
     (ADJP JJ JJR JJS)
     (ADVP WHADVP RB RBR RBS NEG PP)
     (NON-ADVP NP VP AP ADJP PP SQ SBARQ WHNP WHADJP WHPP)
         ; XP, but not explicit 'ADVP'; aimed at topicalization. Assume
         ; fronted SBARs are not considered topicalized: "Although he won,
         ; he's dissatisfied"; allow "Why are you leaving, I ask" as 
         ; topicalized, though probably will be misparsed aby BLLIP.
     (THIS-DAY yesterday today tomorrow tonight)
     (WEEKDAY |Monday| | Monday| Monday 
       ; NB: the preprocessed Brown corpus may contain "pipe-enclosed" NNPs
       |Tuesday| | Tuesday| Tuesday |Wednesday| | Wednesday| Wednesday
       |Thursday| | Thursday| Thursday |Friday| | Friday| Friday
       |Saturday| | Saturday| Saturday |Sunday| | Sunday| Sunday)
     (TIME-PERIOD time present past future femtosecond picosecond nanosecond 
       microsecond millisecond second minute hour morning noon afternoon evening 
       night midnight day week-day holiday week month year decade century 
       millenium eon aeon January February March April May June July August 
       September October November December Christmas spring summer fall winter
       dusk dawn start beginning interim moment instant intermission pause
       |January| |February| |March| |April| |May| |June| |July| |August| 
       |September| |October| |November| |December| |Christmas|
       | January| | February| | March| | April| | May| | June| | July| | August| 
       | September| | October| | November| | December| | Christmas|)
     (next/last next last)
     (of/to/by of to by); these are usually argument-suppliers
     (WHNP WDT)
     (WHXP WHNP WHADJP WHPP WHADVP)
     (DT DT CD PRP$); initial CD is often a DT
     (a/an a an)
     (RB RBR RBS NEG); ex. of NEG: (NEG (NOT))
     ; (NOT-PREP-OR-SYMB (not (member atm '(IN -SYMB-))))
     ;    replaced by !NOT-PREP-OR-SYMB
     (NP-i NP-1 NP-2 NP-3 NP-4); e.g., rightward displace't
     (CC \, \;); for detecting end of an NP
     (and/or and \& or \'r)
     (SUBORD-CONJ as since although though because whereas even-though if
       provided-that so-that unless whether-or-not)
     (EXTRAP-S SBAR-1 SBAR-2 SBAR-3 SBAR-4 S-1 S2 S-3 S-4)
      ; these SBAR/S variants are used in Brown to correlate it-extra
      ; with a corresponding clause; in ULF, we expect that it-extra
      ; provides enough of a clue to locate the extraposed clause.
     (PP- PP-1 PP-2 PP-3 PP-4 PP-5 PP-6 PP-7 PP-8 PP-9 PP-10)
     (PP WHPP PP-); the PP-i phrases occur in Brown for phrase displacements
                  ; of various sorts (e.g., extraposition) -- '*pseudo-attach*'
     (with without); "with" is special, allowing 
                                       ; verbless S-complements
     (when where whenever wherever why how)
                                 ; e.g., when/where there is peace ...
     (PRED ADJP PP); (VP (VBG|VBN ...) ...) also, but ....
     (ADJP/NP/PP/SBAR/UCP/PRED
            ADJP NP PP SBAR UCP PRED);copular complements
     (POSTMOD ADJP PP SBAR); (VP (VBG ...) ...) also, but
                                          ; allowing general VP seems risky
                                          ; Use separate rules for VBG &
                                          ; VBN postmodifiers
     (DIS-POSTMOD PP-1 PP-2 PP-3 PP-4 PP-5 SBAR-1 SBAR-2 SBAR-3
        SBAR-4 VP-1 VP-2 VP-3 VP-4 ADJP-1 ADJP-2 ADJP-3 ADJP-4); more?
     (JJ JJR JJS); NB: .ADJP also inludes these
     (VB VBZ VBP VBD VBG VBN VBEN)
     (VBG/VBN VBG VBN)
     (VBG/AUXG VBG AUXG)
     (AUX TO AUXZ AUXD AUXP AUXF AUXG AUXEN AUX-CF MD)
     (MD/AUX/TO MD AUX TO)
     (VB/AUX VB VBZ VBP VBD VBG VBN VBEN
                          AUX AUXZ AUXD AUXP AUXG AUXEN AUX-CF MD)
     (POSS-BASE-V VB VBZ VBP AUX AUXZ AUXP); only VB & AUX
               ; (excluding MD) should actually be base forms, but this
               ; list allows for parsing errors
     (PASPART VBN VBEN VBD AUX AUXEN); possible parser-
                         ; assigned POS's for a past/passive participle
     (final-punc \. ? !)
     (it/they/them/this/that it they them this that)
     (have \'ve has \'s having had \'d)
     (have/ve have \'ve)
     (be is \'s are am \'m was were being been \'re)
     (feel feels felt feeling)
     (seem seems seemed)
     (stay stays stayed staying)
     (be/feel/seem/stay be feel seem stay)
     ; (non-be (not (find atm
     ;                '(be is \'s are am \'m was were being been \'re))
     ; replaced by !non-be
     (is/s/am/m is \'s am \'m)
     (was/were was were)
     (are/re are \'re)
     (been/being been being)
     (be/become be is \'s are am \'m was were being been \'s \'re
                             become becomes became becoming)
     (go goes going gone went)
     (say says said adds added remarked comments commented shouts
      shouted yells yelled confides confided whispers whispered asserts
      asserted laughed stated blabbers blabbered blurts blurted mutters
      muttered drawls drawled mouthed sputtered spluttered suggests 
      suggested recites recited repeats repeated expostulated affirms
      affirmed claims claimed exclaims exclaimed asks asked ordered
      wonders wondered snarls snarled gasped purrs purred declares
      declared announce announced alleges alleged avers averred hints
      hinted repeats repeated concludes concluded surmised speculates
      speculated insists insisted maintains maintained); Hmm, this 
          ; probably should be treated as a named list of atoms, as below
 )); end of feature assignment list

; Features that apply to numerous atoms on given named lists
;
(mapc #'(lambda (f+name) (attachfeat (cons (car f+name) (eval (cadr f+name)))))
   '((N-PROPOS-OBJ *n-propos-obj*)
     (N-COMMUN-OBJ *n-commun-obj*)
     (N-COG-OBJ (union *n-propos-obj* *n-commun-obj*))
     (N-PLACE *n-place*)
 ))

;; ADAPTED FROM KNEXT, MODIFIED TO INCLUDE NOUNS THAT ARE LIKELY TO
;; DENOTE EVENTS WHEN USED AS MAIN NOUN IN A PP LIKE "AFTER THE <noun>"
;; "AT THE <np>", "IN THE <np>", "UNTIL THE <np>" (whereas the goal
;; in KNEXT was to identifiy "undergoer NPs" of an event in a phrase
;; of form "NP of <event>")
;;
(defun event-noun (noun); June 12/01; Jan 2/08; Oct 9/10
;~~~~~~~~~~~~~~~~~~~~~~~
; Heuristically test whether `noun' (a singular noun, as an atom) is likely
; to be an event noun. Many nouns ending in -sion, -tion, -al, -ance, -ment
; are event nouns, but there are many event nouns with different endings,
; e.g., accident, adventure, ambush, etc., and many nouns with -sion, ...,
; -ment endings are not event nouns, e.g., tension, corral, chance, cement,
; etc. We first identify event nouns not ending in -sion, ..., -ment,
; then return nil for non-event nouns with such endings, and then accept
; any remaining noun ending in -sion, -tion, -al, -ance, -ment as an event
; noun. Obviously, refinements would definitely be possible.
; 
; A difficulty is that many nouns are ambiguous between event and
; non-event nouns in a fairly balanced way, e.g., "in the injury",
; "in the exhibit", "at its foundation", "before the conclusion".
;
; Note: in PPs with prepositions {during, till, until} we assume we
;       we have an event PP; for {after, before, at, in, up_to} we check
;       for an event noun. e.g., we exclude "He ran after the dog"; 
;       "He stood before the door" from event-PPs.)
; 
  (prog (backword)

        (case noun
          ; morpholically unrecognizable undergoing-event nouns:
          ; CHANGED JAN 2/08; GREATLY EXPANDED OCT 9/10; PRUNED JAN 3/22
          ((abuse accident adventure agony airdrop ambush analysis
            apocalypse apotheosis arrest ascent
            assault ban barter belch bellyache birth birthday
            breach breakage breakdown breakup bribery
            broadcast buildup burglary burnout burp 
            butchery capture catharsis censure change changeover checkup
            chemotherapy clash cleanup colonoscopy crash closure
            cloture collapse conflict conquest cramp crash
            death decay decline decrease defeat delay delerium
            delivery demise departure descent dialysis discovery
            divestiture divorce doomsday drainage eclipse
            electrolysis erasure
            event exchange exodus expenditure exposure
            failure fall falloff fever forfeiture 
            flop gasp genesis giveaway glide haircut
            handover hand-over headache holdup holocaust
            hydrolysis hypnosis hysterectomy
            increase infancy inflow influx injury klatsch lapse 
            laughter launch layoff lecture lesson lobotomy lockout lockup
            loss manufacture marriage martyrdom massage meltdown
            menopause metamorphosis mitosis 
            murder necrosis neglect nightmare 
            orgasm pandemonium paralysis
            pardon parody parse party pass physiotherapy plunder plunge
            praise preview progress psychoanalysis 
            psychotherapy puncture purchase radiotherapy
            rape rapture remand rescue ridicule rinse rise roast
            ruin rupture sabotage scan search seizure setback shift
            shrinkage shutdown siege simulcast slaughter
            sleep slide slip slowdown slumber sneeze spasm speedup
            spill spillage spin spoilage start stasis
            storage struggle stumble surgery surrender
            survey swap synthesis takeover talk test theft therapy
            thrombisis torture toss transfer trip
            tumble turnabout turnaround turnover tutelage twitch
            use upset vasectomy venture voyage 
            worship wrapup) ; 
           (return T) )
          ;
          ; We add a case here for exceptions to the rules at the end, 
          ; returning nil for these; the original test for putting words on
          ; this list was, do they commonly occur with "of" and when they
          ; do, would it be bad to rephrase "x of y" as "y undergoes x"?
          ; But for present ULF purposes, we prune nouns from the exclusion
          ; list if they yield a temporal PP when preceded by "before" or
          ; "after".

          ((foreboding heading ; a fairly complete exclusion list
            clothing coating feeling meaning scaffolding
            bing arcing ding pudding spalding inholding being edging lagging
            legging pegging rigging wigging thing something nothing plaything
            anything everything king stocking Viking sibling cling seedling
            foundling hireling fledgling duckling inkling bestselling
            sapling darling sparling starling sanderling underling sterling
            sling mudsling gosling lemming shortcoming subsuming 
            cunning aborning foregoing churchgoing ping weatherstripping
            ring smattering bedspring offspring earring averring string
            shoestring bowstring sting formatting abetting pacesetting 
            typesetting teletypesetting earsplitting lawgiving earthmoving
            wing lacewing owing batwing zing

            adhesion admiration
            affectation affection allegation ambition ammunition
            approbation argumentation assumption attention 
            aversion bastion caption coalition cohesion collocation 
            combination commendation compassion composition compunction
            condescension condition configuration
            congregation concession connotation constitution
            contention contraption contribution convention 
            denotation derision description destination
            devotion diction dimension direction discretion dissertation
            duration emotion erudition evasion exception
            expectation explanation
            exultation fiction foundation fraction friction
            function gumption habitation illusion imagination imprecision
            impression inclination inclusion indecision
            indignation information injunction inscription 
            intention intuition invitation
            irresolution junction jurisdiction lesion limitation
            location locomotion locution mansion midsection
            munition nation notion nutrition obsession occasion
            omission opposition option organization passion pension
            permission perspiration petition pigmentation portion position
            possession potion precaution procession profession profusion
            proportion proposition propulsion protestation provision
            position precision preposition prescription
            presumption presupposition protusion question recommendation
            recursion religion reputation respiration
            retribution revulsion sanction satisfaction
            seclusion section situation station
            stupefaction succession suction suggestion superstition
            supposition suspicion tension torsion tradition version
            vision workstation

            cabal cannibal gimbal decal radical umbilical geochemical
            radiochemical physiochemical petrochemical bifocal
            reciprocal rascal mescal medal pedal sandal vandal
            conceal ideal cornmeal oatmeal cereal seal goldenseal
            teal veal weal commonweal zeal offal gal illegal madrigal paschal
            marshal withal wherewithal special official financial dial radial
            gerundial sundial cordial binomial multinomial monomial polynomial
            baronial marsupial aerial serial material factorial
            editorial terrestrial extraterrestrial sial initial credential
            exponential differential essential potential vial extremal decimal
            hexadecimal animal planetesimal infinitesimal mammal isothermal
            normal paranormal abnormal orthonormal canal arsenal signal
            cardinal ordinal original aboriginal marginal virginal criminal
            nominal terminal urinal annal diagonal orthogonal professional
            supranational eternal journal communal coal charcoal foal
            goal shoal pal sepal principal opal estoppal cathedral liberal
            numeral general mineral collateral integral admiral spiral
            oral coral moral corporal pectoral chaparral corral mitral central
            neutral plural mural natural transversal vassal metal
            petal barbital orbital genital capital hospital total portal
            pedestal crystal dual individual manual equal usual actual factual
            intellectual victual spiritual bisexual homosexual heterosexual
            naval rival oval larval interval loyal

            significance riddance impedance chance ambiance fiance
            affiance alliance appliance lance balance imbalance semblance
            parlance conformance ordnance countenance sustenance provenance
            ordinance finance luminance governance forbearance clearance
            encumbrance hindrance furtherance temperance
            intemperance perseverance durance endurance insurance
            nuisance reflectance inductance conductance transconductance
            capacitance inheritance acquaintance susceptance inertance
            stance circumstance instance emittance remittance transmittance
            nuance grievance relevance connivance contrivance allowance
            abeyance conveyance annoyance

            apartment armament cement compartment complement compliment
            condiment decrement department detriment
            disappointment document element embodiment emolument 
            figment filament fragment government impediment implement increment
            instalment instrument integument ligament liniment moment monument
            ointment ornament parliament pediment pigment regiment sacrament
            sediment segment sentiment tenement testament);
           (return nil) )
         )
        (setq backword (reverse (coerce (string noun) 'list)))
        ; Exceptions to the following have been pretty fully enumerated above
        (if (or (ends-in backword '(#\I #\N #\G))
                (ends-in backword '(#\S #\I #\O #\N))
                (ends-in backword '(#\T #\I #\O #\N))
                (ends-in backword '(#\A #\L))
                (ends-in backword '(#\A #\N #\C #\E))
                (ends-in backword '(#\M #\E #\N #\T)) )
            (return T) )
 )); end of event-noun

;; ** FROM KNEXT; NOT USED HERE CURRENTLY
(defun attribute-noun (noun); June 13/01
;~~~~~~~~~~~~~~~~~~~~~~~~~~~
; Heuristically test whether `noun' (a singular noun, as an atom) is likely
; to be an `attribute noun' such as "kindness", "heat", "excitement", etc.
; (typically de-adjectival).
;
  (prog (backword)

        (case noun
          ; Morphologically unrecognizable cases:
          ; Note: many cases are covered pretty well by using "have"
          ;       (i.e., having a property) so completeness is not vital
          ((heat cold warmth excitement wrath) ; ** others ??
           (return T) )
          ; Add a case here for exceptions to the rules that follow,
          ; returning nil for these
          ((witness business illness governess)
           (return nil) ))

        (setq backword (reverse (coerce (string noun) 'list)))
        (if (ends-in backword '(#\N #\E #\S #\S))
            (return T) )
 )); end of attribute-noun

(defun abstract-noun (noun); NOT CLEAR IF THIS MAY BE NEEDED
;~~~~~~~~~~~~~~~~~~~~~~~~~
; noun: a noun stem
;
; The abstract nouns include most that end in "-ism", "-ity", "-ment",
; "-ion", "-ing", or an NN or NNS with no determiner. Many common nouns 
; should be treated as abstract, eg., basis, case, interest, manner, 
; science, etc., etc. Use 'attribute-noun' above?
;
 NIL) ; STUB

