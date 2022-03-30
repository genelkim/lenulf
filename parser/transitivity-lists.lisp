
(in-package :lenulf)

;                                                          Dec 12/20
; Starting point for this file was
;     ~schubert/elf-from-sentences/purely-intransitive-verbs.lisp
;
; Much reliance on ~schubert/pref-parser/word-parser/alvey/
; ================================================================
; The lists here were intended primarily to facilitate correct gap insertion
; in Treebank constituency parses. But I expanded them to also help with
; other disambiguation tasks, like deciding whether an infinitive is
; a term or and adverbial (e.g., "He tried to relax" vs. "He painted to
; relax"). For verbs, I'LL PROBABLY TAKE (MONO-)TRANSITIVE AS THE DEFAULT 
; CATEGORY, i.e., taking an NP object (plus possibly one or two non-NPs) 
; as complement.
;
; The *purely-intransitive-verbs* are ones which, if appearing in a context
; where an NP gap is expected, are unlikely to dominate the expected gap.
;
; I've eliminated obscure verbs and apparent non-verbs, and many idioms
; that Treebank parsers would parse phrasally. (I've left a lot of verb +
; particle combinations, since the ULF parser should combine them (but
; *when* -- just prior to gap insertion?)

; ** I think I may want to add verbs like "walk", even though we have
;    "walk the streets" and "walk the line", i.e., rarely transitive verbs,
;    because the objects can scarcely be gapped: ?"What did you walk _?"
;    (though we could have "What streets did you walk?")

(defparameter *purely-intransitive-verbs* (make-hash-table))

(mapcar #'(lambda (x) (setf (gethash x *purely-intransitive-verbs*) t))
;``````````````````````````````````````````````````````````````````````
; NB: Many of these verbs take non-NP complements.
;
; Verbs never followed by an NP gap. Note: "behave" is included, even
; though it can be followed by an NP like "himself", because this can't
; be gapped (*"Whom did he behave?"). Verbs like "be" that can really
; only be followed by a predicate, but where that predicate can be a 
; nominal and can be gapped, are listed as *pseudo-transitive-verbs*.
; Note that particles make a difference to transitivity. E.g., "back"
; can be transitive, while "back_away" is not; by contrast, "bargain"
; is intransitive, whereas "bargain_away" is transitive.
'( ; TO BE FILTERED

abound accede accrue acquiesce act_up adhere agonize allude amble amount anticorrelate antipathize apologize appear arse_around ask_around autodestruct back_away backpedal band_together bang_away bargain base_jump battle_it_out bawl bed-wet behave blast_off blaze_away bleed_to_death blossom bode boil_over bone_up boogie bottom_out bow_down brachiate branch branch_off breeze bring_it bubble_up buckle bugger_off bulge bungee_jump burgeon burst_out busk butt_out cackle caddy calve camp_out campaign care carom carouse catch_on caterwaul cavort cha-cha charleston chat chatter chillax chip_away chirp chortle clam_up clash clomp cloud_over co-occur coconspire coexist cohabit collide collude come come_along come_apart come_away come_back come_correct come_forward come_on_over come_through come_to_grips come_to_life come_to_order come_together come_up come_with commune complain condescend conflict conk_out consist consort converge converse cooperate correspond counterstrike crackle creep creep_up crop_up crouch crump cuddle_up daddle dawn decamp default degenerate deliquesce desist deviate diddle-daddle digress dine_out disagree disco dispose dissent diverge do_battle do_business dote dream_on drift_apart drift_off drone_on drop_away drop_dead drop_round duckwalk duet dwell dwindle eddy edge_up effervesce effloresce egress elapse elope emerge emigrate emote empathize equivocate err evanesce exclaim face_off faint fall_apart fall_away fall_behind fall_in fall_in_love fall_on_deaf_ears fall_out fall_over fall_through fall_together fare fast fawn feel_down fend fess_up feud fib fiddle_about fiddle_around file_in fire_away fishtail fission fizz flail_about flake_out flare_up flatline flicker flounder fluoresce fly_off foam fool_about fool_around fornicate fraternize freestyle freeze_over freeze_up fuck_about function gad gallivant galumph gambol gander gang gape garden genuflect get_along get_by get_carried_away get_cold_feet get_hitched get_in_the_way get_into_trouble get_laid get_lost get_lucky get_together give_birth give_chase glance glimmer glisten glower go_all_the_way go_apeshit go_awry go_back go_bankrupt go_mad go_overboard go_places go_platinum go_public go_steady go_to_war go_wrong grandstand gravitate grind_to_a_halt grouch grow_cold grunt guffaw gyre hang_a_left hang_a_right hang_in hang_out hang_tough hang-glide happen happen_along have_a_go heal_up hearken_back hee-haw hibernate hide_out hit_the_ceiling hit_the_fan hit_the_roof hold_fire hold_hands hold_true hole_out hotdog hotfoot_it hunker ice_over immigrate impend inch intercede interfere introspect iridesce jaw_away jet jet_off jitter jut keel keel_over keep_on_trucking keep_one\'s_eyes_peeled keep_watch kill_time kneel knuckle_down kowtow lactate lapse lay_over lay_rubber leaf level_off lie lie_back lie_down lie_low linger live_in_sin loaf lol look_alike look_round lose_count lose_weight lust luxuriate make_merry make_off make_sense make_time make_way malinger mambo masturbate meddle meditate menstruate meow merengue mingle misspeak misstep moo moonwalk mouth_off move_in move_on muck_about mushroom mutiny nosedive oink opt oscillate ovulate pamphleteer pander partake party pay_attention pay_one\'s_last_respects peak pee peep_out peer pelt_down perspire pertain phone_home pick_holes pick_up_the_pieces pick_up_the_tab pig_out pinch-hit pipe_down pirouette plateau play_hookey play_second_fiddle play_to_the_crowd plummet polka pontificate pour_down powwow preside press_ahead prevail pull_ahead pull_strings pull_the_plug pull_the_trigger punch_in push_the_envelope pussyfoot putter quibble quiver rage ramp reach_out read_lips rebel recidivate recoil recriminate recur reek reign relapse remain reminisce result retrogress reverberate reverse_the_charge revolt ribbit ripen_up rise rock_on rock_out rocket_up roister roll_around roll_in roll_on rush_off salivate salsa sashay scamper scheme scrape_by scuffle seem shape_up shimmer shimmy shop_around shrimp shudder sidle silt_up simmer_down simper sin sit_on_the_fence skateboard skitter skydive slalom slave_away sleep_in sleepwalk slither slouch slug_away smolder snicker sniffle snigger snore snowshoe snuggle_up sojourn spaz_out spazz_out spill_over spill_the_beans spin_out spiral splish spring_up sproing stand_back stand_by stand_out stand_pat stare steal_away steal_the_show steer_clear step_aside step_forward step_in stick_together stomp_off stop_dead stop_off stop_on_a_dime stop_over stride strike_back strut_one\'s_stuff succumb sunbathe sweat_bullets swelter take_a_look take_advantage take_aim take_care take_heed take_ill take_inventory take_notice take_one\'s_time take_part take_place take_pride take_refuge take_root take_shape take_sick take_the_floor take_turns talk_dirty talk_shop tamper tattle teethe telemarket tense_up tent thieve throb thumb_a_ride tie_the_knot tip_the_scale top_out totter transit transmigrate trek tremble trot_off troupe trundle_along tsk tsktsk tsk-tsk tumble_down turn_on_a_dime turn_one\'s_back turn_to_dust vacation vacillate vape verge video_conference waddle waft walk_out wallow want_in want_out water_ski watusi waver wayfare wee-wee weight-train welter whack_off wither window-shop wise_up wither_away wuther yackety-yak yak yap yarr yaw yo-yo zone_out zoom_in zoom_out)
 ); end of *purely-intransitive-verbs*


(defparameter *complement-free-verbs* (make-hash-table))

(mapcar #'(lambda (x) (setf (gethash x *complement-free-verbs*) t))
;``````````````````````````````````````````````````````````````````````
; NB: These take neither NP-complements nor any others; thus they are
;     included in the *purely-intransitive-verbs*
;
; Verbs never followed by an NP gap. Note: "behave" is included, even
; though it can be followed by an NP like "himself", because this can't
; be gapped (*"Whom did he behave?"). Verbs like "be" that can really
; only be followed by a predicate, but where that predicate can be a 
; nominal and can be gapped, are listed as *pseudo-transitive-verbs*.
; Note that particles make a difference to transitivity. E.g., "back"
; can be transitive, while "back_away" is not; by contrast, "bargain"
; is intransitive, whereas "bargain_away" is transitive.
'(
act_up amble autodestruct bawl bed-wet behave bleed_to_death boil_over 
boogie bulge busk cackle calve camp_out carousen caterwaul 
cavort cha-cha charleston chillax chirp chortle clam_up clomp cloud_over 
come_apart come_through come_to_life conk_out cough crackle crop_up crouch 
crump decamp deliquesce diddle-daddle dine_out disco dote dream_on drift_apart 
drop_dead dwell dwindle eddy effervesce effloresce elapse err evanesce 
faint fall_apart fall_on_deaf_ears fall_through fare fast fend file_in fishtail 
fizz flail_about flake_out flare_up flatline flicker flounder fluoresce foam 
fornicate freeze_over freeze_up fuck_about function gad gallivant galumph 
gambol gander garden get_carried_away get_cold_feet get_hitched get_into_trouble 
get_laid get_lost get_lucky glimmer glisten go_all_the_way go_awry go_bankrupt 
go_mad go_overboard go_places go_platinum go_wrong grandstand grind_to_a_halt 
grow_cold grunt gyre hang_a_left hang_a_right hang_in hang_tough hang-glide 
happen_along heal_up hee-haw hibernate hide_out hit_the_ceiling hit_the_fan 
hit_the_roof hold_fire hold_hands hold_true hotdog hunker ice_over impend 
iridesce jitter keel keel_over keep_on_trucking kill_time kneel knuckle_down 
lactate lay_over lie_low live_in_sin loaf lol look_alike look_round lose_weight
make_merry malinger mambo masturbate menstruate meow merengue misstep moo 
moonwalk oink oscillate ovulate pamphleteer party peak pee perspire phone_home
pick_up_the_pieces pick_up_the_tab pinch-hit pipe_down pirouette plateau 
play_hookey play_to_the_crowd polka pull_strings pull_the_trigger punch_in 
push_the_envelope putter quiver ramp read_lips recidivate recur reverberate 
reverse_the_charge ribbit ripen_up rock_on rock_out rocket_up roister 
roll_around roll_in roll_on salsa scamper shape_up shimmer shimmy shop_around 
shrimp shudder silt_up simmer_down simper skitter skydive slalom sleep sleep_in 
sleepwalk slither slouch smolder sniffle snore snowshoe sojourn spaz_out 
spazz_out spin_out spiral splish sproing stand_by stand_out stand_pat 
steal_away steal_the_show step_in stick_together stomp_off stop_dead 
stop_on_a_dime strut_one\'s_stuff sunbathe sweat_bullets swelter take_ill 
take_shape take_sick take_the_floor teethe tense_up tent thieve throb 
thumb_a_ride tie_the_knot tremble trundle_along tsk tsktsk tsk-tsk 
turn_on_a_dime turn_to_dust vacation vape waddle waft water_ski watusi 
waver wear_off wee-wee weight-train whack_off wither window-shop wither_away 
wuther yackety-yak yo-yo zone_out)
 ); end of *complement-free-verbs*


(defparameter *ditransitive-verbs* (make-hash-table))
 ; commonly taking 2 NP obj's, & readily allowing gapping of either NP.

(mapcar #'(lambda (x) (setf (gethash x *ditransitive-verbs*) T))
;```````````````````````````````````````````````````````````````
; [Mostly from 'allot', 'get', 'give', 'make', 'send', and 'throw' classes 
; in VerbNet, and others by "branching out" from members of those classes. 
; Some of these really involve a predicative nominal: appoint,
; brand (the innocent man they branded a traitor), call, christen,
; crown, dub, label, name, nickname, rename, term, vote; but some 
; can also be truly ditransitive (call me a cab, vs. call me a fool)
;
; The transitive verbs with an additional predicate complement
; should be separately listed; there are several in "rarely-ditransitive-
; verbs.lisp as well (anoint, declare, name, render).

; In the ELF files I moved rare ditransitives to "rarely-ditransitive-
; verbs.lisp";
; most of those allow benefactive (recipient) arguments; I've left
; some of the more commonly occurring ones here.

; Tests: Should readily allow at least one of
;        "What did you <verb> that one?"
;        "Which one did you <verb> that one?"
;        "Whom did you <verb> one?"
; For example, I relegated "advance" to the *weakly-ditransitive-verbs*,
; because "What did you advance him?", or "Who(m) did you advance the money?"
; sound pretty unusual to me. Where I give examples, this tends to indicate
; my uncertaintly about the correctness of having the verb in this category.
 '(
   airmail allocate allot ask assign award bequeath bet bill bring buy
   carve charge cost deliver deny ; "She charged him the full amount"
   FedEx feed fetch fine forgive get give grant guarantee hand hand-deliver
   issue knit lease lend loan lob mail make nickname offer overcharge overpay
   owe pawn pay peddle permit; "permitted him one cigarette"
   pitch pledge pour prepare prescribe proffer promise remit rename sell send
   serve show slip supply teach tell throw tip wager wire wish
 )); end of *ditransitive-verbs*


(defparameter *weakly-ditransitive-verbs* (make-hash-table))

(mapcar #'(lambda (x) (setf (gethash x *weakly-ditransitive-verbs*) t))
;``````````````````````````````````````````````````````````````````````
 ; The items in here were originally "starred items" in a comprehensive
 ; list; while many can function perfectly well as ditransitives, they
 ; usually don't freely gap either one or the other position; verbs like
 ; "mean" (as in "She meant him no harm") are left out altogether, as
 ; phrasings like "What harm did she mean him?" or "Who(m) did she mean 
 ; harm?" are just extremely improbable.
 '(
   accord advance allow answer apportion bake bat bear begrudge blow
   boil brew build cable cash cause cede ; "caused me grief" "blew him a kiss"
   chuck cite cook crochet deal deny dictate disburse dribble 
   earn find ; "earned him a medal ; "found him a cozy armchair"
   fix flash fling forbid forward fry grill hawk hock kick land 
   leave microwave mix net order; left her the estate; mixed himself a drink
   pass play pose post punt quote read refund rent repay save secure sew 
   ship sing sneak spare telex toss transmit type write yield)
 ); end of *weakly-ditransitive-verbs*

(defparameter *pp[arg]-taking-verbs* (make-hash-table))
; Originally this was to contain all verbs that subcategorize for a PP.
; But parses don't distinguish between complements and adjuncts, and
; PP-adjuncts can always be gapped, there's really no benefit here for
; gap-postulation. However, it may turn out to be useful to identify
; verbs that *just* take a PP complement. The complement/adjunct
; distinction again is often unclear, but since these verbs don't
; (easily/often) take an NP complement, they can be used along with
; "purely intransitive verbs" (with which they overlap because of the
; fuzziness of the adjunct/complement distinction) to identify verbs
; that, e.g., don't (easily) allow for a wh-nominal complement. E.g.,
; contrast "I recall where she lives" vs. "I live where she lives."
; NB: Verbs that take an S[that] complement or NP[wh] complement,
; like "agree", "attest", "care", etc., are omitted, because a following
; wh-phrase is usually understood as an object, even though these verbs
; generally don't take NP objects. E.g., "He agreed {what Alice said /
; where Alice stood on the matter} was unclear"; "He cares what others
; think of him". (So these are "silent preposition" examples.)
(mapcar #'(lambda (x) (setf (gethash x *pp[arg]-taking-verbs*) t))
;```````````````````````````````````````````````````````````
; The initial focus was on verbs that readily allow a PP complement gap 
; (in addition ; to allowing a gap of type PP/NP, as all PP-complement-
; taking verbs do).
;
; But later I needed a good list of PP-taking verbs so that ones that
; are *not* PP-taking can have their PP adjuncts turned into ADVP.
; Consequently, ignoring the subtleties immediately following, I've
; put some verbs like "agree" and "balk" back in the list (but not ones
; whose _pp subcat is much rarer than others -- e.g., "check" is more
; often simply _np, less often _pp[with]_np[wh]; also PP[of] and PP[to]
; are treated more or less uniformly as supplying args.)
;
; [SO, THE FOLLOWING IS NOT LONGER STRICTLY CONFORMED WITH:]
; E.g., "On what topics does the course focus?" is perfectly colloquial
; (as is "What topics does the course focus on?"); whereas we exclude 
; "agree", since gap-examples like "With whom do you agree?" probably
; occur relatively rarely compared with, "Who do you agree with _?";
; Other excluded verbs: ?"Out of what jail did you bail him _?", vs.,
; "What jail did you bail him out of _?"; or ?"At what did he balk _?"
; vs. "What did he balk at _?"; or ?"Into what did you bang _?" vs.,
; "What did you bang into _?"; or ?"On what does this comment bear _?
; vs. "What does this comment bear on _?"; or ?"Into whose car did he 
; break?", vs. "Whose car did he break into _?"
;
; NB: PP adjuncts (as opposed to complements) are a different matter -
;     they gap easily as a whole, e.g., "On what grounds did he balk
;     at helping _?", vs. ??"What grounds did he balk at helping on _?";
;     ??Off what list did you cross him?" vs "What list did you cross
;     him off (of)?"; ??Of what was he cured?" vs "What was he cured of?"
;     ??"At what are you driving?" vs "What are you driving at?"
'(abscond abstain ascertain adhere adjust
  affiliate agonize agonise agree ally apologize apologise appeal 
  arbitrate argue arrive atone balk
  band_together belong benefit 
  bicker bitch blaspheme blaze 
  brag break campaign check
  chum co-star co-habit collaborate collide
  collocate collude come come_across come_away come_back come_down come_out 
  come_off come_over come_round comment commiserate commune
  communicate complain comply concentrate concur condescend condole
  confer confide confirm conflict conform connive consent consist
  consort conspire converse cooperate cope
  copulate correspond crawl cringe crusade culminate curtsy dabble
  dance dawn deal default defect deliberate delve demur depart depend
  derive desist despair detour deviate devolve dicker differ 
  differentiate digress dine direct disagree disapprove discourse
  discriminate disengage dismount dispose dissent dither
  dive diverge dote dovetail duel eavesdrop elope emanate embark
  emerge emigrate encamp encroach enquire enthuse escape experiment
  fall fawn feast fend feud fiddle flee flinch flirt flock focus
  forage fraternize fraternise fret frown fulminate function fuss
  gamble gape gargle gasp gawk gaze gibe glance glare glisten gloat
  glow glower gnaw go gogle gossip graduate grapple gravitate grieve
  grin groan grouse grovel grumble haggle hanker happen hark 
  harmonize harmonise harp hearken hinge hint hiss hobnob holler
  home_in honk hoot hop hover hunger impinge incline indulge infringe
  inhere inquire insist interact interfere intermarry intermingle
  intertwine intervene intrude inveigh itch jest jockey joke joust
  kneel kowtow labor lag languish lapse lash laugh leaf lean lecture
  leer liaise lie listen loaf long look lust luxuriate major marvel
  masquerade mate matter meddle mediate mesh mess metamorphose
  migrate militate mingle moan monkey moralize moralise motion
  muck muscle muse negotiate nestle nose number object obsess obtrude
  occur officiate opt oscillate overflow pal_around pale pander pant
  partake participate partner peck peek peep peer perch persist pertain
  philosophise philosophize pine pivot plead plod point pontificate
  pore pose pounce prate prattle pray predominate preside prevail
  prey proceed profit pry puff_away puff_up pulsate pun pussyfoot
  put_up puzzle quail quake quarrel quest queue_up quibble quiver
  race rage rail rain_down ramble ramble_on range rat rave react reason
  rebel rebound recede reckon recline recoil recriminate recuperate
  reek refer reflect refrain reign rejoice relate rely remark reminisce
  remonstrate renege repent reply repose reside resort resound respond
  rest result retaliate retrogress return reunite revel revert revolt
  revolve rhapsodise rhapsodize rhyme ricochet riffle rifle riot rise
  rise_up roam robe romance room root ruminate rummage scintillate
  scoff scowl secede seethe sentimentalise sentimentalize shack_up
  shop shudder shy_away side sidle_up sift signal sin sit_in skate
  skimp skirmish skirt slaver sleep smile snicker snigger snipe
  socialise socialize spar sparkle speak speak_out specialise specialize
  speculate splurge spurt_out spy squabble squeal stand star stare 
  stem step step_up step_down stick stick_out stick_up stink stock_up
  stoop stoop_down stray strive struggle stumble subscribe subsist
  succumb suck_up suffer surrender swarm swerve swoop_down sympathise
  sympathize talk tamper tangle team team_up teem teeter tend testify
  thrill throng thrum thud tire toady tool_along tool_around tower toy
  trail travel tread trespass trifle triumph tumble tune_in tussle vacation
  vacillate vanish verge vibrate vie volunteer vote vouch wade wail
  wait walk wallow wander war waver weary weasel weep welch well_out
  welsh welter wince wink winter withdraw wonder wrangle wriggle yearn
  yield zero_in zoom zoom_in
 )); end of *pp[arg]-taking-verbs*

"NOTE: The hash table *v_np_pp* is computed in the file
       extract-data-from-alvey.lisp. 
 The verbs considered are those in *strong-np+pp-taking-verbs* (below).
 Alvey is used to find what prepositions the verb accepts in the PP
 complement position; the keys to the table are (verb-stem preposition)
 pairs."

"NOTE: The hash table *v_{np}_* is computed in the file 
       extract-data-from-alvey.lisp. 
 This is indexed by a verb, and yields a non-NIL value (an occurrence
 count in Alvey) if it has more entries lacking an NP complement than
 ones that have such a complement; i.e., the verbs are weakly transitive.

 At the same time, the global list *weakly-transitive-verbs* is created.
 Note that these are actually *at most* weakly transitve, i.e., they
 include the non-NP-taking verbs."

(defparameter *strong-np+pp-taking-verbs* (make-hash-table))

(mapcar #'(lambda (x) (setf (gethash x *strong-np+pp-taking-verbs*) t))
;````````````````````````````````````````````````````````````````````
; The goal here is to enable guessing whether a PP following an NP object
; of a verb is likely to be an argument supplier, rather than an adverbial.
; So, consider PPs coming after a *presumed* NP complement. For example,
; "rush" doesn't require an NP object ("rushed to the door"), but even when
; it does have an NP object, it expects a PP ("rushed him to the clinic").
; We exclude the separate category of np+pred-taking verbs, where the
; pred may well be a PP (rather than either an adverbial or an argument
; supplier). We also don't list v_np_p[of] or v_np_pp[to] verbs, as we
; assume the PP in these cases is always an argument-supplier.
; Also v_np_pp[as] are omitted here -- they are considered separately
'(absent abstract acquaint adorn adulterate affiliate afflict aim alienate 
;                          ``````````````` unsure: .p-arg or .p [=> adv-a]?
align alloy ally alternate amalgamate analogize anoint answer apparel arm 
;                                               `````` ditto
arraign assimilate assist associate attire augment avert balance ballast 
;                                   `````` ditto  etc. [leave as .p-arg for now]
bamboozle ban banish barrage barricade barter base bedaub bedeck beg beguile 
berate beseech beset bestow bet betroth bid bilk bill blackmail
blame blanket blend blow bludgeon bombard book borrow bribe browbeat 
buffer bum burden busy buttress buy cajole canvass cart cast caution center 
centre change channel charge cheat chuck cite clap classify cloak closet
co-opt coach coat coax coerce collect combine commend compare compensate 
compliment compound conceal concentrate concern conclude condemn confer 
confide confiscate confound confront confuse congratulate connect conscript
console content contort contrast convert corelate correlate couch cradle 
cram credit criticise criticize dab dangle daub debate deceive deck decorate 
deduct defend deflect delete deluge deport derive desensitize detach deter 
dice differentiate dignify dilute dip disaffiliate disassociate discharge 
disconnect discourage discuss disengage disentangle dislodge dismiss displace
disqualify dissociate dissuade distinguish distract divert divide dock dose 
douse dower dowse draft dragoon drape draw drench dress drum dunk dupe earmark
ease elevate elicit eliminate emancipate embed embellish emblazon emboss 
embroider embroil encapsulate encase encircle enclose encrust encumber endow 
enfold engage engrave enmesh enrich enrol enroll ensnare entangle entice 
entomb entrap entrust entwine envelop equate equip estimate estrange etch 
evacuate evict exact exchange exclude exculpate excuse exempt exert exile 
expand expel expend expunge extort extract extrapolate extricate extrude exude
face familiarise familiarize fashion fault favor favour feather feed festoon 
fetter file fill fit fix flank flavor flavour flick fling flood flush focus 
foist fold forewarn forgive form free freight fringe furnish galvanise galvanize 
garnish gird give goad gorge grace graft grind groom guard gull hammer hang 
harden harness heap hustle illuminate imbed imbue immerse immunise immunize 
impale impanel implant implicate import impose impregnate impress imprint 
incarnate incite inclose incorporate inculcate indoctrinate indulge infect 
infer infest infiltrate inform infuse inherit inhibit initiate inject inoculate
inscribe insert inset insinuate install instil instill instruct insulate 
insure integrate interest interlace interleave interlink interpose intersperse
intertwine interweave intrench intrude intrust inundate invest involve isolate
join jolly keep lace ladle lard launch lavish lay lead leave lecture lend 
let levy liberate lift line litter load lock lop make marinate mark match 
mate merge mingle mire misinform mislead mistake mix moor mount multiply nag 
negotiate nick nose notch obtain obtrude occupy off-load organise organize 
ornament oust outfit outlay overcharge overlay overstock pack pair panel 
pardon partition partner pattern pave pay peel pelt penalise penalize pepper 
pester picture pierce pile pilot pipe pit plague plaster plate pluck plunge 
ply point poke portion post pour precede predicate preface prejudice prepare 
prescribe press pressure price pride prime print procure prod prohibit 
project proof propagandise propagandize prospect protect protrude pry pull 
punctuate punish purchase purge purvey push put quench question quiz quote 
railroad rain raise rake rally ram rank rearm reassure rebuke recline 
recompense reconcile redeem reel reflect regale reimburse reinstate 
remove remunerate replace reproach reprove request require rescue 
reserve resolve restock restrain retrieve reunite reward rhyme rig 
ring rinse robe rouse route rush sandwich sate satisfy saturate save scatter 
scent schedule school scoop score scour scout scrape screw search season 
seclude secure seduce segregate separate set sever shame shape share sheathe
shell shelter shield shower slate sling smear smite snag soak solicit sort 
spangle spark spatter spend spice splash splice split spray spread spring
sprinkle square squeeze squirt stack staff stake stamp stampede steep stint 
strew strip stuff substitute subsume subtract sue superimpose supplement 
supplicate supply surcharge surfeit suspect suspend swamp swap swathe 
sweep swerve swindle switch swop tack tag take talk tear telex tell test 
thank threaten ticket tip top tore toss trade train transcribe transfer 
transform translate transmit transplant transport trawl treat trick trim 
trouble trust tuck tune turn tutor twist unify unleash unload upbraid 
use usher vaccinate value wad wager walk wangle warn waste wean weave wedge
weld wheedle will win wind wire withdraw withhold worm wrap wrest wring yield)
); end of *strong-np+pp-taking-verbs*


(defparameter *rarely-transitive-verbs* (make-hash-table))

(mapcar #'(lambda (x) (setf (gethash x *rarely-transitive-verbs*) t))
;````````````````````````````````````````````````````````````````````
; These are verbs that do occasionally take a referential NP object,
; but normally don't. I decided to put "care" under pure transitives,
; despite "What do you care _?", which seems like an idiomatic variant 
; of "Why do you care _?". "Stand" is an oddball as it is transitive
; only if preceded by "can {not, n't}". But there's also "stood his 
; ground", and various combinations with particles (up, down, by, in).
;
'(attest bleep budget caddie cater disappear feel growl plead sound
  spring stand stay); "How much pain do you feel _?"    `````??
      ;??````  ```` ; "What case did he plead _?" "I can't stand it";
            ;         "Whose execution did he stay _?"
 ; that's all, based on Alvey
 ); end of *rarely-transitive-verbs*

(defparameter *pseudo-transitive-verbs* (make-hash-table))

(mapcar #'(lambda (x) (setf (gethash x *pseudo-transitive-verbs*) t))
;`````````````````````````````````````````````````````````````
; These are a small subset of the *pred-taking-verbs*
;
; The goal is to identify verbs that can only be followed by a pred,
; not a gapped referential NP, but are not "strictly intransitive",
; in that they can seem to have an NP gap (predicatively interpreted);
; e.g., "Who did the Unabomber turn out to be _?"; "What kind of doctor did
; he become _?", "What shade of red did he blush?"
;
; Based on Alvey v.le "SC_AP" verbs. I also ckecked "(ARITY 2) (PFORM",
; since verbs allowing a pred complement are largely subsumed by those
; allowing a PP complement. However, most verbs that subcategorize for
; a PP needed to be excluded, as they usually diallow an NP as gap filler
; after the verb, i.e., the verb would be followed by the preposition 
; (a PP with an NP gap).
;
; A quick intuitive filter is viability of the question form "What/who did
; they <verb>?", but excluding the numerous instances where <verb> can also 
; simply be transitive, taking a referential NP as object).
;
'(be become blush); "What color did he blush _?"
 ; that's all, based on Alvey
 ); end of *pseudo-transitive-verbs*

(defparameter *pred-taking-verbs* (make-hash-table))

(mapcar #'(lambda (x) (setf (gethash x *pred-taking-verbs*) t))
;`````````````````````````````````````````````````````````````
; These are intended to include all verbs that allow for questions
; starting with a phrase like "how big", i.e., with an ADJP gap as a
; complement, possibly in addition to other complements (NPs, etc.)
; These include the pseudo-transitive verbs. Search Alvey v.le using
; string "C_AP", i.e., subject or object control of an ADJP. This list
; includes some "strictly intransitive" verbs (not taking NP complements)
; like "remain". Not all V[_np_pred] are included; e.g., "called her
; brilliant" is ok, but you can't gap the AP: ??"How smart did he call
; her _?" (cf., "How smart did he consider her _?"; similarly "He came 
; clean", vs *"How clean did he come _?"; some cases are hard to decide,
; e.g., "How smart do you find her _?" is possible but quite unlikely;
; similarly, "How cold do you keep the vaccine _?", "How large does he
; loom _?", etc..
;
; NOTE THE OVERLAP WITH *np+pred-taking-verbs*; the reason is that in
; inserting pred gaps, we may be looking at a passive; e.g., although
; "consider" requires an NP object, in the passive the semantic object is
; in subject position; e.g., "I don't know how smart he is considered _."
; In the gap insertion code, such passives are not distinguished from
; object-less cases, as in "How good did it taste _?".
;
'(appear be become blush consider deem feel get grow keep like look loom
  make plead push remain scrub seal seem set slice smell sound stay
  taste turn wipe); "seal" is doubtful - can't gap "shut" in "seal shut";
  ; "How spicy do you like/want it _?", "How cold did it turn _?";
  ; "How clean was the crime scene wiped _?"
 ); end of *pred-taking-verbs*

(defparameter *np+pred-taking-verbs* (make-hash-table))

(mapcar #'(lambda (x) (setf (gethash x *np+pred-taking-verbs*) t))
;````````````````````````````````````````````````````````````````
'( appoint brand call christen consider deem dub elect keep label
   like make name reckon render scrub see set slice term vote wipe
   ; that's all I got from Alvey; actually, I'm not sure we need this
   ; category for gap-insertion decisions, since these are transitive
   ; verbs (the assumed default category), and the predicate is rarely
   ; gapped: ?"What did you appoint him?"; though "call", "christen"
   ; and "name" do allow this ...; however, we have "How smart do you
   ; reckon him _?", "How sick did the poison render him _?", etc.
 )); end of *np+pred-taking-verbs*

(defparameter *np+pp[pred]-taking-verbs* (make-hash-table))

(mapcar #'(lambda (x) (setf (gethash x *np+pp[pred]-taking-verbs*) t))
;````````````````````````````````````````````````````````````````
; e.g., "considered it of great interest"; "deemed it out of bounds",
;       "kept it in the shed", "liked it with whipped cream", "noticed
;       a tricycle in the driveway", "saw her at the mall", ...
;       (judge, observe, term, think, watch, etc.? hard to disambiguate:
;       "observed it on {the hike, the trunk of the elm tree}")
'(consider deem keep like imagine notice picture see spot term)
 ); end of *np+pp[pred]-taking-verbs*


; For the time being, the following is not needed for gap insertion.
; Consider gapped examples like "What was he disguised as _?", "What did
; you disguise yourself as _?", "As what was he disguised _?", or "As
; what did you disguise yourself _?". in the 1st 2 cases, this is handled
; as completing and object-less preposition with an NP, and the second 2
; as completing a verb with a PP -- and since virtually all verbs allow
; a following PP, this is handled by the "permissive" insertion of a PP
; gap after a verb plus, possibly, an NP
;
(defparameter *pred[as]-taking-verbs* (make-hash-table))

(mapcar #'(lambda (x) (setf (gethash x *pred[as]-taking-verbs*) t))
;````````````````````````````````````````````````````````````````
; e.g., "posed as a salesman", "acted as an intermediary"; Alvey: (PFORM AS)
'(act come_across debut double dress emerge enrol excel figure function 
  masquerade parade part pass pose practice practise precipitate
  qualify rank sit_in serve turn_out )
 ); end of *pred[as]-taking-verbs*

; ** outlier: "differ as to their diet"


(defparameter *np+pred[as]-taking-verbs* (make-hash-table))

(mapcar #'(lambda (x) (setf (gethash x *np+pred[as]-taking-verbs*) t))
;````````````````````````````````````````````````````````````````
'(acclaim acknowledge appoint brand cast categorize certify choose classify
  define denounce deride describe diagnose disguise dismiss dress elect
  employ engage enrol enroll establish esteem hail intend interpret label
  mark market nominate paint palm_off pass_off peg picture portray propose 
  put_down rank rate reckon recognize recommend regard reincarnate reinstate 
  remember render represent reveal rig_out set_up show strike tag take 
  treat view visualize zone) ; based on Alvey; actually, we probably
  ; won't use this category for gap-insertion decisions, since these are
  ; transitive verbs (the assumed default category. But this is useful
  ; for the pred/advp distinction. Ambiguous: "made it as {an actor, a gift
  ; for his mother}"; "posted it as {a precaution, first-class mail}".
 ); end of *np+pred[as]-taking-verbs*

(defparameter *pp+pred[as]-taking-verbs* (make-hash-table))

(mapcar #'(lambda (x) (setf (gethash x *pp+pred[as]-taking-verbs*) t))
;````````````````````````````````````````````````````````````````
; e.g., "thought of her as a guardian angel"; feature OF_AS in Alvey
; e.g., "looked on her as a friend"; feature ON_AS in Alvey;
; e.g., "referred to her as his assistant"; feature TO_AS in Alvey
'(conceive denounce think look refer)
 ); end of *pp+pred[as]-taking-verbs*


(defparameter *whs-taking-verbs* (make-hash-table))

(mapcar #'(lambda (x) (setf (gethash x *whs-taking-verbs*) t))
;`````````````````````````````````````````````````````````````
; This was motivated by contrasts like "I arrived WHEN SHE LEFT", vs
; "I know WHEN SHE LEFT", i.e., ADVP vs whs-nominal. So here I've
; picked a subset of (SUBCAT WHS) verbs from the Alvey lexicon, 
; viz., ones where a when-clause complement can naturally come right
; after the verb and would be interpreted as a nominal in that case.
; That includes some simple transitives that Alvey doesn't list,
; like "abandon", "abbreviate", "abide", "abhor", ..., "love", etc.
; So even though the present list gives the common cases, it may be
; better to include everything but purely transitive verbs and maybe
; some pure PP-taking verbs, like "collude"
'(anticipate arrange ascertain ask assess attest be calculate care check 
                                                ;``` e.g., "this is why he ..."
  choose confide confirm consider contemplate count decide deliberate 
  demonstrate describe determine discern disclose discount discover 
  discuss devine divulge doubt enquire establish exclude explain express 
  fathom feel figure_out find find_out forecast foresee foretell forget 
  guess hash_out hear imagine indicate infer inquire judge 
  know learn let make map mind note notice observe perceive ponder predict 
  preordain prescribe prophesy prove puzzle query question realize realise 
  recall recollect reflect remember say see sense settle show smell 
  sort_out specify state suggest take tell think understand verify 
  watch wonder)
 ); end of *whs-taking-verbs*

(defparameter *np+whs-taking-verbs* (make-hash-table))

(mapcar #'(lambda (x) (setf (gethash x *np+whs-taking-verbs*) t))
;``````````````````````````````````````````````````````````````
; This is largely a subset of the above, but typically interprets
; a whs complement as a nominal even after another np verb argument,
; e.g., "He told Alice when he was leaving". Similarly for "where"
'(ask guarantee pledge promise remind show teach tell)
 ); ed of *np+whs-taking-verbs*

(defparameter *pp+whs-taking-verbs* (make-hash-table))

(mapcar #'(lambda (x) (setf (gethash x *pp+whs-taking-verbs*) t))
;``````````````````````````````````````````````````````````````
; This is largely a subset of *whs-taking-verbs*, but typically interprets
; a whs complement as a nominal even after a pp verb argument, e.g.,
; e.g., "He checked with Alice when she would arrive; "He divulged to
; Alice when he would start work"; "He forecast on Friday when the hurricane
; would arrive"; "He inferred from the schedule when she would arrive";
; "I know from experience when she will finish the job".
'(ascertain check confide confirm consider decide deliberate determine
  disclose divulge enquire establish forecast foresee foretell forget
  guess indicate infer inquire know learn ponder predict prophesy recall
  reveal specify state verify wonder)
 ); end of *pp+whs-taking-verbs*

(defparameter *inf-taking-verbs* (make-hash-table))

(mapcar #'(lambda (x) (setf (gethash x *inf-taking-verbs*) t))
;`````````````````````````````````````````````````````````````
; This includes verbs that take a VP[inf] as first or second complement.
;
; The purpose of this list is to enable likely distinctions between
; infinitives that are stand-alone object complements of the verb, as in
;   "Bob decided to catch a bus"
; and infinitive purpose adverbials, as in
;   "Bob ran to catch the bus",
; which we render in ULF as, e.g., (adv-a (for.p (to (catch.v (the.d bus.n)))))
; Note that for the listed verbs a standalone infinitive is not interpreted as
; an ADVP; if another complement is present, then in ADVP[inf] may follow; e.g.,
;   "He arranged to surprise her" vs. "He arranged the flowers to surprise her",
;    or "He arranged to dine out to surprise her".
; However, for a PP-complement or adjunct before the infinitive, the infinitive
; is still unlikely to be an adverbial, e.g., 
;   "He attempted yesterday jointly with Alice to solve the problem".
; Some ambiguities remain, e.g., "He looks to her to cheer him up" vs
; "He looks at cartoons to cheer him up"; "He trained to be vigilant" vs
; "He trained to stay fit".
'(abide ache afford aim appear arrange ask aspire attempt begin bother burn
  burst care come cease chance choose claim clamor clamour come commence 
  compete condescend connive consent conspire continue contract contrive 
  cooperate covenant crave decide decline deign delight demand deserve desire
  die disdain dread elect endeavor expect fail fear fix forbear forget get 
  grow happen hasten hate have hesitate hope incline intend itch learn like long 
  love manage mean need neglect offer omit pant petition pine plan pledge 
  plot pray prefer prepare presume pretend proceed profess promise propose 
  purport qualify race refuse rejoice remember resolve scheme scorn scramble 
  seek seem set_out stand start strain strive struggle swear tend threaten 
  trouble try undertake venture vote vouchsafe vow wait want wish yearn)
 ); end of *inf-taking-verbs*

(defparameter *inf-tolerating-verbs* (make-hash-table))

(mapcar #'(lambda (x) (setf (gethash x *inf-tolerating-verbs*) t))
;`````````````````````````````````````````````````````````````
; This is an odd little group where the infinitive denotes some sort of 
; emotion-evoking perception or thought. The infinitive seems part of a
; tacit ADVP (but not a purpose-ADVP).
;  "He awoke to see a stranger stand by the tent"
;    (This seems to mean something like, "He awoke and thereupon saw ...")
;  "He blanched to see her so emaciated"
;    (This seems to mean something like "He blanched at seeing her ...",
;    and that also applies to the remaining verbs)
'(awake blanch blush exult shudder tremble )
 ); end of *inf-tolerating-verbs*

; There are also odd uses of ADVP[inf], as in "He retook the exam, only
; to fail again".

(defparameter *np+inf-taking-verbs* (make-hash-table))

(mapcar #'(lambda (x) (setf (gethash x *np+inf-taking-verbs*) t))
;`````````````````````````````````````````````````````````````
; The purpose of this list is to enable likely distinctions between
; infinitives after an NP object that are part of the complement structure
; and those that function as adverbials, e.g.,
;   "Bob presumed/asked her to be nice", vs. "Bob complimented her to be nice"
; In Alvey these are SC_NP_INF (promise) and OC_INF (persuade) verbs.
; Ambiguities remain, e.g., "acknowledged him to be friendly" (i.e., acknow-
; ledged that he was friendly vs. acknowleged him in order to be friendly;
; but a quick google check favors the former (object control) reading). But
; it also depends on whether there's intervening material: "acknowledged him
; just to be friendly", "acknowledged him with a nod {,} to be friendly".
; Similarly, "aided him get the grant" vs. "aided him just to get the grant".
; But intervening "not/never" may still favor the OC reading: "aided him not 
; to get caught". Also, the combination of object type and infinitive type
; matters: "asserted it to be moot", vs. "asserted it {just} to be flippant"
'(acknowledge adjudge adjure advise aid allow appoint
  argue ask assume authorize badger beckon
  beg believe beseech bribe cable cause caution challenge choose
  claim command commission compel condition consider constrain
  convince counsel declare deduce deem defy delegate depress ; "It depressed ..."
  deputize deputise designate desire determine direct discover
  egg_on embolden employ empower enable encourage engage
  enjoin entitle entreat equip estimate exhort expect fancy 
  fear feel find forbid force foreordain get guess hate help 
  imagine impel implore incline induce inspire 
  instruct intend invite judge know lead licence license 
  like love mean motion motivate nag name need 
  nominate notify obligate oblige order pay 
  perceive permit persuade pester petition pick predestinate 
  predestine predetermine predicate predict predispose prefer preordain 
  press pressure presume proclaim profess 
  prompt pronounce prove push qualify reckon 
  recognise recognize remind report request 
  require reveal schedule school select set shape 
  slate spur steel suffer summon summons supplicate suppose surmise 
  suspect teach telegraph telex tell tempt 
  train trouble trust understand urge want warn 
  will wish; I've deleted a lot retroactively; doing statistical
           ; spot checks "suffered it/them TO SO-AND-SO": term or ADVP?
)); end of *np+inf-taking-verbs*

(defparameter *strong-np+inf-taking-verbs* (make-hash-table))

(mapcar #'(lambda (x) (setf (gethash x *strong-np+inf-taking-verbs*) t))
;`````````````````````````````````````````````````````````````````````
; These are a subset of the above np-inf-taking verbs verbs where even 
; material between the NP and the infinitive leaves us with a preference
; for the OC reading (because they strongly select for an infinitive
; complement, following the NP). E.g., "allowed him regularly and without
; much ado to use his tractor". A pretty good test is whether it's likely
; we would use a verb in the following way: "He <verb>ed someone/something
; in order to ..."; if not, it probably belongs here.
'(adjudge adjure allow appoint argue ask assign assume authorize
  avow badger bid beckon beg believe beseech cause caution challenge
  claim command commission compel condition convince counsel declare
  deduce deem delegate designate desire determine discover doom embolden 
  empower enable enjoin entitle entreat equip exert exhort expect fancy 
  fear feel find forbid force foreordain groom guess hate imagine impel 
  implore incline induce influence inspire instigate instruct intend 
  judge know lead licence license like love mean motion motivate name 
  need nominate notify obligate oblige order perceive permit persuade 
  petition predestinate predestine predetermine predict predispose prefer 
  preordain press pressure presume prompt pronounce reckon recognise 
  recognize remind require schedule school select shape slate spur 
  steel suffer supplicate suppose surmise suspect teach telegraph telex 
  tell tempt thank timetable train trouble trust understand urge want 
  warn warrant will wish
 )); end of *strong-np+inf-taking-verbs*

(defparameter *inf-taking-extraposition-verbs* (make-hash-table))

(mapcar #'(lambda (x) (setf (gethash x *inf-taking-extraposition-verbs*) t))
;`````````````````````````````````````````````````````````````
;  We need to distinguish verbs allowing extraposed infinitives 
;    from others, e.g. "It amuses him to watch the capuchin monkeys" vs
;    "He amuses her to try to win her heart". Alvey: OC_INF EQU_EXTRAP
'(amuse annoy astonish astound delight depress disappoint discourage dismay 
  distress disturb embarrass excite fascinate fatigue frighten frustrate 
  gall grieve horrify hurt infuriate inspire interest irk irritate nark 
  nauseate pain peeve perplex perturb petrify pique please rankle relieve 
  rile sadden satisfy shock sicken suit surprise terrify thrill unnerve 
  unsettle upset vex worry wound
)); end of *inf-taking-extraposition-verbs*

; ** We'll also need *pp-inf-taking-verbs*, e.g., "appealed to him to ..."
;    But we really need to particularize the prepositions

; ** I was considering possible *prog-taking-verbs* and *np+prog-taking-verbs*,
;    like those in, e.g., "He came running", or "He sent them packing", 
;    or "He found her sitting on the porch", so that the VP[-ing] wouldn't
;    be reified or made into an adverbial. But now I think this might best 
;    be considered pred-taking or np+pred-taking respectively; maybe 
;    create vp[pred]-taking, np+vp[pred]-taking lists? See SC_ING & OC_ING
;    verbs in Alvey. 
;
;    But I find that I want to treat the subject-control _VP[-ing] verbs
;    in Alvey as taking gerunds (thus nominals) as complements, e.g.,
;    {abide, anticipate, admit, attempt, etc.
;      
;    Concerning OC_ING verbs in Alvey, I guess not all np+vp[pred]-taking 
;    verbs allow other predicates, e.g., "appreciate". So let's enumerate
;    np+prog-taking verbs:
;
(defparameter *np+prog-taking-verbs* (make-hash-table))

(mapcar #'(lambda (x) (setf (gethash x *np+prog-taking-verbs*) t))
;`````````````````````````````````````````````````````````
'(abide anticipate appreciate behold bring catch countenance dread fancy
  find get hate hear imagine keep like mind need notice observe overhear
  prevent remember see send set show smell spy stand start stop visualize
  visualise want watch))
 ; Note: "mind" and "stand" are really only used as OC_ING with negation,
 ;       or in questions, as in "{don't mind, can't stand} him smoking weed".

(defparameter *inf-taking-nouns* (make-hash-table))

(mapcar #'(lambda (x) (setf (gethash x *inf-taking-nouns*) t))
;`````````````````````````````````````````````````````````````
; The purpose of this list is to help determine whether a postnominal infinitive
; is a noun complement or an implicit relative clause; e.g., "the hope to be
; nominated ..." vs., "the student to be nominated"; or "the desire to help"
; vs "the student to help" (mind you, the latter case involves modality and
; a phrase-final gap). I used Alvey n.le with feature VPINF, but this missed
; about half of the 16 I came up with off the top of my head (now added).
; So this may be very incomplete. 
'(
 anxiety association attempt call calling carte_blanche challenge chance claim commission commitment compulsion concession contract craving cry curiosity demand desire determination disposition effort effrontery endeavor endeavour entreaty fitness freedom grace guarantee hope impatience impulse impulsion inability incapacity incentive inclination indisposition inducement injunction instinct intent intention invitation leave means message mission moment monopoly mood motion motivation necessity need notion offer opportunity option order permission power proclivity promise propensity proposal qualifications readiness reason refusal requirement resolution resolve room rush scurry sign signal suggestion temptation tendency threat turn undertaking urge warning way wherewithal will wish yearning yen zeal)
); end of *inf-taking-nouns*

(defparameter *inf-eager-adjectives* (make-hash-table))

(mapcar #'(lambda (x) (setf (gethash x *inf-eager-adjectives*) t))
;`````````````````````````````````````````````````````````````
; Alvey labels them SC_INF (subject-control infinitives)
; I added a few, e.g., "out (to make trouble)"
'(geared_up able about afraid ambitious anxious apt ashamed born bound
 careful certain competent content curious destined determined
 disappointed disinclined disposed doomed due eager elated eligible engaged
 fated fit free frightened game glad greedy happy impatient impotent
 inclined incompetent indisposed ineligible interested keen liable
 licenced licensed like likely loath loth minded nervous out overjoyed
 pleased poised powerless prepared prone proud pushed qualified quick
 raring ready relieved reluctant reputed rumored rumoured scared set
 sorry sure swift thankful unable unlikely unqualified welcome willing 
 wont worthy)
); end of *inf-eager-adjectives*

(defparameter *inf-easy-adjectives* (make-hash-table))

(mapcar #'(lambda (x) (setf (gethash x *inf-easy-adjectives*) t))
;`````````````````````````````````````````````````````````````
; Alvey labels them VPINF (infinitive-complement-taking adjectives)
; Seems quite incomplete, and I've made a few additions
'(characteristic crazy difficult easy gratifying hard impossible 
 incompetent inessential insensitive interesting normal permissible 
 practicable proper right safe set slow sufficient suitable terrible
 thoughtful thoughtless worthy)
 ); end of *inf-easy-adjectives*


(defparameter *prepositions* (make-hash-table))

(mapcar #'(lambda (x) (setf (gethash x *prepositions*) t))
;````````````````````````````````````````````````````````
; Alvey prepositions taking NP complements (plus a handful they missed)
'(aboard about above across after against along amid amidst among amongst 
  around as at before behind below beside between betwixt beyond by down 
  during for from in in_favour_of in_front_of inside into like near of 
  off on on_board onto out outside out_of over past round than through 
  throughout till to together toward towards under underneath until up 
  upon with within without)
); end of *prepositions*

(defparameter *pre-prepositions* (make-hash-table))

(mapcar #'(lambda (x) (setf (gethash x *pre-prepositions*) t))
;````````````````````````````````````````````````````````
; Alvey pre-prepositions taking PP complements
'(out in up down because on off from to )
 ); end of *pre-prepositions*

(defparameter *s-taking-prepositions* (make-hash-table))

(mapcar #'(lambda (x) (setf (gethash x *s-taking-prepositions*) t))
;````````````````````````````````````````````````````````
; Alvey prepositions taking sentential complements
'(after although because before if lest like once otherwise since so
 though till unless until while whilst whereas whereupon when whenever)
 ); end of *s-taking-prepositions*

; The following verb list was aimed at helping to create a predicate
;  ![at-most-weakly-transitive-verb]
; to help repair BLIPP parses such as
;  *"He asked [S [NP his friend] [VP to collaborate with him.]]", or
;  *"They prompted [NP [NP newcomers] [SBAR to pair up]]."
; If the infinitive verb is at most weakly transitive, as for "collaborate"
; and "pair up", the bad S or NP needs to be pried apart.

; HOWEVER, I've so far written a "quick & dirty" version of that predicate,
; using by precluding 
;    *purely-intransitive-verbs*,
;    *rarely-transitive-verbs*, and
;    *pseudo-transitive-verbs*.

; In general, deciding whether an infinitive after an NP-object of a verb
; should attach to the N of the NP object, or to the verb, is pretty subtle.
; E.g., "He found some tasks to keep busy" vs. "He found some tasks to finish".
; This depends both on the affinity of the main verb for a purpose modifier
; or infinitive complement, on the affinity of the object noun for an
; infinitive complement or adjunct, and on the affinity of the infinitive
; verb for an NP object (the stronger the latter affinity, the more likely
; it is that if the infinitive has no NP complement, there's a gap present
; in the infinitive, and it postmodifies the noun).
;
; HOWEVER, I THINK I BACK-BURNERED THIS EFFORT because it seemed that simpler
; heuristics would suffice for the majority of cases; AND BESIDES, THIS
; ISN'T RELEVANT TO COMPUTING ULFS FOR BROWN ENTRIES (WHERE ATTACHMENTS
; SHOULD BE CORRECT), JUST POTENTIALLY FOR CORRECTING BLLIP OR OTHER
; PARSES. STILL, ![at-most-weakly-transitive-verb] is already in use!
; Currently, I define this as being purely intransivie, weakly transitive,
; or pseudo-transitive; I don't know how different this is from what I
; hope to get out of the following data.
;
; Words calculated as weakly transitive to be used to expel rather strongly 
; transitive ones manually, to build an *at-most-weakly-transitive-verbs*
; list from *weakly-transitive-verb-list*: I'll mark the at-most-weakly-
; transitive verbs with asterisks; first I'll do that for verbs where
; it's natural to say "in order to <verb> *PP" (e.g., "in order to adjust
; to the new job with minimum delay"), and extract the starred verbs;
; from the resulting list, I'll remove verbs that tend to attach to a
; preceding N  (think about "picked up something to <verb>" or "contacted
; someone  to <verb>", i.e., these are cases where there's likely to be
; a gap after the verb after all when they occur as N-postmodifying
; infinitives. 

"I didn't mark this down, but I believe this is the *weakly-transitive-verbs*
 list computed in the file extract-data-from-alvey.lisp.

I probably noted that this still includes a lot of quite frequently
transitive verbs, and decided to mark the ones that seem only weakly
transitive with '*', extract these, and filter again. But I never did
the latter. See above for the mental 'tests' to be used for selection."

'(ABATE *ABORT ABRADE *ACCELERATE *ACCLIMATIZE ACCREDIT ACCUMULATE ACIDIFY
 *AD-LIB ADDLE ADJOIN *ADJUST ADORE *ADVERTISE  *AFFILIATE  *AGE AGGLOMERATE
 AIL AIR *ALIGN ALLOW *ALLY ALTER *AMALGAMATE AMASS AMELIORATE AMEND
 AMERICANISE AMERICANIZE AMPUTATE *ANALOGIZE ANGER ANGLE ANGLICISE
 ANGLICIZE ANTE *ANTICIPATE APOSTROPHIZE *APPLAUD *ARM ARTICULATE *ASCEND
 ASCERTAIN ASPHYXIATE *ASSEMBLE ASSESS *ASSOCIATE ATROPHY *ATTACK ATTAIN
 ATTENUATE *ATTEST ATTRIBUTE *AUDITION AUGUR AUTOMATE AVAIL AVOID *AWAKEN
 *BAKE *BALANCE BALLS *BAND *BANQUET *BARNSTORM *BARRACK BASTE *BATH *BATHE
 *BAWL BEAM BECOME BEFALL BEGRUDGE *BELAY *BELCH BELLY *BELLY-LAND BELT
 *BENEFIT BERTH BETTER BLACK BLACKEN BLEACH BLEAT *BLEED BLESS *BLINK
 *BLISTER BLOT BOG BOLLOCKS BOLSTER *BOLT BOMB *BONE BOOB BOOTLEG BORROW
 BOSS BOTANISE BOTANIZE BOTTLE-FEED *BOUND *BOWL *BRAKE BRAZEN *BREAST-FEED
 *BREATHE *BREED BREW BRICK BRIGHTEN BROIL BROWN BRUISE BUCK BUDGE BULK
 BUNCH BUNGLE BURGLE *BURP BUS *BUSHWHACK BUTTON CALCIFY CAMBER *CANCEL
 CANDY CANKER *CANOE *CANTER CANVAS CANVASS *CAPITALISE *CAPSIZE CARBONISE
 CARBONIZE CARD CARRY CARVE CASCADE CASH CATALOG CATALOGUE CAVE
 *CELEBRATE CENTRALISE CENTRALIZE *CHAIN-SMOKE CHALK CHANCE *CHANT CHAP
 CHAPERON CHAPERONE CHAR CHAUFFEUR CHEAPEN *CHEAT *CHEER CHEESE *CHILL
 CHINK CHURN CIPHER *CIRCULATE CIVILISE CIVILIZE *CLACK CLANG CLANK
 CLARIFY CLATTER CLEAN CLEAVE *CLIMAX CLINCH CLINK CLOG CLOT CLOY *CLUB
 *CLUCK CLUTCH COAGULATE COAL COARSEN COCK COLLAPSE COLLECTIVISE
 *COLLECTIVIZE COMMAND COMPERE COMPOSE COMPREHEND COMPUTE *CONCENTRATE
 CONCEPTUALISE CONCEPTUALIZE CONCRETE CONDENSE *CONFEDERATE *CONFER
 CONFLATE CONGEAL CONGEST CONJOIN CONJUGATE *CONK_OUT CONQUER CONSOLIDATE
 CONSTIPATE CONTEST CONTORT CONTOUR CONTRADICT CONTRAST *CONTRIBUTE
 CONTRIVE *CONVENE *COOK *COOL_OFF COOP *COORDINATE CORD CORDON CORNER 
 CORRODE CORRUGATE CORRUPT *COUGH COUNTER *COUNTERATTACK COUNTERBALANCE
 COUNTERFEIT COURT COX COZEN CRACKLE CRANE *CRASH-DIVE *CRASH-LAND CRAYON
 CREOSOTE *CRIB CRIMSON CRINKLE CRISP CRISSCROSS *CROAK *CROCHET CROOK
 CROP CROSS-EXAMINE CROSS-FERTILIZE CROSS-POLLINATE CROSS-REFER
 *CROSSBREED CRUMBLE CRUMPLE CRUSH CRYSTALLISE CRYSTALLIZE CUE CURDLE
 *CURL_UP CURVE *CUSS CYCLOSTYLE CYPHER DAMPEN *DANCE DARESAY DARKEN DART
 DECAY *DECELERATE DECENTRALISE DECENTRALIZE DECIMALISE DECIMALIZE
 DECLARE DECOMPOSE DECREASE DEEPEN DEFER DEFLATE DEFLECT *DEFROST
 DEGRADE DEHYDRATE DEICE DEMOCRATISE DEMOCRATIZE DENT DEPOSE DEPUTISE
 DEPUTIZE DERAIL DERIVE DEROGATE *DESEGREGATE DESERVE DESICCATE DESIRE
 *DESTROY DETERIORATE DETERMINE DETEST DETONATE DEVALUE *DEVELOP DIAL
 *DIET DIFFERENTIATE DIFFUSE DIGEST DIKE DIM DIMINISH DIN DIRTY
 DISAFFILIATE *DISARM *DISBAND DISBELIEVE DISBURSE DISCARD *DISCLAIM
 DISCOLOR DISCOLOUR DISCUSS *DISEMBARK *DISENGAGE DISENTANGLE DISGORGE
 DISINTEGRATE DISLIKE DISMANTLE *DISOBEY *DISPERSE DISPORT *DISROBE
 DISSECT DISSEMBLE DISSIMULATE *DISSIPATE DISTEND DISTIL DISTILL
 DISTINGUISH DISUNITE DIVE-BOMB *DIVERSIFY DODGE DOLE DOLL DONATE DOT
 DOUBLE-CHECK *DOUBLE-PARK DOUBLE-STOP DOUBLE-TALK DRAGOON *DRAIN
 DRAMATISE DRAMATIZE *DRESS DRIFT *DRINK *DRIP-DRY DROPKICK *DRY DULL DUMP
 DYE DYKE EARN EASE *EJACULATE EMBEZZLE *EMBRACE EMBROIDER *EMPLANE
 ENCIPHER *ENLIST *ENPLANE *ENROL *ENROLL *ENTERTAIN ENTRAIN ENUNCIATE ENVY
 ERODE ESCALATE ESSAY ETCH *EVADE *EVANGELISE *EVANGELIZE EVAPORATE EVEN
 *EVOLVE *EXAGGERATE EXCEED EXCRETE *EXHALE EXHIBIT *EXPECTORATE EXPORT
 EXTEND EXUDE FACE FAKE FALTER FAMISH FAN FARROW FATHOM *FEAST FEATURE
 *FEDERATE *FERMENT FETCH FIDGET FIELD FINE FIRE FIRM FLAG *FLAIL FLANNEL
 FLATTEN FLICK FLIP FLOOD *FLOURISH *FOCUS FOG FORESEE FORGIVE FORSWEAR
 FOSSILISE FOSSILIZE FOUL FOUNDER FOX FRACTURE FRAGMENT FRAY FREAK
 FRISK FRIZZLE FROST FROTH FRUCTIFY FRY FUME FUR FURL FUSE *GALLOP
 GANGRENE GAS GASIFY *GATECRASH *GAZUMP *GERMINATE *GIGGLE GINGER GIVE
 GLASS GLEAN GOAD GOBBLE GOUGE *GOVERN GRAB GRANULATE GRAY *GRAZE GREY
 GRILL GRIP GROUP *GROWL GULP HABITUATE *HALT HAM HANDLE *HARMONISE
 *HARMONIZE HARROW HATCH HATE HAZE HEAD *HEAL HEAT *HECKLE HECTOR HEIGHTEN
 HEM HERD *HIDE *HIE *HIKE *HOBBLE HOE HOLD HOOVER *HOP HOT HUSH *HUSTLE
 HYBRIDISE HYBRIDIZE ICE IDEALISE IDEALIZE IGNITE *IMBIBE IMPOSE
 IMPROVISE INCH INCLINE INCREASE *INCUBATE INDEX *INDULGE *INDUSTRIALISE
 *INDUSTRIALIZE INFER INFLATE INFLECT *INHALE INHERIT INK *INTEGRATE
 INTENSIFY *INTERBREED INTERCHANGE INTERJECT *INTERLOCK INTERRUPT
 *INTERSECT *INTERTWINE INTONE INTOXICATE INTRIGUE *INTRUDE INURE *INVADE
 INVESTIGATE INVIGILATE IONISE IONIZE IRON ITALICISE ITALICIZE JACK JAG
 *JAM JANGLE JAZZ JERK JERRY-BUILD JET JIGGLE JINGLE JOCKEY JOGGLE *JOIN
 JOSH JOSTLE KEEN KEYBOARD KID *KILL *KISS *KNIT KNOT LACK LADDER LAP
 *LATHER LAUNCH LAUNDER LAYER LEAGUE LENGTHEN LESSEN LET *LEVITATE
 LIBERALISE LIBERALIZE LICK LIFT LIKE *LIMBER_UP LIQUEFY LIQUIDATE LISP
 LITHOGRAPH *LITIGATE LIVEN LOAN LOATHE LOB *LODGE LOOP LOOSEN *LOOT LOP
 LOUSE LOWER LULL LUMP *LUNCH MACERATE MALT MAP MARKET MASS *MASTICATE
 *MASTURBATE MAT *MATE *MATERIALISE *MATERIALIZE *MATRICULATE *MATURE MELD
 MELIORATE MELLOW *MEND MERIT *METAMORPHOSE METRICISE METRICIZE MILDEW
 MILK MILL MIME MINCE MIND *MINGLE *MINISTER MIRE MISBEHAVE MISCALCULATE
 MISCOUNT MISCUE MISDEAL MISHEAR MISHIT MISTIME MISTRANSLATE
 MISUNDERSTAND MIX *MOBILISE *MOBILIZE MODERATE *MODERNISE *MODERNIZE
 MOISTEN *MOLT MORTIFY *MOULT *MOUTH_OFF *MOW *MUG MUSTER MUTE NAG NAP 
 *NARRATE NARROW NASALISE NASALIZE *NAVIGATE NEAR *NEST NORMALISE NORMALIZE 
 NOTE *OBEY OBTAIN *OBTRUDE OCCASION OFFER *OOZE OPE *OPINE ORBIT ORIGINATE
 OSSIFY OVERACT OVERARCH OVERBALANCE OVERCALL OVERCAPITALISE
 OVERCAPITALIZE OVERCOME OVERDRAW *OVERDRESS OVERESTIMATE OVERHANG
 *OVERLAP OVERMAN OVERPRINT OVERSHOOT *OVERSIMPLIFY OVERSTOCK OVERTAKE
 OVERTRUMP OVERTURN OVERWORK OWN OXIDISE OXIDIZE PACE *PACK *PADDLE
 PADLOCK *PAIR PALATALIZE *PARACHUTE PARAPHRASE PARCEL PARCH PARSE
 PARTICULARISE PARTICULARIZE PASSIVIZE *PASTURE PECULATE *PEDAL PEDDLE
 PEG PENSION PEP *PERCH PERISH PERJURE *PERK_UP PERMEATE PERSONALISE
 PERSONALIZE *PET PETITION *PHONE PHOTOGRAPH *PICKET PIERCE PILFER PINCH
 PINK PIONEER *PLAGIARISE *PLAGIARIZE PLOUGH PLOW PLUG PLY POKE POLE
 POLISH POLITICALISE POLITICALIZE POLITICISE POLITICIZE POLL POSTMARK
 POSTPONE POUND *POUT POWDER *PRACTICE *PRACTISE *PREACH PRECLUDE PREMIERE
 PRESS PRICKLE PRIDE *PRIMP PRINK PRINT PROCESS *PROCREATE PROCURE
 PRODUCE *PROMENADE PROOFREAD PROP PROPAGATE *PROPOSE *PROSELYTISE
 *PROSELYTIZE *PROSPECT *PROSPER PSYCH PSYCHE PUBLISH *PUCKER_UP *PUKE PULP
 PULVERISE PULVERIZE PUNCTURE PURL PURPOSE *PUTREFY PUTT QUARRY QUAVER
 QUERY QUICKEN QUIET QUIETEN RADIATE RADIO RAID *RAMIFY *RANT *RAP
 *RATIONALISE *RATIONALIZE RAVEL *RE-FORM REACH REACTIVATE *READJUST REAP
 REAR *RECANT *RECAP *RECAPITULATE RECEIVE RECESS *RECIPROCATE RECITE RECK
 RECLINE *RECONNOITER *RECONNOITRE *RECONSIDER RECORD RECRUIT RECYCLE
 REDDEN REDECORATE REDOUBLE REECHO *REFEREE REFINE REFIT REFLATE REFLOAT
 *REFORM *REFUEL *REGENERATE *REGISTER *REGROUP *REHEARSE REIN *REJUVENATE
 REKINDLE *RELAX RELISH *RELOAD *REMARRY *REMEMBER REMOUNT REOPEN
 REORGANISE REORGANIZE REPLAY REPORT REPOSE REPRINT *REPRODUCE RESENT
 *RESETTLE RESHUFFLE *RESIGN *RESIST *RESTOCK *RESURFACE RETAIL RETHINK
 RETRACT *RETRENCH *REUNITE REVERSE REVIEW REVISE *REVIVE REVOKE RHYME
 *RIPEN RIPOSTE RIPPLE RISK *ROBE *ROCK ROMANTICISE ROMANTICIZE ROPE
 ROTATE ROUGE ROUGHEN *ROVE RUFFLE RUPTURE RUSH RUST RUSTICATE SACK
 SACRIFICE *SAIL *SALUTE SAVOUR SCALD SCALE SCAN SCAR *SCAVENGE SCISSOR
 SCOUR *SCRATCH *SCREECH SCRIBBLE *SCRIMP SCRUB SCRUNCH SCUFF SCULL
 SCULPTURE *SCUTTLE SEGMENT SEIZE SEMAPHORE SENSE *SEPARATE *SERVE SEVER
 SHANGHAI SHARPEN SHATTER *SHAVE SHEAR SHED SHELL SHEW SHIP *SHIT *SHOOT
 *SHOPLIFT SHORT-CIRCUIT SHORTEN *SHOWER *SHRIEK *SHRIVEL SHRUG SHUFFLE
 SHUNT SHUSH SHUTTLE SICK SIDESTEP SIGHT *SIGHT-READ SILT SILVER *SING
 *SKETCH SKITTER SKITTLE SLACKEN SLAKE SLAM SLANT SLEDGE *SLENDERISE
 *SLENDERIZE SLICK SLIVER *SLOG *SLOUGH SLOW SLUE SLUICE *SLUM SLURP
 *SMARTEN_UP SMASH SMUDGE *SNAKE SNARL SNIP SNORT SNUFF *SOB *SOBER_UP SOFTEN
 SOIL *SOLICIT *SOLIDIFY *SOLILOQUISE *SOLILOQUIZE SOUP SOW SPADE SPANK
 SPARE *SPAWN SPELL *SPEW *SPIN SPIRIT *SPIT *SPLASH_AROUND SPLATTER SPLAY 
 *SPLURGE SPLUTTER SPOOF SPORT SPOUT SPREAD-EAGLE *SPRING-CLEAN *SPUTTER SQUASH
 SQUELCH STAB *STABILISE *STABILIZE STAGGER STAIN STALK *STALL STAMPEDE
 *STARVE STATE STAVE STEADY STEEP STEEPEN STEM STEW STIFFEN STIFLE STING
 STIPPLE STOW *STRAIGHTEN_UP STRANGULATE STRATIFY STREAM STRENGTHEN STRIDE
 STRING *STUDY *STUMP *SUB *SUBCONTRACT SUBDIVIDE *SUBLEASE *SUBLET SUBMERGE
 *SUBSTITUTE *SUCKLE SUE *SUFFOCATE SUM *SUMMER SUPERVISE *SURFACE *SURRENDER
 *SURVIVE SWALLOW SWAY *SWEAT *SWERVE SWIRL SWISH SYNCHRONISE SYNCHRONIZE
 *SYNDICATE TAIL TAILOR TAKE TAN *TAP TAPER TARNISH TART TAT *TATTLE *TAXI
 TEASE TELESCOPE TELEVISE TENDER TENSE TERMINATE TEST *THICKEN *THIEVE
 THIN THRASH THREATEN THRESH *THROTTLE_BACK TICK TICKLE *TIDY_UP TIE TING 
 *TINKLE TITIVATE TITTIVATE TOLL *TOOT TOPPLE TOTAL TOUGHEN *TOUR TRACK *TRAMP
 TRANSFER TRANSGRESS TRAP TRAVERSE *TRAWL *TREAD TREAT TREBLE TRICKLE
 *TRILL TRIPLE *TROOP TRUE TRUMPET TRUNDLE TUG *TUNNEL TURN *TUT-TUT TWANG
 TWINE TWIRL TWITCH TWO-TIME ULCERATE UMPIRE *UNDERACT UNDERESTIMATE
 *UNDRESS UNFIT UNFOLD *UNIONISE *UNIONIZE *UNPACK UNRAVEL UNROLL *UNSADDLE
 UNVEIL *UNWIND UP USHER VACUUM VALET VAMP VAPORISE VAPORIZE *VAULT
 VELARIZE VERBALISE VERBALIZE VERSIFY VICTUAL VITRIFY VITUPERATE
 VIVISECT *VOCALISE *VOCALIZE VOCIFERATE *VOMIT VOUCHSAFE WAFT WAG WAGGLE
 *WAKE *WAKE_UP WAKEN WALLPAPER WARD WARP *WASH WASTE WATER WEAKEN WEARY WEATHER
 *WED *WEEP WELD WHEEZE WHIFF WHILE *WHIMPER WHIRL *WHISPER WHITEN WIGGLE
 WILE *WILT *WIN WINCH *WIND_DOWN WINE WING *WINK WIRE *WITHDRAW WITNESS WOBBLE
 WORSEN WRINKLE *YELL YELLOW *YODEL ZIP)

; I've forgotten what this list is -- it overlaps heavily with the above!
'(*ABORT ACCELERATE ACCLIMATIZE AD-LIB *ADJUST *ADVERTISE AFFILIATE AGE
       *ALIGN ALLY *AMALGAMATE *ANALOGIZE *ANTICIPATE *APPLAUD *ARM ASCEND
       *ASSEMBLE ASSOCIATE *ATTACK ATTEST AUDITION *AWAKEN *BAKE *BALANCE
       BAND BANQUET BARNSTORM BARRACK *BATH *BATHE BAWL *BELAY BELCH
       BELLY-LAND BENEFIT BLEED BLINK BLISTER BOLT *BONE BOUND BOWL
       BRAKE *BREAST-FEED BREATHE *BREED BURP BUSHWHACK *CANCEL CANOE
       CANTER CAPITALISE CAPSIZE *CELEBRATE CHAIN-SMOKE *CHANT *CHEAT
       *CHEER CHILL *CIRCULATE CLACK CLIMAX *CLUB CLUCK *COLLECTIVIZE
       CONCENTRATE CONFEDERATE *CONFER CONK_OUT *CONTRIBUTE CONVENE *COOK
       COOL_OFF COORDINATE COUGH *COUNTERATTACK CRASH-DIVE *CRASH-LAND
       *CRIB CROAK *CROCHET *CROSSBREED CURL_UP *CUSS DANCE DECELERATE
       *DEFROST DESEGREGATE *DESTROY *DEVELOP DIET DISARM DISBAND DISCLAIM
       DISEMBARK DISENGAGE *DISOBEY *DISPERSE DISROBE DISSIPATE DIVERSIFY
       *DOUBLE-PARK *DRAIN DRESS *DRINK *DRIP-DRY *DRY EJACULATE *EMBRACE
       *EMPLANE *ENLIST *ENPLANE *ENROL *ENROLL *ENTERTAIN *EVADE *EVANGELISE
       *EVANGELIZE EVOLVE EXAGGERATE EXHALE EXPECTORATE FEAST *FEDERATE
       *FERMENT FLAIL FLOURISH FOCUS GALLOP GATECRASH GAZUMP GERMINATE
       GIGGLE *GOVERN GRAZE GROWL HALT HARMONISE HARMONIZE *HEAL *HECKLE
       *HIDE HIE HIKE *HOBBLE HOP HUSTLE *IMBIBE *INCUBATE INDULGE
       *INDUSTRIALISE *INDUSTRIALIZE *INHALE INTEGRATE INTERBREED
       INTERLOCK INTERSECT *INTERTWINE INTRUDE *INVADE JAM *JOIN *KILL *KISS
       KNIT LATHER LEVITATE LIMBER_UP LITIGATE LODGE *LOOT LUNCH
       MASTICATE MASTURBATE MATE MATERIALISE MATERIALIZE MATRICULATE
       MATURE *MEND METAMORPHOSE MINGLE MINISTER *MOBILISE *MOBILIZE
       *MODERNISE *MODERNIZE MOLT MOULT MOUTH_OFF *MOW MUG *NARRATE
       *NAVIGATE NEST *OBEY OBTRUDE OOZE OPINE OVERDRESS OVERLAP
       *OVERSIMPLIFY *PACK *PADDLE *PAIR PARACHUTE PASTURE *PEDAL PERCH
       PERK_UP *PET *PHONE *PICKET *PLAGIARISE *PLAGIARIZE POUT *PRACTICE
       *PRACTISE PREACH PRIMP PROCREATE *PROMENADE *PROPOSE PROSELYTISE
       PROSELYTIZE PROSPECT PROSPER PUCKER_UP PUKE PUTREFY RAMIFY RANT
       RAP *RATIONALISE *RATIONALIZE *RE-FORM *READJUST RECANT RECAP
       RECAPITULATE RECIPROCATE RECONNOITER RECONNOITRE *RECONSIDER
       *REFEREE *REFORM REFUEL *REGENERATE *REGISTER REGROUP *REHEARSE
       *REJUVENATE RELAX *RELOAD REMARRY *REMEMBER REPRODUCE RESETTLE
       RESIGN *RESIST RESTOCK RESURFACE RETRENCH *REUNITE *REVIVE RIPEN
       *ROBE *ROCK ROVE *SAIL *SALUTE SCAVENGE *SCRATCH SCREECH SCRIMP
       SCUTTLE *SEPARATE *SERVE *SHAVE SHIT *SHOOT *SHOPLIFT SHOWER SHRIEK
       SHRIVEL *SIGHT-READ *SING *SKETCH SLENDERISE SLENDERIZE SLOG SLOUGH
       SLUM SMARTEN_UP SNAKE SOB SOBER_UP SOLICIT *SOLIDIFY SOLILOQUISE
       SOLILOQUIZE SPAWN *SPEW *SPIN SPIT SPLASH_AROUND SPLURGE
       *SPRING-CLEAN SPUTTER *STABILISE *STABILIZE STALL *STARVE
       STRAIGHTEN_UP *STUDY STUMP SUB SUBCONTRACT *SUBLEASE *SUBLET
       *SUBSTITUTE SUCKLE *SUFFOCATE SUMMER SURFACE SURRENDER SURVIVE
       SWEAT SWERVE SYNDICATE *TAP TATTLE TAXI *THICKEN THIEVE
       THROTTLE_BACK *TIDY_UP TINKLE *TOOT *TOUR TRAMP TRAWL TREAD TRILL
       TROOP TUNNEL TUT-TUT UNDERACT *UNDRESS *UNIONISE *UNIONIZE *UNPACK
       UNSADDLE UNWIND *VAULT *VOCALISE *VOCALIZE VOMIT *WAKE *WAKE_UP *WASH
       *WED WEEP WHIMPER WHISPER WILT *WIN *WIND_DOWN WINK *WITHDRAW YELL
       YODEL)

; Very weakly transitive verbs, unlikely to form an infinitive with
; an object-gap that attaches to a preceding noun. E.g., "He had time
; to adjust"
'(ABORT ADJUST ADVERTISE ALIGN AMALGAMATE ANALOGIZE ANTICIPATE APPLAUD
       ARM ASSEMBLE ATTACK AWAKEN BAKE BALANCE BATH BATHE BELAY BONE
       BREAST-FEED BREED CANCEL CELEBRATE CHANT CHEAT CHEER CIRCULATE
       CLUB COLLECTIVIZE CONFER CONTRIBUTE COOK COUNTERATTACK
       CRASH-LAND CRIB CROCHET CROSSBREED CUSS DEFROST DESTROY DEVELOP
       DISOBEY DISPERSE DOUBLE-PARK DRAIN DRINK DRIP-DRY DRY EMBRACE
       EMPLANE ENLIST ENPLANE ENROL ENROLL ENTERTAIN EVADE EVANGELISE
       EVANGELIZE FEDERATE FERMENT GOVERN HEAL HECKLE HIDE HOBBLE
       IMBIBE INCUBATE INDUSTRIALISE INDUSTRIALIZE INHALE INTERTWINE
       INVADE JOIN KILL KISS LOOT MEND MOBILISE MOBILIZE MODERNISE
       MODERNIZE MOW NARRATE NAVIGATE OBEY OVERSIMPLIFY PACK PADDLE
       PAIR PEDAL PET PHONE PICKET PLAGIARISE PLAGIARIZE PRACTICE
       PRACTISE PROMENADE PROPOSE RATIONALISE RATIONALIZE RE-FORM
       READJUST RECONSIDER REFEREE REFORM REGENERATE REGISTER REHEARSE
       REJUVENATE RELOAD REMEMBER RESIST REUNITE REVIVE ROBE ROCK SAIL
       SALUTE SCRATCH SEPARATE SERVE SHAVE SHOOT SHOPLIFT SIGHT-READ
       SING SKETCH SOLIDIFY SPEW SPIN SPRING-CLEAN STABILISE STABILIZE
       STARVE STUDY SUBLEASE SUBLET SUBSTITUTE SUFFOCATE TAP THICKEN
       TIDY_UP TOOT TOUR UNDRESS UNIONISE UNIONIZE UNPACK VAULT
       VOCALISE VOCALIZE WAKE WAKE_UP WASH WED WIN WIND_DOWN WITHDRAW)


