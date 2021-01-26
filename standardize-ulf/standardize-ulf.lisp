;;; Gene Kim, 2020-10-16
;;;
;;; Standardize ULFs parsed by Len's ULF parser to the annotation standard.
;;; This will enable better type analysis and improve interface with other ULF
;;; tools.

(in-package :ulf-nlog)

(defun remove-token-index (idxsym &key (delim #\~))
  "Removes the indexing from a symbol token, assuming the indexing is provided
  with a delimiter at the end of the symbol. Default delimiter, tilde."
  (let ((symstr (symbol-name idxsym)))
    (intern (subseq symstr 0 (position delim symstr :from-end t))
            :ulf-nlog)))

(defun remove-token-indices (idxulf &key (delim #\~))
  "Removes the indexings from a ulf, assuming the indexing is provided with a
  delimiter at the end of the symbol. Default delimiter, tilde."
  (cond
    ((null idxulf) idxulf)
    ((atom idxulf) (remove-token-index idxulf :delim delim))
    (t (mapcar #'(lambda (x) (remove-token-indices x :delim delim))
               idxulf))))

;; TODO: move this to utilities
(defun count-elems (elements &key (test #'equal))
  (let ((frequencies (make-hash-table :test test))
        (bags (make-hash-table :test #'eql)))
    (dolist (e elements) (incf (gethash e frequencies 0)))
    (maphash (lambda (k v) (push k (gethash v bags))) frequencies)
    (values bags frequencies)))

(defun lemmatize-len-aux! (aux)
  "Lemmatizes surface-form auxiliary symbols using the pattern.en library and
  adds the appropriate tense. 'be' and 'have' are converted to verbs and other
  auxiliaries are simply mapped to aux-v since this is a stronger assumption.
  If tense is already available, just use that.
  The function assumes that the input, `aux` is either a symbol or a pair of
  tense and aux symbols.
  e.g.
    is.aux -> (pres be.v)
    was.aux -> (past be.v)
    have.aux -> (pres have.v)
    had.aux -> (past have.v)
    did.aux -> (past do.aux-v)
    (pres can.aux) -> (pres can.aux-v)
  "
  ;; Set up the pattern en package link in ulf2english if not yet ready.
  ;; TODO: move this to a more universal location.
  (when (not ulf2english::*setup-complete*)
    (ulf2english::setup-pattern-en-env #'ulf2english::python-over-py4cl))

  ;; No change if the format doesn't match the expected format.
  (when (not (or (len-aux? aux)
                 (and (listp aux) (= (length aux) 2)
                      (lex-tense? (first aux))
                      (len-aux? (second aux)))))
    (return-from lemmatize-len-aux! aux))

  (labels
    ((get-tense (wrd)
       (let* (;; Returns a list of lists where the inner list is the full conjugation info.
              (tenses (python-eval
                        (let ((*package* (find-package :ulf-nlog)))
                          (format nil "list(tenses(\"~s\"))" wrd))))
              ;; Pull out tenses, first element, and count.
              (tense-counts (count-elems (mapcar #'first (coerce tenses 'list))))
              ;; Get highest count.
              (max-tense-count (apply #'max (alexandria:hash-table-keys tense-counts)))
              ;; Get first corresponding tense.
              (mode-tense (read-from-string
                            (first (gethash max-tense-count tense-counts))))
              ;; Convert tense to ULF format.
              (ulf-tense (cdrassoc mode-tense '((present . pres)
                                                (infinitive . pres)
                                                (past . past)))))
         ulf-tense))) ; end of labels defs
    (let*
      ((sym (if (atom aux) aux (second aux)))
       (sympair (multiple-value-list (split-by-suffix sym)))
       (wrd (first sympair))
       (lemma (read-from-string
                (python-eval
                  (let ((*package* (find-package :ulf-nlog)))
                    (format nil "str(lemma(\"~s\"))" wrd)))))
       ;; Tense
       (ulf-tense (if (atom aux) (get-tense wrd) (first aux)))
       ;; Determine suffix.
       (ulf-suffix (case lemma
                     (be 'v)
                     (have 'v)
                     (otherwise 'aux-v))))

      ;; Build.
      (list ulf-tense (add-suffix lemma ulf-suffix)))))

;;
;; Len's parser-specific suffix matching.
;;
(defun len-aux? (inx)
  (in-intern (inx x :ulf-nlog)
    (equal 'aux (nth-value 1 (split-by-suffix x)))))

(defun len-adv? (inx)
  (in-intern (inx x :ulf-nlog)
    (equal 'adv (nth-value 1 (split-by-suffix x)))))

(defun replace-suffix! (sym new-suffix)
  (multiple-value-bind (lemma _) (split-by-suffix sym)
    (declare (ignore _))
    (add-suffix lemma new-suffix)))

(defun rel-pro? (sym)
  "Returns t if the symbol is a possible relativizer with a pronoun suffix."
  (multiple-value-bind (lemma suffix) (split-by-suffix sym)
    (and (eql suffix 'pro)
         (member lemma '(who that tht which whom what when where whose)))))

(defparameter *determiners*
  ; TODO: look at wiktionary data to get full list.
  '(all every some a an many most much few several the no that which my her his your their))

(defun adj-det? (sym)
  "Returns t if the symbol is a possible determiner with an adjective suffix."
  (multiple-value-bind (lemma suffix) (split-by-suffix sym)
    (and (eql suffix 'a)
         (member lemma *determiners*))))
(defun pro-det? (sym)
  "Returns t if the symbol is a possible determiner with a pronoun suffix."
  (multiple-value-bind (lemma suffix) (split-by-suffix sym)
    (and (eql suffix 'pro)
         (member lemma *determiners*))))

(defun remove-vp-tense! (vp)
  "Removes the tense from the head verb of the ULF verb phrase."
  (let ((vphead (find-vp-head vp :callpkg :ulf-nlog)))
    (cond
      ((atom vphead) vp) ; already not tensed
      ((and (listp vphead) (= 2 (length vphead)) (lex-tense? (first vphead)))
       (replace-vp-head vp (second vphead) :callpkg :ulf-nlog))
      (t
        (error "Unknown VP head structure: ~s~%    Source VP: ~s~%" vphead vp)))))

(defun tensed-vp? (vp)
  (not (atom (find-vp-head vp :callpkg :ulf-nlog))))

(defun lex-to-noun! (lex-adj)
  "Converts a lexical ULF item to a noun."
  (add-suffix (split-by-suffix lex-adj) 'n))

(defun lex-unknown? (term)
  "Returns whether the given term is a lexical and of an unknown type."
  (and (atom term)
       (let ((types (ulf:phrasal-ulf-type? term :callpkg :ulf-nlog)))
         (equal types '(unknown)))))

(defun add-bars! (term)
  "Add bars around a symbol by adding a space before intern."
  (intern
    (concatenate 'string " " (symbol-name term))
    :ulf-nlog))

(defun merge-lex-names! (names)
  (intern (apply 'concatenate
                 (cons 'string (mapcar #'symbol-name names)))
          :ulf-nlog))

;; Removes periods from ULFs.
(defparameter *ttt-remove-periods*
    '(/ (_*1 (! \. (\.)) _*2)
       (_*1 _*2)))

;; Removes double parenthesis.
(defparameter *ttt-remove-parenthesis*
    '(/ ((_+)) (_+)))

;; Removes all punctuations.
(defparameter *ttt-remove-punctuations*
  '(/ (_*1 punct? _*2)  
      (_*1 _*2)))

(defparameter *punct-list*
    '(\: \' \. \, \- \_ \{ \} \[ \] \~
      \;
     )) 

(defun punct? (x)
  (in-intern (x new-x :ulf-nlog) (if (member new-x *punct-list*) t nil)))

;; Fixing the possessives form

;; n+preds singular pred case
(defparameter *ttt-fix-possessives-sing*
  '(/ ((det? (n+preds _!1 |'S|)) _!2)
    (((det? _!1) 's) _!2)))

;; n+preds plural preds case
(defparameter *ttt-fix-possessives-pl*
  '(/ ((det? (n+preds _!1 _+ |'S|)) _!2)
    (((det? (n+preds _!1 _+)) 's) _!2)))

;; Rules used for performing domain-specific fixes.
(defparameter *ttt-ulf-fixes*
  (list
    ;; N+PREDS bug.
    '(/ n+pred n+preds)
    ;; Negation
    '(/ (! not.adv |N'T.ADV|) not)
    ;; Add tense, lemmatize, and canonicalize suffix for auxiliaries.
    '(/ (! (lex-tense? len-aux?))
        (lemmatize-len-aux! !))
    '(/ (! (_*1 len-aux? _*2) ~ (lex-tense? len-aux?))
        (_*1 (lemmatize-len-aux! len-aux?) _*2))

    ;; Remove tense under ka/to operators.
    '(/ ((!1 to ka) tensed-vp?)
        (!1 (remove-vp-tense! tensed-vp?)))

    ;; Fix head-less determiners.
    '(/ (det? adj?)
        (det? (adj? {ref}.n)))

    ;; Convert adjective modifiers of implicit nouns to nouns.
    '(/ (lex-adjective? {ref}.n)
        (lex-to-noun! lex-adjective?))

    ;; Modifiers.
    ;; (ADV A) -> (MOD-A A)
    '(/ (len-adv? (! adj? pp?))
        ((replace-suffix! len-adv? mod-a)
         !))
    ;; (ADV N) -> (MOD-N N)
    '(/ (len-adv? noun?)
        ((replace-suffix! len-adv? mod-n)
         noun?))

    ;; Quantifiers
    ;; (K (QUANT.A ...)) -> (QUANT.D ...)
    '(/ (k (adj-det? noun?))
        ((replace-suffix! adj-det? d)
         noun?))
    ;; (QUANT.PRO N) -> (QUANT.D N)
    '(/ (pro-det? noun?)
        ((replace-suffix! pro-det? d)
         noun?))

    ;; Introduce N+PREDS
    ;; ((k/Q X) PRED) -> (k/Q (N+PREDS X PRED))
    '(/ (((!1 det? k) (!2 ~ (n+preds _*))) (!3 pred? ~ verb? tensed-verb?))
        (!1 (n+preds !2 !3)))

    ;; (V ... ADV ...) -> (V ... ADV-A ...)
    ;; This makes the stricter assumption about modification, since this can't
    ;; float around. Also, semantically a verb adverbial count be operate at
    ;; sentence-level semantics, but not vice-versa.
    '(/ ((! verb? tensed-verb?) _*1 len-adv? _*2)
        (! _*1
           (replace-suffix! len-adv? adv-a)
           _*2))

    ;; Fix unsuffixed (or barred) atoms.
    '(/ lex-unknown? (add-bars! lex-unknown?))

    ;; Merge adjacent names that aren't a part of coordinated terms.
    '(/ (! (_*1 (<> lex-name? (+ lex-name?)) _*2)
           ~ ((! set-of lex-coord?) _* (<> lex-name? (+ lex-name?)) _*)
             (_* (<> lex-name? (+ lex-name?)) _* lex-coord? _+))
        (_*1 (merge-lex-names! (<>)) _*2))

    ;; DETERMINERS
    ;; (<D> x ...) -> (<D> (x ...))
    '(/ (det? _! _+) (det? (_! _+)))

    ;; RELATIVIZERS
    ;; (n+preds <N> (rel-pro ..)) ->  (n+preds <N> (rel ...))
    '(/ (n+preds noun? _*1 (rel-pro? _+) _*2)
        (n+preds noun?
                 _*1
                 ((replace-suffix! rel-pro? rel) ; replace pro with rel
                  _+)
                 _*2))
    ;; ((k/<D> <N>) (rel-pro ..)) -> (k/<D> (n+preds <N> (rel ..)))
    '(/ (((! k det?) noun?) (rel-pro? _+))
        (! (n+preds noun? ((replace-suffix! rel-pro? rel) _+))))

    ;; ((pres be.v) <term>) -> ((pres be.v) (= <term>))
    '(/ ((!1 (lex-tense? be.v)) term?)
        (!1 (= term?)))

    ;; Removing periods from ULFs.
    *ttt-remove-periods*  
    
    ;; Removing double parenthesis from ULFs.
    *ttt-remove-parenthesis*

    ;; Fixing the possessives form
    *ttt-fix-possessives-sing*
    *ttt-fix-possessives-pl*
    ))

(defun fix-ulf (ulf)
  "Fixes the parsed ULF with domain-specific fixes which may not generalize
  outside of this package. Assumes the token-indexing has already been
  removed."
  (ttt:apply-rules *ttt-ulf-fixes* ulf :deepest t))

(defun canonicalize (ulf)
  "Canonicalizes ULF variations for the ulf-nlog package.
  Operations:
  1. Lift negations
  "
  (lift-not ulf))

(defparameter *debug-parse-ulf* nil)

(defun parse-ulf (str)
  "Parse an English sentence into ULF.
  Uses the lenulf package for the core annotation, which is then post-processed."
  (let*
    ((rawparse
       ;; The lenulf package requires *package* to be :lenulf
       ;; TODO: find a solution that is lenulf internal.
       ;; TODO: remove intern-symbols-recursive and add a callpkg argument to lenulf's external functions.
       (let ((*package* (find-package :lenulf)))
                 (intern-symbols-recursive (lenulf:english-to-ulf str)
                                           :ulf-nlog)))
     (token-removed (remove-token-indices rawparse))
     (fixed (fix-ulf token-removed))
     (canon (canonicalize fixed)))
    (when *debug-parse-ulf*
      (format t "======parse-ulf stages======~%")
      (format t "raw: ~s~%token-removed: ~s~%fixed: ~s~%canon: ~s~%"
              rawparse token-removed fixed canon))
    canon))

;;
;; PROCESSING RULES THAT SHOULD BE MOVED TO ULF-LIB ONCE COMPLETE
;;

(defun direct-not-stop? (x)
  "Predicate for operators that stop 'not' directly below it."
  (or (advformer? x)
      (detformer? x)
      (modformer? x)
      (sent-reifier? x)
      (noun-reifier? x)
      (verb-reifier? x)
      (tensed-sent-reifier? x)))

(defun arg-not-stop? (x)
  "Predicate for operators that stop 'not' at its arguments."
  (member x '(n+preds np+preds sub rep qt-attr)))

(defun not-stop? (x) (or (direct-not-stop? x) (arg-not-stop? x)))

(defparameter *ulf-nots* '(not not.adv-s))
(defun ulf-not? (x) (member x *ulf-nots*))

(defun lift-not (ulf)
  "Lift each \"not\" in the given ULF to the appropriate, non-floating level.
  The negation may still not be operating over a sentence if it is locally
  modifying a predicate or verb phrase. This is either explicitly marked with
  local bracketing or may occur because a type-shifter or macro stops further
  lifting.
  Examples:
    (not (every.d person.n) (pres know.v))
    -> (not ((every.d person.n) (pres know.v)))
    (he.pro ((pres know.v) (that (i.pro ((pres be.v) not green.a)))))
    -> (he.pro ((pres know.v) (that (not (i.pro ((pres be.v) green.a))))))
    (i.pro ((pres know.v) (to (not sleep.v))))
    -> (i.pro ((pres know.v) (to (not sleep.v))))
    (the.d (n+preds man.n (sub tht.rel (i.pro ((pres do.aux-s) not (know.v *h))))))
    -> (the.d (n+preds man.n (sub tht.rel (not (i.pro ((pres do.aux-s) (know.v *h)))))))
  High-level recursive algorithm description:
    Call with a ULF, returns a pair
     1. The ULF with all negations lifted or removed if lifted position is not found.
     2. The negations that need placing.
    Base case:
     - atom, return atom and nil
    Recursive case:
     Call recursively on all list elements
     - if there are negations to be placed and current list starts with a lift-stopping operator, place negations to be lifted appropriately,
     There are two types of lift-stopping operator, one which placed negations
     right below it (e.g. type-shifters) and those that place them right
     around one of its arguments (e.g. n+preds).
     - list of length >2 with a negation in it, extract negations and add to lists before returning
    At the top-level just add on all the negations.
  "
  (labels
    ((add-negs (expr negs)
       (reduce #'list negs :initial-value expr :from-end t))

     (rechelper (form)
       (cond
         ;; Base case
         ((atom form)
          (list form nil))
         ;; Simple recursive case (no lift-stopping and length <= 2 or no negations)
         ;; Recurse and return the removed ULFs together and append negation lists.
         ((and (not (not-stop? (first form)))
               (or (<= (length form) 2)
                   (not (find-if #'ulf-not? form))))
          (let ((recres (mapcar #'rechelper form)))
            (list (mapcar #'first recres)
                  (apply #'append (mapcar #'second recres)))))
         ;; Simple-ish recursive case (no lift-stopping but has negations)
         ;; Recurse and remove current negations.
         ((not (not-stop? (first form)))
          ;; TODO: optimize with single pass split (i think I have a function for this already in util)
          (let* ((nonot (remove-if #'ulf-not? form))
                 (nots (remove-if-not #'ulf-not? form))
                 (recres (rechelper nonot))
                 (reculf (first recres))
                 (recnots (second recres)))
            (list reculf (append nots recnots))))
         ;; Direct lift-stopping
         ((direct-not-stop? (first form))
          (let* ((nonot (remove-if #'ulf-not? form))
                 (nots (remove-if-not #'ulf-not? form))
                 (recres (mapcar #'rechelper nonot))
                 (newulf (mapcar #'first recres))
                 (allnots (append nots (apply #'append (mapcar #'second recres)))))
            (assert (= (length newulf) 2) (newulf)
                    "All direct not stopping operators must have a single argument: ~s~%" newulf)
            (list
              ;; ULF with negations added below operator.
              (list (car newulf)
                    (add-negs (second newulf) allnots))
              nil)))
         ;; Argument lift-stopping.
         ;; NB: negations not in the arguments are lifted.
         (t
          (let* ((nonot (remove-if #'ulf-not? form))
                 (nots (remove-if-not #'ulf-not? form))
                 (recres (mapcar #'rechelper nonot))
                 (negadded-args
                   (mapcar
                     #'(lambda (argres) (add-negs (first argres) (second argres)))
                     (cdr recres))))
            (list
              ;; ULF with negation in args
              (cons (first form) negadded-args)
              nots))))) ; end of rechelper
     ) ; end of labels definitions

    ; labels body
    ; Recurse and add in any stray negations.
    (let* ((recres (rechelper ulf))
           (newulf (first recres))
           (nots (second recres)))
      (add-negs newulf nots))))
