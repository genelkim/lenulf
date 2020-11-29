;; Copied over from "elf-from-sentences.lisp"                      Sep 20/20
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run Charniak parser
;; Jonathan Gordon, 2011-12-19
;; parse-all and sep-sentences added by LKS
;; support for the Berkeley parser was added by Gene Louis Kim

(in-package :lenulf)

;; Example:
;; (parse "This is a test.")

(defparameter *parser* "/p/nl/tools/bllip-parser/first-stage/PARSE/parseIt")
;                         ^^^^ changed back from krr (Jan 12); nl had been out of space

;; This is the traditional Charniak language model.
;; McClosky's (slower to load but more accurate) model is
;;   /p/nl/tools/bllip-parser/first-stage/DATA/
;;      ^^ now krr, at least until nl is enlarged; but the reranking
;;         parser is still in nl
(defparameter *pdata* "/p/nl/tools/reranking-parser/first-stage/DATA/EN/")

;; Flag for loading the K&K parser if necessary.
(defparameter *k&k-setup-complete* nil)
;; Flag for loading the K&M parser if necessary.
(defparameter *k&m-setup-complete* nil)
;; TODO: set this to cloned directory in deps.
(defparameter *k&m-path* "/u/gkim21/research/berkeley-parser/self-attentive-parser/src/")
;; TODO: change this to correct relative path after setting up download script.
(defparameter *k&m-pretrained-model* "/u/gkim21/research/berkeley-parser/k&m/model/model-BERT_dev=94.81.pt")
(defparameter *k&m-dict* "/u/gkim21/research/berkeley-parser/k&m/model/dict")


(defun parse-all (str)
; Here we allow str to consist of multiple sentences separated by
; one of {. ? !}. Return a list of (successive) sentence parses.
  (let ((strings (sep-sentences str)))
    (mapcar #'parse strings)))

(defun sep-sentences (str)
; Produce a list of strings corresponding to sentences in str
; separated by one of {. ? !}.
  (let ((chars (coerce str 'list)) sent sents ch)
    (loop (setq sent nil)
          (loop (if (char= (first chars) #\Space); drop initial blanks,
                    (pop chars) (return nil)))   ; then exit loop
          (if (null chars) (return-from sep-sentences (reverse sents)))
          (loop (setq ch (pop chars))
                (push ch sent)
                (if ; punctuation followed by a blank?
                    (and (member ch '(#\. #\? #\!))
                         (char= (first chars) #\Space))
                         (return nil)) ; exit loop
                (if (null chars) (return nil)))
          (push (coerce (reverse sent) 'string) sents)
          (if (null chars)
              (return-from sep-sentences (reverse sents))))
 )); end of sep-sentences


(defun parse (str &key (parser "BLLIP"))
;; Here str (a string) is assumed to be a single sentence
;; The keyword argument can be "bllip", or "k&k", case-insensitive.
  (cond
    ;; BLLIP parser
    ;; We create a new file via a 'let', to be used for the stream,
    ;; and delete it at the end.
    ((equal (string-upcase parser) "BLLIP")
     (let ((filename (format nil "~a.txt" (gensym)))
           (result))
       (with-open-file (to-parse filename :direction :output
                                          :if-exists :supersede)
         (format to-parse (preproc-for-parse str)))
       (setf result (lispify-parser-output
                     (output-from-cmd (format nil "~a ~a ~a"
                                              *parser* *pdata* filename))))
       (delete-file filename)
       result))
    ;; K&K parser (basic Berkeley parser)
    ((equal (string-upcase parser) "K&K")
     (when (not *k&k-setup-complete*)
       (format t "Loading K&K parser...")
       (finish-output)
       (py4cl:python-exec "import benepar")
       (py4cl:python-exec "benepar.download('benepar_en2')")
       (py4cl:python-exec "parser = benepar.Parser('benepar_en2')")
       (setf *k&k-setup-complete* t)
       (format t "Done!~%"))
     (lispify-parser-output
       (py4cl:python-eval (format nil "str(parser.parse(\"~a\"))" str))))
    ;; K&M parser (Kato and Matsubara gap filling).
    ((equal (string-upcase parser) "K&M")
     (parse-km str))
    (t (error "Unknown parser selection: ~a~%" parser))))

(defun parse-km (str)
;; Calls the pretrained K&K parser for K&M through python. This is slightly
;; more complicated than the standard K&K parser since it isn't part of the
;; benepar package. Then post-processes it with the Lisp package for recovering
;; the PTB format. This function is independently implemented since it's
;; complicated enough already.
  (when (not *k&m-setup-complete*)
    (format t "Loading K&M parser...")
    (finish-output) ; flush.
    ;; Set up nltk tokenizer.
    (py4cl:python-exec "import nltk")
    (py4cl:python-exec "nltk.download('punkt')")
    (py4cl:python-exec "from nltk.tokenize import word_tokenize")
    ;; Add the parser code to the system path (since it isn't packaged and installed).
    (py4cl:python-exec "import sys")
    (py4cl:python-exec (format nil "sys.path.append(\"~a\")" *k&m-path*))
    ;; Load up parser.
    (py4cl:python-exec "import torch")
    (py4cl:python-exec "import parse_nk")
    (py4cl:python-exec (format nil
                               "info = (torch.load(\"~a\") if torch.cuda.is_available() else torch.load(\"~a\", map_location=lambda storage, location: storage))"
                               *k&m-pretrained-model*
                               *k&m-pretrained-model*))
    (py4cl:python-exec "parser = parse_nk.NKChartParser.from_spec(info['spec'], info['state_dict'])")
    (setf *k&m-setup-complete* t)
    (format t "Done!~%"))

  ;; Parse sentence.
  (py4cl:python-exec (format nil "tokens = word_tokenize(\"~a\")" str))
  (py4cl:python-exec "predicted, _ = parser.parse([('UNK', token) for token in tokens])")
  (let ((pyout (py4cl:python-eval "predicted.convert().linearize()"))
        (mid-file (format nil "~a.txt" (gensym)))
        (end-file (format nil "~a.txt" (gensym)))
        result)
    ;; TODO: speed this up by calling the cf->ptb internal code directly rather than through the file system.
    (with-open-file (to-recover mid-file :direction :output
                                         :if-exists :supersede)
      (format to-recover pyout))
    (ptb2cf:cf->ptb :cf mid-file :ptb end-file :dictionary *k&m-dict*)
    (setf result (lispify-parser-output
                   (with-open-file (s end-file)
                     (let ((data (make-string (file-length s))))
                       (read-sequence data s)
                       data))))
    (delete-file mid-file)
    (delete-file end-file)
    result))


;; prefix sentence string with <s>, postfix with </s>
(defun preproc-for-parse (str)
  (format nil "<s> ~a </s>" str))

(defun exec-from-command (command)
	(car (split-str command " ")))

(defun args-from-command (command)
	(cdr (split-str command " ")))

(defun split-str (str sep)
	(cond
		((null (search sep str))
		 (list str))

		(t (append
				 (list (subseq str 0 (search sep str)))
				 (split-str (subseq str (+ 1 (search sep str)) (length str)) sep)))))

;; Adapted from 'port.lisp' from 'asdf-install'.
;; Comment by LKS: The loop syntax here is an extension of the basic
;; loop syntax; the latter allows a sequence of nonatomic list structures
;; to be repeatedly evaluated until there's a return or throw. I find
;; the extended syntax unpalatable, totally out of sync with the spirit
;; of Lisp. I haven't tried it, but I suspect the same result would be
;; obtained with the standard lisp loop syntax
;;    (loop (let ((line (read-line stream nil)))
;;          (if line (write-line line out-stream)
;;                   (return))))
(defun output-from-cmd (command);; 'command' is a string, to be interpreted
                                ;; as a command (used here to invoke the
                                ;; Charniak parser on a sentence string
                                ;; bracketed by <s> ... </s>)
  "Returns a string containing the result of executing the command."
  (with-output-to-string (out-stream)
    #+SBCL
    (let* ((process (sb-ext:run-program
                      (exec-from-command command)
                      (args-from-command command)
                      :output :stream :wait nil :directory *default-pathname-defaults*))
           (stream (sb-ext:process-output process)))
      (loop for line = (read-line stream nil)
            while line
            do (write-line line out-stream))
      ;; Wait for process to finish before closing stream.
      (sb-ext:process-wait process)
      (close stream))

    #+ALLEGRO
    (multiple-value-bind
      (stream error-stream pid)
      (excl:run-shell-command command :output :stream :wait nil)
      (progn
        (loop for line = (read-line stream nil)
              while line
              do
              (write-line line out-stream))

        ;; Reap parsing subprocess.  Wait if necessary.
        ;; Shouldn't need to wait long since we've read the full
        ;; stream from its output already, so the process should
        ;; be close to completing.
        (sys:reap-os-subprocess :pid pid :wait t)

        ;; Close stream.
        (close stream)))))


;; Note: I'd actually use 'tree-from-string' in epik/tools/lisp/utils, but
;; this doesn't depend on a regex package. Below by Lenhart Schubert. -BVD

(defun lispify-parser-output (char-string)
  "Convert a sentence parse, as a string, to a LISP S-expression"
  (read-from-string (preslash-unsafe-chars char-string)) )

(defun preslash-unsafe-chars (char-string)
  "Prefix '\' to unsafe characters # ` ' : ; , . \ | in 'aString'."
  (let ((chars (coerce char-string 'list)) result)
       (dolist (ch chars)
           (cond ((alphanumericp ch) (push ch result))
                 ((member ch '(#\( #\) #\")) (push ch result)); unbalanced "
                 ((member ch
                   '(#\# #\` #\' #\: #\; #\, #\. #\\ #\|) )
                  (push #\\ result) (push ch result) )
                 (T (push ch result)) ))
        (coerce (reverse result) 'string)))
