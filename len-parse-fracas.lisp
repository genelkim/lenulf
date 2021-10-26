; Parse fracas dataset in a json file with Len's ULF parser and add that back into the json file.
; 
; Assumes a json file which is a list of dictionaries.
; The dictionary contains premises under "p1", "p2", etc. the hypothesis under
; "h" and question under "q". Each of these is a dictionary containing a "text"
; entry.

(ql:quickload :cl-json)
(ql:quickload :cl-util)
(load "init") ; load parser

(in-package :lenulf)

(defun process-sentence (sent-data)
  (if (null sent-data)
    (return-from process-sentence nil))
  (english-to-ulf (cdr (assoc 'text sent-data))))

(defun process-entry (entry)
  "Goes through all premises and the question and hypothesis text in this entry
  and adds the string form of the result of procesing the text with Len's ULF
  parser. This is added as an entry labeled \"lenulf\""
  (let ((premise-keys '(p-1 p-2 p-3 p-4 p-5 p-6)))
    (loop for key in (append premise-keys (list 'h 'q)) do
          (let ((sent-cons (assoc key entry))
                sent-data)
            (when (not (null sent-cons))
              (setf sent-data (cdr sent-cons))
              ;; Process the sentence with len's parser and add to the entry
              ;; data.
              (setf (cdr (assoc key entry))
                    (acons 'lenulf
                           (write-to-string (process-sentence sent-data))
                           sent-data)))))))

;; Main script body.
(let* ((json:*json-symbols-package* nil)
       (jdat (with-open-file (s "data/fracas.json") (json:decode-json s)))
       (idx 0))
  (loop for entry in jdat do
        (progn
          (setf idx (1+ idx))
          (format t "~%~s out of ~s~%" idx (length jdat))
          (process-entry entry)))
  (with-open-file (str "data/fracas-len.json"
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (json:encode-json jdat str)))

