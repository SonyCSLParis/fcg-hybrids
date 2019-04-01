;;;;; Copyright (c) Sony Computer Science Laboratories Paris
;;;;;               VUB Artificial Intelligence Laboratory, Brussels
;;;;;
;;;;; Written by Remi van Trijp (remi.vantrijp@sony.com)
;;;;; -----------------------------------------------------------------------------------

; (ql:quickload :fcg-hybrids)

(in-package :fcg)

;;;; Basic function.
;;;; ---------------------------------------------------------------------------------
(export '(preprocess-using-dependency-tree))

(defun preprocess-using-dependency-tree (utterance &key preprocessing-steps
                                                   cxn-inventory
                                                   (model "en"))
  (assert (stringp utterance))
  (let ((syntactic-analysis (nlp-tools::get-penelope-dependency-analysis utterance :model model)))
    (dolist (step (or preprocessing-steps
                      (get-configuration cxn-inventory :preprocessing-steps-for-dependency-parser)))
      (setf syntactic-analysis (funcall step syntactic-analysis)))
    (let* ((utterance-as-list (nlp-tools::dp-build-utterance-as-list-from-dependency-tree syntactic-analysis))
           (basic-transient-structure (de-render utterance-as-list :de-render-with-scope
                                                 :cxn-inventory cxn-inventory)))
    (values syntactic-analysis utterance-as-list basic-transient-structure))))

;;; Some examples of preprocessing functions working on a dependency analysis.
;;; -------------------------------------------------------------------------------------
(export '(dependency-string-append-compounds-in-np
          dependency-string-append-named-entities
          dependency-string-append-compounds))

(defun compound-in-noun-chunk-p (noun-chunk)
  "Given a noun-chunk, check whether it contains a hyphenized compound."
  (find "-" noun-chunk :test #'equal))
;; (compound-in-noun-chunk-p '("a" "fun" "-" "loving" "criminal"))

(defun retrieve-compounds-from-chunk (noun-chunk)
  "Retrieve the compounds within a noun-chunk based on hyphens."
  (if (null (compound-in-noun-chunk-p noun-chunk))
    (remove-if-not #'consp noun-chunk)
    (let ((position (position "-" noun-chunk :test #'equal)))
      (retrieve-compounds-from-chunk (append (subseq noun-chunk 0 (1- position))
                                             (list (append (flatten (nth (1- position) noun-chunk))
                                                           '("-")
                                                           (flatten (nth (1+ position) noun-chunk))))
                                             (subseq noun-chunk (+ 2 position)))))))
;; (retrieve-compounds-from-chunk '("a" "fun" "-" "loving" "attention" "-" "seeking" "criminal"))

(defun handle-chunk-in-NP (noun-chunk syntactic-analysis)
  "Given a syntactic analysis, turn compounds within noun phrases into single strings again."
  (let ((compounds (retrieve-compounds-from-chunk noun-chunk)))
    (dolist (compound compounds)
      (let* ((first-position (sublist-position compound syntactic-analysis
                                               :key #'(lambda (x)
                                                        (rest (assoc :token x)))))
             (last-position (+ first-position (length compound)))
             (compound-string (apply #'string-append
                                     (mapcar #'(lambda (x)
                                                 (rest (assoc :token x)))
                                             (subseq syntactic-analysis first-position last-position))))
             (head-word-spec (nth (1- last-position) syntactic-analysis))    
             (new-spec (loop for x in head-word-spec
                             collect (if (eql :token (first x))
                                       (cons :token compound-string)
                                       x))))
        (setf syntactic-analysis (append (subseq syntactic-analysis 0 first-position)
                                         (list new-spec)
                                         (subseq syntactic-analysis last-position)))))
    syntactic-analysis))

(defun dependency-string-append-compounds-in-NP (dependency-tree)
  "Do not split the compounds in noun phrases."
  (let* ((utterance (nlp-tools::dp-build-utterance-from-dependency-tree dependency-tree))
         (noun-chunks (nlp-tools::get-penelope-noun-chunks utterance)))
    (loop for noun-chunk in noun-chunks
          when (compound-in-noun-chunk-p noun-chunk) ;; There is a compound
          do (setf dependency-tree (handle-chunk-in-NP noun-chunk dependency-tree))
          finally (return dependency-tree))))

;; dependency-string-append-named-entities
;; -----------------------------------------------------------------------
(defun get-penelope-named-entities-without-cardinals (sentence)
  "Return the named entities but not the cardinals."
  (loop for ne-result in (nlp-tools::get-penelope-named-entities sentence)
        unless (string= "CARDINAL" (second ne-result))
        collect (first ne-result)))

(defun get-penelope-named-entities-without-cardinals-and-dates (sentence)
  "Return the named entities but not the cardinals."
  (loop for ne-result in (nlp-tools::get-penelope-named-entities sentence)
        unless  (find (second ne-result) (list "CARDINAL" "DATE") :test #'string=)
        collect (first ne-result)))

(defun dependency-string-append-named-entities (dependency-tree)
  (let* ((utterance (nlp-tools:dp-build-utterance-from-dependency-tree dependency-tree))
         (named-entities (get-penelope-named-entities-without-cardinals-and-dates utterance)))
    (loop for named-entity in named-entities
          when (find #\space named-entity) ;; Composed of multiple parts.
          do (setf dependency-tree (nlp-tools::dp-combine-tokens-in-dependency-analysis
                                    (split-sequence::split-sequence #\Space named-entity)
                                    dependency-tree named-entity))
          finally (return dependency-tree))))

(defun check-for-chunk (string dependencies &optional string-so-far)
  "If the de-render chunked some strings, we take the last word as its main category."
  (let* ((dependency (first dependencies))
         (new-string (rest (assoc :token dependency))))
    (if (string= string new-string)
      dependency
      (let ((compound-string (if string-so-far (string-append string-so-far " " new-string) new-string)))
        (if (string= string compound-string)
          dependency
          (check-for-chunk string (rest dependencies) compound-string))))))

(defun find-compounds (syntactic-analysis &optional currently-handling compounds)
  "Extracts compounds from the syntactic dependency analysis."
  (cond ((null syntactic-analysis)
         (reverse compounds))
        ((string= "compound" (rest (assoc :dependency (first syntactic-analysis))))
         (find-compounds (rest syntactic-analysis)
                         (append currently-handling
                                 (list (rest (assoc :TOKEN (first syntactic-analysis)))))
                         compounds))
        ((null currently-handling)
         (find-compounds (rest syntactic-analysis) currently-handling compounds))
        (t
         (find-compounds (rest syntactic-analysis)
                         nil
                         (cons (append currently-handling
                                       (list (rest (assoc :TOKEN (first syntactic-analysis)))))
                               compounds)))))
;; (find-compounds (get-penelope-dependency-analysis "I saw the Red Hot Chilli Peppers in concert"))

(defun dependency-string-append-compounds (dependency-tree)
  (loop for compound in (find-compounds dependency-tree)
        do (setf dependency-tree
                 (nlp-tools::dp-combine-tokens-in-dependency-analysis compound dependency-tree))
        finally (return dependency-tree)))

(export '(dependency-string-promote-adverbs-in-np))
(defun dependency-string-promote-adverbs-in-np (dependency-tree)
  (loop for dependent in dependency-tree
        collect (if (string= "RB" (nlp-tools::dp-get-tag dependent))
                  (let ((parent-node (find (nlp-tools:dp-get-head-id dependent) dependency-tree
                                           :key #'(lambda(node)
                                                    (parse-integer (nlp-tools:dp-get-node-id node)))
                                           :test #'=)))
                    (if (and parent-node (string= "JJ" (nlp-tools::dp-get-tag parent-node)))
                      (loop for spec in dependent
                            collect (if (eql :head--id (first spec))
                                      (cons :head--id (nlp-tools:dp-get-head-id parent-node))
                                      spec))
                      dependent))
                  dependent)))
; (translate-and-show "I ate a really bad apple.")

(export '(dependency-remove-punct))
(defun dependency-remove-punct (dependency-tree)
  (loop for dependent in dependency-tree
        unless (string= "punct" (rest (assoc :DEPENDENCY dependent)))
        collect dependent))

;; Compare:
;; --------
;; (preprocess-using-dependency-tree "I saw Barack Obama" :preprocessing-steps (list #'dependency-string-append-named-entities))
;; (preprocess-using-dependency-tree "I saw the fun-loving criminals" :preprocessing-steps (list #'dependency-string-append-compounds-in-np))
;; (preprocess-using-dependency-tree "I saw Barack Obama and his ever-radiant wife Michelle" :preprocessing-steps (list #'dependency-string-append-named-entities))
;; (preprocess-using-dependency-tree "I saw Barack Obama and his ever-radiant wife Michelle" :preprocessing-steps (list #'dependency-string-append-compounds-in-np #'dependency-string-append-named-entities))
;; (preprocess-using-dependency-tree "The news pictures of the photographer" :preprocessing-steps (list #'dependency-string-append-compounds))
