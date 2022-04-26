;; Copyright 2019-present
;;           Sony Computer Science Laboratories Paris
;;           Remi van Trijp (http://www.remivantrijp.eu)

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at

;;     http://www.apache.org/licenses/LICENSE-2.0

;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;=========================================================================

(in-package :fcg)

;;;; Most preprocessing steps can be done in an NLP-pipeline outside of the Babel2/FCG
;;;; environment. However, it is also possible to do manipulations on the preprocessing-
;;;; results in Lisp. This file provides some examples for dependency parsing.

;;;; Manipulating Spacy dependency parse results
;;;; ---------------------------------------------------------------------------------
(export '(preprocess-using-dependency-tree))

(defun preprocess-using-dependency-tree (utterance &key preprocessing-steps
                                                   cxn-inventory
                                                   (model "en"))
  (assert (stringp utterance))
  (let ((syntactic-analysis (nlp-tools::get-penelope-dependency-analysis utterance :model model))
        (keys-and-values nil))
    (dolist (step (or preprocessing-steps
                      (get-configuration cxn-inventory :preprocessing-steps-for-dependency-parser)))
      (multiple-value-bind (modified-analysis keyword value)
          ;; Optionally, a preprocessing step can return a keyword and value to be stored in the
          ;; transient structure.
          (funcall step syntactic-analysis)
        (setf syntactic-analysis modified-analysis)
        (when keyword (push (list keyword value) keys-and-values))))
    (let* ((utterance-as-list (nlp-tools::dp-build-utterance-as-list-from-dependency-tree syntactic-analysis))
           (basic-transient-structure (de-render utterance-as-list :de-render-with-scope
                                                 :cxn-inventory cxn-inventory)))
      (loop for key-and-value in keys-and-values
            do ;; store the information in the transient structure.
            (set-data basic-transient-structure (first key-and-value) (second key-and-value)))
      (set-data basic-transient-structure :dependency-tree syntactic-analysis)
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
; (handle-chunk-in-np (retrieve-compounds-from-chunk '("a" "fun" "-" "loving" "criminal")) (nlp-tools:get-penelope-dependency-analysis "a fun-loving criminal"))

(defun dependency-string-append-compounds-in-NP (dependency-tree)
  "Do not split the compounds in noun phrases."
  (let* ((utterance (nlp-tools::dp-build-utterance-from-dependency-tree dependency-tree))
         (noun-chunks (nlp-tools::get-penelope-noun-chunks utterance)))
    (loop for noun-chunk in noun-chunks
          for chunk-as-list = (split-sequence::split-sequence #\Space noun-chunk)
          when (compound-in-noun-chunk-p chunk-as-list) ;; There is a compound
          do (setf dependency-tree (handle-chunk-in-NP chunk-as-list dependency-tree))
          finally (return dependency-tree))))
;; (dependency-string-append-compounds-in-NP (nlp-tools:get-penelope-dependency-analysis "I saw a fun-loving criminal."))

;; dependency-string-append-named-entities
;; -----------------------------------------------------------------------
(defun get-penelope-named-entities-without-cardinals (sentence)
  "Return the named entities but not the cardinals."
  (loop for ne-result in (nlp-tools::get-penelope-named-entities sentence)
        unless (string= "CARDINAL" (second ne-result))
        collect (first ne-result)))

(defun get-penelope-named-entities-for-beng (sentence)
  "Return the named entities but not the cardinals."
  (loop for ne-result in (nlp-tools::get-penelope-named-entities sentence)
        unless (string= "CARDINAL" (second ne-result))
        collect ne-result))

(defun get-penelope-named-entities-without-cardinals-and-dates (sentence)
  "Return the named entities but not the cardinals."
  (loop for ne-result in (nlp-tools::get-penelope-named-entities sentence)
        unless  (find (second ne-result) (list "CARDINAL" "DATE") :test #'string=)
        collect (first ne-result)))

;; This function returns multiple values for storing in transient structures.
(defun dependency-string-append-named-entities (dependency-tree)
  (let* ((utterance (nlp-tools:dp-build-utterance-from-dependency-tree dependency-tree))
         (named-entities-result (nlp-tools::get-penelope-named-entities utterance))
         (named-entities (mapcar #'first named-entities-result)))
    (loop for named-entity in named-entities
          when (find #\space named-entity) ;; Composed of multiple parts.
          do (setf dependency-tree (nlp-tools::dp-combine-tokens-in-dependency-analysis
                                    (split-sequence::split-sequence #\Space named-entity)
                                    dependency-tree named-entity))
          finally (return (values dependency-tree :named-entities named-entities-result)))))
;; (preprocess-using-dependency-tree "He voted for Barack Obama" :preprocessing-steps (list #'dependency-string-append-named-entities))

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
;; (find-compounds (nlp-tools::get-penelope-dependency-analysis "I saw the Red Hot Chilli Peppers in concert"))

(export '(dependency-string-append-compounds))
(defun dependency-string-append-compounds (dependency-tree)
  "Treat compounds as a single string."
  (loop for compound in (find-compounds dependency-tree)
        do (setf dependency-tree
                 (nlp-tools::dp-combine-tokens-in-dependency-analysis compound dependency-tree))
        finally (return dependency-tree)))
;; (dependency-string-append-compounds (nlp-tools::get-penelope-dependency-analysis "I saw the Red Hot Chilli Peppers in concert"))

(export '(dependency-string-promote-adverbs-in-np))
(defun dependency-string-promote-adverbs-in-np (dependency-tree)
  "Lift adverbs in an NP one position higher in the dependency tree."
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
;; (dependency-string-promote-adverbs-in-np (nlp-tools:get-penelope-dependency-analysis "I read a very good book."))

(export '(dependency-remove-punct))

(defun dependency-remove-punct (dependency-tree)
  (loop for dependent in dependency-tree
        unless (string= "punct" (rest (assoc :DEPENDENCY dependent)))
        collect dependent))
;; Remove punctuation from the dependency tree.

;; dependency-promote-conjuncts
;; -----------------------------------------------------------------------
(export '(dependency-promote-conjuncts))

(defun climb-to-first-conjunct (id dependency-tree)
  (dolist (x dependency-tree)
    (let ((node-id (rest (assoc :node--id x))))
      (when (string= id node-id)
        (let ((head-id (format nil "~a" (rest (assoc :head--id x)))))
          (if (member (rest (assoc :dependency x)) '("conj" "cc") :test #'string=)
            (return (climb-to-first-conjunct head-id dependency-tree))
            (return (values (parse-integer head-id) (rest (assoc :node--id x))))))))))

(defun dependency-promote-conjuncts (dependency-tree)
  "Treat conjuncts as items on the same level."
  (let ((ids-to-be-expanded nil))
    (reverse (loop for dependent in (reverse dependency-tree)
                   collect (cond ((member (rest (assoc :dependency dependent)) '("conj" "cc") :test #'string=)
                                  (let ((old-head-spec (assoc :head--id dependent)))
                                    (multiple-value-bind (the-head the-node)
                                        (climb-to-first-conjunct (format nil "~a" (rest old-head-spec))
                                                                 dependency-tree)
                                      (pushnew the-node ids-to-be-expanded :test #'string=)
                                      (substitute `(:head--id . ,the-head) old-head-spec dependent :test #'equal))))
                                 ((member (rest (assoc :node--id dependent)) ids-to-be-expanded :test #'string=)
                                  (append dependent `(:first-conjunct)))
                                 (t
                                  dependent))))))

;; Compare:
;; --------
;; (preprocess-using-dependency-tree "I saw Barack Obama" :preprocessing-steps (list #'dependency-string-append-named-entities))
;; (preprocess-using-dependency-tree "I saw the fun-loving criminals" :preprocessing-steps (list #'dependency-string-append-compounds-in-np))
;; (preprocess-using-dependency-tree "I saw Barack Obama and his ever-radiant wife Michelle" :preprocessing-steps (list #'dependency-string-append-named-entities))
;; (preprocess-using-dependency-tree "I saw Barack Obama and his ever-radiant wife Michelle" :preprocessing-steps (list #'dependency-string-append-compounds-in-np #'dependency-string-append-named-entities))
