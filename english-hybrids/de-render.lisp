;; Copyright 2019 Sony Computer Science Laboratories Paris
;;                Remi van Trijp (http://www.remivantrijp.eu)

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

;; Basic de-render that builds on :de-render-with-scope
;; Does not assume a hybrid.
;; ------------------------------------------------------------------------------------------------------------
(defmethod de-render ((utterance list) (mode (eql :english-de-render)) &key cxn-inventory &allow-other-keys)
  "De-render based on :de-render-with-scope but does some post-processing."
  (let ((transient-structure
         (de-render utterance :de-render-with-scope :cxn-inventory cxn-inventory)))

    ;; Check for unknown strings.
    (set-data transient-structure :unknown-strings nil)    
    (loop for string in (reverse utterance)
          unless (or (word-in-dictionary-p string cxn-inventory)
                     (find string (get-data transient-structure :unknown-strings) :test #'equalp))
          ;;no lex cxn existing
          do (set-data transient-structure :unknown-strings
                       (cons string (get-data transient-structure :unknown-strings))))

    ;; Keep the utterance.
    (set-data transient-structure :utterance utterance)
    ;; We return the transient structure:
    transient-structure))

(defmethod de-render ((utterance string) (mode (eql :english-de-render)) &key cxn-inventory &allow-other-keys)
  "De-renders just like de-render-with-scope, but uses English tokenizer to split the utterance into tokens."
  (declare (ignorable mode))
  (de-render (tokenize-english-sentence utterance) :english-de-render :cxn-inventory cxn-inventory))

;;; (defmethod de-render ((utterance list) (mode (eql :english-de-render)) &key cxn-inventory &allow-other-keys)
;;;   "Takes a list of one string and de-renders the string."
;;;   (assert (single-solution-p utterance))
;;;   (de-render (first utterance) mode :cxn-inventory cxn-inventory))


;;; +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;;; DE-RENDER methods for the FCG hybrid.
;;; Uses the translation functions in /hybrids/semantic-dependency-parser.lisp
;;; -----------------------------------------------------------------------------------

;;; 1. Get a syntactic dependency tree from Penelope.
;;; 2. Make the following modifications to the analysis:
;;;    (a) Reconstruct adjectival compounds
;;;        e.g. Re-compose ("home" "-" "brewn") into the string "home-brewn"
;;;    (b) Treat named entities as single strings
;;;        e.g. ("Barack" "Obama") -> "Barack Obama"
;;;    (c) Treat compound nouns as single strings
;;;        e.g. ("climate" "change") -> "climate change"
;;;
;;;    Each time, we take the information from the last string as the information to
;;;    keep in the analysis, and we throw away what was found for the preceding strings.
;;;
;;; 3. We take the list of strings that we extract from the modified dependency analysis.
;;;    It is important to pass the LIST of strings instead of passing the sentence as a
;;;    string, because this avoids the next method to perform tokenization again and thereby
;;;    create inconsistencies in the analysis and translation.
;;;        The list of strings is passed to the de-render method that specializes on
;;;    :english-de-render. This de-render method will give us a basic transient structure that
;;;    has all the form constraints and that has a root-unit filled in. It also adds several
;;;    things to the data of the transient structure, such as the :utterance and :unknown-strings.
;;;    Next, we call the translation function that populates the basic transient structure with
;;;    additional units based on the modified dependency analysis.
;;;    +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(defparameter *punctuation-list* (loop for char in (coerce ".?! ,:; ()[]{}'`<>$^&*-\/#$\"" 'list)
                                       collect (format nil "~a" char))
  "List of what counts as a punctuation character.")

(defun punct-p (string)
  (find string *punctuation-list* :test #'string=))
;; (punct-p "?")
;; (punct-p "a")

(defun remove-punctuation-from-list-of-strings (list-of-strings)
  (remove-if #'punct-p list-of-strings))
;; (remove-punctuation-from-list-of-strings '("John" "," "my" "friend" "," "where" "are" "you" "?"))

(defmethod de-render ((utterance string) (mode (eql :english-with-dependency-parser))
                      &key cxn-inventory &allow-other-keys)
  (declare (ignorable mode))
  (unless cxn-inventory (setf cxn-inventory *fcg-constructions*))
  (multiple-value-bind (syntactic-analysis utterance-as-list)
      (preprocess-using-dependency-tree utterance
                                        :preprocessing-steps (list #'dependency-string-append-compounds-in-np
                                                                   #'dependency-string-append-named-entities
                                                                   #'dependency-string-append-compounds
                                                                   #'dependency-string-promote-adverbs-in-np
                                                                   #'dependency-remove-punct)
                                        :cxn-inventory cxn-inventory)
    (let ((basic-transient-structure (de-render
                                      ;; We remove punctuation for the time being.
                                      (remove-punctuation-from-list-of-strings utterance-as-list)
                                      :english-de-render :cxn-inventory cxn-inventory)))
      (set-data basic-transient-structure :pos-tags
                (loop for constituent in syntactic-analysis
                      collect (list (nlp-tools::dp-get-token constituent)
                                    (nlp-tools::dp-get-tag constituent))))
      (set-data basic-transient-structure :dependency-analysis syntactic-analysis)
      (translate-dependency-tree basic-transient-structure syntactic-analysis *dependency-specs*))))

(export '(translate-and-show))

(defun translate-and-show (utterance &key (mode :english-with-dependency-parser))
  (let ((transient-structure (show-translated-sentence utterance mode)))
    (pprint (get-data transient-structure :dependency-analysis))
    transient-structure))
