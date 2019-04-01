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

(defmethod de-render ((utterance string) (mode (eql :english-with-dependency-parser))
                      &key cxn-inventory &allow-other-keys)
  (declare (ignorable mode))
  (unless cxn-inventory (setf cxn-inventory *fcg-constructions*))
  (multiple-value-bind (syntactic-analysis utterance-as-list)
      (preprocess-using-dependency-tree utterance
                                        :preprocessing-steps (list #'dependency-string-append-compounds-in-np
                                                                   #'dependency-string-append-named-entities
                                                                   #'dependency-string-append-compounds
                                                                   #'dependency-remove-punct)
                                        :cxn-inventory cxn-inventory)
    (let ((basic-transient-structure (de-render utterance-as-list :english-de-render :cxn-inventory cxn-inventory)))
      (set-data basic-transient-structure :pos-tags
                (loop for constituent in syntactic-analysis
                      collect (list (nlp-tools:dp-get-token constituent)
                                    (nlp-tools:dp-get-tag constituent))))                      
      (translate-dependency-tree basic-transient-structure syntactic-analysis *dependency-specs*))))
;; (show-translated-sentence "Sales of home-brewn beer have fallen 2% in 50 years, affecting beer fanatics.")
