;; Copyright Sony Computer Science Laboratories Paris
;; Author: Remi van Trijp (http://www.remivantrijp.eu)

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

(defparameter *fcg-english* nil "Parameter for the base model of English.")

(export '(constituents dependents footprints boundaries agreement form meaning))

(def-fcg-constructions english-base-model
  :cxn-inventory *fcg-english*
  :cxn-inventory-type hashed-fcg-construction-set
  :feature-types ((fcg::constituents set)
                  (fcg::dependents set)
                  (fcg::footprints set)
                  (fcg::boundaries set-of-predicates)
                  (fcg::agreement sequence)
                  (args sequence)
                  (fcg::form set-of-predicates)
                  (fcg::meaning set-of-predicates))
 :fcg-configurations (;; Form predicates
                      (:form-predicates meets)
                      ;; ----------------------------------------------------------------------------------
                      ;; Construction sets
                      ;; ----------------------------------------------------------------------------------
                      (:parse-order structural hashed-string hashed-lex-id cxn)
                      (:hashed-labels hashed-string hashed-meaning hashed-lex-id)
                      ;; ----------------------------------------------------------------------------------
                      ;; Render and De-rendering
                      ;; ----------------------------------------------------------------------------------
                      (:de-render-mode . :english-hybrid)
                      ;; ----------------------------------------------------------------------------------
                      ;; Node and Goal tests
                      ;; ----------------------------------------------------------------------------------
                      (:node-tests :update-references
                       :check-duplicate :restrict-nr-of-nodes)
                      (:update-boundaries-feature . constituents)
                      (:parse-goal-tests :no-applicable-cxns)
                      (:production-goal-tests :no-applicable-cxns)
                      ;; ----------------------------------------------------------------------------------
                      ;; Construction Supplier
                      ;; ----------------------------------------------------------------------------------
                      ;;
                      ;; For guiding search
                      ;; ----------------------------------------------------------------------------------
                      (:queue-mode . :depth-first-avoid-duplicates)
                      (:max-search-depth . 100)
                      (:max-nr-of-nodes . 1500)
                      ;; ----------------------------------------------------------------------------------
                      ;; Miscellaneous
                      ;; ----------------------------------------------------------------------------------
                      (:draw-meaning-as-network . t)
                      (:shuffle-cxns-before-application . nil)
                      ;; For learning
                      (:consolidate-repairs . t))
 :visualization-configurations ((:show-wiki-links-in-predicate-networks . nil)
                                (:show-constructional-dependencies . nil)
                                (:with-search-debug-data . t))
 :hierarchy-features (constituents dependents)

 ;; Verb-to-Verb-construction
 (def-fcg-cxn Verb-to-Verb-construction
              ((?second-verb-phrase
                (sem-cat (semantic-subject ?subject-phrase)))
               <-
               (?to
                --
                (hash form ((string ?to "to"))))
               (?verb-phrase
                --
                (syn-cat (phrase-type verb-phrase))
                (parent ?clause)
                (hash form ((meets ?verb1 ?to ?verb-phrase)
                            (meets ?to ?verb2 ?verb-phrase))))
               (?verb1
                --
                (syn-cat (lex-class verb))
                (parent ?verb-phrase))
               (?verb2
                --
                (syn-cat (lex-class verb))
                (parent ?second-verb-phrase))
               (?clause
                --
                (fields (subject-phrase ?subject-phrase)
                        (verb-phrase ?verb-phrase))))
              :cxn-set structural)

 ;; S <- NP VP
 (def-fcg-cxn Subject-Predicate-cxn
              (<-
               (?clause
                (footprints (Subject-Predicate-cxn))
                (fields (subject-phrase ?subject-phrase)
                        (verb-phrase ?verb-phrase))
                --
                (footprints (not Subject-Predicate-cxn))
                (syn-cat (clause-type ?clause-type))
                (constituents (?subject-phrase ?verb-phrase))
                (hash form ((meets ?subject-phrase ?verb-phrase ?clause))))
               (?verb-phrase
                --
                (syn-cat (phrase-type verb-phrase)))
               (?subject-phrase
                --
                (syn-cat (phrase-type noun-phrase))))
              :disable-automatic-footprints t
              :cxn-set structural)

 ;; S <- NP ADV VP
 (def-fcg-cxn Subject-Somephrase-Predicate-cxn
              (<-
               (?clause
                (footprints (Subject-Predicate-cxn))
                (fields (subject-phrase ?subject-phrase)
                        (verb-phrase ?verb-phrase))
                --
                (footprints (not Subject-Predicate-cxn))
                (syn-cat (clause-type ?clause-type))
                (constituents (?subject-phrase ?verb-phrase))
                (hash form ((meets ?subject-phrase ?another-phrase ?clause)
                            (meets ?another-phrase ?verb-phrase ?clause))))
               (?verb-phrase
                --
                (syn-cat (phrase-type verb-phrase)))
               (?subject-phrase
                --
                (syn-cat (phrase-type noun-phrase)))
               (?main-verb
                --
                (functional-structure (subject ?subject))
                (parent ?verb-phrase))
               (?subject
                --
                (parent ?subject-phrase)))
              :disable-automatic-footprints t
              :cxn-set structural))

;;;  ;; S <- NP WH-S
;;;  (def-fcg-cxn Interrogative-Subject-Predicate-cxn
;;;               (
;;;                <-
;;;                (?clause
;;;                 (footprints (interrogative-subject-predicate-cxn))
;;;                 (fields (subject-phrase ?subject-phrase)
;;;                         (verb-phrase ?verb-phrase))
;;;                 --
;;;                 (footprints (not interrogative-subject-predicate-cxn))
;;;                 (syn-cat (clause-type wh-interrogative))
;;;                 (constituents (?subject-phrase ?subordinate-clause)))
;;;                (?subordinate-clause
;;;                 --
;;;                 (syn-cat (clause-type interrogative-without-wh-constituent))
;;;                 (constituents (?verb-phrase)))
;;;                (?verb-phrase
;;;                 --
;;;                 (syn-cat (phrase-type verb-phrase)))
;;;                (?subject-phrase
;;;                 --
;;;                 (syn-cat (phrase-type noun-phrase)))
;;;                (root
;;;                 --
;;;                 (form ((meets ?subject-phrase ?verb-phrase ?clause)))))
;;;                :disable-automatic-footprints t
;;;                :cxn-set structural)

;;;  ;;  VP <- V NP
;;;  (def-fcg-cxn VP-Object-cxn
;;;               (<-
;;;                (?clause
;;;                 (fields (object-phrase ?object-phrase))
;;;                 --
;;;                 (fields (verb-phrase ?verb-phrase)))
;;;                (?verb-phrase
;;;                 (footprints (VP-Object-cxn))
;;;                 --
;;;                 (footprints (not VP-Object-cxn))
;;;                 (syn-cat (phrase-type verb-phrase))
;;;                 (constituents (?main-verb ?object-phrase)))
;;;                (?main-verb
;;;                 --
;;;                 (syn-cat (lex-class verb)))
;;;                (?object-phrase
;;;                 --
;;;                 (syn-cat (phrase-type noun-phrase))))
;;;               :disable-automatic-footprints t
;;;               :cxn-set structural)

;;;  ;; S <- NP S'
;;;  (def-fcg-cxn Topical-Object-cxn
;;;               (<-
;;;                (?clause
;;;                 --
;;;                 (syn-cat (clause-type wh-interrogative))
;;;                 (constituents (?object-phrase ?subclause)))
;;;                (?subclause
;;;                 (fields (object-phrase ?object-phrase))
;;;                 --
;;;                 (syn-cat (clause-type interrogative-without-wh-constituent)
;;;                          (is-matrix-clause -))
;;;                 (fields (subject-phrase ?subject-phrase)
;;;                         (verb-phrase ?verb-phrase))
;;;                 (constituents (?subject-phrase ?verb-phrase)))
;;;                (?object-phrase
;;;                 --
;;;                 (syn-cat (phrase-type noun-phrase)))
;;;                (root
;;;                 --
;;;                 (form ((meets ?object-phrase ?subclause ?clause)))))
;;;               :cxn-set structural
;;;               :cxn-inventory *fcg-english*)

;;;  (def-fcg-cxn clausal-noun-phrase-cxn
;;;               (<-
;;;                (?clausal-noun-phrase
;;;                 (syn-cat (clause-type simple-declarative)
;;;                          (is-matrix-clause -))
;;;                 --
;;;                 (syn-cat (phrase-type noun-phrase))
;;;                 (constituents (?noun-phrase ?verb-phrase)))
;;;                (?noun-phrase
;;;                 --
;;;                 (syn-cat (phrase-type noun-phrase)))
;;;                (?verb-phrase
;;;                 --
;;;                 (syn-cat (phrase-type verb-phrase)))
;;;                (root
;;;                 --
;;;                 (form ((meets ?noun-phrase ?verb-phrase ?clausal-noun-phrase)))))
;;;               :cxn-set structural
;;;               :cxn-inventory *fcg-english*)

;;;  ;; Reify-Object-Phrase
;;;  (def-fcg-cxn Reify-Object-Phrase-cxn
;;;               (<-
;;;                (?clause
;;;                 (fields (object-phrase ?subordinate-clause))
;;;                 --
;;;                 (fields (verb-phrase ?verb-phrase)))
;;;                (?verb-phrase
;;;                 (footprints (reify-object-phrase-cxn))
;;;                 --
;;;                 (footprints (not reify-object-phrase-cxn))
;;;                 (syn-cat (phrase-type verb-phrase))
;;;                 (constituents (?main-verb ?subordinate-clause)))
;;;                (?main-verb
;;;                 --
;;;                 (syn-cat (lex-class verb)))
;;;                (?subordinate-clause
;;;                 (syn-cat (phrase-type noun-phrase))
;;;                 --
;;;                 (syn-cat (clause-type ?clause-type)
;;;                          (is-matrix-clause -))))
;;;               :disable-automatic-footprints t
;;;               :cxn-set structural
;;;               :cxn-inventory *fcg-english*))

                
#|
(ql:quickload :fcg-hybrids)

(comprehend "The boy kicked the ball" :cxn-inventory *fcg-english*)

(comprehend "Severe weather sweeping parts of the US continues to bring record breaking cold temperatures to the Pacific northwest."
            :cxn-inventory *fcg-english*)






; and heavy snow to mountains in northern California and Nevada

(comprehend "Emergency warming shelters were opened throughout Oregon and western Washington as temperatures plunged into the teens (below zero in centigrade) and forecasters said an Arctic blast would last for several days."
            :cxn-inventory *fcg-english*)

(comprehend "Sunday's snow showers blew into the Pacific northwest from the Gulf of Alaska, dumping up to six in (15 cm) across the Seattle area."
            :cxn-inventory *fcg-english*)

(comprehend "The National Weather Service said Seattle's low on Sunday was cold, breaking a mark set in 1948."
            :cxn-inventory *fcg-english*)

 ;; Corrective Constructions
 ;; ------------------------
 (def-fcg-cxn V-to-V-cxn
              (<-
               (?verb1
                --
                (parent ?verb-phrase)
                (syn-cat (lex-class verb)))
               (?verb2
                --
                (syn-cat (lex-class verb)))
               (root
                --
                (form ((string ?to "to")
                       (meets ?verb1 ?to ?unitx)
                       (meets ?to ?verb2 ?unity)))))
              :attributes (:label :hashed-string :string "corrective-cxn"
                           :repair #'make-complex-verb)
              :cxn-inventory *fcg-english*))

|#