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
                      (:parse-order marked structural hashed-string hashed-lex-id cxn)
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
                      (:node-expansion-mode . :default)
                      (:construction-inventory-processor-mode . :default)
                      (:priority-mode . :nr-of-applied-cxns)
                      (:cxn-supplier-mode . :hashed-ordered-by-label)
                      ;; For guiding search
                      ;; ----------------------------------------------------------------------------------
                      (:queue-mode . :depth-first)
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
 :hierarchy-features (constituents dependents))

;;;  ;; Double Verb construction
;;;  (def-fcg-cxn Double-Verb-cxn
;;;               (<-
;;;                (?verb-phrase
;;;                 (footprints (double-verb-cxn))
;;;                 (gr (subject ?subject-phrase)
;;;                     (object ?second-verb-phrase))
;;;                 --
;;;                 (footprints (not double-verb-cxn))
;;;                 (syn-cat (phrase-type verb-phrase))
;;;                 (constituents (?second-verb-phrase)))
;;;                (?second-verb-phrase
;;;                 (gr (subject ?subject-phrase)
;;;                     (predicate ?second-verb-phrase))
;;;                 --
;;;                 (syn-cat (phrase-type verb-phrase))))
;;;               :disable-automatic-footprints t
;;;               :cxn-set structural)

;;;  ;; S <- NP VP
;;;  (def-fcg-cxn Subject-Predicate-cxn
;;;               (<-
;;;                (?clause
;;;                 (footprints (Subject-Predicate-cxn))
;;;                 --
;;;                 (footprints (not Subject-Predicate-cxn))
;;;                 (syn-cat (clause-type simple-declarative))
;;;                 (constituents (?subject-phrase ?verb-phrase)))
;;;                (?verb-phrase
;;;                 (gr (subject ?subject-phrase)
;;;                     (predicate ?verb-phrase))
;;;                 --
;;;                 (syn-cat (phrase-type verb-phrase)))
;;;                (?subject-phrase
;;;                 --
;;;                 (syn-cat (phrase-type noun-phrase))))
;;;               :disable-automatic-footprints t
;;;               :cxn-set structural)

;;;  ;; S <- NP aux VP
;;;  (def-fcg-cxn WH-Object-cxn
;;;               (<-
;;;                (?object-phrase
;;;                 --
;;;                 (parent ?interrogative-clause)
;;;                 (syn-cat (phrase-type noun-phrase))
;;;                 (hash form ((meets ?object-phrase ?aux ?interrogrative-clause))))
;;;                (?interrogative-clause
;;;                 --
;;;                 (syn-cat (clause-type wh-interrogative))
;;;                 (constituents (?object-phrase ?remainder)))
;;;                (?remainder
;;;                 --
;;;                 (syn-cat (clause-type interrogative-without-wh-constituent))
;;;                 (constituents (?aux ?subject-phrase ?verb-phrase)))
;;;                (?aux
;;;                 --
;;;                 (syn-cat (lex-class aux)))
;;;                (?verb-phrase
;;;                 (footprints (WH-object-cxn))
;;;                 (gr (subject ?subject-phrase)
;;;                     (predicate ?verb-phrase)
;;;                     (object ?object-phrase))
;;;                 --
;;;                 (syn-cat (phrase-type verb-phrase))
;;;                 (footprints (not WH-object-cxn)))
;;;                (?subject-phrase
;;;                 --
;;;                 (syn-cat (phrase-type noun-phrase))))
;;;               :disable-automatic-footprints t
;;;               :cxn-set marked))