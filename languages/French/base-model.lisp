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

(defparameter *fcg-french* nil "Parameter for the base model of French.")

(def-fcg-constructions french-base-model
  :cxn-inventory *fcg-french*
  :cxn-inventory-type hashed-fcg-construction-set
  :feature-types ((fcg::constituents set)
                  (fcg::dependents set)
                  (fcg::footprints set)
                  (fcg::boundaries set-of-predicates)
                  (args sequence)
                  (fcg::form set-of-predicates)
                  (fcg::meaning set-of-predicates))
 :fcg-configurations (;; Form predicates
                      (:form-predicates meets)
                      ;; ----------------------------------------------------------------------------------
                      ;; Construction sets
                      ;; ----------------------------------------------------------------------------------
                      (:parse-order hashed-string hashed-lex-id cxn)
                      (:hashed-labels hashed-string hashed-meaning hashed-lex-id)
                      ;; ----------------------------------------------------------------------------------
                      ;; Render and De-rendering
                      ;; ----------------------------------------------------------------------------------
                      (:de-render-mode . :french-hybrid)
                      ;; ----------------------------------------------------------------------------------
                      ;; Node and Goal tests
                      ;; ----------------------------------------------------------------------------------
                      (:node-tests :update-references
                       :check-duplicate :restrict-nr-of-nodes)
                      (:update-boundaries-feature . dependents)
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
 :hierarchy-features (dependents constituents))