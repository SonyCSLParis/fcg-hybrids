;; Copyright Sony Computer Science Laboratories Paris
;; Authors: Remi van Trijp (http://www.remivantrijp.eu)
;;          Martina Galletti martina.galletti@sony.com
;;          Ines Blin ines.blin@sony.com

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

(defvar *fcg-italian* nil "Global variable for the base model of Italian.")

(def-fcg-constructions italian-base-model
  :cxn-inventory *fcg-italian*
  :cxn-inventory-type hashed-fcg-construction-set
  :feature-types ((dependents set)
                  (footprints set)
                  (boundaries set-of-predicates)
                  (form set-of-predicates)
                  (meaning set-of-predicates))
  :fcg-configurations (;; ----------------------------------------------------------------------------------
                       ;; Tagsets
                       ;; ----------------------------------------------------------------------------------

                       ;; Render and De-rendering
                       ;; ----------------------------------------------------------------------------------
                       (:de-render-mode . :italian-hybrid)
                       ;; ----------------------------------------------------------------------------------
                       ;; Form predicates
                       ;; ----------------------------------------------------------------------------------
                       (:form-predicates meets)
                       ;; ----------------------------------------------------------------------------------
                       ;; Construction Sets
                       ;; ----------------------------------------------------------------------------------
                       (:production-order hashed-meaning cxn hashed-lex-id)
                       (:parse-order hashed-string hashed-lex-id cxn)
                       (:hashed-labels hashed-string hashed-meaning hashed-lex-id)
                       ;; ----------------------------------------------------------------------------------
                       ;; Node tests
                       ;; ----------------------------------------------------------------------------------
                       (:node-tests :check-duplicate :restrict-nr-of-nodes)
                       (:update-boundaries-feature . constituents)
                       (:parse-goal-tests :no-applicable-cxns)
                       (:production-goal-tests :no-applicable-cxns)
                       ;; ----------------------------------------------------------------------------------
                       ;; Miscellaneous
                       ;; ----------------------------------------------------------------------------------
                       (:draw-meaning-as-network . t)
                       (:use-meta-layer . nil)
                       )
  :visualization-configurations ((:show-constructional-dependencies . nil)
                                 (:with-search-debug-data . t))
  :hierarchy-features (dependents)

  (configure-tag-sets *fcg-italian*
                      :pos-tag-set *italian-pos-tag-set*
                      :fcg-categories *italian-fcg-categories*))
