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

(defmethod represent-functional-structure ((dependency-tree list)
                                           (transient-structure coupled-feature-structure)
                                           (key (eql :italian)) &optional (language *universal-dependencies*))
  (declare (ignore key language))
  (let* ((boundaries (fcg-get-boundaries transient-structure))
         (word-specs (make-word-specs-for-boundaries boundaries dependency-tree))
         ;; We assume that we only have the ROOT unit, and that we need to add units for each word.
         (new-units (loop for word-spec in word-specs
                          ;; (1) We check whether the word has a head:
                          for dependency-head = (unless ;; Unless the word doesn't point to a higher node:
                                                    (equal (word-dependency-spec-head-id word-spec)
                                                           (word-dependency-spec-node-id word-spec))
                                                  (word-dependency-spec-unit-name
                                                   (find (word-dependency-spec-head-id word-spec)
                                                         word-specs :key #'word-dependency-spec-node-id :test #'equal)))
                          
                          ;; (2) We check whether the word has dependents:
                          for dependent-specs = (loop for other-word-spec in word-specs
                                                      when (and (not (equal word-spec other-word-spec))
                                                                (= (word-dependency-spec-head-id other-word-spec)
                                                                   (word-dependency-spec-node-id word-spec)))
                                                      collect other-word-spec)
                          for dependent-unit-names = (mapcar #'word-dependency-spec-unit-name dependent-specs)

                          ;;(3) We check whether the dependents involve GRAMMATICAL FUNCTIONS.
                          for functional-structure = (loop for dependent-spec in dependent-specs
                                                           for function = (word-dependency-spec-syn-role dependent-spec)
                                                            append (cond
                                                                    ((subject-p function language)
                                                                     `((subject ,(word-dependency-spec-unit-name dependent-spec))))
                                                                    ((direct-object-p function language)
                                                                     `((direct-object ,(word-dependency-spec-unit-name dependent-spec))))
                                                                    ((indirect-object-p function language)
                                                                     `((indirect-object ,(word-dependency-spec-unit-name dependent-spec))))
                                                                    ((attribute-p function language)
                                                                     `((attribute ,(word-dependency-spec-unit-name dependent-spec))))
                                                                    ((object-predicate-p function language)
                                                                     `((object-predicate ,(word-dependency-spec-unit-name dependent-spec))))
                                                                     ((agent-p function language)
                                                                      `((agent ,(clear-retrieve-agent-unit-name dependent-spec word-specs))))
                                                                     (t
                                                                      nil)))

                          ;; (4) We get the categorical information from the morphologizer
                          for categorial-spec = (retrieve-categorical-information (word-dependency-spec-pos-tag word-spec)
                                                                                  *italian-pos-tags-spacy*)

                          ;; (4) We collect the unit
                          collect `(,(word-dependency-spec-unit-name word-spec)
                                    ,@(find-all-features-for-fcg-category
                                       (first categorial-spec)
                                       *italian-fcg-categories*
                                       :features-so-far `(,@(if dependent-specs `((dependents ,dependent-unit-names)))
                                                          ,@(if dependency-head `((dep-head ,dependency-head)))
                                                          ,@(if functional-structure `((functional-structure ,functional-structure)))
                                                          ,@(rest categorial-spec)))))))
       ;; Now we add the new units to the transient structure.
      ;; This is situated in the left-pole-structure of the transient structure (right-pole-structure is no longer used).
      (setf (left-pole-structure transient-structure)
            (append (left-pole-structure transient-structure) new-units))
      (set-data transient-structure :dependency-tree dependency-tree)
      (set-data transient-structure :word-specs word-specs)
      transient-structure))

; (comprehend "mi piacciono le ciliegie" :cxn-inventory *fcg-italian*)


