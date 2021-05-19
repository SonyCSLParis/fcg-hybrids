
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

;; 1- Interfacing with the NLP-tools package
;; 2- Dependency conversion tables

(in-package :fcg)

;; 1- Interfacing with the :nlp-tools package
;; ------------------------------------------------------------------------------------------------
(export '(word-dependency-spec
          word-dependency-spec-string word-dependency-spec-syn-role word-dependency-spec-unit-name
          word-dependency-spec-pos-tag word-dependency-spec-node-id word-dependency-spec-head-id
          word-dependency-spec-conjunct-type
          make-word-specs-for-boundaries))

(defstruct word-dependency-spec
  "A STRUCT to keep track of dependency-structure information and unit information."
  string syn-role unit-name pos-tag node-id head-id conjunct-type)

(defun make-word-specs-for-boundaries (boundaries dependency-tree)
  "A useful function to have for customizing the translate-dependency-tree method."
  (loop for boundary in boundaries
        for dependency in dependency-tree
        collect (make-word-dependency-spec :string (nlp-tools::dp-get-token dependency)
                                           :unit-name (first boundary)
                                           :syn-role (nlp-tools::dp-get-dependency dependency)
                                           :pos-tag (nlp-tools::dp-get-tag dependency)
                                           :node-id (parse-integer
                                                     (nlp-tools::dp-get-node-id dependency))
                                           :head-id (nlp-tools::dp-get-head-id dependency)
                                           :conjunct-type (loop for x in dependency
                                                                when (eql x :first-conjunct)
                                                                return t))))


;; 2- Generic function + default method for representing dependency structures in the transient structure.
;; -----------------------------------------------------------------------------------------------------------
(export '(represent-dependency-structure))

(defgeneric represent-dependency-structure (dependency-tree transient-structure key &key &allow-other-keys)
  (:documentation "Method for representing information from dependency parsing in transient structures."))

(defmethod represent-dependency-structure ((dependency-tree list)
                                           (transient-structure coupled-feature-structure)
                                           (key t)
                                           &key &allow-other-keys)
    "Given a dependency tree provided by the :NLP-TOOLS package, expand the transient structure with head-dependent relations.
     The transient structure is assumed to only contain a ROOT unit with the feature boundaries."
    (declare (ignorable key))
    (let* ((boundaries (fcg-get-boundaries transient-structure))
           (word-specs (make-word-specs-for-boundaries boundaries dependency-tree)) ;; For keeping track of unit names
           (new-units (loop for word-spec in word-specs
                            ;; We check whether the word has dependents:
                            for dependents = (loop for other-word-spec in word-specs
                                                   when (and (not (equal word-spec other-word-spec))
                                                             (= (word-dependency-spec-head-id other-word-spec)
                                                                (word-dependency-spec-node-id word-spec)))
                                                   collect (word-dependency-spec-unit-name other-word-spec))
                            ;; We check whether the word has a head:
                            for dependency-head = (unless ;; Unless the word doesn't point to a higher node:
                                                      (equal (word-dependency-spec-head-id word-spec)
                                                             (word-dependency-spec-node-id word-spec))
                                                    (word-dependency-spec-unit-name
                                                     (find (word-dependency-spec-head-id word-spec)
                                                           word-specs :key #'word-dependency-spec-node-id :test #'equal)))
                            ;; We collect the unit
                            collect `(,(word-dependency-spec-unit-name word-spec)
                                      ,@(if dependents `((dependents ,dependents)))
                                      (dependency (,@(if dependency-head `((head ,dependency-head)))
                                                   (pos ,(word-dependency-spec-pos-tag word-spec))
                                                   (edge ,(word-dependency-spec-syn-role word-spec))))))))
      ;; Now we add the new units to the transient structure.
      ;; This is situated in the left-pole-structure of the transient structure (right-pole-structure is no longer used).
      (setf (left-pole-structure transient-structure)
            (append (left-pole-structure transient-structure) new-units))
      (set-data transient-structure :dependency-tree dependency-tree)
      transient-structure))


;; 2- Dependency conversion tables.
;; ------------------------------------------------------------------------------------------------

(export '(fcg-get-dependency-conversion-table fcg-set-dependency-conversion-table))

(defgeneric retrieve-category-from-conversion-table (category-name conversion-table))

(defmethod retrieve-category-from-conversion-table ((category-name t)
                                                    (conversion-table list))
  (second (assoc category-name conversion-table)
                 :test #'equal))

(defun fcg-get-dependency-conversion-table (&optional cxn-inventory)
  (or (when cxn-inventory (get-configuration cxn-inventory :dependency-conversion-table))
      t)) ;; At least always return T.

(defun fcg-set-dependency-conversion-table (cxn-inventory data)
  (set-configuration cxn-inventory :dependency-conversion-table data))
