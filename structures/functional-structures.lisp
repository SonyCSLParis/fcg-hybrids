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

(export '(represent-functional-structure))

(defgeneric represent-functional-structure (dependency-tree transient-structure key &optional language)
  (:documentation "Generic function for customizing how the functional structure of an utterance is represented in different languages."))

;; Usage:
;; ------
;; > The argument "key" can be used for customizing different methods for the same language.
;; > The argument language should be used for customizing based on a languge.
;;
;; Example:
;; (represent-functional-structure dependency-tree transient-structure t :english)
;;
;; The default method, implemented here, simply adds information from a spacy-dependency parse (provided by the :nlp-tools package)
;; to a transient structure.

(defmethod represent-functional-structure ((dependency-tree list)
                                           (transient-structure coupled-feature-structure)
                                           (key t) ;; We assume the Penelope interface with SpaCy
                                           &optional (language t)) ;; Works for any language
  "Given a dependency tree provided by the :NLP-TOOLS package, expand the transient structure with head-dependent relations.
   The transient structure is assumed to only contain a ROOT unit with the feature boundaries."
  (declare (ignore key language))
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
                                    ,@(if dependency-head `((dependency-head ,dependency-head)))
                                    ,@(if dependents `((dependents ,dependents)))
                                    (syn-cat ((pos-tag ,(word-dependency-spec-pos-tag word-spec))
                                              (dependency-relation ,(word-dependency-spec-syn-role word-spec))))))))
    ;; Now we add the new units to the transient structure.
    ;; This is situated in the left-pole-structure of the transient structure (right-pole-structure is no longer used).
    (setf (left-pole-structure transient-structure)
          (append (left-pole-structure transient-structure) new-units))
    transient-structure))
;;
;; About functional structure:
;; ---------------------------
;; The functional structure of constructions consists in identifying grammatical functions such as
;; SUBJECT, DIRECT OBJECT, and so on. These relations can either be identified through functional
;; constructions (e.g. the Subject-Predicate construction), through preprocessing (dependency parsing),
;; or a combination of both. Below are a couple of helper functions to identify the most important
;; relations.
