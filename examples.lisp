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

;;;;; Example of the basic syntactic dependency parser using the spacy interface from :nlp-tools
;;;;; -------------------------------------------------------------------------------------------------
(defmethod de-render ((utterance string) (mode (eql :spacy-dependency-parser))
                      &key (key t) cxn-inventory (model "en") &allow-other-keys)
  (declare (ignorable mode cxn-inventory))
  (let* (; Step 1: Get the dependency tree:
         (dependency-tree (nlp-tools:get-penelope-dependency-analysis utterance :model model))
         ; Step 2: Use the dependency tree for segmenting the utterance into a list of strings:
         (utterance-as-list (nlp-tools::dp-build-utterance-as-list-from-dependency-tree dependency-tree))
         ;; Step 3: Use the list of strings for building a basic transient structure:
         (basic-transient-structure (de-render utterance-as-list :de-render-with-scope
                                               :cxn-inventory cxn-inventory)))
    ;; Step 4: Expand the transient structure with information from the dependency tree:
    (represent-dependency-structure dependency-tree basic-transient-structure key)))

;; (de-render "I like cherries" :spacy-dependency-parser :model "en")          ;; English
;; (de-render "Ik hou van kersen" :spacy-dependency-parser :model "nl")        ;; Dutch
;; (de-render "Ich mag Kirschen" :spacy-dependency-parser :model "de")         ;; German
;; (de-render "Mi piacciono le ciliegie" :spacy-dependency-parser :model "it") ;; Italian
;; (de-render "Me gustan las cerezas" :spacy-dependency-parser :model "es")    ;; Spanish
;; (de-render "J'aime les cerises" :spacy-dependency-parser :model "fr")       ;; Spanish

;;;; Example of treating named entities such as "Barack Obama" as one string instead of two nodes in
;;;; the dependency tree. This could be done in the spacy NLP-pipeline directly, but here we manipulate
;;;; the results in Lisp.
(defmethod de-render ((utterance string) (mode (eql :spacy-dependency-parser-with-additional-preprocessing))
                      &key key cxn-inventory (model "en") &allow-other-keys)
  (declare (ignorable mode))
  (multiple-value-bind (dependency-tree utterance-as-list base-transient-structure)
      (preprocess-using-dependency-tree utterance :preprocessing-steps (list #'dependency-string-append-named-entities)
                                        :model model :cxn-inventory cxn-inventory)
    (represent-dependency-structure dependency-tree base-transient-structure key)))
;; (de-render "I saw Barack Obama." :spacy-dependency-parser-with-additional-preprocessing)