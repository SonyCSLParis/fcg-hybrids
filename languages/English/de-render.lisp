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

(defmethod de-render ((utterance string) (mode (eql :english-hybrid))
                      &key (key :english) cxn-inventory (model "en") &allow-other-keys)
  (declare (ignorable mode cxn-inventory))
  ; Step 1: Get the dependency and constituent analysis:
  (multiple-value-bind (dependency-tree constituent-tree)
      (get-english-sentence-analysis utterance)
  (let* (; Step 2: Use the dependency tree for segmenting the utterance into a list of strings:
         (utterance-as-list (nlp-tools::dp-build-utterance-as-list-from-dependency-tree dependency-tree))
         ;; Step 3: Use the list of strings for building a basic transient structure:
         (basic-transient-structure (de-render utterance-as-list :de-render-with-scope
                                               :cxn-inventory cxn-inventory)))
    ;; Step 4: Expand the transient structure with information from the dependency tree:
    (setf basic-transient-structure
          (represent-functional-structure dependency-tree basic-transient-structure key *english-dependency-specs*))
    ;; Step 5: Expand the transient structure with information from the constituent tree:
    (setf basic-transient-structure
          (represent-constituent-structure constituent-tree basic-transient-structure key cxn-inventory))
    basic-transient-structure)))
