;; Copyright 2022 Sony Computer Science Laboratories Paris
;; Authors:       Remi van Trijp (http://www.remivantrijp.eu)

;; Copyright 2024-present Sony Computer Science Laboratories - Paris
;; Authors:               Remi van Trijp (http://www.remivantrijp.eu)

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

(defmethod de-render ((utterance string) (mode (eql :dutch-hybrid))
                      &key (key :dutch) cxn-inventory (model "nl") &allow-other-keys)
  (declare (ignorable mode))
  ;; Step 1: Get the dependency analysis from the NLP-tools:
  (multiple-value-bind (basic-transient-structure dependency-tree)
      (de-render-basic-transient-structure utterance cxn-inventory model)
    (declare (ignorable dependency-tree))
    ;; Step 2: Expand the transient structure with information from the dependency tree.
    (setf basic-transient-structure
          (represent-functional-structure 
           dependency-tree basic-transient-structure key *dutch-fcg-categories*))
    ;; Return the transient structure:
    basic-transient-structure))
