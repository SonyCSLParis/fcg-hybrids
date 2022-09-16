;; Copyright 2022 Sony Computer Science Laboratories Paris
;; Authors:       Remi van Trijp (http://www.remivantrijp.eu)

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
                                           (key (eql :french))
                                           cxn-inventory)
  ;; We will simply call the method for universal dependencies.
  (declare (ignore key))
  (represent-functional-structure dependency-tree transient-structure :universal-dependencies 
                                  cxn-inventory))