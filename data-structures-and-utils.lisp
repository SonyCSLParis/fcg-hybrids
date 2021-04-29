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

;;; Helper functions.
;;; -------------------------------------------------------------------------------------
(defun calculate-boundaries-and-form-constraints (base-transient-structure
                                                  unit-tree
                                                  &optional (cxn-inventory *fcg-constructions*))
  "Update the boundaries feature of the ROOT unit of a transient structure, and adds form constraints to the ROOT."
  (let* ((strings (fcg-extract-selected-form-constraints base-transient-structure '(string)))
         ;; Remi 26/04/2021
         ;; Temporary cip-node is necessary to be able to reuse the normal fcg processing functions.
         ;; Would be better to rewrite the update-list-of-boundaries function.
         (temp-node (make-instance 'cip-node
                                   :construction-inventory cxn-inventory
                                   :car (make-cxn-application-result
                                         :resulting-cfs 
                                         (make-instance 'coupled-feature-structure
                                                        :left-pole (append
                                                                    (left-pole-structure
                                                                     base-transient-structure)
                                                                    unit-tree)))))
         (old-boundaries (fcg-get-boundaries base-transient-structure))
         (new-boundaries (update-list-of-boundaries old-boundaries temp-node))
         (new-form-constraints (infer-all-constraints-from-boundaries
                                new-boundaries
                                (get-updating-references cxn-inventory)
                                (fcg-get-transient-unit-structure temp-node))))
    `(root
      (boundaries ,new-boundaries)
      (form ,(append strings new-form-constraints)))))
