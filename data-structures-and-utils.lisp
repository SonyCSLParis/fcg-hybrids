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

;;; Hybrid pathname.
;;; -------------------------------------------------------------------------------------
(defparameter *dev-tools-pathname* nil)
(setf *dev-tools-pathname* (make-pathname :directory (pathname-directory (lispworks:current-pathname))))

;;; Data structures.
;;; -------------------------------------------------------------------------------------
(export '(word-dependency-spec
          word-dependency-spec-string word-dependency-spec-syn-role word-dependency-spec-unit-name
          word-dependency-spec-pos-tag word-dependency-spec-node-id word-dependency-spec-head-id))

(defstruct word-dependency-spec string syn-role unit-name pos-tag node-id head-id)

;;; Helper functions.
;;; -------------------------------------------------------------------------------------

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

(defun calculate-boundaries-and-form-constraints (base-transient-structure
                                                  unit-tree
                                                  &optional (cxn-inventory *fcg-constructions*))
  "When the tree is translated, update the boundaries and form constraints."
  (let* ((strings (fcg-extract-selected-form-constraints base-transient-structure '(string)))
         (temp-node (make-instance 'cip-node
                                   :construction-inventory cxn-inventory
                                   :car (make-cxn-application-result
                                         :resulting-cfs 
                                         (make-instance 'coupled-feature-structure
                                                        :left-pole (append
                                                                    (left-pole-structure
                                                                     base-transient-structure)
                                                                    unit-tree)))))
         (old-boundaries (fcg::fcg-get-boundaries base-transient-structure))
         (new-boundaries (fcg::update-list-of-boundaries old-boundaries temp-node))
         (new-form-constraints (infer-all-constraints-from-boundaries
                                new-boundaries
                                (get-updating-references cxn-inventory)
                                (fcg-get-transient-unit-structure temp-node))))
    `(root
      (boundaries ,new-boundaries)
      (form ,(append strings new-form-constraints)))))
