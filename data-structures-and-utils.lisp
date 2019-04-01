;;;;; Copyright (c) Sony Computer Science Laboratories Paris
;;;;;               VUB Artificial Intelligence Laboratory, Brussels
;;;;;
;;;;; Written by Remi van Trijp (remi.vantrijp@sony.com)
;;;;; -----------------------------------------------------------------------------------

(in-package :fcg)

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
