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

;; Remi 26/04/2021:
;; The contents of this file are depreciated and will not be
;; maintained in future updates.

(in-package :fcg)

;;; Translate dependency tree.
;;; -------------------------------------------------------------------------------------
(defgeneric translate-dependency-tree (base-transient-structure
                                       dependency-tree
                                       conversion-table
                                       &key cxn-inventory &allow-other-keys))

(defmethod translate-dependency-tree ((base-transient-structure coupled-feature-structure)
                                      (dependency-tree list)
                                      (conversion-table t)
                                      &key cxn-inventory &allow-other-keys)
  (declare (ignore cxn-inventory))
  (let* ((boundaries (fcg-get-boundaries base-transient-structure))
         (word-specs (make-word-specs-for-boundaries boundaries dependency-tree))
         (units-id-and-name (loop for spec in word-specs
                                  collect (list (word-dependency-spec-node-id spec)
                                                (word-dependency-spec-unit-name spec))))
         (structure-to-append (loop for word-spec in word-specs
                                    for parent = (unless (= (word-dependency-spec-node-id word-spec)
                                                            (word-dependency-spec-head-id word-spec))
                                                   (second (assoc (word-dependency-spec-head-id word-spec)
                                                                  units-id-and-name)))
                                    for subunits = (loop for other-word-spec in word-specs
                                                         when (and
                                                               (not (equal word-spec other-word-spec))
                                                               (= (word-dependency-spec-head-id other-word-spec)
                                                                  (word-dependency-spec-node-id word-spec)))
                                                         collect (word-dependency-spec-unit-name other-word-spec))
                                    collect (make-unit :name (word-dependency-spec-unit-name word-spec)
                                                       :features `((parent ,parent)
                                                                   (subunits ,subunits)
                                                                   (form ((string
                                                                           ,(word-dependency-spec-unit-name word-spec)
                                                                           ,(word-dependency-spec-string word-spec))))
                                                                   (dependency
                                                                    ((pos-tag
                                                                      ,(intern (upcase (word-dependency-spec-pos-tag word-spec)) :fcg))
                                                                     (edge
                                                                      ,(intern (upcase (word-dependency-spec-syn-role word-spec)) :fcg))))))))
         (new-root (calculate-boundaries-and-form-constraints base-transient-structure structure-to-append)))
    (setf (left-pole-structure base-transient-structure)
          (cons new-root structure-to-append))
    base-transient-structure))

;;; Showing the tree.
;;; -------------------------------------------------------------------------------------
(defun show-translated-sentence (utterance de-render-mode &key (cxn-inventory *fcg-constructions*))
  "For showing the translated transient structure in the web interface."
  (let ((transient-structure (de-render utterance de-render-mode
                                        :cxn-inventory cxn-inventory)))
    (add-element (make-html-fcg-light transient-structure :cxn-inventory cxn-inventory
                                      :configuration (visualization-configuration cxn-inventory)
                                      :feature-types (feature-types cxn-inventory)))
    transient-structure))
