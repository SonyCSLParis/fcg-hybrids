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

(defun clear-retrieve-agent-unit-name (by-spec word-specs)
  "When we have an AGENT relation in the CLEAR dependency labels, find the actual agent (pobj)."
  (loop for word-spec in word-specs
        when (and (string= "pobj" (word-dependency-spec-syn-role word-spec))
                  (equal (word-dependency-spec-head-id word-spec)
                         (word-dependency-spec-node-id by-spec)))
        return (word-dependency-spec-unit-name word-spec)))

;; Functional Structure:
;; -------------------------------------------------------------------------------------------------------------
;; We use the same method for simultaneously representing information regarding functional structure,
;; dependency relations, and part-of-speech information.

(defun retrieve-lexical-fcg-category (word-dependency-spec 
                                      &optional (pos-tags *english-pos-tags-spacy*))
  (second (or (assoc (word-dependency-spec-syn-role word-dependency-spec) 
                     pos-tags :test #'string=)
              (assoc (word-dependency-spec-pos-tag word-dependency-spec) 
                     pos-tags :test #'string=))))

(defmethod represent-functional-structure ((dependency-tree list)
                                           (transient-structure coupled-feature-structure)
                                           (key (eql :english))
                                           cxn-inventory)
  (declare (ignore key cxn-inventory))
  (let* ((language *english-dependency-specs*)
         (boundaries (fcg-get-boundaries transient-structure))
         (word-specs (make-word-specs-for-boundaries boundaries dependency-tree))
         ;; We assume that we only have the ROOT unit, and that we need to add units for each word.
         (new-units 
          (loop for word-spec in word-specs
                  ;; (1) We check whether the word has a head:
                  for dependency-head = (unless ;; Unless the word doesn't point to a higher node:
                                            (equal (word-dependency-spec-head-id word-spec)
                                                   (word-dependency-spec-node-id word-spec))
                                          (word-dependency-spec-unit-name
                                           (find (word-dependency-spec-head-id word-spec)
                                                 word-specs :key #'word-dependency-spec-node-id 
                                                 :test #'equal)))
                          
                  ;; (2) We check whether the word has dependents:
                  for dependent-specs 
                  = (loop for other-word-spec in word-specs
                          when (and (not (equal word-spec other-word-spec))
                                    (= (word-dependency-spec-head-id other-word-spec)
                                       (word-dependency-spec-node-id word-spec)))
                            collect other-word-spec)
                for dependent-unit-names 
                  = (mapcar #'word-dependency-spec-unit-name dependent-specs)

                  ;;(3) We check whether the dependents involve GRAMMATICAL FUNCTIONS.
                  for functional-structure 
                  = (loop for dependent-spec in dependent-specs
                          for function = (word-dependency-spec-syn-role dependent-spec)
                          append (cond
                                  ((subject-p function language)
                                   `((subject ,(word-dependency-spec-unit-name dependent-spec))))
                                  ((direct-object-p function language)
                                   `((direct-object 
                                      ,(word-dependency-spec-unit-name dependent-spec))))
                                  ((indirect-object-p function language)
                                   `((indirect-object 
                                      ,(word-dependency-spec-unit-name dependent-spec))))
                                  ((attribute-p function language)
                                   `((attribute ,(word-dependency-spec-unit-name dependent-spec))))
                                  ((object-predicate-p function language)
                                   `((object-predicate ,(word-dependency-spec-unit-name dependent-spec))))
                                  ((agent-p function language)
                                   `((agent ,(clear-retrieve-agent-unit-name dependent-spec word-specs))))
                                  (t
                                   nil)))

                          ;; (4) We collect the unit
                          collect `(,(word-dependency-spec-unit-name word-spec)
                                    ,@(find-all-features-for-fcg-category
                                       (retrieve-lexical-fcg-category word-spec *english-pos-tags-spacy*)
                                       *english-fcg-categories*
                                       :features-so-far `(,@(if dependent-specs `((dependents ,dependent-unit-names)))
                                                          ,@(if dependency-head `((dep-head ,dependency-head)))
                                                          ,@(if functional-structure `((functional-structure ,functional-structure)))))))))
      ;; Now we add the new units to the transient structure.
      ;; This is situated in the left-pole-structure of the transient structure (right-pole-structure is no longer used).
      (setf (left-pole-structure transient-structure)
            (append (left-pole-structure transient-structure) new-units))
      (set-data transient-structure :dependency-tree dependency-tree)
      (set-data transient-structure :word-specs word-specs)
      transient-structure))

;; Constituent Structure (based on BeNePar):
;; -------------------------------------------------------------------------------------------------------------

(defun flatten-constituent-tree (constituent-tree)
  (if (null constituent-tree)
    constituent-tree
    (let ((current-category (first constituent-tree))
          (children (rest constituent-tree)))
      (cond ((symbolp (first children)) children)
            ((and (not (string= "S" (symbol-name current-category)))
                  (member current-category (mapcar #'first children)))
             (flatten-constituent-tree `(,current-category
                                         ,@(loop for child in children
                                                 append (if (eql current-category (first child))
                                                          (rest child)
                                                          (list child))))))
            ((and (string= "S" (symbol-name current-category))
                  (= (length children) 1)
                  (string= "VP" (symbol-name (caar children))))
             (flatten-constituent-tree (first children)))
            (t
             `(,current-category ,@(mapcar #'flatten-constituent-tree children)))))))

;; (comprehend "I will be going" :cxn-inventory *fcg-english*)
(defmethod represent-constituent-structure ((constituent-tree list)
                                            (transient-structure coupled-feature-structure)
                                            (key (eql :english))
                                            (cxn-inventory t)
                                            &optional (language t))
  "Represent constituent structure using BeNePar, assuming already a dependency structure."
  (declare (ignore key language))
  (let* (;; We already have units for all terminal nodes of the constituent structure:
         (original-unit-names (mapcar #'first (fcg-get-boundaries transient-structure)))
         (original-units (fcg-get-transient-unit-structure transient-structure))
         (units nil))
    (setf constituent-tree (flatten-constituent-tree constituent-tree))
    ;; We will first traverse the tree and create a list of units that have parent- and
    ;; constituent-features. We will afterwards replace the terminal nodes with the original
    ;; units from the dependency tree so we merge the constituent- and dependency-tree information.
    (let ((queue (list (list nil (make-unit-id (first constituent-tree)) constituent-tree (first constituent-tree)))))
      (tagbody
       start
       ;; If we have finished every unit, go to the end.
       (when (null queue)
         (go end))
       ;; We now handle the first element in the queue.
       (let* ((current (pop queue))
              (parent (first current)) ;; E.g. s-unit
              (unit-name (second current)) ;; e.g. np-unit
              (unit-spec (third current)) ;; e.g. ((ART "the") (NN "cat")
              (category (fourth current))) ;; e.g. NP
         ;; If it is a terminal node, simply create a unit for it.
         (if (terminal-node-p unit-spec)
           (push `(,unit-name
                   (parent ,parent)) units)
           ;; If it is not a terminal node, create a unit with constituents.
           ;; We will add their children to the front of the queue.
           (let ((new-nodes-to-queue (loop for constituent in (rest unit-spec)
                                           collect (list unit-name
                                                         (make-unit-id (first constituent))
                                                         constituent
                                                         (first constituent)))))
             (setf queue (append new-nodes-to-queue queue))
             (push `(,unit-name
                     ,@(find-all-features-for-fcg-category (fetch-syntactic-category category) *english-fcg-categories*
                                                       :features-so-far `(,@(if parent `((parent ,parent)))
                                                                          (constituents ,(mapcar #'second new-nodes-to-queue)))))
                   units)))
       ;; Now we reiterate
       (go start))
       end))
    ;; Now we have all the units, we can collect all the terminal-node units.
    (setf units (reverse units))
    (let ((terminal-node-units (loop for unit in units
                                     unless (has-constituents-p unit)
                                     collect unit))
          (subst-pairs nil))
      ;; We now replace those units with the original units from the dependency tree for
      ;; unit name consistency.
      (loop for constituent-tree-unit in terminal-node-units
            for original-unit-name in original-unit-names
            do (let* ((original-unit (assoc original-unit-name original-units))
                      (merged-unit `(,(first original-unit)
                                     ,@(append (rest constituent-tree-unit)
                                               (rest original-unit)))))
                 (push (cons (first constituent-tree-unit) original-unit-name) subst-pairs)
                 (setf units (subst merged-unit constituent-tree-unit units :test #'equal))))
      (setf units (sublis subst-pairs units))
      ;; Now we have the new unit structure.
      (setf (left-pole-structure transient-structure)
            (cons (get-root original-units) units))
      ;; We need to recalculate the boundaries and add a root unit:
      (let ((new-root (calculate-boundaries-and-form-constraints transient-structure nil cxn-inventory)))
        (setf (left-pole-structure transient-structure)
              (cons new-root units))
        transient-structure))))