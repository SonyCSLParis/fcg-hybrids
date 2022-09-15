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

(defun convert-features-from-word-spec (word-spec conversion-table)
  (let* ((source (cl-ppcre:split "__" (word-dependency-spec-pos-tag word-spec)))
         (lex-class (second (assoc (first source) *french-pos-tags-spacy* :test #'string=)))
         (features-and-values (loop for fv-pair in (cl-ppcre::split "\\|" (second source))
                                    collect (cl-ppcre::split "=" fv-pair))))
      `(syn-cat ((lex-class ,lex-class)
                 ,@(loop for fv-pair in features-and-values
                         for feature-spec = (assoc (first fv-pair) conversion-table :test #'string=)
                         collect `(,(second feature-spec)
                                   ,(second (assoc (second fv-pair) (third feature-spec) :test #'string=))))))))

(defmethod represent-functional-structure ((dependency-tree list)
                                           (transient-structure coupled-feature-structure)
                                           (key (eql :french))
                                           &optional language)
  (declare (ignore key))
  (let* ((boundaries (fcg-get-boundaries transient-structure))
         (word-specs (make-word-specs-for-boundaries boundaries dependency-tree))
         ;; weight assume that only the ROOT currently exists
         (new-units (loop for word-spec in word-specs
                            ;; (1) We check whether the word has a head:
                            for dependency-head = (unless
                                                      (equal (word-dependency-spec-head-id word-spec)
                                                             (word-dependency-spec-node-id word-spec))
                                                    (word-dependency-spec-unit-name
                                                     (find (word-dependency-spec-head-id word-spec)
                                                           word-specs :key #'word-dependency-spec-node-id :test #'equal)))
                            ;; (2) We check whether the word has dependents
                            for dependent-specs = (loop for other-word-spec in word-specs
                                                        when (and (not (equal word-spec other-word-spec))
                                                                  (= (word-dependency-spec-head-id other-word-spec)
                                                                     (word-dependency-spec-node-id word-spec)))
                                                          collect other-word-spec)
                          for dependent-unit-names = (mapcar #'word-dependency-spec-unit-name dependent-specs)
                            ;; (3) TODO: figure out functional relations

                            ;; (4) We collect the units
                            collect `(,(word-dependency-spec-unit-name word-spec)
                                      ,@(if dependency-head `((dep-head ,dependency-head)))
                                      ,@(if dependent-specs `((dependents ,dependent-unit-names)))
                                      ,(convert-features-from-word-spec word-spec *french-category-conversion*)))))
    (Setf (left-pole-structure transient-structure)
          (append (left-pole-structure transient-structure) new-units))
    (set-data transient-structure :dependency-tree dependency-tree)
    (set-data transient-structure :word-specs word-specs)
    transient-structure))
