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

(defun configure-tag-sets (inventory 
                           &key (pos-tag-set *universal-pos-tags*)
                           (dependency-labels *universal-dependencies*) 
                           (feature-tag-set *universal-feature-set*) 
                           (fcg-categories *fcg-hybrids-categories*))
  "For configuring the base models."
  (set-configuration inventory :pos-tag-set pos-tag-set)
  (set-configuration inventory :dependency-labels dependency-labels)
  (set-configuration inventory :feature-tag-set feature-tag-set)
  (set-configuration inventory :fcg-categories fcg-categories)
  nil)

(defun convert-features-from-word-spec (word-spec 
                                        &key (pos-tag-set *universal-pos-tags*)
                                        (feature-tag-set *universal-feature-set*))
  "For converting a long string of spacy into lex-class and feature-value pairs."
  (let* ((source (cl-ppcre:split "__" (word-dependency-spec-pos-tag word-spec)))
         (pos-tag (first (cl-ppcre:split "_" (first source))))
         (lex-class (second (assoc pos-tag pos-tag-set :test #'string=)))
         (features-and-values (loop for fv-pair in (cl-ppcre::split "\\|" (second source))
                                    collect (cl-ppcre::split "=" fv-pair))))
    (values
     `(syn-cat ((lex-class ,lex-class)
                ,@(loop for fv-pair in features-and-values
                        for feature-spec = (assoc (first fv-pair) feature-tag-set 
                                                  :test #'string=)
                        collect `(,(second feature-spec)
                                  ,(second (assoc (second fv-pair) 
                                                  (third feature-spec) :test #'string=))))))
     lex-class)))