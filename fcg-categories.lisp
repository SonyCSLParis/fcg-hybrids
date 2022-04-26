
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

;;; Definition of an FCG-CATEGORY.
;;; ------------------------------------------------------------------------------
;;; An FCG-category is a triple of the form (category-name (parents) (fv-pairs)).
;;; The feature-value pairs of the fcg-category are meant to be the VALUE of a
;;; feature in the construction, such as SYN-CAT. Overwritting a value only goes
;;; one level deep.

(defun fcg-category-name (fcg-category)
  (first fcg-category))
;; (fcg-category-name '(noun-phrase (phrase)))

(defun fcg-category-parents (fcg-category)
  (second fcg-category))
;; (fcg-category-parents '(noun-phrase (phrase)))

(defun fcg-category-features (fcg-category)
  (when fcg-category
    (subseq fcg-category 2)))
;; (fcg-category-features '(noun-phrase (phrase) (syn-cat ((phrase-type np))) (parent ?parent)))

(defun find-feature-for-fcg-category (feature-name fcg-category-name fcg-categories &key (features-so-far))
  "Return only a specific feature-value pair for a category."
  (let* ((fcg-category (assoc fcg-category-name fcg-categories))
         (parents (fcg-category-parents fcg-category)))
    (tagbody
     point-a
     (setf features-so-far (union features-so-far
                                  (second (assoc feature-name (fcg-category-features fcg-category)))
                                  :key #'first))
     (when parents
       (setf fcg-category (assoc (first parents) fcg-categories)
             parents (append (rest parents) (fcg-category-parents fcg-category)))
       (go point-a)))
    `(,feature-name ,features-so-far)))

(defun find-feature-value-for-fcg-category (feature-name fcg-category-name fcg-categories &key (features-so-far))
  (feature-value (find-feature-for-fcg-category feature-name fcg-category-name fcg-categories :features-so-far features-so-far)))

(defun first-or-var (list-or-var)
  "Helper function that returns either the argument if it's a variable, and its first element if it is a list."
  (if (variable-p list-or-var)
    list-or-var
    (first list-or-var)))

(defun find-all-features-for-fcg-category (category fcg-categories &key (features-so-far nil))
  "Given a category-name, obtain all the features or a particular feature associated with it."
  (let* ((fcg-category (assoc (if (consp category) (second category) category) fcg-categories))
         (features (fcg::rename-variables (fcg-category-features fcg-category))))
    (loop for fv-pair in features
          for old-feature = (assoc (feature-name fv-pair) features-so-far)
          for new-feature = `(,(feature-name fv-pair)
                              ,(let ((old-value (feature-value old-feature))
                                     (new-value (feature-value fv-pair)))
                                 (cond ((null old-value) new-value)
                                       ((null new-value) old-value)
                                       ((variable-p old-value) new-value)
                                       ((variable-p new-value) old-value)
                                       (t
                                        (union old-value new-value :key #'first)))))
          do (setf features-so-far (if old-feature
                                     (substitute new-feature old-feature features-so-far :test #'equal)
                                     (cons new-feature features-so-far))))
    (loop for parent in (fcg-category-parents fcg-category)
          do (setf features-so-far (find-all-features-for-fcg-category parent fcg-categories
                                                                       :features-so-far features-so-far)))
    features-so-far))
