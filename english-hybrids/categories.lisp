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

;;; ----------------------------------------------------------------------------
;;; *IMPORTANT*: Any changes in the *english-grammar-categories* must remain
;;; compatible with the "type hierarchy" at the end of the file, which is based
;;; on Paul Van Eecke's type hierarchies (see Van Eecke, 2018).
;;; ----------------------------------------------------------------------------

(in-package :fcg)

(export '(clause clausal-subject matrix-clause subclause intransitive-clause transitive-clause ditransitive-clause
                 phrase NP VP dativeNP advP PrepNP by-phrase
                 verb aux auxpass
                 noun compound proper-noun common-noun adjective participle
                 determiner article quantifier pronoun personal-pronoun possessive-pronoun demonstrative
                 demonstrative-pronoun args referent
                 discourse-structure object-complement
                 functional-structure head *english-grammar-categories*))

;;; 1. Definition of the English grammar categories.
;;; ------------------------------------------------------------------------------

(defparameter *english-grammar-categories* nil "Contains a type hierarchy of the categories of the grammar.")
(setf *english-grammar-categories*
      '(;; CLAUSES
        ;; ---------------------------------------------------------------
        (clause ()
                (referent ?ref)
                (syn-cat ((is-matrix-clause ?plus-or-minus)
                          (clause-type ?clause-type))))
        (matrix-clause (clause)
                       (syn-cat ((is-matrix-clause +))))
        (subclause (clause)
                   (syn-cat ((is-matrix-clause -))))
        ;; Phrases
        ;; ---------------------------------------------------------------
        (phrase ()
                (referent ?ref)
                (parent ?parent)
                (head ?head)
                (syn-cat ((phrase-type ?phrase-type)
                          (agreement ?agreement))))
        (NP (phrase)
            (syn-cat ((phrase-type NP))))
        (advp (phrase)
              (syn-cat ((phrase-type AdvP))))
        (PrepNP (NP)
                (syn-cat ((phrase-type PrepNP))))
        (by-phrase (PrepNP)
                   (syn-cat ((case by-phrase))))
        (AdjP (phrase)
              (syn-cat ((phrase-type AdjP))))
        (object-complement (phrase)
                           (syn-cat ((phrase-type Object-Complement))))
        (VP (phrase)
            (referent ?ev-ref)
            (tam ((tense ?tense)
                  (aspect ((perfect ?perfect)
                           (progressive ?prog)))
                  (modality ?modality)))
            (syn-cat ((phrase-type VP))))
        ;; Mixtures
        ;; ---------------------------------------------------------------
        (clausal-subject (subclause NP))
        ;; Lex-classes
        ;; ---------------------------------------------------------------
        (verb ()
              (syn-cat ((lex-class verb)
                        (agreement ?agr)
                        (finite ?finite)
                        (verb-form ?verb-form))))
        (aux (verb)
             (syn-cat ((lex-class aux))))
        (auxpass (aux)
                 (syn-cat ((is-passive-marker +))))
        (noun ()
              (referent ?ref)
              (args (?output ?input))
              (syn-cat ((agreement ?agr)
                        (lex-class noun))))
        (common-noun (noun)
                     (syn-cat ((lex-class common-noun))))
        (compound (noun))
        (proper-noun (noun)
                     (syn-cat ((lex-class proper-noun))))
        (adjective ()
                   (referent ?ref)
                   (args (?output ?input))
                   (syn-cat ((lex-class adjective))))
        (determiner ()
                    (referent ?ref)
                    (args (?ref ?input))
                    (syn-cat ((lex-class determiner))))
        (article (determiner)
                 (syn-cat ((lex-class article))))))

;;; 2. Definition of a CATEGORY-SPEC.
;;; ------------------------------------------------------------------------------
;;; A category-spec is a triplet of the form (category-name (parents) (fv-pairs)).
;;; The feature-value pairs of the category-spec are meant to be the VALUE of a
;;; feature in the construction, such as SYN. Overwritting a value only goes
;;; one level deep.

(defun category-name (category-spec)
  (first category-spec))

(defun category-parents (category-spec)
  (second category-spec))

(defun category-features (category-spec)
  (when category-spec
    (subseq category-spec 2)))

(defun find-feature-for-category (feature-name category category-tree &key (features-so-far))
  "Return only a specific feature-value pair for a category."
  (let* ((category-spec (assoc category category-tree))
         (parents (category-parents category-spec)))
    (tagbody
     point-a
     (setf features-so-far (union features-so-far
                                  (second (assoc feature-name (category-features category-spec)))
                                  :key #'first))
     (when parents
       (setf category-spec (assoc (first parents) category-tree)
             parents (append (rest parents) (category-parents category-spec)))
       (go point-a)))
    `(,feature-name ,features-so-far)))

(defun find-feature-value-for-category (feature-name category category-tree &key (features-so-far))
  (feature-value (find-feature-for-category feature-name category category-tree :features-so-far features-so-far)))

(defun first-or-var (list-or-var)
  "Helper function that returns either the argument if it's a variable, and its first element if it is a list."
  (if (variable-p list-or-var)
    list-or-var
    (first list-or-var)))

(defun find-all-features-for-category (category category-tree &key (features-so-far nil))
  "Given a category-name, obtain all the features or a particular feature associated with it."
  (let* ((category-spec (assoc (if (consp category) (second category) category) category-tree))
         (features (fcg::rename-variables (category-features category-spec))))
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
    (loop for parent in (category-parents category-spec)
          do (setf features-so-far (find-all-features-for-category parent category-tree
                                                                   :features-so-far features-so-far)))
    features-so-far))

;;; 3. Expansion operator to be used in FCG constructions
;;; ------------------------------------------------------------------------------

;;; (defmethod fcg-expand ((type (eql :category-tree)) &key value source bindings merge?))
;;;

(defparameter *spacy-pos-tag-conversion-table* nil "List of POS Tags and their corresponding categories in the grammar.")

;; Note: this conversion table is far from complete, but contains only relevant conversions.
(setf *spacy-pos-tag-conversion-table*
      '(;;("AFX" affix)
        ;;("JJ" adjective)
        ;;("JJR" adjective)
        ;;("JJS" adjective)
        ("NN" noun)
        ("NNS" noun)
        ("NNP" proper-noun)
        ("NNPS" proper-noun)
        ("VB" verb)
        ("VBD" verb)
        ("VBG" verb)
        ("VBN" verb)
        ("VBP" verb)
        ("VBZ" verb)
        ("DT" determiner)
        ("JJ" adjective)
        ("PRP$" possessive-pronoun)))