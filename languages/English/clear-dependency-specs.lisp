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

;;;;; This file supports syntactic dependency parsing for English using
;;;;; SpaCy (https://spacy.io/models/en).
;;;;;
;;;;; The Spacy Syntactic Dependency Parser uses the CLEAR style by ClearNLP.
;;;;; http://www.mathcs.emory.edu/~choi/doc/cu-2012-choi.pdf
;;;;; (CLEAR = Center for Computational Language and EducAtion Research, University of Colorado Boulder)
;;;;;
;;;;; This file contains specifications about these dependency-tags and some
;;;;; helper functions to access information from these specs.

(export '(*english-dependency-specs*))

(defparameter *functions-that-are-categories* nil "List of functions that are categories in the grammar.")
(setf *functions-that-are-categories* '(("aux" aux)
                                        ("auxpass" auxpass)
                                        ("det" determiner)))

;;;;; ------------------------------------------------------------------------------------------
;;;;; Dependency Specs for English.
;;;;; ------------------------------------------------------------------------------------------

(defvar *english-dependency-specs* nil "List of dependency relations and associated specifications.")

(setf *english-dependency-specs*
      (list ;; Subject-Related
            (make-dependency-spec :label "nsubj"
                                  :keywords '(core-argument functional nominal subject)
                                  :description "Nominal Subject")
            (make-dependency-spec :label "nsubjpass"
                                  :keywords '(core-argument functional nominal subject)
                                  :description "Nominal Passive Subject")
            (make-dependency-spec :label "csubj"
                                  :keywords '(core-argument functional clausal-subject subject)
                                  :description "Clausal subject.")
            (make-dependency-spec :label "csubjpass"
                                  :keywords '(core-argument functional clausal-subject subject)
                                  :description "Clausal passive subject.")
            (make-dependency-spec :label "expl"
                                  :keywords '(core-argument functional nominal subject)
                                  :description "Expletive (existential there).")
            ;; Object-Related:
            (make-dependency-spec :label "dobj"
                                  :keywords '(core-argument functional nominal object direct-object)
                                  :description "Direct object")
            (make-dependency-spec :label "dative"
                                  :keywords '(core-argument functional nominal object indirect-object)
                                  :description "Indirect Object")
            (make-dependency-spec :label "agent"
                                  :keywords '(core-argument functional nominal agent by-phrase)
                                  :description "Preposition 'by' that introduces the agent of a passive verb")
            (make-dependency-spec :label "attr"
                                  :keywords '(core-argument functional nominal object attribute)
                                  :description "Non-VP predicate after copula.")
            (make-dependency-spec :label "oprd"
                                  :keywords '(core-argument functional nominal complement object-predicate)
                                  :description "Object predicate.")
            ;; Auxiliaries
            (make-dependency-spec :label "aux"
                                  :keywords '(lexical verbal aux)
                                  :description "Auxiliary.")
            (make-dependency-spec :label "auxpass"
                                  :keywords '(lexical verbal aux passive)
                                  :description "Passive auxiliary.")
            ;; hmod - modifier in hyphenation -
            ;; hyph hyphen
            ;; Complements
            (make-dependency-spec :label "acomp"
                                  :keywords '(functional adjectival complement)
                                  :description "Adjectival complement.")
            (make-dependency-spec :label "ccomp"
                                  :keywords '(functional clausal complement)
                                  :description "Clausal complement.")
            (make-dependency-spec :label "xcomp"
                                  :keywords '(functional clausal complement)
                                  :description "Open clausal complement.")
            (make-dependency-spec :label "complm"
                                  :keywords '(marker clausal complementizer)
                                  :description "Subordinating conjunction.")
            ;; Modifiers
            (make-dependency-spec :label "advcl"
                                  :keywords '(functional clausal adverbial)
                                  :description "Adverbial clause modifier.")
            (make-dependency-spec :label "relcl"
                                  :keywords '(functional clausal phrase)
                                  :description "Root of the relative clause.")
            (make-dependency-spec :label "acl"
                                  :keywords '(functional clausal phrase)
                                  :description "Clausal modifier of a noun.")
            (make-dependency-spec :label "advmod"
                                  :keywords '(functional advberbial adverbial)
                                  :description "Adverbial modifier.")
            (make-dependency-spec :label "mark"
                                  :keywords '(functional adverbial advp)
                                  :description "Marker for adverbial clause modifier.")
            (make-dependency-spec :label "neg"
                                  :keywords '(lexical adverbial adverbial)
                                  :description "Negation modifier.")
            (make-dependency-spec :label "npadvmod"
                                  :keywords '(functional nominal adverbial)
                                  :description "NP as adverbial modifier.")
            ;; Coordination-related modifiers.
            (make-dependency-spec :label "conj"
                                  :keywords '(ignoreable nominal conjunt)
                                  :description "Conjunct dependent, e.g. John, Mary and Sam")
            (make-dependency-spec :label "cc"
                                  :keywords '(lexical conjunction)
                                  :description "Coordinating conjunction.")
            (make-dependency-spec :label "preconj"
                                  :keywords '(conjunction conjunction)
                                  :description "Pre-correlative conjunction.")
            ;; NP-modifiers.
            (make-dependency-spec :label "nmod"
                                  :keywords '(lexical nominal modifier)
                                  :description "Unclassified modifier of the head noun.")
            (make-dependency-spec :label "appos"
                                  :keywords '(functional nominal modifier)
                                  :description "Appositional modifier NP of another NP.")
            (make-dependency-spec :label "det"
                                  :keywords '(lexical determiner)
                                  :description "Determiner of an NP.")
            (make-dependency-spec :label "wdt"
                                  :keywords '(lexical determiner determiner)
                                  :description "Determiner of an NP.")
            (make-dependency-spec :label "infmod"
                                  :keywords '(functional verbal modifier)
                                  :description "Infinitival modifier.")
            (make-dependency-spec :label "nn"
                                  :keywords '(lexical nominal compound)
                                  :description "Noun compound modifier.")
            (make-dependency-spec :label "num"
                                  :keywords '(lexical nominal numerical)
                                  :description "Numerical modifier.")
            (make-dependency-spec :label "partmod"
                                  :keywords '(functional clausal complement)
                                  :description "Participial modifier.")
            (make-dependency-spec :label "poss"
                                  :keywords '(lexical possessive-pronoun pronominal)
                                  :description "Possessive modifier.")
            (make-dependency-spec :label "predet"
                                  :keywords '(lexical nominal predeterminer)
                                  :description "Predeterminer word such as all.")
            (make-dependency-spec :label "rcmod"
                                  :keywords '(functional clausal relative-clause)
                                  :description "Relative clause modifier.")
            ;; Prepositional phrase related modifiers.
            (make-dependency-spec :label "pcomp"
                                  :keywords '(functional it-depends complement)
                                  :description "Prepositional complement phrase.")
            (make-dependency-spec :label "pobj"
                                  :keywords '(functional nominal NP)
                                  :description "NP of a prepositional phrase.")
            (make-dependency-spec :label "prep"
                                  :keywords '(functional nominal PrepNP)
                                  :description "Preposition of a PrepNP.")
            ;; Quantifier phrase related modifiers.
            (make-dependency-spec :label "number"
                                  :keywords '(lexical nominal quantifier)
                                  :description "Number quantifier.")
            (make-dependency-spec :label "quantmod"
                                  :keywords '(lexical nominal quantifier)
                                  :description"Quantifier phrase modifier.")
            ;; Miscellaneous modifiers.
            (make-dependency-spec :label "amod"
                                  :keywords '(lexical nominal adjectival)
                                  :description "Adjective.")
            (make-dependency-spec :label "dep"
                                  :keywords '(functional unclassified)
                                  :description "Unclassified dependent.")
            (make-dependency-spec :label "intj"
                                  :keywords '(lexical interjection)
                                  :description "Interjection.")
            (make-dependency-spec :label "meta"
                                  :keywords '(lexical meta)
                                  :description "Meta modifier (randomly inserted in a clause).")
            (make-dependency-spec :label "parataxis"
                                  :keywords '(parenthetical)
                                  :description "Parenthetical modifier.")
            (make-dependency-spec :label "prt"
                                  :keywords '(lexical particle particle)
                                  :description "Particle of a phrasal verb.")
            (make-dependency-spec :label "punct"
                                  :keywords '(punctuation)
                                  :description "Punctuation.")
            ;; Others:
            (make-dependency-spec :label "wp"
                                  :keywords '(functional nominal)
                                  :description "WH pronoun.")
            (make-dependency-spec :label "case"
                                  :keywords '(lexical nominal possessive)
                                  :description "Genetive 's.")
            (make-dependency-spec :label "ROOT"
                                  :keywords '(phrase root)
                                  :description "Root of the dependency tree.")))

(defun object-predicate-p (label &optional (dependency-specs *english-dependency-specs*))
  "Is the dependency relation an object predicate?"
  (dependency-spec-member 'object-predicate (get-dependency-spec label dependency-specs)))

;;;;; ------------------------------------------------------------------------------------------
;;;;; Helper functions for data structures.
;;;;; ------------------------------------------------------------------------------------------

(defmethod retrieve-category-from-conversion-table ((category-name t)
                                                    (conversion-table clear-dependency))
  (second (assoc category-name (clear-dependency-specs conversion-table)
                 :test #'string=)))

(defun lex-class-p (dependency-spec)
  (eql 'lexical (second dependency-spec)))

(defun dependency-spec-category (dependency-spec)
  (fourth dependency-spec))

(defun english-retrieve-category (word-spec)
  (or (assoc (word-dependency-spec-syn-role word-spec) *functions-that-are-categories* :test #'string=)
      (assoc (word-dependency-spec-pos-tag word-spec) *spacy-pos-tag-conversion-table* :test #'string=)))

(defun find-dependency-spec (dependency-tag)
  "Get the information associated with a dependency-tag."
  (assoc dependency-tag (clear-dependency-specs *dependency-specs*) :test #'string=))

(defun explain-dependency-tag (tag)
  (nth 4 (find-dependency-spec tag)))
;; (explain-dependency-tag "oprd")

;;;;; ------------------------------------------------------------------------------------------
;;;;; Knowledge about the Translation.
;;;;; ------------------------------------------------------------------------------------------

(defun subject-p (tag)
  (member 'subject (find-dependency-spec tag)))
;; (subject-p "nsubj")
;; (subject-p "dobj")

(defun passive-subject-p (tag)
  (member tag '("nsubjpass" "csubjpass") :test #'string=))

(defun object-p (tag)
  (member 'object (find-dependency-spec tag)))
;; (object-p "dobj")
;; (object-p "nsubj")

(defun indirect-object-p (tag)
  (member 'indirect-object (find-dependency-spec tag)))
;; (indirect-object-p "dative")

(defun dative-p (tag)
  (indirect-object-p tag))
;; (indirect-object-p "dative")

(defun core-function-p (tag)
  (or (subject-p tag)
      (object-p tag)
      (dative-p tag)))

(defun clausal-dependent-p (tag &optional pos-tag)
  (or (member 'clausal (find-dependency-spec tag))
      ;; In some cases, we might have a clausal complement.
      ;; We check whether the head is verbal.
      (and pos-tag
           (member 'it-depends (find-dependency-spec tag))
           (string= "V" (subseq pos-tag 0 1)))))
;; (clausal-dependent-p "csubj")

(defun functional-dependent-p (tag)
  (member 'functional (find-dependency-spec tag)))
;; (functional-dependent-p "nsubj")
;; (functional-dependent-p "det")

(defun verbal-root-p (root)
  (string= "V" (subseq (word-dependency-spec-pos-tag root) 0 1)))

(defun verb-p (word-spec)
  (let ((pos-tag (word-dependency-spec-pos-tag word-spec)))
    (or (string= "V" (subseq pos-tag 0 1))
        (string= "MD" pos-tag)))) ;; For modal auxiliaries.

(defun adjective-p (word-spec)
  (member (word-dependency-spec-pos-tag word-spec)
          '("JJ" "JJR" "JJS") :test #'string=))

(defun by-phrase-p (word-spec)
  (string= "agent" (word-dependency-spec-syn-role word-spec)))

(defun PrepNP-p (word-spec)
  (string= "prep" (word-dependency-spec-syn-role word-spec)))

(defun auxiliary-p (word-spec)
  (member (word-dependency-spec-syn-role word-spec)
          '("aux" "auxpass") :test #'string=))

(defun negation-p (word-spec)
  (string= "neg" (word-dependency-spec-syn-role word-spec)))

(defun dependency-root-p (word-spec)
  (string= "ROOT" (word-dependency-spec-syn-role word-spec)))

(defun adverbial-modifier-p (function-or-word-spec)
  (string= "advmod" (if (eql (type-of function-or-word-spec) 'word-dependency-spec)
                      (word-dependency-spec-syn-role word-spec)
                      function-or-word-spec)))

(defun particle-p (word-spec)
  (string= "prt" (word-dependency-spec-syn-role word-spec)))

(defun genitive-p (word-spec)
  (string= "case" (word-dependency-spec-syn-role word-spec)))

(defun possessor-p (word-spec)
  (string= "poss" (word-dependency-spec-syn-role word-spec)))