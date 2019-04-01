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

;;;;; The FCG-dependency-parser hybrid uses the Spacy dependency parser
;;;;; (see semantic-dependency-parser.lisp in this folder).
;;;;;
;;;;; The Spacy Syntactic Dependency Parser uses the CLEAR style by ClearNLP.
;;;;; http://www.mathcs.emory.edu/~choi/doc/cu-2012-choi.pdf
;;;;; (CLEAR = Center for Computational Language and EducAtion Research, University of Colorado Boulder)
;;;;;
;;;;; This file contains specifications about these dependency-tags and some
;;;;; helper functions to access information from these specs.

;;;;; ------------------------------------------------------------------------------------------
;;;;; Data structures.
;;;;; ------------------------------------------------------------------------------------------

(export '(clear-dependency clear-dependency-specs make-clear-dependency clear-dependency-p))

(defparameter *functions-that-are-categories* nil "List of functions that are categories in the grammar.")
(defparameter *dependency-specs* nil "List of Tags and relevant information.")

(defstruct clear-dependency specs)

(setf *functions-that-are-categories* '(("aux" aux)
                                        ("auxpass" auxpass)
                                        ("det" determiner)))

(setf *dependency-specs* (make-clear-dependency
                          :specs '(;; Subject-Related:
                                   ("nsubj" functional nominal subject "Nominal Subject")
                                   ("nsubjpass" functional nominal subject "Nominal Passive Subject")
                                   ("csubj" functional clausal-subject subject "Clausal subject.")
                                   ("csubjpass" functional clausal-subject subject "Clausal passive subject.")
                                   ("expl" functional nominal subject "Expletive (existential there).")
                                   ;; Object-Related:
                           ("dobj" functional nominal object "Direct object")
                           ;; Other core functions:
                           ("agent" functional nominal by-phrase "Agent of passive verb")
                           ("attr" functional nominal object "Non-VP predicate after copula.")
                           ("dative" functional nominal indirect-object "Indirect Object")
                           ("oprd" functional nominal object-complement "Object predicate (= Obj Complement).")
                           ;; Auxiliaries
                           ("aux" lexical verbal aux "Auxiliary.")
                           ("auxpass" lexical verbal auxpass "Passive auxiliary.")
                           ;; hmod - modifier in hyphenation -
                           ;; hyph hyphen
                           ;; Complements
                           ("acomp" functional adjectival complement "Adjectival complement.")
                           ("ccomp" functional clausal object "Clausal complement.")
                           ("xcomp" functional clausal complement "Open clausal complement.")
                           ("complm" marker clausal complementizer "Subordinating conjunction.")
                           ;; Modifiers
                           ("advcl" functional clausal adverbial "Adverbial clause modifier.")
                           ("relcl" functional clausal phrase "Root of the relative clause.")
                           ("acl" functional clausal phrase "Clausal modifier of a noun.")
                           ("advmod" functional advberbial adverbial "Adverbial modifier.")
                           ("mark" functional adverbial advp "Marker for adverbial clause modifier.")
                           ("neg" lexical adverbial adverbial "Negation modifier.")
                           ("npadvmod" functional nominal adverbial "NP as adverbial modifier.")
                           ;; Coordination-related modifiers.
                           ("conj" functional nominal conjunt "Conjunct dependent, e.g. John, Mary and Sam")
                           ("cc" conjunction conjunction conjunction "Coordinating conjunction.")
                           ("preconj" conjunction conjunction conjunction "Pre-correlative conjunction.")
                           ;; NP-modifiers.
                           ("nmod" lexical nominal modifier "Unclassified modifier of the head noun.")
                           ("appos" functional nominal modifier "Appositional modifier NP of another NP.")
                           ("det" lexical determiner determiner "Determiner of an NP.")
                           ("wdt" lexical determiner determiner "Determiner of an NP.")
                           ("infmod" functional verbal modifier "Infinitival modifier.")
                           ("nn" lexical nominal compound "Noun compound modifier.")
                           ("num" lexical nominal numerical "Numerical modifier.")
                           ("partmod" functional clausal complement "Participial modifier.")
                           ("poss" lexical possessive-pronoun pronominal "Possessive modifier.")
                           ("predet" lexical nominal predeterminer "Predeterminer word such as all.")
                           ("rcmod" functional clausal relative-clause "Relative clause modifier.")
                           ;; Prepositional phrase related modifiers.
                           ("pcomp" functional it-depends complement "Prepositional complement phrase.")
                           ("pobj" functional nominal NP "NP of a prepositional phrase.")
                           ("prep" functional nominal PrepNP "Preposition of a PrepNP.")
                           ;; Quantifier phrase related modifiers.
                           ("number" lexical nominal quantifier "Number quantifier.")
                           ("quantmod" lexical nominal quantifier "Quantifier phrase modifier.")
                           ;; Miscellaneous modifiers.
                           ("amod" lexical nominal adjectival "Adjective.")
                           ("dep" functional unclassified unclassified "Unclassified dependent.")
                           ("intj" lexical interjection interjection "Interjection.")
                           ("meta" lexical meta meta "Meta modifier (randomly inserted in a clause).")
                           ("parataxis" parenthetical parenthetical parenthetical "Parenthetical modifier.")
                           ("prt" lexical particle particle "Particle of a phrasal verb.")
                           ("punct" lexical punctuation punctuation "Punctuation.")
                           ;; Others:
                           ("wp" functional nominal nominal "WH pronoun.")
                           ("case" lexical nominal possessive "Genetive 's.")
                           ("ROOT" root root phrase "Root of the dependency tree."))))

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

(defun adverbial-modifier-p (word-spec)
  (string= "advmod" (word-dependency-spec-syn-role word-spec)))

(defun particle-p (word-spec)
  (string= "prt" (word-dependency-spec-syn-role word-spec)))

(defun genitive-p (word-spec)
  (string= "case" (word-dependency-spec-syn-role word-spec)))

(defun possessor-p (word-spec)
  (string= "poss" (word-dependency-spec-syn-role word-spec)))