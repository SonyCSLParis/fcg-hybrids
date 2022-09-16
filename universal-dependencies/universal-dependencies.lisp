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

;; This file uses the Universal Dependencies Project (https://universaldependencies.org/)
;; as an example for how to create lists of dependency specs.
;; For documentation of how the UD project's annotation schemes:
;; https://universaldependencies.org/guidelines.html

(export '(dependency-spec *universal-dependencies*
                          get-dependency-spec dependency-spec-member
                          core-argument-p subject-p direct-object-p indirect-object-p))

(defstruct dependency-spec label keywords fcg-category description)
(defvar *universal-dependencies* nil "A list of universal dependencies and related information.")

(defun get-dependency-spec (label &optional (dependency-specs *universal-dependencies*))
  "Get the specifications associated with a dependency label."
  (find label dependency-specs :key #'dependency-spec-label :test #'string=))
;; (get-dependency-spec "nsubj" *english-dependency-specs*)

(defun dependency-spec-member (keyword dependency-spec)
  "Check whether a keyword is associated with a dependency spec."
  (when dependency-spec
    (member keyword (dependency-spec-keywords dependency-spec))))
;; (dependency-spec-member 'subject (get-dependency-spec "nsubj" *english-dependency-specs*))

(defun describe-dependency-label (label &optional (dependency-specs *universal-dependencies*))
  "If a description is available for the dependency relation, return it."
  (let ((dependency-spec (get-dependency-spec label dependency-specs)))
    (when dependency-spec
      (dependency-spec-description dependency-spec))))
;; (describe-dependency-label "nsubj" *universal-dependencies*)
;; (describe-dependency-label "nsubject" *universal-dependencies*)

(defun core-argument-p (label &optional (dependency-specs *universal-dependencies*))
  "Is the relation a core argument such as subject or object?"
  (dependency-spec-member 'core-argument (get-dependency-spec label dependency-specs)))
;; (core-argument-p "nsubj" *universal-dependencies*)

(defun subject-p (label &optional (dependency-specs *universal-dependencies*))
  "Is the dependency relation a subject?"
  (dependency-spec-member 'subject (get-dependency-spec label dependency-specs)))
;; (subject-p "nsubj" *universal-dependencies*)

(defun direct-object-p (label &optional (dependency-specs *universal-dependencies*))
  "Is the dependency relation a direct object?"
  (dependency-spec-member 'direct-object (get-dependency-spec label dependency-specs)))
;; (direct-object-p "obj" *universal-dependencies*)

(defun indirect-object-p (label &optional (dependency-specs *universal-dependencies*))
  "Is the dependency relation an indirect object?"
  (dependency-spec-member 'indirect-object (get-dependency-spec label dependency-specs)))
;; (indirect-object-p "iobj" *universal-dependencies*)

(defun agent-p (label &optional (dependency-specs *universal-dependencies*))
  "Is the dependency relation an agent in a passive construction?"
  (dependency-spec-member 'agent (get-dependency-spec label dependency-specs)))

(defun attribute-p (label &optional (dependency-specs *universal-dependencies*))
  "Is the dependency relation an attribute in a copula construction?"
  (dependency-spec-member 'attribute (get-dependency-spec label dependency-specs)))

(setf *universal-dependencies*
      (list
       ;; Subject-related:
       (make-dependency-spec :label "nsubj"
                             :keywords '(core-argument nominal subject)
                             :description "Nominal subject, e.g. 'The CAR is red.'")
       (make-dependency-spec :label "csubj"
                             :keywords '(core-argument clausal subject)
                             :description "Clausal subject, e.g. 'What she SAID is interesting.'")
       (make-dependency-spec :label "nsubj:pass"
                             :keywords '(core-argument nominal passive subject)
                             :description "Passive nominal subject, e.g. The THIEF was caught by the police.")
       (make-dependency-spec :label "csubj:pass"
                             :keywords '(core-argument clausal passive subject)
                             :description "Clausal passive subject.")
       ;; Object-related:
       (make-dependency-spec :label "obj"
                             :keywords '(core-argument nominal object direct-object)
                             :description "Object, e.g. 'I read a BOOK.'")
       (make-dependency-spec :label "iobj"
                             :keywords '(core-argument nominal object indirect-object)
                             :description "Indirect object, e.g. 'She gave ME a book.'")
       (make-dependency-spec :label "ccomp"
                             :keywords '(core-argument clausal complement clausal-complement)
                             :description "Clausal complement, e.g. 'He said that you LIKE to swim.'")
       (make-dependency-spec :label "xcomp"
                             :keywords '(core-argument clausal complement open-clausal-complement)
                             :description "Open clausal complement, e.g. 'I started to WORK here yesterday.'")
        ;; Non-core dependents:
        (make-dependency-spec :label "obl"
                              :keywords '(non-core-dependent nominal oblique)
                              :description "Oblique, e.g. 'She gave a book to her DAUGHTER.'")
        (make-dependency-spec :label "obl:agent"
                              :keywords '(agent non-core-dependent oblique)
                              :description "Agent in a passive sentence.")
        (make-dependency-spec :label "vocative"
                              :keywords '(non-core-dependent nominal vocative)
                              :label "Vocative, e.g. 'GUYS, take it easy!'")
        (make-dependency-spec :label "eplx"
                              :keywords '(non-core-dependent nominal expletive)
                              :description "Expletive, e.g. 'THERE is a hidden object in this room.'")
        (make-dependency-spec :label "dislocated"
                              :keywords '(non-core-dependent nominal dislocated)
                              :description "Disolacted element, e.g. in French: 'Il faut pas la manger, la PLASTICINE.'")
        (make-dependency-spec :label "advcl"
                              :keywords '(non-core-dependent clausal adverbial-clause)
                              :description "Adverbial clause modifier, e.g. 'The accident happened as night was FALLING.'")
        ;; Modifier words
        (make-dependency-spec :label "advmod"
                              :keywords '(non-core-dependent lexical adverbial-modifier)
                              :description "Adverbial modifier, e.g. 'ABOUT 200 people came to the party.'")
        (make-dependency-spec :label "discourse"
                              :keywords '(non-core-dependent discourse)
                              :description "Discourse element, e.g. the smiley in 'Iguazu is in Argentina :)'")
        ;; Function words
        (make-dependency-spec :label "aux"
                              :keywords '(function-word lexical aux)
                              :description "Auxiliary, e.g. 'She HAS won an award.'")
        (make-dependency-spec :label "aux:pass"
                              :keywords '(function-word lexical aux passive)
                              :description "Passive auxiliary, e.g. 'The thief WAS caught.'")
        (make-dependency-spec :label "cop"
                              :keywords '(function-word lexical aux)
                              :description "Copula, e.g. 'Bill IS honest.'")
        (make-dependency-spec :label "mark"
                              :keywords '(function-word lexical marker)
                              :description "Marker of a subordinate clause, e.g. 'He says THAT you like to swim.'")
        ;; Nominal dependents
        (make-dependency-spec :label "nmod"
                              :keywords '(nominal-dependent nominal nominal-modifier)
                              :description "Nominal modifier, e.g. 'the CHAIR's office' or 'the office of the CHAIR'")
        (make-dependency-spec :label "appos"
                              :keywords '(nominal-dependent nominal apposition)
                              :description "Apposition, e.g. 'Barack Obama, the former US PRESIDENT, gave a speech.'")
        (make-dependency-spec :label "nummod"
                              :keywords '(nominal-dependent number numerical-modifier)
                              :description "Numerical modifier, e.g. 'I spent FOURTY euros on that.'")
        (make-dependency-spec :label "acl"
                              :keywords '(nominal-dependent clausal adnominal-clause)
                              :description "Clausal modifier of noun, e.g. 'I have a dog NAMED Bingo.'")
        (make-dependency-spec :label "amod"
                              :keywords '(nominal-dependent lexical adjectival-modifier)
                              :description "Adjectival modifier, e.g. 'It is a RED car.'")
        (make-dependency-spec :label "det"
                              :keywords '(function-word lexical determiner)
                              :description "Determiner, e.g. 'THE car is red.'")
        (make-dependency-spec :label "clf"
                              :keywords '(function-word lexical classifier)
                              :description  "Classifier. Does not occur in the languages currently supported.")
        (make-dependency-spec :label "case"
                              :keywords '(function-word lexical case-marker)
                              :description "Case marker, e.g. 'The office OF the chair.'")
        ;; Coordination:
        (make-dependency-spec :label "conj"
                              :keywords '(conjunction lexical coordination)
                              :description "Conjunct, e.g. Bill is big and HONEST.")
        (make-dependency-spec :label "cc"
                              :keywords '(coordinating-conjunction lexical coordination)
                              :description "Coordinating conjunction, e.g. 'Bill is big AND honest.'")
        ;; MWE (multiword expressions):
        (make-dependency-spec :label "fixed"
                              :keywords '(mwe)
                              :description "Fixed multiword expression, e.g. 'He cried because OF you.'")
        (make-dependency-spec :label "flat"
                              :keywords '(mwe)
                              :description "Flat multiword expression, e.g. 'New YORK' or 'President OBAMA'")
        (make-dependency-spec :label "compound"
                              :keywords '(mwe lexical compound)
                              :description "Compound multiword expression, e.g. 'a phone BOOK'")
        ;; Loose
        (make-dependency-spec :label "list"
                              :keywords '(loose lexical list)
                              :description "List of comparable items, e.g. 'Long lines, silly RULES, rude STAFF, Ok FOOD'")
        (make-dependency-spec :label "parataxis"
                              :keywords '(loose parataxis)
                              :description "Parataxis, e.g. 'Let's face it, we're ANNOYED.'")
        ;; Special
        (make-dependency-spec :label "orphan"
                              :keywords '(special orphan)
                              :description "Cases of head ellipsis where promotion would lead to errors, e.g. 'Marie won gold and Peter BRONZE.'")
        (make-dependency-spec :label "goeswith"
                              :keywords '(special goeswith)
                              :description "Parts of words that were wrongly edited, e.g. 'They come here with OUT legal permission.'")
        (make-dependency-spec :label "reparandum"
                              :keywords '(special reparandum)
                              :description "For indicating a disfluency corrected by a speech repair, e.g. 'Go to the RIGH- to the left'")
        ;; Other
        (make-dependency-spec :label "punct"
                              :keywords '(other punctuation)
                              :description "Punctiation, e.g. 'Go home !'")
        (make-dependency-spec :label "root"
                              :keywords '(other root)
                              :description "The root of the utterance, e.g. 'I LIKE cherries.'")
        (make-dependency-spec :label "dep"
                              :keywords '(other lexical unspecified-dependency)
                              :description "An unspecified dependency, e.g. 'My dad doesn't really not that GOOD.'")))