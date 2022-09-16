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

(defvar *universal-feature-set* nil "Features used by universaldependencies.org.")

;; Note: this feature set has been reduced to *ONLY* support the languages 
;; ----- for which fcg-hybrids provides a base model.

(setf *universal-feature-set*
      '(;; Lexical features
        ("PronType" subtype
         (("Prs" personal)
          ("Rel" relative)
          ("Dem" demonstrative)
          ("Neg" negative)
          ("Ind" indefinite)
          ("Int" interrogative)
          ("Art" article)
          ("Exc" exclamative)))
        ("NumType" subtype
         (("Card" cardinal)
          ("Ord" ordinal)))
        ("Poss" is-possessive
         (("Yes" +)))
        ("Reflex" is-reflexive
         (("Yes" +)))
        ;; Nominal inflection:
        ("Gender" gender
         (("Masc" masculine)
          ("Fem" feminine)))
        ("Number" number
         (("Sing" singular)
          ("Plur" plural)))
        ("Degree" degree
         (("Cmp" comparative)
          ("Sup" superlative)
          ("Abs" absolutive)))
        ("Person" person
         (("1" 1)
          ("2" 2)
          ("3" 3)))
        ("Definite" is-definite
         (("Def" +)
          ("Ind" -)))
        ("Clitic" is-clitic
         (("Yes" +)))
        ;;; VERBS:
        ("VerbForm" verb-form
         (("Fin" finite)
          ("Inf" infinitive)
          ("Part" participle)
          ("Ger" gerund)))
        ("Mood" mood
         (("Ind" indicative)
          ("Imp" imperative)
          ("Cnd" conditional)
          ("Sub" subjunctive)))
        ("Tense" tense
         (("Pres" present)
          ("Imp" imperfect)
          ("Past" past)
          ("Fut" future)))
        ("Voice" voice
         (("Pass" passive)))
        ("Polarity" polarity
         (("Neg" negative)
          ("Pos" positive)))))