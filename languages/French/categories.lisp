;; Copyright 2022 Sony Computer Science Laboratories Paris
;; Author         Remi van Trijp (http://www.remivantrijp.eu)

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

(defvar *french-fcg-categories* nil "FCG-categories for French grammars. Not used for the time being.")
(defvar *french-category-conversion* nil "Features and value translation")

(setf *french-category-conversion*
      '(;; Lexical features
        ("PronType" subtype
         (("Prs" personal) ;; il, elle, se
          ("Rel" relative) ;; qui, dont, ou
          ("Dem" demonstrative) ;; ceux
          ("Neg" negative) ;; personne, aucun
          ("Ind" indefinite) ;; quiconque
          ("Int" interrogative) ;; que
          ("Art" article) ;; le, un
          ("Exc" exclamative))) ;; quel!
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
        ("Person" person
         (("1" 1)
          ("2" 2)
          ("3" 3)))
        ("Definite" is-definite
         (("Def" +)
          ("Ind" -)))
        ;;; VERBS:
        ("VerbForm" verb-form
         (("Fin" finite)
          ("Inf" infinitive)
          ("Part" participle)))
        ("Mood" mood
         (("Ind" indicative)
          ("Imp" imperative)
          ("Cnd" conditional)
          ("Sub" subjunctive)))
        ("Tense" tense
         (("Pres" present)
          ("Imp" imperfect-past) ;; imparfait
          ("Past" simple-past)
          ("Fut" future)))
        ("Voice" voice
         (("Pass" passive)))
        ("Polarity" polarity
         (("Neg" negative)
          ("Pos" positive)))))