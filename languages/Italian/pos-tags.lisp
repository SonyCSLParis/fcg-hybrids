;; Copyright Sony Computer Science Laboratories Paris
;; Authors: Remi van Trijp (http://www.remivantrijp.eu)
;;          Martina Galletti martina.galletti@sony.com
;;          Ines Blin ines.blin@sony.com

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

(defvar *italan-pos-tag-set* nil "Tagset used by SpaCy for Italian. See http://www.italianlp.it/docs/ISST-TANL-POStagset.pdf")

(setf *italian-pos-tag-set*
      '(;; A = Adjective
        ("A" adjective)
        ("AP" possessive-adjective)
        ;; B = Adverb
        ("B" adverb)
        ("BN" negation-adverb)
        ;; C = conjunction
        ("CC" coordinative-conjunction)
        ("CS" subordinative-conjunction)
        ;; D = Determiner
        ("DE" exclamative-determiner)
        ("DI" indefinite-determiner)
        ("DQ" interrogative-determiner)
        ("DR" relative-determiner)
        ("DD" demonstrative-determiner)
        ;; E = Preposition
        ("E" preposition)
        ("EA" articulated-preposition)  ;; del, alla, dei, nelle
        ;; F = Punctuation
        ("FB" balanced-punctuation)
        ("FC" closed-boundary-punctuation)
        ("FF" comma-hyphen)
        ("FS" sentence-boundary-punctuation)
        ;; I = Interjection
        ("I" interjection)
        ;; N =  Number
        ("N" cardinal-number)
        ("NO" ordinal-number)
        ;; P = Pronoun
        ("PC" clitic-pronoun)
        ("PD" demonstrative-pronoun)
        ("PE" personal-pronoun)
        ("PI" indefinite-pronoun)
        ("PP" possessive-pronoun)
        ("PQ" interrogative-pronoun)
        ("PR" relative-pronoun)
        ;; R = article
        ("RD" definite-article)
        ("RI" indefinite-article)
        ;; S = noun
        ("S" common-noun)
        ("SA" abbreviation)
        ("SP" proper-noun)
        ;; V = Verb
        ("VA" aux)
        ("VM" modal-aux)
        ("V" main-verb)
        ;; Others
        ("T" predeterminer)
        ("SYM" symbol)
        ("X" other)))
