;; Copyright Sony Computer Science Laboratories Paris
;; Copyright (c) 2021-Present Sony Computer Science Laboratories Paris
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

(defvar *italian-fcg-categories* nil "FCG-categories for Italian grammars.")

(setf *italian-fcg-categories*
      '(;; Adjective
        (adjective ()
                   (referent ?ref)
                   (universal-pos adjective))
        (ordinal-number (adjective))
        ;; Adverb
        (adverb ()
                (universal-pos adverb))
        (negation-adverb (adverb))
        ;; Noun
        (noun ()
              (referent ?ref)
              (universal-pos noun))
        (common-noun (noun))
        (abbreviation (noun))
        (proper-noun (noun)
                     (universal-pos proper-noun))
        ;; Verb
        (verb ()
              (referent ?ev-ref)
              (universal-pos verb))
        (main-verb (verb))
        (adposition ()
                    (universal-pos adposition))
        (preposition (adposition))
        (aux ()
             (universal-pos aux))
        (modal-aux (aux))
        ;; Determiner
        (determiner ()
                    (referent ?ref)
                    (universal-pos determiner))
        (definite-article (determiner))
        (indefinite-article (determiner))
        (exclamative-determiner (determiner))
        (indefinite-determiner (determiner))
        (relative-determiner (determiner))
        (interrogative-determiner (determiner))
        (demonstrative-determiner (determiner))
        (possessive-adjective (determiner))
        (predeterminer (determiner))
        ;; Pronoun
        (pronoun ()
                 (Referent ?ref)
                 (universal-pos pronoun))
        (clitic-pronoun (pronoun))
        (demonstrative-pronoun (pronoun))
        (personal-pronoun (pronoun))
        (indefinite-pronoun (pronoun))
        (possessive-pronoun (pronoun))
        (interrogative-pronoun (pronoun))
        (relative-pronoun (pronoun))
        ;; Punctuation
        (punctuation ()
                     (universal-pos punctuation))
        (balanced-punctuation (punctuation))
        (closed-boundary-punctuation (punctuation))
        (comma-hyphen (punctuation))
        (sentence-boundary-punctuation (punctuation))
        ;; Number
        (numeral ()
                 (universal-pos numeral))
        (cardinal-number (numeral))))
