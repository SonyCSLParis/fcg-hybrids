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

(defvar *english-pos-tags-spacy* nil "Tagset used by SpaCy for English.")

(setf *english-pos-tags-spacy*
      '(("$" currency)
        ("," comma)
        ("'" closing-quotation-mark)
        ("-LRB-" left-round-bracket)
        ("-RRB-" right-round-bracket)
        ("." punctuation-mark)
        (":" colon)
        ("ADD" email)
        ("AFX" affix)
        ("CC" coordinating-conjunction)
        ("CD" cardinal-number)
        ("DT" determiner)
        ("EX" existential-there)
        ("FW" foreign-word)
        ("HYPH" hyphen)
        ("IN" preposition)
        ("JJ" adjective)
        ("JJR" comparative-adjective)
        ("JJS" superlative-adjective)
        ("LS" list-marker)
        ("MD" modal-auxiliary)
        ("NFP" superfluous-punctuation)
        ("NN" noun)
        ("NNP" proper-singular-noun)
        ("NNPS" proper-plural-noun)
        ("NNS" plural-noun)
        ("PDT" predeterminer)
        ("POS" possessive-ending)
        ("PRP" personal-pronoun)
        ("PRP$" possessive-pronoun)
        ("RB" adverb)
        ("RBR" comparative-adverb)
        ("RBS" superlative-adverb)
        ("RP" particle)
        ("SYM" symbol)
        ("TO" infinitival-to)
        ("UH" interjection)
        ("VB" verb-base-form)
        ("VBD" verb-past)
        ("VBN" verb-past-participle)
        ("VBG" verb-ing-form)
        ("VBZ" verb-3sg)
        ("WDT" wh-determiner)
        ("WP" wh-personal-pronoun)
        ("WP$" wh-possessive-pronoun)
        ("WRB" wh-adverb)
        ("XX" unknown)
        ("`" opening-quotation-mark)
        ;; Adding dependency relations that function as categories as well:
        ("aux" aux)
        ("auxpass" auxpass)))
