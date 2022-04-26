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

(defvar *italan-pos-tags-spacy* nil "Tagset used by SpaCy for Italian. See http://www.italianlp.it/docs/ISST-TANL-POStagset.pdf")

(setf *italian-pos-tags-spacy*
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

(defun get-fcg-category-name (name lst)
  (second (assoc name lst :test #'string=)))

(defun parse-morphologizer-spec (spacy-morphology-string pos-tag-set)
  "Parse string of morphological features into list of feature-value pairs"
  (let ((split (split-string spacy-morphology-string "__")))
    (assert (= 2 (length split)))
    (values (second  (assoc (first split) pos-tag-set :test #'string=))
            (loop for fv-spec in (split-sequence::split-sequence #\| (second split))
                  collect (split-sequence::split-sequence #\= fv-spec)))))
;; (parse-morphologizer-spec "pc__clitic=yes|number=sing|person=1|prontype=prs")

(defun retrieve-categorical-information (spacy-morphology-string pos-tag-set)
  (multiple-value-bind (fcg-category feature-value-pairs)
      (parse-morphologizer-spec spacy-morphology-string pos-tag-set)
    (let (agreement syn-cat)
      (loop for fv-pair in feature-value-pairs
            for feature = (downcase (first fv-pair))
            do (cond ((member feature '("gender" "number" "person") :test #'string=)
                      (push (list (read-from-string feature) (read-from-string (second fv-pair))) agreement))
                     ((string= feature "prontype")
                      nil)
                   (t
                    (push (list (read-from-string feature) (read-from-string (second fv-pair))) syn-cat))))
      `(,fcg-category
        ,@(if agreement `((agreement ,agreement)))
        (syn-cat ,(cons `(lex-class ,fcg-category) syn-cat))))))


#|
Mood=Ind
Tense=Pres
VerbForm=Fin
Tense=Past
VerbForm=Part
VerbForm=Inf
POS=VERB
Tense=Past
VerbForm=Part
POS=VERB|VerbForm=Inf, Definite=Ind|Gender=Masc|Number=Sing|POS=DET|PronType=Art, Number=Sing|POS=ADJ, POS=CCONJ, NumType=Card|POS=NUM, Definite=Def|Gender=Masc|Number=Sing|POS=ADP|PronType=Art, Definite=Def|Gender=Fem|Number=Plur|POS=ADP|PronType=Art, Gender=Fem|Number=Plur|POS=NOUN, Clitic=Yes|POS=PRON|Person=3|PronType=Prs, Gender=Fem|Number=Plur|POS=ADJ, Gender=Fem|Number=Plur|POS=DET|Poss=Yes|PronType=Prs, Gender=Masc|Number=Plur|POS=ADJ, Definite=Def|Number=Sing|POS=ADP|PronType=Art, Definite=Def|Gender=Masc|Number=Sing|POS=DET|PronType=Art, Gender=Masc|NumType=Ord|Number=Sing|POS=ADJ, POS=ADV, POS=NOUN, Number=Sing|POS=NOUN, POS=VERB|VerbForm=Ger, Gender=Masc|Number=Sing|POS=DET|Poss=Yes|PronType=Prs, POS=INTJ, Clitic=Yes|Number=Sing|POS=PRON|Person=2|PronType=Prs, Mood=Ind|Number=Sing|POS=AUX|Person=1|Tense=Pres|VerbForm=Fin, Gender=Fem|Number=Sing|POS=AUX|Tense=Past|VerbForm=Part, Definite=Def|Gender=Fem|Number=Plur|POS=DET|PronType=Art, Mood=Ind|Number=Sing|POS=VERB|Person=3|Tense=Imp|VerbForm=Fin, Gender=Fem|POS=NOUN, Gender=Fem|Number=Plur|POS=VERB|Tense=Past|VerbForm=Part, Gender=Fem|Number=Sing|POS=DET|PronType=Tot, Mood=Cnd|Number=Sing|POS=AUX|Person=3|Tense=Pres|VerbForm=Fin, Mood=Cnd|Number=Plur|POS=AUX|Person=3|Tense=Pres|VerbForm=Fin, Gender=Masc|Number=Plur|POS=PRON|PronType=Ind, Number=Plur|POS=PRON|Person=3|PronType=Prs, Mood=Ind|Number=Plur|POS=AUX|Person=3|Tense=Imp|VerbForm=Fin, Gender=Masc|Number=Plur|POS=VERB|Tense=Past|VerbForm=Part, Number=Plur|POS=NOUN, POS=SCONJ, Number=Sing|POS=DET|PronType=Ind, POS=ADV|PronType=Neg, Clitic=Yes|POS=VERB|PronType=Prs|VerbForm=Inf, Gender=Fem|Number=Plur|POS=AUX|Tense=Past|VerbForm=Part, Gender=Fem|Number=Plur|POS=DET|PronType=Ind, Gender=Fem|Number=Sing|POS=PRON|PronType=Ind, POS=ADJ, Number=Sing|POS=PRON|PronType=Rel, Gender=Fem|NumType=Ord|Number=Sing|POS=ADJ, Number=Sing|POS=PRON|PronType=Ind, Gender=Masc|Number=Sing|POS=PRON|Person=3|PronType=Prs, Gender=Masc|Number=Plur|POS=AUX|Tense=Past|VerbForm=Part, Clitic=Yes|POS=VERB|Person=3|PronType=Prs|VerbForm=Ger, Mood=Ind|Number=Sing|POS=AUX|Person=3|Tense=Imp|VerbForm=Fin, Mood=Ind|Number=Plur|POS=VERB|Person=3|Tense=Imp|VerbForm=Fin, POS=DET|Poss=Yes|PronType=Prs, Gender=Masc|Number=Plur|POS=DET|Poss=Yes|PronType=Prs, Mood=Sub|Number=Sing|POS=AUX|Person=3|Tense=Pres|VerbForm=Fin, Gender=Masc|Number=Plur|POS=DET|PronType=Ind, Gender=Masc|Number=Sing|POS=PRON|PronType=Dem, Mood=Ind|Number=Sing|POS=VERB|Person=3|Tense=Past|VerbForm=Fin, Clitic=Yes|Gender=Masc|Number=Plur|POS=VERB|Person=3|PronType=Prs|VerbForm=Ger, Gender=Fem|Number=Sing|POS=DET|Poss=Yes|PronType=Prs, Gender=Masc|Number=Sing|POS=DET|PronType=Dem, Clitic=Yes|Gender=Masc|Number=Sing|POS=VERB|Person=3|PronType=Prs|VerbForm=Inf, Clitic=Yes|POS=PRON|PronType=Prs, Gender=Masc|Number=Plur|POS=DET|PronType=Tot, Clitic=Yes|Gender=Masc|Number=Plur|POS=PRON|Person=3|PronType=Prs, Clitic=Yes|Number=Sing|POS=PRON|Person=1|PronType=Prs, Degree=Cmp|Number=Plur|POS=ADJ, Clitic=Yes|Gender=Masc|Number=Plur|POS=VERB|Person=3|PronType=Prs|VerbForm=Inf, Number=Sing|POS=PRON|Person=3|PronType=Prs, Degree=Cmp|Number=Sing|POS=ADJ, Gender=Masc|Number=Plur|POS=DET|PronType=Dem, Degree=Abs|POS=ADV, Clitic=Yes|Gender=Fem|Number=Sing|POS=VERB|Person=3|PronType=Prs|VerbForm=Inf, Mood=Cnd|Number=Plur|POS=VERB|Person=3|Tense=Pres|VerbForm=Fin, Clitic=Yes|Gender=Masc|Number=Sing|POS=AUX|Person=3|PronType=Prs|VerbForm=Inf, Gender=Fem|Number=Sing|POS=DET|PronType=Dem, POS=DET|PronType=Exc, Number=Plur|POS=PRON|Person=1|PronType=Prs, Mood=Ind|Number=Plur|POS=AUX|Person=1|Tense=Pres|VerbForm=Fin, Clitic=Yes|Number=Plur|POS=PRON|Person=1|PronType=Prs, Mood=Ind|Number=Plur|POS=VERB|Person=1|Tense=Pres|VerbForm=Fin, Mood=Ind|Number=Plur|POS=VERB|Person=3|Tense=Past|VerbForm=Fin, Mood=Ind|Number=Sing|POS=VERB|Person=1|Tense=Past|VerbForm=Fin, Number=Sing|POS=DET|PronType=Dem, Mood=Ind|Number=Sing|POS=AUX|Person=3|Tense=Past|VerbForm=Fin, Mood=Ind|Number=Sing|POS=VERB|Person=3|Tense=Fut|VerbForm=Fin, Gender=Fem|NumType=Ord|Number=Plur|POS=ADJ, Mood=Sub|Number=Sing|POS=VERB|Person=3|Tense=Imp|VerbForm=Fin, Mood=Ind|Number=Plur|POS=AUX|Person=3|Tense=Past|VerbForm=Fin, Number=Sing|POS=DET|PronType=Int, POS=PRON|PronType=Int, Clitic=Yes|Gender=Masc|Number=Sing|POS=PRON|Person=3|PronType=Prs, Mood=Ind|Number=Plur|POS=VERB|Person=1|Tense=Past|VerbForm=Fin, Mood=Sub|Number=Plur|POS=VERB|Person=3|Tense=Pres|VerbForm=Fin, Gender=Fem|Number=Plur|POS=PRON|PronType=Ind, Number=Sing|POS=ADP, Mood=Ind|Number=Sing|POS=AUX|Person=3|Tense=Fut|VerbForm=Fin, Foreign=Yes|POS=X, Mood=Sub|Number=Sing|POS=VERB|Person=3|Tense=Pres|VerbForm=Fin, Clitic=Yes|POS=VERB|Person=3|PronType=Prs|VerbForm=Inf, Clitic=Yes|POS=AUX|Person=3|PronType=Prs|VerbForm=Inf, Clitic=Yes|Gender=Masc|Mood=Imp|Number=Plur,Sing|POS=VERB|Person=1,3|PronType=Prs|Tense=Pres|VerbForm=Fin, Mood=Sub|Number=Sing|POS=AUX|Person=3|Tense=Imp|VerbForm=Fin, Gender=Fem|Number=Sing|POS=PRON|Poss=Yes|PronType=Prs, Number=Plur|POS=VERB|Tense=Pres|VerbForm=Part, POS=INTJ|Polarity=Neg, Mood=Ind|Number=Sing|POS=AUX|Person=1|Tense=Imp|VerbForm=Fin, Number=Plur|POS=PRON|PronType=Rel, Mood=Sub|Number=Plur|POS=VERB|Person=3|Tense=Imp|VerbForm=Fin, Gender=Fem|Number=Sing|POS=DET|PronType=Ind, Gender=Fem|Number=Sing|POS=PRON|PronType=Dem, Mood=Sub|Number=Plur|POS=AUX|Person=3|Tense=Pres|VerbForm=Fin, Gender=Fem|Number=Plur|POS=DET|PronType=Dem, Gender=Masc|Number=Plur|POS=PRON|PronType=Rel, Clitic=Yes|Number=Plur|POS=VERB|Person=1|PronType=Prs|VerbForm=Ger, POS=INTJ|Polarity=Pos, Gender=Fem|Number=Sing|POS=PRON|Person=3|PronType=Prs, Gender=Fem|Number=Sing|POS=DET|PronType=Int, POS=DET|PronType=Int, Gender=Masc|NumType=Ord|Number=Plur|POS=ADJ, Gender=Fem|Number=Plur|POS=DET|PronType=Int, Mood=Cnd|Number=Plur|POS=AUX|Person=1|Tense=Pres|VerbForm=Fin, POS=PRON|Person=3|PronType=Prs, Degree=Abs|Gender=Masc|Number=Plur|POS=ADJ, Gender=Masc|Number=Sing|POS=DET|PronType=Ind, Number=Sing|POS=PRON|Person=1|PronType=Prs, Gender=Masc|Number=Plur|POS=PRON|PronType=Dem, Clitic=Yes|Number=Sing|POS=PRON|Person=3|PronType=Prs, Clitic=Yes|Gender=Fem|POS=VERB|Person=3|PronType=Prs|VerbForm=Inf, Clitic=Yes|Gender=Fem|POS=PRON|Person=3|PronType=Prs, Mood=Ind|Number=Plur|POS=VERB|Person=1|Tense=Fut|VerbForm=Fin, Degree=Abs|Gender=Fem|Number=Sing|POS=ADJ, Gender=Masc|Number=Sing|POS=DET|PronType=Tot, Clitic=Yes|POS=AUX|PronType=Prs|VerbForm=Inf, Gender=Fem|Number=Plur|POS=DET|PronType=Tot, Mood=Ind|Number=Sing|POS=VERB|Person=1|Tense=Pres|VerbForm=Fin, Gender=Fem|Number=Plur|POS=PRON|PronType=Dem, Degree=Abs|Gender=Masc|Number=Sing|POS=ADJ, NumType=Ord|POS=ADJ, POS=DET|PronType=Rel, Gender=Masc|Number=Sing|POS=PRON|PronType=Rel, Gender=Masc|Number=Plur|POS=PRON|Poss=Yes|PronType=Prs, Mood=Ind|Number=Plur|POS=VERB|Person=2|Tense=Pres|VerbForm=Fin, Mood=Imp|Number=Plur|POS=VERB|Person=2|Tense=Pres|VerbForm=Fin, Clitic=Yes|Gender=Fem|Number=Sing|POS=PRON|Person=3|PronType=Prs, Number=Sing|POS=PRON|Person=2|PronType=Prs, Mood=Cnd|Number=Sing|POS=VERB|Person=3|Tense=Pres|VerbForm=Fin, Mood=Ind|Number=Sing|POS=VERB|Person=2|Tense=Pres|VerbForm=Fin, Mood=Ind|Number=Sing|POS=VERB|Person=1|Tense=Fut|VerbForm=Fin, Mood=Ind|Number=Sing|POS=AUX|Person=2|Tense=Pres|VerbForm=Fin, Mood=Ind|Number=Plur|POS=AUX|Person=2|Tense=Pres|VerbForm=Fin, Clitic=Yes|Number=Plur|POS=PRON|Person=2|PronType=Prs, Clitic=Yes|Number=Sing|POS=VERB|Person=1|PronType=Prs|VerbForm=Inf, Mood=Imp|Number=Sing|POS=VERB|Person=2|Tense=Pres|VerbForm=Fin, Mood=Ind|Number=Sing|POS=AUX|Person=1|Tense=Fut|VerbForm=Fin, Mood=Ind|Number=Plur|POS=VERB|Person=2|Tense=Fut|VerbForm=Fin, Mood=Ind|Number=Plur|POS=VERB|Person=3|Tense=Fut|VerbForm=Fin, Mood=Cnd|Number=Sing|POS=VERB|Person=1|Tense=Pres|VerbForm=Fin, Clitic=Yes|POS=VERB|PronType=Prs|VerbForm=Ger, Mood=Ind|Number=Sing|POS=VERB|Person=1|Tense=Imp|VerbForm=Fin, Mood=Ind|Number=Plur|POS=AUX|Person=1|Tense=Imp|VerbForm=Fin, Mood=Cnd|Number=Sing|POS=AUX|Person=1|Tense=Pres|VerbForm=Fin, Clitic=Yes|Gender=Masc|Number=Plur|POS=VERB|Person=3|PronType=Prs|Tense=Past|VerbForm=Part, Number=Sing|POS=PRON|PronType=Int, Mood=Ind|Number=Sing|POS=AUX|Person=2|Tense=Imp|VerbForm=Fin, Mood=Ind|Number=Plur|POS=VERB|Person=1|Tense=Imp|VerbForm=Fin, Number=Plur|POS=PRON|Person=2|PronType=Prs, Clitic=Yes|Number=Plur|POS=VERB|Person=2|PronType=Prs|VerbForm=Inf, Clitic=Yes|Number=Plur|POS=VERB|Person=1|PronType=Prs|VerbForm=Inf, Mood=Sub|Number=Plur|POS=AUX|Person=3|Tense=Imp|VerbForm=Fin, Mood=Ind|Number=Plur|POS=AUX|Person=2|Tense=Fut|VerbForm=Fin, Mood=Ind|Number=Plur|POS=AUX|Person=3|Tense=Fut|VerbForm=Fin, Definite=Def|POS=DET|PronType=Art, Mood=Sub|Number=Sing|POS=VERB|Person=2|Tense=Pres|VerbForm=Fin, POS=SYM, Clitic=Yes|Mood=Imp|Number=Sing|POS=VERB|Person=2|PronType=Prs|Tense=Pres|VerbForm=Fin, Clitic=Yes|Gender=Masc|Mood=Imp|Number=Sing|POS=VERB|Person=2,3|PronType=Prs|Tense=Pres|VerbForm=Fin, Mood=Ind|Number=Sing|POS=VERB|Person=2|Tense=Fut|VerbForm=Fin, Clitic=Yes|Gender=Fem|POS=VERB|Person=3|PronType=Prs|VerbForm=Ger, Degree=Abs|Gender=Fem|Number=Plur|POS=ADJ, Number=Sing|POS=PRON|PronType=Dem, POS=AUX|VerbForm=Ger, Gender=Masc|Number=Sing|POS=PRON|Poss=Yes|PronType=Prs, Clitic=Yes|Gender=Masc|Number=Sing|POS=PRON|Person=3|PronType=Prs|VerbForm=Inf, POS=PRON|PronType=Ind, Clitic=Yes|Mood=Imp|Number=Plur|POS=VERB|Person=1|PronType=Prs|Tense=Pres|VerbForm=Fin, POS=X, Gender=Masc|POS=ADJ, Clitic=Yes|Gender=Fem|Number=Sing|POS=AUX|Person=3|PronType=Prs|VerbForm=Inf, Gender=Fem|Number=Plur|POS=PRON|Person=3|PronType=Prs, Gender=Masc|Number=Plur|POS=PRON|Person=3|PronType=Prs, Mood=Cnd|Number=Sing|POS=VERB|Person=2|Tense=Pres|VerbForm=Fin, Clitic=Yes|Number=Sing|POS=VERB|Person=2|PronType=Prs|VerbForm=Inf, Clitic=Yes|Gender=Fem|Number=Sing|POS=VERB|Person=3|PronType=Prs|Tense=Past|VerbForm=Part, Mood=Sub|Number=Plur|POS=VERB|Person=2|Tense=Imp|VerbForm=Fin, POS=PART, Number=Sing|POS=VERB|Tense=Pres|VerbForm=Part, NumType=Ord|Number=Sing|POS=ADJ, Number=Plur|POS=DET|PronType=Int, Clitic=Yes|Mood=Sub|Number=Plur|POS=VERB|Person=1|PronType=Prs|Tense=Pres|VerbForm=Fin, Number=Plur|POS=DET|PronType=Rel, Mood=Sub|Number=Sing|POS=VERB|Person=1|Tense=Imp|VerbForm=Fin, Clitic=Yes|Gender=Fem|Number=Sing|POS=VERB|Person=3|PronType=Prs|VerbForm=Ger, Clitic=Yes|Gender=Masc|Number=Sing|POS=VERB|Person=3|PronType=Prs|VerbForm=Ger, Clitic=Yes|Number=Sing|POS=VERB|Person=1|PronType=Prs|VerbForm=Ger, Clitic=Yes|Number=Sing|POS=AUX|Person=1|PronType=Prs|VerbForm=Ger, Clitic=Yes|Gender=Masc|Number=Plur|POS=AUX|Person=3|PronType=Prs|VerbForm=Inf, Clitic=Yes|Mood=Imp|Number=Plur,Sing|POS=VERB|Person=1,2|PronType=Prs|Tense=Pres|VerbForm=Fin, Mood=Imp|Number=Plur|POS=AUX|Person=2|Tense=Pres|VerbForm=Fin, NumType=Range|POS=NUM, Number=Plur|POS=PRON|PronType=Dem, POS=VERB|Tense=Past|VerbForm=Part, Clitic=Yes|POS=ADV|PronType=Prs, Clitic=Yes|Mood=Ind|Number=Plur|POS=VERB|Person=1|PronType=Prs|Tense=Pres|VerbForm=Fin, Gender=Masc|POS=PRON|PronType=Rel, Clitic=Yes|Gender=Masc|Mood=Imp|Number=Plur,Sing|POS=VERB|Person=2,3|PronType=Prs|Tense=Pres|VerbForm=Fin, Clitic=Yes|Number=Sing|POS=AUX|Person=2|PronType=Prs|VerbForm=Inf, Clitic=Yes|Number=Sing|POS=VERB|Person=2|PronType=Prs|VerbForm=Ger, Mood=Imp|Number=Sing|POS=AUX|Person=2|Tense=Pres|VerbForm=Fin, Clitic=Yes|Gender=Fem|Mood=Imp|Number=Sing|POS=VERB|Person=2,3|PronType=Prs|Tense=Pres|VerbForm=Fin, Mood=Sub|Number=Plur|POS=AUX|Person=1|Tense=Imp|VerbForm=Fin, Mood=Ind|Number=Sing|POS=AUX|Person=1|Tense=Past|VerbForm=Fin, Clitic=Yes|Gender=Masc|Number=Sing|POS=VERB|Person=3|PronType=Prs|Tense=Past|VerbForm=Part, Clitic=Yes|Gender=Masc|Number=Plur,Sing|POS=VERB|Person=3|PronType=Prs|VerbForm=Inf, Definite=Ind|POS=DET|PronType=Art, Clitic=Yes|Gender=Fem,Masc|Number=Sing|POS=VERB|Person=3|PronType=Prs|Tense=Past|VerbForm=Part, Definite=Ind|Gender=Masc|Number=Plur|POS=DET|PronType=Art, Definite=Def|Number=Plur|POS=ADP|PronType=Art, Clitic=Yes|Gender=Fem|Number=Plur|POS=VERB|Person=3|PronType=Prs|VerbForm=Inf, POS=DET|PronType=Ind, Number=Plur|POS=DET|PronType=Dem, Clitic=Yes|Gender=Fem|Number=Plur|POS=PRON|Person=3|PronType=Prs, Number=Plur|POS=DET|PronType=Tot, Clitic=Yes|POS=AUX|Person=3|PronType=Prs|VerbForm=Ger, Number=Plur|POS=PRON|PronType=Ind, Clitic=Yes|Gender=Fem,Masc|Number=Plur,Sing|POS=VERB|Person=3|PronType=Prs|Tense=Past|VerbForm=Part, Clitic=Yes|Number=Plur|POS=VERB|PronType=Prs|VerbForm=Inf, Number=Plur|POS=PRON|Poss=Yes|PronType=Prs, Number=Sing|POS=PRON|Poss=Yes|PronType=Prs, Number=Plur|POS=ADP, Clitic=Yes|Gender=Masc|Number=Sing|POS=ADV|Person=3|PronType=Prs, Clitic=Yes|Mood=Imp|Number=Plur|POS=VERB|Person=1,2|PronType=Prs|Tense=Pres|VerbForm=Fin, Clitic=Yes|Gender=Fem|Number=Plur|POS=VERB|Person=3|PronType=Prs|Tense=Past|VerbForm=Part, Mood=Sub|Number=Sing|POS=AUX|Person=1|Tense=Imp|VerbForm=Fin, Mood=Cnd|Number=Plur|POS=AUX|Person=2|Tense=Pres|VerbForm=Fin, Mood=Cnd|Number=Plur|POS=VERB|Person=2|Tense=Pres|VerbForm=Fin, Clitic=Yes|Gender=Fem|Number=Plur|POS=ADV|Person=3|PronType=Prs, POS=DET|PronType=Tot, POS=PRON|PronType=Dem, Clitic=Yes|Gender=Masc|Mood=Imp|Number=Plur|POS=VERB|Person=2,3|PronType=Prs|Tense=Pres|VerbForm=Fin, Definite=Ind|Number=Sing|POS=DET|PronType=Art, NumType=Ord|POS=NUM, Clitic=Yes|Gender=Fem|Number=Plur|POS=VERB|Person=3|PronType=Prs|VerbForm=Ger, Gender=Masc|POS=DET|PronType=Dem, Clitic=Yes|Gender=Masc|Number=Plur,Sing|POS=VERB|Person=3|PronType=Prs|Tense=Past|VerbForm=Part, Gender=Masc|Number=Sing|POS=NOUN|Tense=Past|VerbForm=Part, Gender=Masc|Number=Plur|POS=DET|PronType=Int, Gender=Masc|Number=Plur|POS=PRON|PronType=Int, Gender=Fem|Number=Plur|POS=PRON|PronType=Int, Mood=Imp|Number=Sing|POS=VERB|Person=3|Tense=Pres|VerbForm=Fin, Gender=Masc|Number=Sing|POS=DET|PronType=Int, Gender=Fem|Number=Sing|POS=PRON|PronType=Int, Number=Plur|POS=PRON|PronType=Int, Mood=Cnd|Number=Sing|POS=AUX|Person=2|Tense=Pres|VerbForm=Fin, Gender=Masc|Number=Sing|POS=PRON|PronType=Int, Clitic=Yes|Number=Plur|POS=PRON|PronType=Prs, Foreign=Yes|Number=Sing|POS=X, Mood=Ind|Number=Plur|POS=AUX|Person=1|Tense=Fut|VerbForm=Fin, POS=PRON|PronType=Prs, Mood=Sub|Number=Plur|POS=AUX|Person=2|Tense=Pres|VerbForm=Fin, Clitic=Yes|Mood=Ind|Number=Sing|POS=VERB|Person=3|PronType=Prs|Tense=Pres|VerbForm=Fin, Mood=Ind|POS=VERB|Person=3|Tense=Pres|VerbForm=Fin, Mood=Cnd|Number=Plur|POS=VERB|Person=1|Tense=Pres|VerbForm=Fin, Mood=Sub|Number=Plur|POS=VERB|Person=1|Tense=Pres|VerbForm=Fin, Mood=Ind|Number=Plur|POS=AUX|Person=2|Tense=Imp|VerbForm=Fin, POS=SCONJ|PronType=Rel, Mood=Sub|Number=Plur|POS=VERB|Person=1|Tense=Imp|VerbForm=Fin, POS=PRON|Person=3|PronType=Rel, Clitic=Yes|Number=Plur|POS=VERB|Person=2|PronType=Prs|VerbForm=Ger, Mood=Sub|Number=Sing|POS=VERB|Person=3|VerbForm=Fin, Clitic=Yes|Mood=Ind|Number=Sing|POS=VERB|Person=1,3|PronType=Prs|Tense=Past|VerbForm=Fin, Mood=Ind|POS=VERB|Tense=Pres|VerbForm=Fin, Degree=Cmp|POS=ADJ, Mood=Ind|Number=Sing|POS=AUX|Person=2|Tense=Fut|VerbForm=Fin, Definite=Def|Number=Plur|POS=DET|PronType=Art, Number=Sing|POS=DET|Poss=Yes|PronType=Prs, Gender=Masc|Number=Sing|POS=ADP, Gender=Fem|POS=ADJ, Mood=Sub|Number=Plur|POS=VERB|Person=2|Tense=Pres|VerbForm=Fin, Clitic=Yes|Gender=Fem|Mood=Imp|Number=Plur|POS=VERB|Person=2,3|PronType=Prs|Tense=Pres|VerbForm=Fin, Clitic=Yes|Number=Plur|POS=PRON|Person=3|PronType=Prs, Gender=Masc|POS=DET|Poss=Yes|PronType=Prs, Gender=Fem|Number=Plur|POS=PROPN, Definite=Ind|Gender=Fem|Number=Plur|POS=DET|PronType=Art, Number=Sing|POS=DET|PronType=Art, Gender=Fem|Number=Sing|POS=ADJ|Poss=Yes|PronType=Prs, Foreign=Yes|POS=NOUN, Clitic=Yes|Gender=Fem|Mood=Imp|Number=Plur|POS=VERB|Person=1,3|PronType=Prs|Tense=Pres|VerbForm=Fin, Clitic=Yes|Gender=Masc|Mood=Imp|Number=Plur|POS=VERB|Person=1,3|PronType=Prs|Tense=Pres|VerbForm=Fin, Gender=Masc|Number=Plur|POS=DET, Clitic=Yes|Gender=Fem|Mood=Imp|Number=Plur,Sing|POS=VERB|Person=1,3|PronType=Prs|Tense=Pres|VerbForm=Fin, Mood=Sub|Number=Plur|POS=AUX|Person=1|Tense=Pres|VerbForm=Fin, Gender=Fem|Number=Plur|POS=VERB|Tense=Past|VerbForm=Fin, Gender=Fem|Number=Plur|POS=DET, Number=Sing|POS=X, Foreign=Yes|Gender=Masc|POS=X, Clitic=Yes|Gender=Fem|Number=Plur|POS=PRON|PronType=Prs, Clitic=Yes|Gender=Masc|Number=Sing|POS=PRON|PronType=Prs, Clitic=Yes|Definite=Def|Gender=Fem|Number=Plur|POS=PRON|PronType=Art, Gender=Masc|Number=Plur|POS=VERB|Tense=Past|VerbForm=Fin, Definite=Def|Gender=Fem|POS=DET, Definite=Def|POS=DET, Foreign=Yes|POS=PROPN, NumType=Card|POS=PROPN, Gender=Fem|Number=Sing|POS=DET, Degree=Abs|Gender=Masc|Number=Sing|POS=ADV, Gender=Masc|Number=Plur|POS=NOUN|Tense=Past|VerbForm=Part, Mood=Imp|Number=Plur|POS=VERB|Person=2, Clitic=Yes|Number=Plur|POS=AUX|Person=1|PronType=Prs|VerbForm=Inf, Gender=Masc|Number=Sing|POS=DET, Number=Sing|POS=DET, Gender=Masc|Number=Sing|POS=PRON, POS=DET

|#