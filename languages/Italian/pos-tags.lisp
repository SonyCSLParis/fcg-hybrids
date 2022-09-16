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
                      (push (list (read-from-string feature) 
                                  (read-from-string (second fv-pair))) agreement))
                     ((string= feature "prontype")
                      nil)
                   (t
                    (push (list (read-from-string feature) 
                                (read-from-string (second fv-pair))) syn-cat))))
      `(,fcg-category
        ,@(if agreement `((agreement ,agreement)))
        (syn-cat ,(cons `(lex-class ,fcg-category) syn-cat))))))