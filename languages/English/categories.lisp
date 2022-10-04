;; Copyright 2021 Sony Computer Science Laboratories Paris
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

(defvar *english-fcg-categories* nil "FCG-categories for English grammars.")

(setf *english-fcg-categories*
      '(;; CLAUSES
        ;; ---------------------------------------------------------------
        (clause ()
                (referent ?ref))
        (simple-declarative-clause (clause)
                                   (syn-cat ((clause-type simple-declarative)
                                             (is-matrix-clause -))))
        (main-simple-declarative-clause (simple-declarative-clause)
                                        (syn-cat ((is-matrix-clause +))))
        (subordinating-conjunction-clause (clause)
                                          (syn-cat ((clause-type subordinate)
                                                    (is-matrix-clause -))))
        (WH-question (clause)
                     (syn-cat ((clause-type WH-interrogative)
                               (is-matrix-clause -))))
        (main-wh-question (wh-question)
                          (syn-cat ((is-matrix-clause +))))
        (subject-aux-inversion-question (clause)
                                        (syn-cat ((clause-type inversion-interrogative)
                                                  (is-matrix-clause -))))
        (main-subject-aux-inversion-question (subject-aux-inversion-question)
                                             (syn-cat ((is-matrix-clause +))))
        (sbarq-subconstituent-without-wh-phrase (clause)
                                                (syn-cat ((clause-type interrogative-without-wh-constituent))))
        
        ;; Phrases
        ;; ---------------------------------------------------------------
        (phrase ()
                (referent ?ref)
                (parent ?parent)
                (syn-cat ((phrase-type ?phrase-type))))
        (noun-phrase (phrase)
                     (syn-cat ((phrase-type noun-phrase)
                               (agreement ?agreement))))
        (WH-noun-phrase (noun-phrase))
        (adjective-phrase (phrase)
                          (syn-cat ((phrase-type adjective-phrase))))
        (adverbial-phrase (phrase)
                          (syn-cat ((phrase-type adverbial-phrase))))
        (WH-adverbial-phrase (adverbial-phrase))
        (prepositional-phrase (phrase)
                              (syn-cat ((phrase-type prepositional-phrase))))
        (WH-prepositional-phrase (prepositional-phrase))
        (verb-phrase (phrase)
                     (syn-cat ((phrase-type verb-phrase)
                               (agreement ?agreement)
                               (TAM ((tense ?tense)
                                     (aspect ((perfect ?perfect)
                                              (progressive ?progressive)))
                                     (modality ?modality))))))

        ;; Lexical Categories
        ;; ---------------------------------------------------------------
        ;; Verbal:
        (verb ()
              (referent ?ev-ref)
              (syn-cat ((lex-class verb)
                        (agreement ?agreement)
                        (finite ?finite)
                        (verb-form ?verb-form))))
        (aux (verb)
             (syn-cat ((lex-class aux)
                       (is-passive-marker -))))
        (modal-auxiliary (aux))
        (auxpass (verb)
                 (syn-cat ((lex-class aux)
                           (is-passive-marker +))))
        (verb-base-form (verb))
        (verb-past (verb)
                   (syn-cat ((verb-form ed-form))))
        (verb-past-participle (verb-past))
        (verb-ing-form (verb)
                       (syn-cat ((verb-form ing-form))))
        (verb-3sg (verb))
        ;; Nominal:
        (noun ()
              (referent ?ref)
              (args (?output ?input))
              (syn-cat ((agreement (- - ?sg ?pl))
                        (lex-class noun))))
        (proper-noun (noun)
                     (syn-cat ((lex-class proper-noun))))
        (proper-singular-noun (proper-noun))
        (proper-plural-noun (proper-noun))
        (plural-noun (noun))
        ;; Pronouns:
        (pronoun (noun)
                 (syn-cat ((lex-class pronoun))))
        (personal-pronoun (pronoun)
                          (syn-cat ((lex-class personal-pronoun))))
        (wh-personal-pronoun (personal-pronoun))
        ;; Adjectival:
        (adjective ()
                   (syn-cat ((lex-class adjective)))
                   (referent ?ref)
                   (args (?output ?input)))
        (comparative-adjective (adjective))
        (superlative-adjective (adjective))
        ;; Preposition:
        (preposition ()
                     (syn-cat ((lex-class preposition))))
        ;; Adverbial:
        (adverb ()
                (syn-cat ((lex-class adverb))))
        (comparative-adverb (adverb))
        (superlative-adverb (adverb))
        (wh-adverb (adverb))
        ;; Conjunctions:
        (conjunction ()
                     (syn-cat ((lex-class conjunction))))
        (cconjunction (conjunction) ;; and, or, ...
                      (syn-cat ((conjunction-type coordination))))
        (sconjunction (conjunction) ;; if, while, ...
                      (syn-cat ((conjunction-type subordination))))
        ;; Others:
        (possessive-ending ()
                           (syn-cat ((lex-class genitive-marker))))
        (particle ()
                  (syn-cat ((lex-class particle))))
        (affix ()
               (syn-cat ((lex-class affix))))
        (punctuation ()
                     (syn-cat ((orthography punctuation))))
        (infinitival-to ()
                        (syn-cat ((lex-class infinitival-to))))
        (interjection ()
                      (syn-cat ((lex-class interjection))))
        ;; Determiners:
        (existential-there ()
                           (referent ?ref)
                           (syn-cat ((lex-class existential-there)
                                     (agreement (- - ?sg ?pl)))))
        (determiner ()
                    (referent ?ref)
                    (args (?ref ?input))
                    (syn-cat ((lex-class determiner)
                              (agreement ?agr))))
        (wh-determiner ()
                       (syn-cat ((determiner-type wh-determiner))))
        (possessive-pronoun (determiner)
                            (syn-cat ((determiner-type possessive-pronoun))))
        (wh-possessive-pronoun (possessive-pronoun))
        (article (determiner)
                 (syn-cat ((determiner-type article))))
        (indefinite-article (article)
                            (syn-cat ((definite -)
                                      (agreement (- - + -)))))
        (definite-article (article)
                          (syn-cat ((definite +)
                                    (agreement (- - ?sg ?pl)))))))
