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

;; The FCG-hybrids package aims to be compatible with the developments of
;; the Universal Dependencies Project (https://universaldependencies.org/).
;; For documentation of how the UD project's annotation schemes:
;; https://universaldependencies.org/guidelines.html

;; ---------------------------------------------------------------------------
;; Syntactic relations
;; (based on a revised version of this article:
;;  https://nlp.stanford.edu/pubs/USD_LREC14_paper_camera_ready.pdf)
;; ---------------------------------------------------------------------------

(defvar *universal-dependencies* nil "A list of universal dependencies and related information.")

(setf *universal-dependencies*
      '(;; Subject-related:
        ("nsubj" core-argument nominal subject "Nominal subject, e.g. 'The CAR is red.'")
        ("csubj" core-argument clausal subject "Clausal subject, e.g. 'What she SAID is interesting.'")
        ("nsubj:pass" core-argument nominal subject passive "Passive nominal subject, e.g. The THIEF was caught by the police.")
        ("csubj:pass" core-argument clausal subject passive "Clausal passive subject.")
        ;; Object-related:
        ("obj" core-argument nominal object direct-object "Object, e.g. 'I read a BOOK.'")
        ("iobj" core-argument nominal object indirect-object "Indirect object, e.g. 'She gave ME a book.'")
        ("ccomp" core-argument clausal complement clausal-complement "Clausal complement, e.g. 'He said that you LIKE to swim.'")
        ("xcomp" core-argument clausal complement open-clausal-complement "Open clausal complement, e.g. 'I started to WORK here yesterday.'")
        ;; Non-core dependents:
        ("obl" non-core-dependent nominal oblique "Oblique, e.g. 'She gave a book to her DAUGHTER.'")
        ("vocative" non-core-dependent nominal vocative "Vocative, e.g. 'GUYS, take it easy!'")
        ("eplx" non-core-dependent nominal expletive "Expletive, e.g. 'THERE is a hidden object in this room.'")
        ("dislocated" non-core-dependent nominal dislocated "Disolacted element, e.g. in French: 'Il faut pas la manger, la PLASTICINE.'")
        ("advcl" non-core-dependent clausal adverbial-clause "Adverbial clause modifier, e.g. 'The accident happened as night was FALLING.'")
        ;; Modifier words
        ("advmod" non-core-dependent lexical adverbial-modifier "Adverbial modifier, e.g. 'ABOUT 200 people came to the party.'")
        ("discourse" non-core-dependent discourse "Discourse element, e.g. the smiley in 'Iguazu is in Argentina :)'")
        ;; Function words
        ("aux" function-word lexical aux "Auxiliary, e.g. 'She HAS won an award.'")
        ("aux:pass" function-word lexical aux passive "Passive auxiliary, e.g. 'The thief WAS caught.'")
        ("cop" function-word lexical aux "Copula, e.g. 'Bill IS honest.'")
        ("mark" function-word lexical marker "Marker of a subordinate clause, e.g. 'He says THAT you like to swim.'")
        ;; Nominal dependents
        ("nmod" nominal-dependent nominal nominal-modifier "Nominal modifier, e.g. 'the CHAIR's office' or 'the office of the CHAIR'")
        ("appos" nominal-dependent nominal apposition "Apposition, e.g. 'Barack Obama, the former US PRESIDENT, gave a speech.'")
        ("nummod" nominal-dependent number numerical-modifier "Numerical modifier, e.g. 'I spent FOURTY euros on that.'")
        ("acl" nominal-dependent clausal adnominal-clause "Clausal modifier of noun, e.g. 'I have a dog NAMED Bingo.'")
        ("amod" nominal-dependent lexical adjectival-modifier "Adjectival modifier, e.g. 'It is a RED car.'")
        ("det" function-word lexical determiner "Determiner, e.g. 'THE car is red.'")
        ("clf" function-word lexical classifier "Classifier. Does not occur in the languages currently supported.")
        ("case" function-word lexical case-marker "Case marker, e.g. 'The office OF the chair.'")
        ;; Coordination:
        ("conj" conjunction lexical coordination "Conjunct, e.g. Bill is big and HONEST.")
        ("cc" coordinating-conjunction lexical coordination "Coordinating conjunction, e.g. 'Bill is big AND honest.'")
        ;; MWE (multiword expressions):
        ("fixed" mwe "Fixed multiword expression, e.g. 'He cried because OF you.'")
        ("flat" mwe "Flat multiword expression, e.g. 'New YORK' or 'President OBAMA'")
        ("compound" mwe lexical compound "Compound multiword expression, e.g. 'a phone BOOK'")
        ;; Loose
        ("list" loose lexical list "List of comparable items, e.g. 'Long lines, silly RULES, rude STAFF, Ok FOOD'")
        ("parataxis" loose parataxis "Parataxis, e.g. 'Let's face it, we're ANNOYED.'")
        ;; Special
        ("orphan" special orphan "Cases of head ellipsis where simple promotion would lead to errors, e.g. 'Marie won gold and Peter BRONZE.'")
        ("goeswith" special goeswith "Parts of words that were wrongly edited, e.g. 'They come here with OUT legal permission.'")
        ("reparandum" special reparandum "For indicating a disfluency corrected by a speech repair, e.g. 'Go to the RIGH- to the left'")
        ;; Other
        ("punct" other punctuation "Punctiation, e.g. 'Go home !'")
        ("root" other root "The root of the utterance, e.g. 'I LIKE cherries.'")
        ("dep" other lexical unspecified-dependency "An unspecified dependency, e.g. 'My dad doesn't really not that GOOD.'")))

(defun get-dependency-spec (relation &optional (dependency-specs *universal-dependencies*))
  "Retrieve the information about a dependency relation."
  (assoc relation dependency-specs :test #'string=))
;; (get-dependency-spec "nsubj")

(defun describe-dependency-relation (relation &optional (dependency-specs *universal-dependencies*))
  "If a description is available for the dependency relation, return it."
  (let* ((dependency-spec (get-dependency-spec relation dependency-specs))
         (description (last-elt dependency-spec)))
    (when (stringp description)
      description)))
;; (describe-dependency-relation "nsubj")
;; (describe-dependency-relation "nsubj:pass")
;; (describe-dependency-relation "case")
