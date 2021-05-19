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

(defvar *penn-treebank-syntactic-tags* nil "Syntactic tags used by the Penn Treebank")

(setf *penn-treebank-syntactic-tags*
      '((ADJP adjective-phrase)
        (ADVP adverbial-phrase)
        (NP noun-phrase)
        (PP prepositional-phrase)
        (S simple-declarative-clause)
        (SBAR subordinating-conjunction-clause)
        (SBARQ wh-question)
        (SINV subject-aux-inversion-question)
        (SQ sbarq-subconstituent-without-wh-phrase)
        (VP verb-phrase)
        (WHADVP WH-adverb-phrase)
        (WHNP WH-noun-phrase)
        (WHPP WH-prepositional-phrase)
        (X unknown-constituent)
        (* understood-subject-of-infinitive-or-imperative)
        (0 zero-that)))
;; Left out: T (trace) and NIL (pied-piping)

(defun fetch-syntactic-category (category &optional (syntactic-categories *penn-treebank-syntactic-tags*))
  "Find a conversion category, if not return itself."
  (or (second (assoc category syntactic-categories :test #'string= :key #'symbol-name))
      category))
;; (fetch-syntactic-category 'np *penn-treebank-syntactic-tags*)
