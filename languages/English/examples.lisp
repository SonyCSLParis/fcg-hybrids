;; Copyright Sony Computer Science Laboratories Paris
;; Author: Remi van Trijp (http://www.remivantrijp.eu)

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

;; We start with the base model.

(progn
  (setf *fcg-constructions* (make-english-base-model-cxns))
  (activate-monitor trace-fcg))

;; Now you can start writing/learning/generating constructions:
(def-fcg-cxn Ambassador-cxn
             ((?ambassador-unit
               (args (?category ?context))
               (syn-cat (agreement (- - + -))
                        (lex-class noun)))
              <-
              (?ambassador-unit
               (HASH meaning ((bind ambassador ?category)))
               --
               (HASH form ((string ?ambassador-unit "ambassador"))))))

;; Change configurations to dependency parser:
(set-configuration *fcg-constructions* :de-render-mode :spacy-dependency-parser)
(comprehend "Angry with Joe Biden, Emmanuel Macron called the ambassador.")

;; Change configurations to dependency parser with additional preprocessing:
(set-configuration *fcg-constructions* :de-render-mode :spacy-dependency-parser-with-additional-preprocessing)

;; But which configurations are out there?
(configure-grammar *fcg-constructions*)

;; I want to integrate my constructions with web resources such as wikipedia:
(set-configuration (visualization-configuration *fcg-constructions*) :show-wiki-links-in-predicate-networks t)

(progn
  (def-fcg-cxn Emmanuel_Macron-cxn
               ((?macron-unit
                 (referent ?macron)
                 (syn-cat (agreement (- - + -))
                          (lex-class proper-noun)))
                <-
                (?macron-unit
                 (HASH meaning ((bind emmanuel_macron ?macron)))
                 --
                 (HASH form ((string ?macron-unit "Emmanuel Macron"))))))

  (def-fcg-cxn called-cxn
               ((?called-unit
                 (referent ?ev)
                 (sem-valence (actor ?x)
                              (undergoer ?y)))
                <-
                (?called-unit
                 (HASH meaning ((sem-frame ?ev call)
                                (arg0 ?ev ?x)
                                (arg1 ?ev ?y)))
                 --
                 (HASH form ((string ?called-unit "called"))))))

  (def-fcg-cxn the-cxn
               (<-
                (?the-unit
                 (HASH meaning ((access-referent-by-category ?ref ?category)))
                 --
                 (HASH form ((string ?the-unit "the")))
                 (dependency (head ?head)
                             (edge "det")))
                (?head
                 (referent ?ref)
                 (args (?category ?context))
                 --
                 (dependents (?the)))))

  (def-fcg-cxn active-transitive-cxn
               (<-
                (?head-unit
                 (sem-valence (actor ?x)
                              (undergoer ?y))
                 --
                 (dependents (?subject ?object)))
                (?subject
                 (referent ?x)
                 --
                 (dependency (head ?head-unit)
                             (edge "nsubj")))
                (?object
                 (referent ?y)
                 --
                 (dependency (head ?head-unit)
                             (edge "dobj"))))))

(comprehend "Angry with Joe Biden, Emmanuel Macron called the ambassador.")
