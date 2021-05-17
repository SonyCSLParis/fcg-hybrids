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

(export '(represent-functional-structure))

(defgeneric represent-functional-structure (dependency-tree transient-structure key &optional language)
  (:documentation "Generic function for customizing how the functional structure of an utterance is represented in different languages."))

;; Usage:
;; ------
;; > The argument "key" can be used for customizing different methods for the same language.
;; > The argument language should be used for customizing based on a languge.
;;
;; Example:
;; (represent-functional-structure dependency-tree transient-structure t :english)
;;
;;
;; About functional structure:
;; ---------------------------
;; The functional structure of constructions consists in identifying grammatical functions such as
;; SUBJECT, DIRECT OBJECT, and so on. These relations can either be identified through functional
;; constructions (e.g. the Subject-Predicate construction), through preprocessing (dependency parsing),
;; or a combination of both. Below are a couple of helper functions to identify the most important
;; relations.




