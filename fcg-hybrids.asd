;; Copyright 2019 Sony Computer Science Laboratories Paris
;;                Remi van Trijp (http://www.remivantrijp.eu)

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
(in-package #:asdf)

(defsystem :fcg-hybrids
  :depends-on (:fcg
               :xmls
               :nlp-tools
               :plot-raw-data
               :type-hierarchies)
  :serial t
  :components ((:file "data-structures-and-utils")
               (:file "preprocessing")
               (:file "syn-dependency")
               (:file "examples")
               (:module "english-hybrids"
                :serial t
                :components ((:file "clear-dependency-tags")
                             (:file "categories")
                             (:file "translate-dependency-tree")
                             (:file "render")
                             (:file "de-render")))))