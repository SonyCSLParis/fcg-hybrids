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

(in-package #:asdf)

(defsystem :fcg-hybrids
  :author "Remi van Trijp <remi.vantrijp@sony.com>"
  :version "1.0"
  :license "Apache 2.0-License"
  :depends-on (:fcg
               :xmls
               :nlp-tools
               :plot-raw-data
               :category-hierarchies)
  :serial t
  :components ((:file "data-structures-and-utils")
               (:file "universal-dependencies")
               (:module "structures" ;; Other possible terminology: layers, dimensions, perspectives
                :serial t
                :components (;; Dependency and functional structures (head-dependent relations):
                             (:file "dependency-structures")
                             (:file "functional-structures")
                             ;; Constituent structures (syntactic hierarchy):
                             (:file "constituent-structures")))
               ;; Functions for manipulating a preprocessing result in lisp:
               (:file "preprocessing")
               ;; Some examples of de-render methods using preprocessing tools with preprocessing:
               (:file "examples")
               (:module "english-hybrids"
                :serial t
                :components ((:file "clear-dependency-tags")
                             (:file "categories")
                             (:file "translate-dependency-tree")
                             (:file "render")
                             (:file "de-render"))))
  :description "Multilingual library for combining symbolic FCG-grammars with NLP tools. Currently, this is achieved by translating
                output from the NLP-tools into transient structures, onto which FCG grammars can operate. In other words, this is
                useful for integrating FCG as a step in an NLP-pipeline.")