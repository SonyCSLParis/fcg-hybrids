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

(export '(represent-constituent-structure))

;; ICA = Immediate Constituent Analysis
;;
;; Contents:
;; 1- Helper functions
;; -------------------------------------------------------------------------
(defun convert-ica-string-to-ica-list (string)
  "Converts a string representing a constituent analysis into a list representation."
  (loop for pair in '(("." "\\.")
                      ("," "\\,")
                      ("''" "PARENTH")
                      ("``" "PARENTH")
                      ("\"" "PARENTH"))
        do (setf string (string-replace string (first pair) (second pair))))
  (read-from-string string))

;; 2- Representing Constituent Structures
;; -------------------------------------------------------------------------

(defgeneric represent-constituent-structure (analysis transient-structure key cxn-inventory &optional language)
  (:documentation "Given a constituent structure analysis, expand the transient structure with units for phrases and constituency relations."))

;; For an example, check /languages/English/structures.lisp

;; Helper functions for dealing with bracketed representation of constituent structures
;; ------------------------------------------------------------------------------------
(defun terminal-node-p (node)
  "Is the node a terminal node in the constituent structure?"
  (if (loop for x in (rest node)
            when (listp x)
            return t)
    nil
    t))
;; (terminal-node-p '(nnp luc steels))
;; (terminal-node-p '(np (det the) (nn book)))

(defun has-constituents-p (unit)
  "Check whether a unit has the constituents feature."
  (unit-feature-value unit 'constituents))
;; (has-constituents-p '(np (constituents (det n))))
;; (has-constituents-p '(n (lex-id test)))

(defun make-unit-id (x)
  "Make a new unit name."
  (make-id (format nil "~a-unit" x)))
;; (make-unit-id 's)
